# ============================================================================
# Global Health Indicators Mapping Script
# Creates choropleth maps using rnaturalearth for better country coverage
# ============================================================================

# Load required libraries
library(WDI)
library(readxl)
library(dplyr)
library(countrycode)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(scales)
library(viridis)

# Create output directory
if (!dir.exists("maps_output")) {
  dir.create("maps_output")
}

# ============================================================================
# SETUP: Load world map from Natural Earth
# ============================================================================
cat("Loading world map from Natural Earth...\n")
world <- ne_countries(scale = "medium", returnclass = "sf")
cat(sprintf("Loaded map with %d countries/territories\n", nrow(world)))

# Initialize year variables
csec_latest_year <- NULL
stunting_latest_year <- NULL
preterm_latest_year <- NULL
wasting_latest_year <- NULL
overweight_latest_year <- NULL
tb_latest_year <- NULL
malaria_latest_year <- NULL

# Aggregate codes to exclude
aggregate_codes <- c("1A", "1W", "4E", "7E", "8S", "B8", "EU", "F1", "JG",
                     "OE", "S1", "S2", "S3", "S4", "T2", "T3", "T4", "T5",
                     "T6", "T7", "V1", "V2", "V3", "V4", "XC", "XD", "XE",
                     "XF", "XG", "XH", "XI", "XJ", "XK", "XL", "XM", "XN",
                     "XO", "XP", "XQ", "XT", "XU", "XY", "Z4", "Z7", "ZF",
                     "ZG", "ZH", "ZI", "ZJ", "ZQ", "ZT")

# ============================================================================
# STEP 1: World Bank Income Classifications
# ============================================================================
cat("\nDownloading World Bank income classifications...\n")
income_df <- tryCatch({
  download.file("https://ddh-openapi.worldbank.org/resources/DR0095333/download",
                destfile = "income_classifications.xlsx", mode = "wb", quiet = TRUE)
 
  income_data <- read_excel("income_classifications.xlsx", sheet = "List of economies")
 
  df <- income_data %>%
    slice(1:218) %>%
    select(Code, `Income group`) %>%
    filter(!is.na(`Income group`), `Income group` != "Aggregates") %>%
    rename(iso_a3 = Code, income_group = `Income group`)
 
  cat(sprintf(" Downloaded: %d economies\n", nrow(df)))
  cat(sprintf(" Matched to map: %d countries\n", sum(df$iso_a3 %in% world$iso_a3_eh)))
 
  df
}, error = function(e) {
  cat(" ERROR:", e$message, "\n")
  data.frame()
})

# ============================================================================
# STEP 2: C-section Data from UNICEF
# ============================================================================
cat("\nDownloading C-section prevalence data...\n")
csec_df <- tryCatch({
  csec_url <- "https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,MNCH,1.0/.MNCH_CSEC.......?format=csv"
  download.file(csec_url, destfile = "csec_data.csv", quiet = TRUE)
 
  csec_raw <- read.csv("csec_data.csv")
  cat(sprintf(" Downloaded: %d observations\n", nrow(csec_raw)))
 
  df <- csec_raw %>%
    filter(SEX == "_T", AGE == "_T") %>%
    group_by(REF_AREA) %>%
    filter(TIME_PERIOD == max(TIME_PERIOD)) %>%
    slice(1) %>%
    ungroup() %>%
    select(REF_AREA, OBS_VALUE, TIME_PERIOD) %>%
    rename(iso_a3 = REF_AREA, csec_rate = OBS_VALUE, year = TIME_PERIOD) %>%
    mutate(csec_rate = as.numeric(csec_rate)) %>%
    filter(!is.na(csec_rate))
 
  cat(sprintf(" Processed: %d countries\n", nrow(df)))
  cat(sprintf(" Matched to map: %d countries\n", sum(df$iso_a3 %in% world$iso_a3_eh)))
  if (nrow(df) > 0) {
    cat(sprintf(" Range: %.1f%% to %.1f%%\n", min(df$csec_rate), max(df$csec_rate)))
    cat(sprintf(" Latest year in data: %s\n", max(df$year)))
  }
 
  csec_latest_year <- max(df$year)
  df %>% select(iso_a3, csec_rate)
}, error = function(e) {
  cat(" ERROR:", e$message, "\n")
  data.frame()
})

# ============================================================================
# STEP 3: Child Stunting Data from World Bank
# ============================================================================
cat("\nDownloading child stunting data...\n")
stunting_df <- tryCatch({
  stunting_data <- WDI(
    country = "all",
    indicator = "SH.STA.STNT.ZS",
    start = 2000,
    end = 2025,
    extra = FALSE
  )
 
  cat(sprintf(" Downloaded: %d observations\n", nrow(stunting_data)))
 
  df <- stunting_data %>%
    filter(!iso2c %in% aggregate_codes) %>%
    group_by(iso2c) %>%
    filter(!is.na(SH.STA.STNT.ZS)) %>%
    filter(year == max(year)) %>%
    ungroup() %>%
    mutate(iso_a3 = countrycode(iso2c, "iso2c", "iso3c", warn = FALSE)) %>%
    filter(!is.na(iso_a3), !is.na(SH.STA.STNT.ZS)) %>%
    select(iso_a3, stunting_rate = SH.STA.STNT.ZS, year)
 
  cat(sprintf(" Processed: %d countries\n", nrow(df)))
  cat(sprintf(" Matched to map: %d countries\n", sum(df$iso_a3 %in% world$iso_a3_eh)))
  if (nrow(df) > 0) {
    cat(sprintf(" Range: %.1f%% to %.1f%%\n", min(df$stunting_rate), max(df$stunting_rate)))
    cat(sprintf(" Latest year in data: %s\n", max(df$year)))
  }
 
  stunting_latest_year <- max(df$year)
  df %>% select(iso_a3, stunting_rate)
}, error = function(e) {
  cat(" ERROR:", e$message, "\n")
  data.frame()
})

# ============================================================================
# STEP 4: Preterm Birth / Low Birth Weight Data from UNICEF
# ============================================================================
cat("\nDownloading preterm birth / low birth weight data from UNICEF...\n")
preterm_df <- tryCatch({
  preterm_url <- "https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,MNCH,1.0/.MNCH_PTB.......?format=csv"
 
  cat(" Attempting to download preterm birth data...\n")
  result <- try(download.file(preterm_url, destfile = "preterm_data.csv", quiet = TRUE), silent = TRUE)
 
  if (inherits(result, "try-error") || !file.exists("preterm_data.csv")) {
    cat(" Preterm data not available, trying low birth weight data...\n")
    lbw_url <- "https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,NUTRITION,1.0/.NT_BW_LBW.......?format=csv"
    download.file(lbw_url, destfile = "preterm_data.csv", quiet = TRUE)
  }
 
  preterm_raw <- read.csv("preterm_data.csv")
  cat(sprintf(" Downloaded: %d observations\n", nrow(preterm_raw)))
 
  df <- preterm_raw %>%
    filter(SEX == "_T" | is.na(SEX)) %>%
    group_by(REF_AREA) %>%
    filter(TIME_PERIOD == max(TIME_PERIOD)) %>%
    slice(1) %>%
    ungroup() %>%
    select(REF_AREA, OBS_VALUE, TIME_PERIOD) %>%
    rename(iso_a3 = REF_AREA, preterm_rate = OBS_VALUE, year = TIME_PERIOD) %>%
    mutate(preterm_rate = as.numeric(preterm_rate)) %>%
    filter(!is.na(preterm_rate))
 
  cat(sprintf(" Processed: %d countries\n", nrow(df)))
  cat(sprintf(" Matched to map: %d countries\n", sum(df$iso_a3 %in% world$iso_a3_eh)))
  if (nrow(df) > 0) {
    cat(sprintf(" Range: %.1f%% to %.1f%%\n", min(df$preterm_rate), max(df$preterm_rate)))
    cat(sprintf(" Latest year in data: %s\n", max(df$year)))
  }
 
  preterm_latest_year <- max(df$year)
  df %>% select(iso_a3, preterm_rate)
}, error = function(e) {
  cat(" ERROR:", e$message, "\n")
  data.frame()
})

# ============================================================================
# STEP 5: Child Wasting Data from World Bank
# ============================================================================
cat("\nDownloading child wasting data...\n")
wasting_df <- tryCatch({
  wasting_data <- WDI(
    country = "all",
    indicator = "SH.STA.WAST.ZS",
    start = 2000,
    end = 2025,
    extra = FALSE
  )
 
  cat(sprintf(" Downloaded: %d observations\n", nrow(wasting_data)))
 
  df <- wasting_data %>%
    filter(!iso2c %in% aggregate_codes) %>%
    group_by(iso2c) %>%
    filter(!is.na(SH.STA.WAST.ZS)) %>%
    filter(year == max(year)) %>%
    ungroup() %>%
    mutate(iso_a3 = countrycode(iso2c, "iso2c", "iso3c", warn = FALSE)) %>%
    filter(!is.na(iso_a3), !is.na(SH.STA.WAST.ZS)) %>%
    select(iso_a3, wasting_rate = SH.STA.WAST.ZS, year)
 
  cat(sprintf(" Processed: %d countries\n", nrow(df)))
  cat(sprintf(" Matched to map: %d countries\n", sum(df$iso_a3 %in% world$iso_a3_eh)))
  if (nrow(df) > 0) {
    cat(sprintf(" Range: %.1f%% to %.1f%%\n", min(df$wasting_rate), max(df$wasting_rate)))
    cat(sprintf(" Latest year in data: %s\n", max(df$year)))
  }
 
  wasting_latest_year <- max(df$year)
  df %>% select(iso_a3, wasting_rate)
}, error = function(e) {
  cat(" ERROR:", e$message, "\n")
  data.frame()
})

# ============================================================================
# STEP 6: Child Overweight Data from World Bank
# ============================================================================
cat("\nDownloading child overweight data...\n")
overweight_df <- tryCatch({
  overweight_data <- WDI(
    country = "all",
    indicator = "SH.STA.OWGH.ZS",
    start = 2000,
    end = 2025,
    extra = FALSE
  )
 
  cat(sprintf(" Downloaded: %d observations\n", nrow(overweight_data)))
 
  df <- overweight_data %>%
    filter(!iso2c %in% aggregate_codes) %>%
    group_by(iso2c) %>%
    filter(!is.na(SH.STA.OWGH.ZS)) %>%
    filter(year == max(year)) %>%
    ungroup() %>%
    mutate(iso_a3 = countrycode(iso2c, "iso2c", "iso3c", warn = FALSE)) %>%
    filter(!is.na(iso_a3), !is.na(SH.STA.OWGH.ZS)) %>%
    select(iso_a3, overweight_rate = SH.STA.OWGH.ZS, year)
 
  cat(sprintf(" Processed: %d countries\n", nrow(df)))
  cat(sprintf(" Matched to map: %d countries\n", sum(df$iso_a3 %in% world$iso_a3_eh)))
  if (nrow(df) > 0) {
    cat(sprintf(" Range: %.1f%% to %.1f%%\n", min(df$overweight_rate), max(df$overweight_rate)))
    cat(sprintf(" Latest year in data: %s\n", max(df$year)))
  }
 
  overweight_latest_year <- max(df$year)
  df %>% select(iso_a3, overweight_rate)
}, error = function(e) {
  cat(" ERROR:", e$message, "\n")
  data.frame()
})

# ============================================================================
# STEP 7: Tuberculosis Incidence
# ============================================================================
cat("\nDownloading tuberculosis incidence data...\n")
tb_df <- tryCatch({
  tb_data <- WDI(
    country = "all",
    indicator = "SH.TBS.INCD",
    start = 2000,
    end = 2025,
    extra = FALSE
  )
 
  cat(sprintf(" Downloaded: %d observations\n", nrow(tb_data)))
 
  df <- tb_data %>%
    filter(!iso2c %in% aggregate_codes) %>%
    group_by(iso2c) %>%
    filter(!is.na(SH.TBS.INCD)) %>%
    filter(year == max(year)) %>%
    ungroup() %>%
    mutate(iso_a3 = countrycode(iso2c, "iso2c", "iso3c", warn = FALSE)) %>%
    filter(!is.na(iso_a3), !is.na(SH.TBS.INCD)) %>%
    select(iso_a3, tb_incidence = SH.TBS.INCD, year)
 
  cat(sprintf(" Processed: %d countries\n", nrow(df)))
  cat(sprintf(" Matched to map: %d countries\n", sum(df$iso_a3 %in% world$iso_a3_eh)))
  if (nrow(df) > 0) {
    cat(sprintf(" Range: %.1f to %.1f per 100,000\n", min(df$tb_incidence), max(df$tb_incidence)))
    cat(sprintf(" Latest year in data: %s\n", max(df$year)))
  }
 
  tb_latest_year <- max(df$year)
  df %>% select(iso_a3, tb_incidence)
}, error = function(e) {
  cat(" ERROR:", e$message, "\n")
  data.frame()
})

# ============================================================================
# STEP 8: Malaria Incidence
# ============================================================================
cat("\nDownloading malaria incidence data...\n")
malaria_df <- tryCatch({
  malaria_data <- WDI(
    country = "all",
    indicator = "SH.MLR.INCD.P3",
    start = 2000,
    end = 2025,
    extra = FALSE
  )
 
  cat(sprintf(" Downloaded: %d observations\n", nrow(malaria_data)))
 
  df <- malaria_data %>%
    filter(!iso2c %in% aggregate_codes) %>%
    group_by(iso2c) %>%
    filter(!is.na(SH.MLR.INCD.P3)) %>%
    filter(year == max(year)) %>%
    ungroup() %>%
    mutate(iso_a3 = countrycode(iso2c, "iso2c", "iso3c", warn = FALSE)) %>%
    filter(!is.na(iso_a3), !is.na(SH.MLR.INCD.P3)) %>%
    select(iso_a3, malaria_incidence = SH.MLR.INCD.P3, year)
 
  cat(sprintf(" Processed: %d countries\n", nrow(df)))
  cat(sprintf(" Matched to map: %d countries\n", sum(df$iso_a3 %in% world$iso_a3_eh)))
  if (nrow(df) > 0) {
    cat(sprintf(" Range: %.1f to %.1f per 1,000\n", min(df$malaria_incidence), max(df$malaria_incidence)))
    cat(sprintf(" Latest year in data: %s\n", max(df$year)))
  }
 
  malaria_latest_year <- max(df$year)
  df %>% select(iso_a3, malaria_incidence)
}, error = function(e) {
  cat(" ERROR:", e$message, "\n")
  data.frame()
})

# ============================================================================
# CREATE MAPS
# ============================================================================

# MAP 1: Income Classifications
if (nrow(income_df) > 0) {
  cat("\n--- Creating Income Classification Map ---\n")
  tryCatch({
    map_data <- world %>%
      left_join(income_df, by = c("iso_a3_eh" = "iso_a3")) %>%
      mutate(income_group = factor(income_group,
                                   levels = c("Low income", "Lower middle income",
                                              "Upper middle income", "High income")))
   
    p1 <- ggplot(data = map_data) +
      geom_sf(aes(fill = income_group), color = "white", size = 0.1) +
      scale_fill_manual(
        values = c(
          "Low income" = "#d73027",
          "Lower middle income" = "#fee08b",
          "Upper middle income" = "#a6d96a",
          "High income" = "#1a9850"
        ),
        labels = c(
          "Low income (GNI = $1,135)",
          "Lower middle income ($1,136 - $4,495)",
          "Upper middle income ($4,496 - $13,935)",
          "High income (GNI > $13,935)"
        ),
        name = "Income Group (GNI per capita, current US$)",
        na.value = "grey90"
      ) +
      labs(
        title = "World Bank Country Income Classifications (FY26)",
        subtitle = "Classification by Gross National Income per capita (Atlas method)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 9),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()
      ) +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE))
   
    print(p1)
    ggsave("maps_output/01_income_classification.png", p1, width = 14, height = 8, dpi = 300)
    cat("? Saved to: maps_output/01_income_classification.png\n")
    cat(sprintf(" Mapped: %d countries with data\n", sum(!is.na(map_data$income_group))))
  }, error = function(e) {
    cat("? Error creating map:", e$message, "\n")
  })
}

# MAP 2: C-section Prevalence
if (nrow(csec_df) > 0) {
  cat("\n--- Creating C-section Prevalence Map ---\n")
  tryCatch({
    map_data <- world %>%
      left_join(csec_df, by = c("iso_a3_eh" = "iso_a3"))
   
    p2 <- ggplot(data = map_data) +
      geom_sf(aes(fill = csec_rate), color = "white", size = 0.1) +
      scale_fill_viridis_c(
        option = "plasma",
        name = "C-Section Rate (%)",
        na.value = "grey90",
        labels = function(x) sprintf("%.0f", x)
      ) +
      labs(
        title = "Cesarean Section Delivery Rates by Country",
        subtitle = sprintf("Percentage of births delivered by C-section (Data up to %s)",
                          ifelse(!is.null(csec_latest_year), csec_latest_year, "latest available year"))
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()
      )
   
    print(p2)
    ggsave("maps_output/02_csection_rates.png", p2, width = 14, height = 8, dpi = 300)
    cat("? Saved to: maps_output/02_csection_rates.png\n")
    cat(sprintf(" Mapped: %d countries with data\n", sum(!is.na(map_data$csec_rate))))
  }, error = function(e) {
    cat("? Error creating map:", e$message, "\n")
  })
}

# MAP 3: Child Stunting
if (nrow(stunting_df) > 0) {
  cat("\n--- Creating Child Stunting Map ---\n")
  tryCatch({
    map_data <- world %>%
      left_join(stunting_df, by = c("iso_a3_eh" = "iso_a3"))
   
    p3 <- ggplot(data = map_data) +
      geom_sf(aes(fill = stunting_rate), color = "white", size = 0.1) +
      scale_fill_viridis_c(
        option = "magma",
        name = "Stunting Rate (%)",
        na.value = "grey90",
        direction = -1,
        labels = function(x) sprintf("%.0f", x)
      ) +
      labs(
        title = "Child Stunting Prevalence (Children Under 5)",
        subtitle = sprintf("Percentage of children with height-for-age below -2 SD (Data up to %s)",
                          ifelse(!is.null(stunting_latest_year), stunting_latest_year, "latest available year"))
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()
      )
   
    print(p3)
    ggsave("maps_output/03_stunting_rates.png", p3, width = 14, height = 8, dpi = 300)
    cat("? Saved to: maps_output/03_stunting_rates.png\n")
    cat(sprintf(" Mapped: %d countries with data\n", sum(!is.na(map_data$stunting_rate))))
  }, error = function(e) {
    cat("? Error creating map:", e$message, "\n")
  })
}

# MAP 4: Preterm Birth/Low Birth Weight
if (nrow(preterm_df) > 0) {
  cat("\n--- Creating Preterm Birth Map ---\n")
  tryCatch({
    map_data <- world %>%
      left_join(preterm_df, by = c("iso_a3_eh" = "iso_a3"))
   
    p4 <- ggplot(data = map_data) +
      geom_sf(aes(fill = preterm_rate), color = "white", size = 0.1) +
      scale_fill_viridis_c(
        option = "inferno",
        name = "Rate (%)",
        na.value = "grey90",
        labels = function(x) sprintf("%.0f", x)
      ) +
      labs(
        title = "Preterm Birth / Low Birth Weight Prevalence by Country",
        subtitle = sprintf("Percentage affected (Data up to %s)",
                          ifelse(!is.null(preterm_latest_year), preterm_latest_year, "latest available year"))
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()
      )
   
    print(p4)
    ggsave("maps_output/04_preterm_birth_rates.png", p4, width = 14, height = 8, dpi = 300)
    cat("? Saved to: maps_output/04_preterm_birth_rates.png\n")
    cat(sprintf(" Mapped: %d countries with data\n", sum(!is.na(map_data$preterm_rate))))
  }, error = function(e) {
    cat("? Error creating map:", e$message, "\n")
  })
}

# MAP 5: Child Wasting
if (nrow(wasting_df) > 0) {
  cat("\n--- Creating Child Wasting Map ---\n")
  tryCatch({
    map_data <- world %>%
      left_join(wasting_df, by = c("iso_a3_eh" = "iso_a3"))
   
    p5 <- ggplot(data = map_data) +
      geom_sf(aes(fill = wasting_rate), color = "white", size = 0.1) +
      scale_fill_viridis_c(
        option = "mako",
        name = "Wasting Rate (%)",
        na.value = "grey90",
        direction = -1,
        labels = function(x) sprintf("%.0f", x)
      ) +
      labs(
        title = "Child Wasting Prevalence (Children Under 5)",
        subtitle = sprintf("Percentage of children with weight-for-height below -2 SD (Data up to %s)",
                          ifelse(!is.null(wasting_latest_year), wasting_latest_year, "latest available year"))
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()
      )
   
    print(p5)
    ggsave("maps_output/05_wasting_rates.png", p5, width = 14, height = 8, dpi = 300)
    cat("? Saved to: maps_output/05_wasting_rates.png\n")
    cat(sprintf(" Mapped: %d countries with data\n", sum(!is.na(map_data$wasting_rate))))
  }, error = function(e) {
    cat("? Error creating map:", e$message, "\n")
  })
}

# MAP 6: Child Overweight
if (nrow(overweight_df) > 0) {
  cat("\n--- Creating Child Overweight Map ---\n")
  tryCatch({
    map_data <- world %>%
      left_join(overweight_df, by = c("iso_a3_eh" = "iso_a3"))
   
    p6 <- ggplot(data = map_data) +
      geom_sf(aes(fill = overweight_rate), color = "white", size = 0.1) +
      scale_fill_viridis_c(
        option = "rocket",
        name = "Overweight Rate (%)",
        na.value = "grey90",
        direction = -1,
        labels = function(x) sprintf("%.0f", x)
      ) +
      labs(
        title = "Child Overweight Prevalence (Children Under 5)",
        subtitle = sprintf("Percentage of children with weight-for-height above +2 SD (Data up to %s)",
                          ifelse(!is.null(overweight_latest_year), overweight_latest_year, "latest available year"))
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()
      )
   
    print(p6)
    ggsave("maps_output/06_overweight_rates.png", p6, width = 14, height = 8, dpi = 300)
    cat("? Saved to: maps_output/06_overweight_rates.png\n")
    cat(sprintf(" Mapped: %d countries with data\n", sum(!is.na(map_data$overweight_rate))))
  }, error = function(e) {
    cat("? Error creating map:", e$message, "\n")
  })
}

# MAP 7: Tuberculosis Incidence
if (nrow(tb_df) > 0) {
  cat("\n--- Creating Tuberculosis Incidence Map ---\n")
  tryCatch({
    map_data <- world %>%
      left_join(tb_df, by = c("iso_a3_eh" = "iso_a3"))
   
    p7 <- ggplot(data = map_data) +
      geom_sf(aes(fill = tb_incidence), color = "white", size = 0.1) +
      scale_fill_viridis_c(
        option = "turbo",
        name = "TB Incidence\n(per 100,000)",
        na.value = "grey90",
        trans = "log10",
        labels = function(x) sprintf("%.0f", x)
      ) +
      labs(
        title = "Tuberculosis Incidence by Country",
        subtitle = sprintf("Cases per 100,000 population (Data up to %s)",
                          ifelse(!is.null(tb_latest_year), tb_latest_year, "latest available year"))
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()
      )
   
    print(p7)
    ggsave("maps_output/07_tuberculosis_incidence.png", p7, width = 14, height = 8, dpi = 300)
    cat("? Saved to: maps_output/07_tuberculosis_incidence.png\n")
    cat(sprintf(" Mapped: %d countries with data\n", sum(!is.na(map_data$tb_incidence))))
  }, error = function(e) {
    cat("? Error creating map:", e$message, "\n")
  })
}

# MAP 8: Malaria Incidence
if (nrow(malaria_df) > 0) {
  cat("\n--- Creating Malaria Incidence Map ---\n")
  tryCatch({
    map_data <- world %>%
      left_join(malaria_df, by = c("iso_a3_eh" = "iso_a3"))
   
    p8 <- ggplot(data = map_data) +
      geom_sf(aes(fill = malaria_incidence), color = "white", size = 0.1) +
      scale_fill_viridis_c(
        option = "cividis",
        name = "Malaria Incidence\n(per 1,000 at risk)",
        na.value = "grey90",
        trans = "log10",
        labels = function(x) sprintf("%.0f", x)
      ) +
      labs(
        title = "Malaria Incidence by Country",
        subtitle = sprintf("Cases per 1,000 population at risk (Data up to %s)",
                          ifelse(!is.null(malaria_latest_year), malaria_latest_year, "latest available year"))
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()
      )
   
    print(p8)
    ggsave("maps_output/08_malaria_incidence.png", p8, width = 14, height = 8, dpi = 300)
    cat("? Saved to: maps_output/08_malaria_incidence.png\n")
    cat(sprintf(" Mapped: %d countries with data\n", sum(!is.na(map_data$malaria_incidence))))
  }, error = function(e) {
    cat("? Error creating map:", e$message, "\n")
  })
}

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================
cat("\n======================================\n")
cat("SUMMARY STATISTICS\n")
cat("======================================\n")

if (nrow(income_df) > 0) {
  cat("\nIncome Classifications:\n")
  print(table(income_df$income_group))
}

if (nrow(csec_df) > 0) {
  cat("\nC-Section Rates:\n")
  cat(sprintf(" Countries: %d\n", nrow(csec_df)))
  cat(sprintf(" Mean: %.1f%%\n", mean(csec_df$csec_rate, na.rm = TRUE)))
  cat(sprintf(" Median: %.1f%%\n", median(csec_df$csec_rate, na.rm = TRUE)))
  cat(sprintf(" Range: %.1f%% - %.1f%%\n",
              min(csec_df$csec_rate, na.rm = TRUE),
              max(csec_df$csec_rate, na.rm = TRUE)))
}

if (nrow(stunting_df) > 0) {
  cat("\nChild Stunting Rates:\n")
  cat(sprintf(" Countries: %d\n", nrow(stunting_df)))
  cat(sprintf(" Mean: %.1f%%\n", mean(stunting_df$stunting_rate, na.rm = TRUE)))
  cat(sprintf(" Median: %.1f%%\n", median(stunting_df$stunting_rate, na.rm = TRUE)))
  cat(sprintf(" Range: %.1f%% - %.1f%%\n",
              min(stunting_df$stunting_rate, na.rm = TRUE),
              max(stunting_df$stunting_rate, na.rm = TRUE)))
}

if (nrow(preterm_df) > 0) {
  cat("\nPreterm Birth / Low Birth Weight Rates:\n")
  cat(sprintf(" Countries: %d\n", nrow(preterm_df)))
  cat(sprintf(" Mean: %.1f%%\n", mean(preterm_df$preterm_rate, na.rm = TRUE)))
  cat(sprintf(" Median: %.1f%%\n", median(preterm_df$preterm_rate, na.rm = TRUE)))
  cat(sprintf(" Range: %.1f%% - %.1f%%\n",
              min(preterm_df$preterm_rate, na.rm = TRUE),
              max(preterm_df$preterm_rate, na.rm = TRUE)))
}

if (nrow(wasting_df) > 0) {
  cat("\nChild Wasting Rates:\n")
  cat(sprintf(" Countries: %d\n", nrow(wasting_df)))
  cat(sprintf(" Mean: %.1f%%\n", mean(wasting_df$wasting_rate, na.rm = TRUE)))
  cat(sprintf(" Median: %.1f%%\n", median(wasting_df$wasting_rate, na.rm = TRUE)))
  cat(sprintf(" Range: %.1f%% - %.1f%%\n",
              min(wasting_df$wasting_rate, na.rm = TRUE),
              max(wasting_df$wasting_rate, na.rm = TRUE)))
}

if (nrow(overweight_df) > 0) {
  cat("\nChild Overweight Rates:\n")
  cat(sprintf(" Countries: %d\n", nrow(overweight_df)))
  cat(sprintf(" Mean: %.1f%%\n", mean(overweight_df$overweight_rate, na.rm = TRUE)))
  cat(sprintf(" Median: %.1f%%\n", median(overweight_df$overweight_rate, na.rm = TRUE)))
  cat(sprintf(" Range: %.1f%% - %.1f%%\n",
              min(overweight_df$overweight_rate, na.rm = TRUE),
              max(overweight_df$overweight_rate, na.rm = TRUE)))
}

if (nrow(tb_df) > 0) {
  cat("\nTuberculosis Incidence:\n")
  cat(sprintf(" Countries: %d\n", nrow(tb_df)))
  cat(sprintf(" Mean: %.1f per 100,000\n", mean(tb_df$tb_incidence, na.rm = TRUE)))
  cat(sprintf(" Median: %.1f per 100,000\n", median(tb_df$tb_incidence, na.rm = TRUE)))
  cat(sprintf(" Range: %.1f - %.1f per 100,000\n",
              min(tb_df$tb_incidence, na.rm = TRUE),
              max(tb_df$tb_incidence, na.rm = TRUE)))
}

if (nrow(malaria_df) > 0) {
  cat("\nMalaria Incidence:\n")
  cat(sprintf(" Countries: %d\n", nrow(malaria_df)))
  cat(sprintf(" Mean: %.1f per 1,000 at risk\n", mean(malaria_df$malaria_incidence, na.rm = TRUE)))
  cat(sprintf(" Median: %.1f per 1,000 at risk\n", median(malaria_df$malaria_incidence, na.rm = TRUE)))
  cat(sprintf(" Range: %.1f - %.1f per 1,000 at risk\n",
              min(malaria_df$malaria_incidence, na.rm = TRUE),
              max(malaria_df$malaria_incidence, na.rm = TRUE)))
}

# Clean up temporary files
if (file.exists("income_classifications.xlsx")) {
  file.remove("income_classifications.xlsx")
}
if (file.exists("csec_data.csv")) {
  file.remove("csec_data.csv")
}
if (file.exists("preterm_data.csv")) {
  file.remove("preterm_data.csv")
}

cat("\n======================================\n")
cat("SCRIPT COMPLETED\n")
cat("======================================\n")
cat("Maps saved to: maps_output/\n")