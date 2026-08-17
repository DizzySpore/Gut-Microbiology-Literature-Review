# ============================================================================
# Early-Life & All Microbiome Studies Geographic Distribution Mapping Script
# CORRECTED VERSION - Compatible with Python country assignment script output
# ============================================================================

# Define library path (adjust if needed for your cluster/environment)
lib_path <- "path/to/your/R/libs"  # <-- Change this to your actual library path

# Load required libraries
library(dplyr, lib.loc = lib_path)
library(ggplot2, lib.loc = lib_path)
library(rnaturalearth, lib.loc = lib_path)
library(rnaturalearthdata, lib.loc = lib_path)
library(sf, lib.loc = lib_path)
library(scales, lib.loc = lib_path)
library(viridis, lib.loc = lib_path)
library(countrycode, lib.loc = lib_path)
library(readr, lib.loc = lib_path)
library(tidyr, lib.loc = lib_path)  # For separate_rows
library(stringr, lib.loc = lib_path)  # For str_replace_all

# Create output directory
if (!dir.exists("maps_output")) {
  dir.create("maps_output")
}

# ============================================================================
# CONFIGURATION - Adjust these
# ============================================================================
# FIXED: Input file from Python script (actual filename)
input_metadata <- "detailed_country_assignments.csv"

# Toggles for what to plot
plot_all_studies <- TRUE      # Set to FALSE to skip all-studies map
plot_early_life <- TRUE       # Set to FALSE to skip early-life map

# Map versions to generate
generate_global_map <- TRUE   # Global map showing HIC/LMIC disparity (RECOMMENDED)
generate_lmic_only_map <- FALSE  # LMIC-only map for detailed LMIC view

# FIXED: LMIC country list (ASCII only, matching Python output)
# Based on World Bank classification
lmic_countries <- c(
  "Afghanistan", "Angola", "Bangladesh", "Benin", "Bhutan", "Bolivia", 
  "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", "Cameroon", 
  "Central African Republic", "Chad", "Comoros", "Congo",
  "Cote d'Ivoire", "Djibouti", "Egypt", "El Salvador", "Eritrea", 
  "Eswatini", "Ethiopia", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", 
  "Haiti", "Honduras", "India", "Indonesia", "Iran", "Kenya", "Kiribati", 
  "Kyrgyzstan", "Laos", "Lebanon", "Lesotho", "Liberia", "Madagascar", 
  "Malawi", "Mali", "Mauritania", "Micronesia", "Mongolia", "Morocco", 
  "Mozambique", "Myanmar", "Nepal", "Nicaragua", "Niger", "Nigeria", 
  "Pakistan", "Papua New Guinea", "Philippines", "Rwanda", "Samoa", 
  "Sao Tome and Principe", "Senegal", "Sierra Leone", "Solomon Islands", 
  "Somalia", "South Sudan", "Sri Lanka", "Sudan", "Syria", "Tajikistan", 
  "Tanzania", "Timor-Leste", "Togo", "Tunisia", "Uganda", "Ukraine", 
  "Uzbekistan", "Vanuatu", "Vietnam", "Yemen", "Zambia", "Zimbabwe",
  # Additional variations that might appear in Python output
  "Tanzania, United Republic of", "Congo, The Democratic Republic of the",
  "Bolivia, Plurinational State of", "Venezuela, Bolivarian Republic of",
  "Iran, Islamic Republic of", "Syria, Syrian Arab Republic",
  "Korea, Democratic People's Republic of", "Lao People's Democratic Republic",
  "Moldova, Republic of"
)

# ============================================================================
# LOAD WORLD MAP
# ============================================================================
cat("Loading world map from Natural Earth...\n")
world <- ne_countries(scale = "medium", returnclass = "sf")
cat(sprintf("Loaded map with %d countries/territories\n", nrow(world)))

# ============================================================================
# LOAD METADATA
# ============================================================================
cat("\nLoading processed GMrepo metadata...\n")
if (!file.exists(input_metadata)) {
  stop(sprintf("Input file '%s' not found. Make sure Python script has run.", input_metadata))
}

# Note: Python outputs TSV with tab separator
metadata <- read_tsv(input_metadata, guess_max = 10000)
cat(sprintf("Loaded %d total projects\n", nrow(metadata)))

# FIXED: Check for actual Python column names
required_cols <- c("clean_countries_list", "Is_Early_Life")
missing_cols <- setdiff(required_cols, names(metadata))
if (length(missing_cols) > 0) {
  stop(sprintf("Missing columns: %s. Check Python script output.", 
               paste(missing_cols, collapse = ", ")))
}

cat("\nProcessing multi-country projects...\n")
cat(sprintf("Original rows: %d\n", nrow(metadata)))

# FIXED: Split multi-country entries and create clean columns
metadata_processed <- metadata %>%
  # Split multi-country entries (semicolon-separated)
  separate_rows(clean_countries_list, sep = "; ") %>%
  # Rename to standard column names for rest of script
  mutate(
    assigned_country = clean_countries_list,
    is_early_life = Is_Early_Life
  )

cat(sprintf("After splitting multi-country: %d rows\n", nrow(metadata_processed)))
cat(sprintf("Note: Multi-country projects are counted once per country\n"))

# Base filter: valid assigned countries
valid_projects <- metadata_processed %>%
  filter(
    assigned_country != "Unknown", 
    !is.na(assigned_country), 
    assigned_country != ""
  )

cat(sprintf("%d projects/rows with valid assigned country\n", nrow(valid_projects)))

# Show unique countries for verification
unique_countries <- unique(valid_projects$assigned_country) %>% sort()
cat(sprintf("\nFound %d unique countries:\n", length(unique_countries)))
cat(paste(head(unique_countries, 20), collapse = ", "), "...\n")

# ============================================================================
# FUNCTION TO CREATE MAP
# ============================================================================
create_map <- function(data_df, plot_type, lmic_filter) {
  suffix <- ifelse(lmic_filter, "_lmic", "_global")
  title_type <- ifelse(plot_type == "all", 
                      "All Gut Microbiome Studies", 
                      "Early-Life (Under 5) Gut Microbiome Studies")
  subtitle <- ifelse(lmic_filter, 
                    "LMICs only", 
                    "Global distribution showing HIC/LMIC disparity")
  
  # Count projects per country
  country_counts <- data_df %>%
    group_by(assigned_country) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(country = assigned_country)
  
  cat(sprintf("\n%s (%s): %d countries, %d total projects\n", 
              title_type, subtitle, nrow(country_counts), sum(country_counts$count)))
  
  if (nrow(country_counts) == 0) {
    cat("No data to plot. Skipping map.\n")
    return(NULL)
  }
  
  # Convert country names to ISO3 codes for joining with map
  country_counts$iso_a3 <- countrycode(
    country_counts$country, 
    "country.name", 
    "iso3c", 
    warn = TRUE
  )
  
  # Report countries that couldn't be matched
  failed_matches <- country_counts %>% filter(is.na(iso_a3))
  if (nrow(failed_matches) > 0) {
    cat("\nWarning: Could not match these countries to ISO3 codes:\n")
    print(failed_matches %>% select(country, count))
  }
  
  country_counts <- country_counts %>% filter(!is.na(iso_a3))
  
  # Join with world map
  map_data <- world %>%
    left_join(country_counts, by = c("iso_a3_eh" = "iso_a3"))
  
  # Calculate max value for legend
  max_count <- max(country_counts$count, na.rm = TRUE)
  
  # Create well-spaced breaks that always include the max
  # Use logarithmically-spaced values to prevent overlap
  breaks_vec <- c(1, 5, 10, 50, 100, 500)
  
  # Only keep breaks that are less than 80% of max (to leave room for max label)
  breaks_vec <- breaks_vec[breaks_vec < (max_count * 0.8)]
  
  # Add the actual max value as the final break
  breaks_vec <- c(breaks_vec, max_count)
  
  # Create map
  p <- ggplot(data = map_data) +
    geom_sf(aes(fill = count), color = "white", size = 0.1) +
    scale_fill_viridis_c(
      option = "plasma",
      name = "Number of\nProjects",
      na.value = "grey90",
      trans = "log10",
      breaks = breaks_vec,
      labels = as.character(breaks_vec),
      limits = c(1, NA)
    ) +
    labs(
      title = paste("Global Distribution:", title_type),
      subtitle = paste("From GMrepo shotgun metagenomes -", subtitle),
      caption = "Country assignment via descriptions, abstracts, full-text, affiliations, institutions"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      plot.caption = element_text(size = 10, hjust = 1),
      legend.position = "bottom",
      legend.key.width = unit(2, "cm"),
      legend.key.height = unit(0.5, "cm"),
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 9),
      panel.grid = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
  
  # Save map
  filename <- paste0("maps_output/", 
                    tolower(str_replace_all(title_type, " ", "_")), 
                    suffix, ".png")
  ggsave(filename, p, width = 14, height = 8, dpi = 300)
  cat(sprintf("Map saved: %s\n", filename))
  
  # Save CSV breakdown
  csv_name <- paste0("maps_output/", 
                    tolower(str_replace_all(title_type, " ", "_")), 
                    suffix, "_breakdown.csv")
  country_counts %>%
    select(country, count) %>%
    arrange(desc(count)) %>%
    write_csv(csv_name)
  cat(sprintf("Breakdown CSV: %s\n", csv_name))
  
  # Top 10 print
  cat(sprintf("\nTop 10 countries (%s, %s):\n", title_type, subtitle))
  print(country_counts %>% 
        select(country, count) %>% 
        arrange(desc(count)) %>% 
        head(10))
  
  return(p)
}

# ============================================================================
# GENERATE MAPS
# ============================================================================
if (plot_all_studies) {
  cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
  cat("--- Processing All Studies ---\n")
  cat(paste(rep("=", 70), collapse = ""), "\n", sep = "")
  
  # Generate global map (shows HIC/LMIC disparity)
  if (generate_global_map) {
    cat("\nGenerating GLOBAL map (all countries)...\n")
    all_df_global <- valid_projects
    create_map(all_df_global, "all", lmic_filter = FALSE)
  }
  
  # Generate LMIC-only map (detailed LMIC view)
  if (generate_lmic_only_map) {
    cat("\nGenerating LMIC-ONLY map...\n")
    all_df_lmic <- valid_projects %>% filter(assigned_country %in% lmic_countries)
    cat(sprintf("After LMIC filter: %d projects\n", nrow(all_df_lmic)))
    create_map(all_df_lmic, "all", lmic_filter = TRUE)
  }
}

if (plot_early_life) {
  cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
  cat("--- Processing Early-Life Studies ---\n")
  cat(paste(rep("=", 70), collapse = ""), "\n", sep = "")
  
  early_df_all <- valid_projects %>% filter(is_early_life == 1)
  cat(sprintf("Early-life projects (all countries): %d\n", nrow(early_df_all)))
  
  # Generate global map (shows HIC/LMIC disparity)
  if (generate_global_map) {
    cat("\nGenerating GLOBAL map (all countries)...\n")
    create_map(early_df_all, "early_life", lmic_filter = FALSE)
  }
  
  # Generate LMIC-only map (detailed LMIC view)
  if (generate_lmic_only_map) {
    cat("\nGenerating LMIC-ONLY map...\n")
    early_df_lmic <- early_df_all %>% filter(assigned_country %in% lmic_countries)
    cat(sprintf("After LMIC filter: %d projects\n", nrow(early_df_lmic)))
    create_map(early_df_lmic, "early_life", lmic_filter = TRUE)
  }
}

# ============================================================================
# FINAL SUMMARY
# ============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("FINAL SUMMARY\n")
cat(paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat(sprintf("Total projects (original): %d\n", nrow(metadata)))
cat(sprintf("Total rows after multi-country split: %d\n", nrow(metadata_processed)))
cat(sprintf("Projects/rows with assigned country: %d\n", nrow(valid_projects)))
cat(sprintf("Unique countries found: %d\n", length(unique(valid_projects$assigned_country))))
cat(sprintf("\nEarly-life projects (original, detected): %d\n", sum(metadata$Is_Early_Life)))
cat(sprintf("Early-life rows (after split): %d\n", sum(metadata_processed$is_early_life)))
cat(sprintf("Early-life with assigned country: %d\n", 
            sum(valid_projects$is_early_life)))


# Calculate LMIC statistics
lmic_count <- sum(valid_projects$assigned_country %in% lmic_countries)
lmic_early <- sum(valid_projects$is_early_life == 1 & 
                 valid_projects$assigned_country %in% lmic_countries)
hic_count <- nrow(valid_projects) - lmic_count
hic_early <- sum(valid_projects$is_early_life) - lmic_early

cat(sprintf("\nHIC vs LMIC Distribution:\n"))
cat(sprintf("  HIC projects: %d (%.1f%%)\n", 
            hic_count, 
            100 * hic_count / nrow(valid_projects)))
cat(sprintf("  LMIC projects: %d (%.1f%%)\n", 
            lmic_count, 
            100 * lmic_count / nrow(valid_projects)))
cat(sprintf("  HIC early-life: %d (%.1f%% of early-life)\n", 
            hic_early,
            100 * hic_early / sum(valid_projects$is_early_life)))
cat(sprintf("  LMIC early-life: %d (%.1f%% of early-life)\n", 
            lmic_early,
            100 * lmic_early / sum(valid_projects$is_early_life)))

cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("SCRIPT COMPLETED\n")
cat(paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("Maps and CSVs saved in 'maps_output/'\n")
cat("\nConfiguration:\n")
cat(sprintf("  - Global maps (HIC/LMIC disparity): %s\n", ifelse(generate_global_map, "ENABLED", "DISABLED")))
cat(sprintf("  - LMIC-only maps: %s\n", ifelse(generate_lmic_only_map, "ENABLED", "DISABLED")))
cat("\nToggle plot_all_studies / plot_early_life to enable/disable study types.\n")
cat("Toggle generate_global_map / generate_lmic_only_map for map versions.\n")