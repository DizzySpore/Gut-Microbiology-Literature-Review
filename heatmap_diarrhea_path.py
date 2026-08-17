#!/usr/bin/env python3

import matplotlib
matplotlib.use('Cairo')  # Use Cairo backend for headless environment
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from glob import glob
import os

# Read all CSV files
csv_files = glob('*.csv')
all_data = []

for file in csv_files:
    df = pd.read_csv(file)
    all_data.append(df)

# Combine all dataframes
combined_df = pd.concat(all_data, ignore_index=True)

# Group by Location and Pathogen, take mean of Value
data_summary = combined_df.groupby(['Location', 'Pathogen'])['Value'].mean().reset_index()

# Create pivot table for heatmap
heatmap_data = data_summary.pivot(index='Pathogen', columns='Location', values='Value')

# Fill NaN with 0
heatmap_data = heatmap_data.fillna(0)

# Sort columns (regions) by total incidence (descending - highest leftmost)
column_sums = heatmap_data.sum(axis=0).sort_values(ascending=False)
heatmap_data = heatmap_data[column_sums.index]

# Sort rows (pathogens) by total incidence (descending - highest topmost)
row_sums = heatmap_data.sum(axis=1).sort_values(ascending=False)
heatmap_data = heatmap_data.loc[row_sums.index]

# Function to format cell text
def format_value(val):
    if val == 0 or np.isnan(val):
        return ''
    elif val >= 1000:
        return f'{val/1000:.1f}k'
    else:
        return f'{val:.1f}'

# Create figure with appropriate size
fig, ax = plt.subplots(figsize=(16, 12))

# Create heatmap with white to red colormap
im = ax.imshow(heatmap_data.values, cmap='Reds', aspect='auto', vmin=0, vmax=heatmap_data.values.max())

# Set ticks and labels
ax.set_xticks(np.arange(len(heatmap_data.columns)))
ax.set_yticks(np.arange(len(heatmap_data.index)))
ax.set_xticklabels(heatmap_data.columns, rotation=45, ha='left', fontsize=9)
ax.set_yticklabels(heatmap_data.index, fontsize=9)

# Move x-axis labels to top
ax.xaxis.tick_top()
ax.xaxis.set_label_position('top')

# Add text annotations with formatted values
for i in range(len(heatmap_data.index)):
    for j in range(len(heatmap_data.columns)):
        val = heatmap_data.values[i, j]
        text_str = format_value(val)
        if text_str:
            # Determine text color based on background intensity
            text_color = 'white' if val > heatmap_data.values.max() * 0.6 else 'black'
            text = ax.text(j, i, text_str, ha='center', va='center', 
                          color=text_color, fontsize=7)

# Add colorbar
cbar = plt.colorbar(im, ax=ax, fraction=0.046, pad=0.04)
cbar.ax.set_ylabel('DALYs Rate (per 100k)', rotation=90, va='bottom', fontsize=10)

# Format colorbar ticks
cbar_ticks = cbar.get_ticks()
cbar_labels = [format_value(tick) for tick in cbar_ticks]
cbar.set_ticks(cbar_ticks)
cbar.set_ticklabels(cbar_labels, fontsize=9)

# Add title
plt.title('Diarrhea DALYs (Rate per 100k) by Pathogen and Region - Under 5, 2021', 
          fontsize=14, pad=20, weight='bold')

# Add grid
ax.set_xticks(np.arange(heatmap_data.shape[1]+1)-.5, minor=True)
ax.set_yticks(np.arange(heatmap_data.shape[0]+1)-.5, minor=True)
ax.grid(which="minor", color="gray", linestyle='-', linewidth=0.5, alpha=0.3)
ax.tick_params(which="minor", size=0)

# Adjust layout to prevent label cutoff
plt.tight_layout()

# Save figure
plt.savefig('diarrhea_heatmap.png', dpi=150, bbox_inches='tight', 
            facecolor='white', edgecolor='none')
print("Heatmap saved as diarrhea_heatmap.png")

plt.close()