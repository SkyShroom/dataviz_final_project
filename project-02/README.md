# Data Visualization 

> Henry Marsh. 

## Mini-Project 1

# Project 02: Weather and Florida Lakes

## Executive Summary

This project examines Atlanta weather patterns and the spatial distribution of lakes across Florida. The motivation was to combine multiple visualization types in one report, including an interactive weather chart, a model visualization, and a spatial map.

The weather section focuses on daily high and low temperatures, seasonal temperature change, and the relationship between daily low and high temperatures. The Florida lakes section focuses on lake locations and county-level lake counts.

## Data Used

This project uses two datasets:

- Atlanta daily weather data
- Florida Lakes shapefile

The weather data includes daily temperature and precipitation values. The Florida Lakes shapefile includes lake polygons and county information used for the spatial map and lake count analysis.

## Requirements Addressed

### 1. Interactive Chart

This project includes an interactive daily temperature chart.

The interactive chart lets the reader hover over the lines to see exact dates and temperature values. This adds more detail without making the static chart crowded.

Interactive file:

[Open interactive temperature chart](/figures/project_02_weather_interactive.html)

### 2. Accessibility

Accessibility was addressed by using colorblind-friendly palettes, high-contrast colors, clearer labels, and alt text for the main figures. The model chart also uses both color and shape so the seasonal groups are not separated by color alone.

### 3. Before and After Redesign

The redesign section focuses on the daily temperature chart. The original version showed the data, but it had weaker color choices and less effective labeling. The redesigned version improves the chart with clearer labels, stronger contrast, colorblind-friendly colors, and an interactive version with hover text.

## Files Included

## Main Findings

The Atlanta weather data shows a clear seasonal pattern. Daily high and low temperatures rise into the summer months and fall again toward winter.

The model visualization shows a strong positive relationship between daily low temperature and daily high temperature. Days with warmer lows usually also have warmer highs.

The Florida lakes map shows that named lakes are not evenly distributed across the state. Central Florida shows stronger lake clustering, while several coastal and southern counties have fewer named lake features.

## Saved Figures

![Weather model visualization](/figures/project_02_weather_model.png)

![Florida lakes spatial map](/figures/project_02_florida_lakes_map.png)

## Future Directions

Future work could add more weather years to compare whether 2019 was typical or unusual. For the Florida lakes section, future analysis could include lake area, water quality, land use, or population density near lakes to better explain the spatial patterns.