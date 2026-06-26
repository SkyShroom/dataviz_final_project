---
title: "Project 02: Weather and Florida Lakes"
author: "Henry Marsh"
date: "2026-06-20"
output:
  html_document:
    keep_md: true
    toc: true
    toc_float: true
---


``` r
library(tidyverse)
library(lubridate)
library(plotly)
library(sf)
library(broom)
library(scales)
library(maps)
library(htmlwidgets)

theme_set(theme_minimal(base_size = 13))
```

## Load Data


``` r
weather_url <- "https://raw.githubusercontent.com/SkyShroom/dataviz_final_project/refs/heads/main/data/tpa_weather_2022.csv"

weather <- readr::read_csv(weather_url, show_col_types = TRUE)
```

```
## Rows: 365 Columns: 7
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## dbl (7): year, month, day, precipitation, max_temp, min_temp, ave_temp
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
glimpse(weather)
```

```
## Rows: 365
## Columns: 7
## $ year          <dbl> 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 20…
## $ month         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
## $ day           <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 1…
## $ precipitation <dbl> 0.00000, 0.00000, 0.02000, 0.00000, 0.00000, 0.00001, 0.…
## $ max_temp      <dbl> 82, 82, 75, 76, 75, 74, 81, 81, 84, 81, 73, 77, 74, 72, …
## $ min_temp      <dbl> 67, 71, 55, 50, 59, 56, 63, 58, 65, 64, 54, 54, 59, 55, …
## $ ave_temp      <dbl> 74.5, 76.5, 65.0, 63.0, 67.0, 65.0, 72.0, 69.5, 74.5, 72…
```


``` r
lake_url <- "/vsicurl/https://raw.githubusercontent.com/SkyShroom/dataviz_final_project/main/data/Florida_Lakes/Florida_Lakes.shp"

fl_lakes <- sf::st_read(lake_url, quiet = TRUE) %>%
  sf::st_transform(4326)
```

## Cleaning of Data


``` r
weather_clean <- weather %>%
  mutate(
    date = as.Date(paste(year, month, day, sep = "-")),
    month_label = lubridate::month(date, label = TRUE, abbr = TRUE),
    month_num = lubridate::month(date),
    day_of_year = lubridate::yday(date),
    tmax = max_temp,
    tmin = min_temp,
    prcp = precipitation,
    temp_range = tmax - tmin,
    rain_day = if_else(prcp > 0, "Rain", "No rain")
  ) %>%
  drop_na(date, tmax, tmin, prcp)

glimpse(weather_clean)
```

```
## Rows: 365
## Columns: 16
## $ year          <dbl> 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 20…
## $ month         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
## $ day           <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 1…
## $ precipitation <dbl> 0.00000, 0.00000, 0.02000, 0.00000, 0.00000, 0.00001, 0.…
## $ max_temp      <dbl> 82, 82, 75, 76, 75, 74, 81, 81, 84, 81, 73, 77, 74, 72, …
## $ min_temp      <dbl> 67, 71, 55, 50, 59, 56, 63, 58, 65, 64, 54, 54, 59, 55, …
## $ ave_temp      <dbl> 74.5, 76.5, 65.0, 63.0, 67.0, 65.0, 72.0, 69.5, 74.5, 72…
## $ date          <date> 2022-01-01, 2022-01-02, 2022-01-03, 2022-01-04, 2022-01…
## $ month_label   <ord> Jan, Jan, Jan, Jan, Jan, Jan, Jan, Jan, Jan, Jan, Jan, J…
## $ month_num     <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
## $ day_of_year   <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 1…
## $ tmax          <dbl> 82, 82, 75, 76, 75, 74, 81, 81, 84, 81, 73, 77, 74, 72, …
## $ tmin          <dbl> 67, 71, 55, 50, 59, 56, 63, 58, 65, 64, 54, 54, 59, 55, …
## $ prcp          <dbl> 0.00000, 0.00000, 0.02000, 0.00000, 0.00000, 0.00001, 0.…
## $ temp_range    <dbl> 15, 11, 20, 26, 16, 18, 18, 23, 19, 17, 19, 23, 15, 17, …
## $ rain_day      <chr> "No rain", "No rain", "Rain", "No rain", "No rain", "Rai…
```


``` r
fl_lakes_clean <- fl_lakes %>%
  st_make_valid() %>%
  st_transform(4326) %>%
  rename_with(~ str_to_lower(gsub("\\.", "_", .x))) %>%
  mutate(
    name = str_squish(name),
    county = str_to_title(str_squish(county)),
    name = na_if(name, ""),
    county = na_if(county, "")
  ) %>%
  filter(!st_is_empty(geometry))

glimpse(fl_lakes_clean)
```

```
## Rows: 4,243
## Columns: 7
## $ perimeter <dbl> 11082.2515, 2834.0741, 18768.2731, 493.2790, 5662.6876, 316.…
## $ name      <chr> "Lake Maitland", "Black Lake", "Lake Jackson", "Halfmoon Lak…
## $ county    <chr> "Orange", "Escambia", "Highlands", "Escambia", "Escambia", "…
## $ objectid  <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 1…
## $ shapearea <dbl> 1818000.097, 31379.777, 13601177.118, 6337.482, 338242.234, …
## $ shapelen  <dbl> 11082.2509, 2834.0741, 18768.2738, 493.2789, 5662.6887, 316.…
## $ geometry  <MULTIPOLYGON [°]> MULTIPOLYGON (((-81.3482 28..., MULTIPOLYGON ((…
```


## Summary


``` r
weather_summary <- weather_clean %>%
  summarise(
    days = n(),
    avg_high_temp = mean(tmax, na.rm = TRUE),
    avg_low_temp = mean(tmin, na.rm = TRUE),
    avg_temp_range = mean(temp_range, na.rm = TRUE),
    total_rainfall = sum(prcp, na.rm = TRUE),
    rain_days = sum(prcp > 0, na.rm = TRUE),
    hottest_day = max(tmax, na.rm = TRUE),
    coldest_day = min(tmin, na.rm = TRUE)
  )

weather_summary
```

```
## # A tibble: 1 × 8
##    days avg_high_temp avg_low_temp avg_temp_range total_rainfall rain_days
##   <int>         <dbl>        <dbl>          <dbl>          <dbl>     <int>
## 1   365          84.5         68.2           16.3           62.0       144
## # ℹ 2 more variables: hottest_day <dbl>, coldest_day <dbl>
```


``` r
fl_lakes_summary <- fl_lakes_clean %>%
  mutate(
    area_sq_km = as.numeric(st_area(st_transform(geometry, 3086))) / 1000000
  ) %>%
  st_drop_geometry() %>%
  summarise(
    total_lakes = n(),
    named_lakes = sum(!is.na(name)),
    counties_in_data = n_distinct(county, na.rm = TRUE),
    avg_lake_area_sq_km = mean(area_sq_km, na.rm = TRUE),
    median_lake_area_sq_km = median(area_sq_km, na.rm = TRUE),
    largest_lake_sq_km = max(area_sq_km, na.rm = TRUE)
  )

fl_lakes_summary
```

```
##   total_lakes named_lakes counties_in_data avg_lake_area_sq_km
## 1        4243        4243               67             1.04507
##   median_lake_area_sq_km largest_lake_sq_km
## 1             0.07776524            1296.12
```


# Visualization 1: Interactive Temperature Line Chart


``` r
temp_long <- weather_clean %>%
  select(date, tmax, tmin) %>%
  pivot_longer(
    cols = c(tmax, tmin),
    names_to = "temperature_type",
    values_to = "temperature"
  ) %>%
  mutate(
    temperature_type = recode(
      temperature_type,
      tmax = "Daily High",
      tmin = "Daily Low"
    )
  )

p_temp <- ggplot(
  temp_long,
  aes(
    x = date,
    y = temperature,
    color = temperature_type,
    group = temperature_type,
    text = paste(
      "Date:", date,
      "<br>Type:", temperature_type,
      "<br>Temperature:", round(temperature, 1)
    )
  )
) +
  geom_line(linewidth = 1.1, alpha = 0.9) +
  scale_color_manual(
    values = c(
      "Daily High" = "#000000",
    "Daily Low" = "#0072B2"
    )
  ) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(
    title = "Atlanta Daily Temperatures in 2019",
    subtitle = "Daily highs and lows rise into summer, then fall back down toward winter",
    x = NULL,
    y = "Temperature",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 25, 25, 25)
  )

interactive_temperature_plot <- ggplotly(p_temp, tooltip = "text") %>%
  layout(
    title = list(
      text = paste0(
        "Atlanta Daily Temperatures in 2019",
        "<br><sup>Daily highs and lows rise into summer, then fall back down toward winter</sup>"
      ),
      x = 0.05
    ),
    legend = list(
      orientation = "h",
      x = 0.05,
      y = -0.18
    ),
    margin = list(
      t = 95,
      b = 95,
      l = 75,
      r = 40
    )
  )

interactive_temperature_plot
```

```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-b86c704019f01d14769e" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-b86c704019f01d14769e">{"x":{"data":[{"x":[18993,18994,18995,18996,18997,18998,18999,19000,19001,19002,19003,19004,19005,19006,19007,19008,19009,19010,19011,19012,19013,19014,19015,19016,19017,19018,19019,19020,19021,19022,19023,19024,19025,19026,19027,19028,19029,19030,19031,19032,19033,19034,19035,19036,19037,19038,19039,19040,19041,19042,19043,19044,19045,19046,19047,19048,19049,19050,19051,19052,19053,19054,19055,19056,19057,19058,19059,19060,19061,19062,19063,19064,19065,19066,19067,19068,19069,19070,19071,19072,19073,19074,19075,19076,19077,19078,19079,19080,19081,19082,19083,19084,19085,19086,19087,19088,19089,19090,19091,19092,19093,19094,19095,19096,19097,19098,19099,19100,19101,19102,19103,19104,19105,19106,19107,19108,19109,19110,19111,19112,19113,19114,19115,19116,19117,19118,19119,19120,19121,19122,19123,19124,19125,19126,19127,19128,19129,19130,19131,19132,19133,19134,19135,19136,19137,19138,19139,19140,19141,19142,19143,19144,19145,19146,19147,19148,19149,19150,19151,19152,19153,19154,19155,19156,19157,19158,19159,19160,19161,19162,19163,19164,19165,19166,19167,19168,19169,19170,19171,19172,19173,19174,19175,19176,19177,19178,19179,19180,19181,19182,19183,19184,19185,19186,19187,19188,19189,19190,19191,19192,19193,19194,19195,19196,19197,19198,19199,19200,19201,19202,19203,19204,19205,19206,19207,19208,19209,19210,19211,19212,19213,19214,19215,19216,19217,19218,19219,19220,19221,19222,19223,19224,19225,19226,19227,19228,19229,19230,19231,19232,19233,19234,19235,19236,19237,19238,19239,19240,19241,19242,19243,19244,19245,19246,19247,19248,19249,19250,19251,19252,19253,19254,19255,19256,19257,19258,19259,19260,19261,19262,19263,19264,19265,19266,19267,19268,19269,19270,19271,19272,19273,19274,19275,19276,19277,19278,19279,19280,19281,19282,19283,19284,19285,19286,19287,19288,19289,19290,19291,19292,19293,19294,19295,19296,19297,19298,19299,19300,19301,19302,19303,19304,19305,19306,19307,19308,19309,19310,19311,19312,19313,19314,19315,19316,19317,19318,19319,19320,19321,19322,19323,19324,19325,19326,19327,19328,19329,19330,19331,19332,19333,19334,19335,19336,19337,19338,19339,19340,19341,19342,19343,19344,19345,19346,19347,19348,19349,19350,19351,19352,19353,19354,19355,19356,19357],"y":[82,82,75,76,75,74,81,81,84,81,73,77,74,72,75,71,66,64,75,78,76,62,57,63,57,63,74,67,55,58,69,76,82,85,83,71,64,70,56,69,72,78,80,76,68,75,84,86,81,77,82,86,87,86,86,84,85,82,80,79,78,83,89,88,89,88,86,86,84,86,79,70,81,84,80,79,87,86,84,85,88,87,77,80,81,81,83,86,88,86,83,83,80,85,88,87,81,76,76,77,85,88,86,86,87,88,88,91,84,85,87,87,88,90,90,87,88,88,89,87,86,87,88,88,90,89,87,89,90,87,86,87,86,88,88,87,89,91,90,88,90,96,92,94,95,92,89,92,92,96,93,89,91,86,88,91,91,92,92,88,91,90,90,92,94,95,95,97,98,94,95,94,92,93,95,92,96,93,95,94,93,91,91,91,94,94,96,93,94,92,93,89,93,94,92,93,93,90,93,93,93,93,95,94,95,94,94,93,95,97,97,97,94,96,92,95,96,95,96,94,94,93,90,92,92,87,87,92,92,93,92,96,94,92,92,95,93,91,92,91,90,91,93,94,91,93,93,94,94,91,87,88,87,90,91,90,87,91,88,92,88,90,92,93,94,93,92,92,93,84,76,77,77,82,86,84,83,83,84,87,88,90,90,89,85,82,88,88,90,86,81,68,73,77,82,86,86,83,83,85,87,87,87,86,88,90,88,86,86,89,87,85,76,74,78,81,78,79,81,79,69,69,75,61,79,78,79,79,82,82,81,78,83,82,77,81,82,80,81,81,84,84,79,80,70,76,80,81,76,68,70,69,74,67,69,69,70,45,46,55,68,75,80,81,71],"text":["Date: 2022-01-01 <br>Type: Daily High <br>Temperature: 82","Date: 2022-01-02 <br>Type: Daily High <br>Temperature: 82","Date: 2022-01-03 <br>Type: Daily High <br>Temperature: 75","Date: 2022-01-04 <br>Type: Daily High <br>Temperature: 76","Date: 2022-01-05 <br>Type: Daily High <br>Temperature: 75","Date: 2022-01-06 <br>Type: Daily High <br>Temperature: 74","Date: 2022-01-07 <br>Type: Daily High <br>Temperature: 81","Date: 2022-01-08 <br>Type: Daily High <br>Temperature: 81","Date: 2022-01-09 <br>Type: Daily High <br>Temperature: 84","Date: 2022-01-10 <br>Type: Daily High <br>Temperature: 81","Date: 2022-01-11 <br>Type: Daily High <br>Temperature: 73","Date: 2022-01-12 <br>Type: Daily High <br>Temperature: 77","Date: 2022-01-13 <br>Type: Daily High <br>Temperature: 74","Date: 2022-01-14 <br>Type: Daily High <br>Temperature: 72","Date: 2022-01-15 <br>Type: Daily High <br>Temperature: 75","Date: 2022-01-16 <br>Type: Daily High <br>Temperature: 71","Date: 2022-01-17 <br>Type: Daily High <br>Temperature: 66","Date: 2022-01-18 <br>Type: Daily High <br>Temperature: 64","Date: 2022-01-19 <br>Type: Daily High <br>Temperature: 75","Date: 2022-01-20 <br>Type: Daily High <br>Temperature: 78","Date: 2022-01-21 <br>Type: Daily High <br>Temperature: 76","Date: 2022-01-22 <br>Type: Daily High <br>Temperature: 62","Date: 2022-01-23 <br>Type: Daily High <br>Temperature: 57","Date: 2022-01-24 <br>Type: Daily High <br>Temperature: 63","Date: 2022-01-25 <br>Type: Daily High <br>Temperature: 57","Date: 2022-01-26 <br>Type: Daily High <br>Temperature: 63","Date: 2022-01-27 <br>Type: Daily High <br>Temperature: 74","Date: 2022-01-28 <br>Type: Daily High <br>Temperature: 67","Date: 2022-01-29 <br>Type: Daily High <br>Temperature: 55","Date: 2022-01-30 <br>Type: Daily High <br>Temperature: 58","Date: 2022-01-31 <br>Type: Daily High <br>Temperature: 69","Date: 2022-02-01 <br>Type: Daily High <br>Temperature: 76","Date: 2022-02-02 <br>Type: Daily High <br>Temperature: 82","Date: 2022-02-03 <br>Type: Daily High <br>Temperature: 85","Date: 2022-02-04 <br>Type: Daily High <br>Temperature: 83","Date: 2022-02-05 <br>Type: Daily High <br>Temperature: 71","Date: 2022-02-06 <br>Type: Daily High <br>Temperature: 64","Date: 2022-02-07 <br>Type: Daily High <br>Temperature: 70","Date: 2022-02-08 <br>Type: Daily High <br>Temperature: 56","Date: 2022-02-09 <br>Type: Daily High <br>Temperature: 69","Date: 2022-02-10 <br>Type: Daily High <br>Temperature: 72","Date: 2022-02-11 <br>Type: Daily High <br>Temperature: 78","Date: 2022-02-12 <br>Type: Daily High <br>Temperature: 80","Date: 2022-02-13 <br>Type: Daily High <br>Temperature: 76","Date: 2022-02-14 <br>Type: Daily High <br>Temperature: 68","Date: 2022-02-15 <br>Type: Daily High <br>Temperature: 75","Date: 2022-02-16 <br>Type: Daily High <br>Temperature: 84","Date: 2022-02-17 <br>Type: Daily High <br>Temperature: 86","Date: 2022-02-18 <br>Type: Daily High <br>Temperature: 81","Date: 2022-02-19 <br>Type: Daily High <br>Temperature: 77","Date: 2022-02-20 <br>Type: Daily High <br>Temperature: 82","Date: 2022-02-21 <br>Type: Daily High <br>Temperature: 86","Date: 2022-02-22 <br>Type: Daily High <br>Temperature: 87","Date: 2022-02-23 <br>Type: Daily High <br>Temperature: 86","Date: 2022-02-24 <br>Type: Daily High <br>Temperature: 86","Date: 2022-02-25 <br>Type: Daily High <br>Temperature: 84","Date: 2022-02-26 <br>Type: Daily High <br>Temperature: 85","Date: 2022-02-27 <br>Type: Daily High <br>Temperature: 82","Date: 2022-02-28 <br>Type: Daily High <br>Temperature: 80","Date: 2022-03-01 <br>Type: Daily High <br>Temperature: 79","Date: 2022-03-02 <br>Type: Daily High <br>Temperature: 78","Date: 2022-03-03 <br>Type: Daily High <br>Temperature: 83","Date: 2022-03-04 <br>Type: Daily High <br>Temperature: 89","Date: 2022-03-05 <br>Type: Daily High <br>Temperature: 88","Date: 2022-03-06 <br>Type: Daily High <br>Temperature: 89","Date: 2022-03-07 <br>Type: Daily High <br>Temperature: 88","Date: 2022-03-08 <br>Type: Daily High <br>Temperature: 86","Date: 2022-03-09 <br>Type: Daily High <br>Temperature: 86","Date: 2022-03-10 <br>Type: Daily High <br>Temperature: 84","Date: 2022-03-11 <br>Type: Daily High <br>Temperature: 86","Date: 2022-03-12 <br>Type: Daily High <br>Temperature: 79","Date: 2022-03-13 <br>Type: Daily High <br>Temperature: 70","Date: 2022-03-14 <br>Type: Daily High <br>Temperature: 81","Date: 2022-03-15 <br>Type: Daily High <br>Temperature: 84","Date: 2022-03-16 <br>Type: Daily High <br>Temperature: 80","Date: 2022-03-17 <br>Type: Daily High <br>Temperature: 79","Date: 2022-03-18 <br>Type: Daily High <br>Temperature: 87","Date: 2022-03-19 <br>Type: Daily High <br>Temperature: 86","Date: 2022-03-20 <br>Type: Daily High <br>Temperature: 84","Date: 2022-03-21 <br>Type: Daily High <br>Temperature: 85","Date: 2022-03-22 <br>Type: Daily High <br>Temperature: 88","Date: 2022-03-23 <br>Type: Daily High <br>Temperature: 87","Date: 2022-03-24 <br>Type: Daily High <br>Temperature: 77","Date: 2022-03-25 <br>Type: Daily High <br>Temperature: 80","Date: 2022-03-26 <br>Type: Daily High <br>Temperature: 81","Date: 2022-03-27 <br>Type: Daily High <br>Temperature: 81","Date: 2022-03-28 <br>Type: Daily High <br>Temperature: 83","Date: 2022-03-29 <br>Type: Daily High <br>Temperature: 86","Date: 2022-03-30 <br>Type: Daily High <br>Temperature: 88","Date: 2022-03-31 <br>Type: Daily High <br>Temperature: 86","Date: 2022-04-01 <br>Type: Daily High <br>Temperature: 83","Date: 2022-04-02 <br>Type: Daily High <br>Temperature: 83","Date: 2022-04-03 <br>Type: Daily High <br>Temperature: 80","Date: 2022-04-04 <br>Type: Daily High <br>Temperature: 85","Date: 2022-04-05 <br>Type: Daily High <br>Temperature: 88","Date: 2022-04-06 <br>Type: Daily High <br>Temperature: 87","Date: 2022-04-07 <br>Type: Daily High <br>Temperature: 81","Date: 2022-04-08 <br>Type: Daily High <br>Temperature: 76","Date: 2022-04-09 <br>Type: Daily High <br>Temperature: 76","Date: 2022-04-10 <br>Type: Daily High <br>Temperature: 77","Date: 2022-04-11 <br>Type: Daily High <br>Temperature: 85","Date: 2022-04-12 <br>Type: Daily High <br>Temperature: 88","Date: 2022-04-13 <br>Type: Daily High <br>Temperature: 86","Date: 2022-04-14 <br>Type: Daily High <br>Temperature: 86","Date: 2022-04-15 <br>Type: Daily High <br>Temperature: 87","Date: 2022-04-16 <br>Type: Daily High <br>Temperature: 88","Date: 2022-04-17 <br>Type: Daily High <br>Temperature: 88","Date: 2022-04-18 <br>Type: Daily High <br>Temperature: 91","Date: 2022-04-19 <br>Type: Daily High <br>Temperature: 84","Date: 2022-04-20 <br>Type: Daily High <br>Temperature: 85","Date: 2022-04-21 <br>Type: Daily High <br>Temperature: 87","Date: 2022-04-22 <br>Type: Daily High <br>Temperature: 87","Date: 2022-04-23 <br>Type: Daily High <br>Temperature: 88","Date: 2022-04-24 <br>Type: Daily High <br>Temperature: 90","Date: 2022-04-25 <br>Type: Daily High <br>Temperature: 90","Date: 2022-04-26 <br>Type: Daily High <br>Temperature: 87","Date: 2022-04-27 <br>Type: Daily High <br>Temperature: 88","Date: 2022-04-28 <br>Type: Daily High <br>Temperature: 88","Date: 2022-04-29 <br>Type: Daily High <br>Temperature: 89","Date: 2022-04-30 <br>Type: Daily High <br>Temperature: 87","Date: 2022-05-01 <br>Type: Daily High <br>Temperature: 86","Date: 2022-05-02 <br>Type: Daily High <br>Temperature: 87","Date: 2022-05-03 <br>Type: Daily High <br>Temperature: 88","Date: 2022-05-04 <br>Type: Daily High <br>Temperature: 88","Date: 2022-05-05 <br>Type: Daily High <br>Temperature: 90","Date: 2022-05-06 <br>Type: Daily High <br>Temperature: 89","Date: 2022-05-07 <br>Type: Daily High <br>Temperature: 87","Date: 2022-05-08 <br>Type: Daily High <br>Temperature: 89","Date: 2022-05-09 <br>Type: Daily High <br>Temperature: 90","Date: 2022-05-10 <br>Type: Daily High <br>Temperature: 87","Date: 2022-05-11 <br>Type: Daily High <br>Temperature: 86","Date: 2022-05-12 <br>Type: Daily High <br>Temperature: 87","Date: 2022-05-13 <br>Type: Daily High <br>Temperature: 86","Date: 2022-05-14 <br>Type: Daily High <br>Temperature: 88","Date: 2022-05-15 <br>Type: Daily High <br>Temperature: 88","Date: 2022-05-16 <br>Type: Daily High <br>Temperature: 87","Date: 2022-05-17 <br>Type: Daily High <br>Temperature: 89","Date: 2022-05-18 <br>Type: Daily High <br>Temperature: 91","Date: 2022-05-19 <br>Type: Daily High <br>Temperature: 90","Date: 2022-05-20 <br>Type: Daily High <br>Temperature: 88","Date: 2022-05-21 <br>Type: Daily High <br>Temperature: 90","Date: 2022-05-22 <br>Type: Daily High <br>Temperature: 96","Date: 2022-05-23 <br>Type: Daily High <br>Temperature: 92","Date: 2022-05-24 <br>Type: Daily High <br>Temperature: 94","Date: 2022-05-25 <br>Type: Daily High <br>Temperature: 95","Date: 2022-05-26 <br>Type: Daily High <br>Temperature: 92","Date: 2022-05-27 <br>Type: Daily High <br>Temperature: 89","Date: 2022-05-28 <br>Type: Daily High <br>Temperature: 92","Date: 2022-05-29 <br>Type: Daily High <br>Temperature: 92","Date: 2022-05-30 <br>Type: Daily High <br>Temperature: 96","Date: 2022-05-31 <br>Type: Daily High <br>Temperature: 93","Date: 2022-06-01 <br>Type: Daily High <br>Temperature: 89","Date: 2022-06-02 <br>Type: Daily High <br>Temperature: 91","Date: 2022-06-03 <br>Type: Daily High <br>Temperature: 86","Date: 2022-06-04 <br>Type: Daily High <br>Temperature: 88","Date: 2022-06-05 <br>Type: Daily High <br>Temperature: 91","Date: 2022-06-06 <br>Type: Daily High <br>Temperature: 91","Date: 2022-06-07 <br>Type: Daily High <br>Temperature: 92","Date: 2022-06-08 <br>Type: Daily High <br>Temperature: 92","Date: 2022-06-09 <br>Type: Daily High <br>Temperature: 88","Date: 2022-06-10 <br>Type: Daily High <br>Temperature: 91","Date: 2022-06-11 <br>Type: Daily High <br>Temperature: 90","Date: 2022-06-12 <br>Type: Daily High <br>Temperature: 90","Date: 2022-06-13 <br>Type: Daily High <br>Temperature: 92","Date: 2022-06-14 <br>Type: Daily High <br>Temperature: 94","Date: 2022-06-15 <br>Type: Daily High <br>Temperature: 95","Date: 2022-06-16 <br>Type: Daily High <br>Temperature: 95","Date: 2022-06-17 <br>Type: Daily High <br>Temperature: 97","Date: 2022-06-18 <br>Type: Daily High <br>Temperature: 98","Date: 2022-06-19 <br>Type: Daily High <br>Temperature: 94","Date: 2022-06-20 <br>Type: Daily High <br>Temperature: 95","Date: 2022-06-21 <br>Type: Daily High <br>Temperature: 94","Date: 2022-06-22 <br>Type: Daily High <br>Temperature: 92","Date: 2022-06-23 <br>Type: Daily High <br>Temperature: 93","Date: 2022-06-24 <br>Type: Daily High <br>Temperature: 95","Date: 2022-06-25 <br>Type: Daily High <br>Temperature: 92","Date: 2022-06-26 <br>Type: Daily High <br>Temperature: 96","Date: 2022-06-27 <br>Type: Daily High <br>Temperature: 93","Date: 2022-06-28 <br>Type: Daily High <br>Temperature: 95","Date: 2022-06-29 <br>Type: Daily High <br>Temperature: 94","Date: 2022-06-30 <br>Type: Daily High <br>Temperature: 93","Date: 2022-07-01 <br>Type: Daily High <br>Temperature: 91","Date: 2022-07-02 <br>Type: Daily High <br>Temperature: 91","Date: 2022-07-03 <br>Type: Daily High <br>Temperature: 91","Date: 2022-07-04 <br>Type: Daily High <br>Temperature: 94","Date: 2022-07-05 <br>Type: Daily High <br>Temperature: 94","Date: 2022-07-06 <br>Type: Daily High <br>Temperature: 96","Date: 2022-07-07 <br>Type: Daily High <br>Temperature: 93","Date: 2022-07-08 <br>Type: Daily High <br>Temperature: 94","Date: 2022-07-09 <br>Type: Daily High <br>Temperature: 92","Date: 2022-07-10 <br>Type: Daily High <br>Temperature: 93","Date: 2022-07-11 <br>Type: Daily High <br>Temperature: 89","Date: 2022-07-12 <br>Type: Daily High <br>Temperature: 93","Date: 2022-07-13 <br>Type: Daily High <br>Temperature: 94","Date: 2022-07-14 <br>Type: Daily High <br>Temperature: 92","Date: 2022-07-15 <br>Type: Daily High <br>Temperature: 93","Date: 2022-07-16 <br>Type: Daily High <br>Temperature: 93","Date: 2022-07-17 <br>Type: Daily High <br>Temperature: 90","Date: 2022-07-18 <br>Type: Daily High <br>Temperature: 93","Date: 2022-07-19 <br>Type: Daily High <br>Temperature: 93","Date: 2022-07-20 <br>Type: Daily High <br>Temperature: 93","Date: 2022-07-21 <br>Type: Daily High <br>Temperature: 93","Date: 2022-07-22 <br>Type: Daily High <br>Temperature: 95","Date: 2022-07-23 <br>Type: Daily High <br>Temperature: 94","Date: 2022-07-24 <br>Type: Daily High <br>Temperature: 95","Date: 2022-07-25 <br>Type: Daily High <br>Temperature: 94","Date: 2022-07-26 <br>Type: Daily High <br>Temperature: 94","Date: 2022-07-27 <br>Type: Daily High <br>Temperature: 93","Date: 2022-07-28 <br>Type: Daily High <br>Temperature: 95","Date: 2022-07-29 <br>Type: Daily High <br>Temperature: 97","Date: 2022-07-30 <br>Type: Daily High <br>Temperature: 97","Date: 2022-07-31 <br>Type: Daily High <br>Temperature: 97","Date: 2022-08-01 <br>Type: Daily High <br>Temperature: 94","Date: 2022-08-02 <br>Type: Daily High <br>Temperature: 96","Date: 2022-08-03 <br>Type: Daily High <br>Temperature: 92","Date: 2022-08-04 <br>Type: Daily High <br>Temperature: 95","Date: 2022-08-05 <br>Type: Daily High <br>Temperature: 96","Date: 2022-08-06 <br>Type: Daily High <br>Temperature: 95","Date: 2022-08-07 <br>Type: Daily High <br>Temperature: 96","Date: 2022-08-08 <br>Type: Daily High <br>Temperature: 94","Date: 2022-08-09 <br>Type: Daily High <br>Temperature: 94","Date: 2022-08-10 <br>Type: Daily High <br>Temperature: 93","Date: 2022-08-11 <br>Type: Daily High <br>Temperature: 90","Date: 2022-08-12 <br>Type: Daily High <br>Temperature: 92","Date: 2022-08-13 <br>Type: Daily High <br>Temperature: 92","Date: 2022-08-14 <br>Type: Daily High <br>Temperature: 87","Date: 2022-08-15 <br>Type: Daily High <br>Temperature: 87","Date: 2022-08-16 <br>Type: Daily High <br>Temperature: 92","Date: 2022-08-17 <br>Type: Daily High <br>Temperature: 92","Date: 2022-08-18 <br>Type: Daily High <br>Temperature: 93","Date: 2022-08-19 <br>Type: Daily High <br>Temperature: 92","Date: 2022-08-20 <br>Type: Daily High <br>Temperature: 96","Date: 2022-08-21 <br>Type: Daily High <br>Temperature: 94","Date: 2022-08-22 <br>Type: Daily High <br>Temperature: 92","Date: 2022-08-23 <br>Type: Daily High <br>Temperature: 92","Date: 2022-08-24 <br>Type: Daily High <br>Temperature: 95","Date: 2022-08-25 <br>Type: Daily High <br>Temperature: 93","Date: 2022-08-26 <br>Type: Daily High <br>Temperature: 91","Date: 2022-08-27 <br>Type: Daily High <br>Temperature: 92","Date: 2022-08-28 <br>Type: Daily High <br>Temperature: 91","Date: 2022-08-29 <br>Type: Daily High <br>Temperature: 90","Date: 2022-08-30 <br>Type: Daily High <br>Temperature: 91","Date: 2022-08-31 <br>Type: Daily High <br>Temperature: 93","Date: 2022-09-01 <br>Type: Daily High <br>Temperature: 94","Date: 2022-09-02 <br>Type: Daily High <br>Temperature: 91","Date: 2022-09-03 <br>Type: Daily High <br>Temperature: 93","Date: 2022-09-04 <br>Type: Daily High <br>Temperature: 93","Date: 2022-09-05 <br>Type: Daily High <br>Temperature: 94","Date: 2022-09-06 <br>Type: Daily High <br>Temperature: 94","Date: 2022-09-07 <br>Type: Daily High <br>Temperature: 91","Date: 2022-09-08 <br>Type: Daily High <br>Temperature: 87","Date: 2022-09-09 <br>Type: Daily High <br>Temperature: 88","Date: 2022-09-10 <br>Type: Daily High <br>Temperature: 87","Date: 2022-09-11 <br>Type: Daily High <br>Temperature: 90","Date: 2022-09-12 <br>Type: Daily High <br>Temperature: 91","Date: 2022-09-13 <br>Type: Daily High <br>Temperature: 90","Date: 2022-09-14 <br>Type: Daily High <br>Temperature: 87","Date: 2022-09-15 <br>Type: Daily High <br>Temperature: 91","Date: 2022-09-16 <br>Type: Daily High <br>Temperature: 88","Date: 2022-09-17 <br>Type: Daily High <br>Temperature: 92","Date: 2022-09-18 <br>Type: Daily High <br>Temperature: 88","Date: 2022-09-19 <br>Type: Daily High <br>Temperature: 90","Date: 2022-09-20 <br>Type: Daily High <br>Temperature: 92","Date: 2022-09-21 <br>Type: Daily High <br>Temperature: 93","Date: 2022-09-22 <br>Type: Daily High <br>Temperature: 94","Date: 2022-09-23 <br>Type: Daily High <br>Temperature: 93","Date: 2022-09-24 <br>Type: Daily High <br>Temperature: 92","Date: 2022-09-25 <br>Type: Daily High <br>Temperature: 92","Date: 2022-09-26 <br>Type: Daily High <br>Temperature: 93","Date: 2022-09-27 <br>Type: Daily High <br>Temperature: 84","Date: 2022-09-28 <br>Type: Daily High <br>Temperature: 76","Date: 2022-09-29 <br>Type: Daily High <br>Temperature: 77","Date: 2022-09-30 <br>Type: Daily High <br>Temperature: 77","Date: 2022-10-01 <br>Type: Daily High <br>Temperature: 82","Date: 2022-10-02 <br>Type: Daily High <br>Temperature: 86","Date: 2022-10-03 <br>Type: Daily High <br>Temperature: 84","Date: 2022-10-04 <br>Type: Daily High <br>Temperature: 83","Date: 2022-10-05 <br>Type: Daily High <br>Temperature: 83","Date: 2022-10-06 <br>Type: Daily High <br>Temperature: 84","Date: 2022-10-07 <br>Type: Daily High <br>Temperature: 87","Date: 2022-10-08 <br>Type: Daily High <br>Temperature: 88","Date: 2022-10-09 <br>Type: Daily High <br>Temperature: 90","Date: 2022-10-10 <br>Type: Daily High <br>Temperature: 90","Date: 2022-10-11 <br>Type: Daily High <br>Temperature: 89","Date: 2022-10-12 <br>Type: Daily High <br>Temperature: 85","Date: 2022-10-13 <br>Type: Daily High <br>Temperature: 82","Date: 2022-10-14 <br>Type: Daily High <br>Temperature: 88","Date: 2022-10-15 <br>Type: Daily High <br>Temperature: 88","Date: 2022-10-16 <br>Type: Daily High <br>Temperature: 90","Date: 2022-10-17 <br>Type: Daily High <br>Temperature: 86","Date: 2022-10-18 <br>Type: Daily High <br>Temperature: 81","Date: 2022-10-19 <br>Type: Daily High <br>Temperature: 68","Date: 2022-10-20 <br>Type: Daily High <br>Temperature: 73","Date: 2022-10-21 <br>Type: Daily High <br>Temperature: 77","Date: 2022-10-22 <br>Type: Daily High <br>Temperature: 82","Date: 2022-10-23 <br>Type: Daily High <br>Temperature: 86","Date: 2022-10-24 <br>Type: Daily High <br>Temperature: 86","Date: 2022-10-25 <br>Type: Daily High <br>Temperature: 83","Date: 2022-10-26 <br>Type: Daily High <br>Temperature: 83","Date: 2022-10-27 <br>Type: Daily High <br>Temperature: 85","Date: 2022-10-28 <br>Type: Daily High <br>Temperature: 87","Date: 2022-10-29 <br>Type: Daily High <br>Temperature: 87","Date: 2022-10-30 <br>Type: Daily High <br>Temperature: 87","Date: 2022-10-31 <br>Type: Daily High <br>Temperature: 86","Date: 2022-11-01 <br>Type: Daily High <br>Temperature: 88","Date: 2022-11-02 <br>Type: Daily High <br>Temperature: 90","Date: 2022-11-03 <br>Type: Daily High <br>Temperature: 88","Date: 2022-11-04 <br>Type: Daily High <br>Temperature: 86","Date: 2022-11-05 <br>Type: Daily High <br>Temperature: 86","Date: 2022-11-06 <br>Type: Daily High <br>Temperature: 89","Date: 2022-11-07 <br>Type: Daily High <br>Temperature: 87","Date: 2022-11-08 <br>Type: Daily High <br>Temperature: 85","Date: 2022-11-09 <br>Type: Daily High <br>Temperature: 76","Date: 2022-11-10 <br>Type: Daily High <br>Temperature: 74","Date: 2022-11-11 <br>Type: Daily High <br>Temperature: 78","Date: 2022-11-12 <br>Type: Daily High <br>Temperature: 81","Date: 2022-11-13 <br>Type: Daily High <br>Temperature: 78","Date: 2022-11-14 <br>Type: Daily High <br>Temperature: 79","Date: 2022-11-15 <br>Type: Daily High <br>Temperature: 81","Date: 2022-11-16 <br>Type: Daily High <br>Temperature: 79","Date: 2022-11-17 <br>Type: Daily High <br>Temperature: 69","Date: 2022-11-18 <br>Type: Daily High <br>Temperature: 69","Date: 2022-11-19 <br>Type: Daily High <br>Temperature: 75","Date: 2022-11-20 <br>Type: Daily High <br>Temperature: 61","Date: 2022-11-21 <br>Type: Daily High <br>Temperature: 79","Date: 2022-11-22 <br>Type: Daily High <br>Temperature: 78","Date: 2022-11-23 <br>Type: Daily High <br>Temperature: 79","Date: 2022-11-24 <br>Type: Daily High <br>Temperature: 79","Date: 2022-11-25 <br>Type: Daily High <br>Temperature: 82","Date: 2022-11-26 <br>Type: Daily High <br>Temperature: 82","Date: 2022-11-27 <br>Type: Daily High <br>Temperature: 81","Date: 2022-11-28 <br>Type: Daily High <br>Temperature: 78","Date: 2022-11-29 <br>Type: Daily High <br>Temperature: 83","Date: 2022-11-30 <br>Type: Daily High <br>Temperature: 82","Date: 2022-12-01 <br>Type: Daily High <br>Temperature: 77","Date: 2022-12-02 <br>Type: Daily High <br>Temperature: 81","Date: 2022-12-03 <br>Type: Daily High <br>Temperature: 82","Date: 2022-12-04 <br>Type: Daily High <br>Temperature: 80","Date: 2022-12-05 <br>Type: Daily High <br>Temperature: 81","Date: 2022-12-06 <br>Type: Daily High <br>Temperature: 81","Date: 2022-12-07 <br>Type: Daily High <br>Temperature: 84","Date: 2022-12-08 <br>Type: Daily High <br>Temperature: 84","Date: 2022-12-09 <br>Type: Daily High <br>Temperature: 79","Date: 2022-12-10 <br>Type: Daily High <br>Temperature: 80","Date: 2022-12-11 <br>Type: Daily High <br>Temperature: 70","Date: 2022-12-12 <br>Type: Daily High <br>Temperature: 76","Date: 2022-12-13 <br>Type: Daily High <br>Temperature: 80","Date: 2022-12-14 <br>Type: Daily High <br>Temperature: 81","Date: 2022-12-15 <br>Type: Daily High <br>Temperature: 76","Date: 2022-12-16 <br>Type: Daily High <br>Temperature: 68","Date: 2022-12-17 <br>Type: Daily High <br>Temperature: 70","Date: 2022-12-18 <br>Type: Daily High <br>Temperature: 69","Date: 2022-12-19 <br>Type: Daily High <br>Temperature: 74","Date: 2022-12-20 <br>Type: Daily High <br>Temperature: 67","Date: 2022-12-21 <br>Type: Daily High <br>Temperature: 69","Date: 2022-12-22 <br>Type: Daily High <br>Temperature: 69","Date: 2022-12-23 <br>Type: Daily High <br>Temperature: 70","Date: 2022-12-24 <br>Type: Daily High <br>Temperature: 45","Date: 2022-12-25 <br>Type: Daily High <br>Temperature: 46","Date: 2022-12-26 <br>Type: Daily High <br>Temperature: 55","Date: 2022-12-27 <br>Type: Daily High <br>Temperature: 68","Date: 2022-12-28 <br>Type: Daily High <br>Temperature: 75","Date: 2022-12-29 <br>Type: Daily High <br>Temperature: 80","Date: 2022-12-30 <br>Type: Daily High <br>Temperature: 81","Date: 2022-12-31 <br>Type: Daily High <br>Temperature: 71"],"type":"scatter","mode":"lines","line":{"width":4.1574803149606305,"color":"rgba(0,0,0,0.9)","dash":"solid"},"hoveron":"points","name":"Daily High","legendgroup":"Daily High","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[18993,18994,18995,18996,18997,18998,18999,19000,19001,19002,19003,19004,19005,19006,19007,19008,19009,19010,19011,19012,19013,19014,19015,19016,19017,19018,19019,19020,19021,19022,19023,19024,19025,19026,19027,19028,19029,19030,19031,19032,19033,19034,19035,19036,19037,19038,19039,19040,19041,19042,19043,19044,19045,19046,19047,19048,19049,19050,19051,19052,19053,19054,19055,19056,19057,19058,19059,19060,19061,19062,19063,19064,19065,19066,19067,19068,19069,19070,19071,19072,19073,19074,19075,19076,19077,19078,19079,19080,19081,19082,19083,19084,19085,19086,19087,19088,19089,19090,19091,19092,19093,19094,19095,19096,19097,19098,19099,19100,19101,19102,19103,19104,19105,19106,19107,19108,19109,19110,19111,19112,19113,19114,19115,19116,19117,19118,19119,19120,19121,19122,19123,19124,19125,19126,19127,19128,19129,19130,19131,19132,19133,19134,19135,19136,19137,19138,19139,19140,19141,19142,19143,19144,19145,19146,19147,19148,19149,19150,19151,19152,19153,19154,19155,19156,19157,19158,19159,19160,19161,19162,19163,19164,19165,19166,19167,19168,19169,19170,19171,19172,19173,19174,19175,19176,19177,19178,19179,19180,19181,19182,19183,19184,19185,19186,19187,19188,19189,19190,19191,19192,19193,19194,19195,19196,19197,19198,19199,19200,19201,19202,19203,19204,19205,19206,19207,19208,19209,19210,19211,19212,19213,19214,19215,19216,19217,19218,19219,19220,19221,19222,19223,19224,19225,19226,19227,19228,19229,19230,19231,19232,19233,19234,19235,19236,19237,19238,19239,19240,19241,19242,19243,19244,19245,19246,19247,19248,19249,19250,19251,19252,19253,19254,19255,19256,19257,19258,19259,19260,19261,19262,19263,19264,19265,19266,19267,19268,19269,19270,19271,19272,19273,19274,19275,19276,19277,19278,19279,19280,19281,19282,19283,19284,19285,19286,19287,19288,19289,19290,19291,19292,19293,19294,19295,19296,19297,19298,19299,19300,19301,19302,19303,19304,19305,19306,19307,19308,19309,19310,19311,19312,19313,19314,19315,19316,19317,19318,19319,19320,19321,19322,19323,19324,19325,19326,19327,19328,19329,19330,19331,19332,19333,19334,19335,19336,19337,19338,19339,19340,19341,19342,19343,19344,19345,19346,19347,19348,19349,19350,19351,19352,19353,19354,19355,19356,19357],"y":[67,71,55,50,59,56,63,58,65,64,54,54,59,55,50,62,56,42,47,53,60,47,46,40,50,54,59,54,41,36,39,49,57,66,67,55,54,54,53,49,47,52,55,56,46,47,59,67,68,60,54,63,67,70,68,68,67,69,64,60,58,59,65,64,66,71,74,75,72,72,48,41,54,67,68,65,65,69,65,57,66,73,63,62,60,63,67,64,69,73,68,65,68,65,72,77,71,66,58,56,62,66,70,75,73,73,75,73,66,61,67,67,68,69,71,72,66,70,69,67,69,71,71,73,76,76,74,75,71,65,64,66,70,72,72,73,75,76,77,75,74,74,79,79,77,78,78,77,72,73,73,75,73,76,75,75,77,77,81,79,80,75,77,81,80,78,79,80,81,80,78,77,77,79,79,78,78,76,77,78,77,78,80,79,78,81,80,81,79,81,78,79,82,80,73,74,76,77,81,83,83,80,77,77,79,81,76,77,81,82,81,81,83,81,76,76,78,80,78,77,75,77,79,80,80,80,79,80,80,78,76,79,77,81,80,77,77,77,79,78,78,80,80,75,73,76,80,80,80,78,74,75,76,77,77,78,76,75,74,75,74,77,76,77,77,77,76,73,76,76,73,65,65,68,67,66,63,64,67,67,65,66,74,74,74,73,71,74,71,75,64,53,49,54,55,63,66,69,69,74,72,71,71,70,72,76,71,67,69,73,70,69,67,67,71,67,65,62,68,62,53,52,53,52,54,65,65,67,68,71,67,64,61,67,63,62,63,61,63,66,67,65,63,62,64,59,60,66,64,56,54,51,46,58,60,59,42,31,31,38,40,48,56,61,66],"text":["Date: 2022-01-01 <br>Type: Daily Low <br>Temperature: 67","Date: 2022-01-02 <br>Type: Daily Low <br>Temperature: 71","Date: 2022-01-03 <br>Type: Daily Low <br>Temperature: 55","Date: 2022-01-04 <br>Type: Daily Low <br>Temperature: 50","Date: 2022-01-05 <br>Type: Daily Low <br>Temperature: 59","Date: 2022-01-06 <br>Type: Daily Low <br>Temperature: 56","Date: 2022-01-07 <br>Type: Daily Low <br>Temperature: 63","Date: 2022-01-08 <br>Type: Daily Low <br>Temperature: 58","Date: 2022-01-09 <br>Type: Daily Low <br>Temperature: 65","Date: 2022-01-10 <br>Type: Daily Low <br>Temperature: 64","Date: 2022-01-11 <br>Type: Daily Low <br>Temperature: 54","Date: 2022-01-12 <br>Type: Daily Low <br>Temperature: 54","Date: 2022-01-13 <br>Type: Daily Low <br>Temperature: 59","Date: 2022-01-14 <br>Type: Daily Low <br>Temperature: 55","Date: 2022-01-15 <br>Type: Daily Low <br>Temperature: 50","Date: 2022-01-16 <br>Type: Daily Low <br>Temperature: 62","Date: 2022-01-17 <br>Type: Daily Low <br>Temperature: 56","Date: 2022-01-18 <br>Type: Daily Low <br>Temperature: 42","Date: 2022-01-19 <br>Type: Daily Low <br>Temperature: 47","Date: 2022-01-20 <br>Type: Daily Low <br>Temperature: 53","Date: 2022-01-21 <br>Type: Daily Low <br>Temperature: 60","Date: 2022-01-22 <br>Type: Daily Low <br>Temperature: 47","Date: 2022-01-23 <br>Type: Daily Low <br>Temperature: 46","Date: 2022-01-24 <br>Type: Daily Low <br>Temperature: 40","Date: 2022-01-25 <br>Type: Daily Low <br>Temperature: 50","Date: 2022-01-26 <br>Type: Daily Low <br>Temperature: 54","Date: 2022-01-27 <br>Type: Daily Low <br>Temperature: 59","Date: 2022-01-28 <br>Type: Daily Low <br>Temperature: 54","Date: 2022-01-29 <br>Type: Daily Low <br>Temperature: 41","Date: 2022-01-30 <br>Type: Daily Low <br>Temperature: 36","Date: 2022-01-31 <br>Type: Daily Low <br>Temperature: 39","Date: 2022-02-01 <br>Type: Daily Low <br>Temperature: 49","Date: 2022-02-02 <br>Type: Daily Low <br>Temperature: 57","Date: 2022-02-03 <br>Type: Daily Low <br>Temperature: 66","Date: 2022-02-04 <br>Type: Daily Low <br>Temperature: 67","Date: 2022-02-05 <br>Type: Daily Low <br>Temperature: 55","Date: 2022-02-06 <br>Type: Daily Low <br>Temperature: 54","Date: 2022-02-07 <br>Type: Daily Low <br>Temperature: 54","Date: 2022-02-08 <br>Type: Daily Low <br>Temperature: 53","Date: 2022-02-09 <br>Type: Daily Low <br>Temperature: 49","Date: 2022-02-10 <br>Type: Daily Low <br>Temperature: 47","Date: 2022-02-11 <br>Type: Daily Low <br>Temperature: 52","Date: 2022-02-12 <br>Type: Daily Low <br>Temperature: 55","Date: 2022-02-13 <br>Type: Daily Low <br>Temperature: 56","Date: 2022-02-14 <br>Type: Daily Low <br>Temperature: 46","Date: 2022-02-15 <br>Type: Daily Low <br>Temperature: 47","Date: 2022-02-16 <br>Type: Daily Low <br>Temperature: 59","Date: 2022-02-17 <br>Type: Daily Low <br>Temperature: 67","Date: 2022-02-18 <br>Type: Daily Low <br>Temperature: 68","Date: 2022-02-19 <br>Type: Daily Low <br>Temperature: 60","Date: 2022-02-20 <br>Type: Daily Low <br>Temperature: 54","Date: 2022-02-21 <br>Type: Daily Low <br>Temperature: 63","Date: 2022-02-22 <br>Type: Daily Low <br>Temperature: 67","Date: 2022-02-23 <br>Type: Daily Low <br>Temperature: 70","Date: 2022-02-24 <br>Type: Daily Low <br>Temperature: 68","Date: 2022-02-25 <br>Type: Daily Low <br>Temperature: 68","Date: 2022-02-26 <br>Type: Daily Low <br>Temperature: 67","Date: 2022-02-27 <br>Type: Daily Low <br>Temperature: 69","Date: 2022-02-28 <br>Type: Daily Low <br>Temperature: 64","Date: 2022-03-01 <br>Type: Daily Low <br>Temperature: 60","Date: 2022-03-02 <br>Type: Daily Low <br>Temperature: 58","Date: 2022-03-03 <br>Type: Daily Low <br>Temperature: 59","Date: 2022-03-04 <br>Type: Daily Low <br>Temperature: 65","Date: 2022-03-05 <br>Type: Daily Low <br>Temperature: 64","Date: 2022-03-06 <br>Type: Daily Low <br>Temperature: 66","Date: 2022-03-07 <br>Type: Daily Low <br>Temperature: 71","Date: 2022-03-08 <br>Type: Daily Low <br>Temperature: 74","Date: 2022-03-09 <br>Type: Daily Low <br>Temperature: 75","Date: 2022-03-10 <br>Type: Daily Low <br>Temperature: 72","Date: 2022-03-11 <br>Type: Daily Low <br>Temperature: 72","Date: 2022-03-12 <br>Type: Daily Low <br>Temperature: 48","Date: 2022-03-13 <br>Type: Daily Low <br>Temperature: 41","Date: 2022-03-14 <br>Type: Daily Low <br>Temperature: 54","Date: 2022-03-15 <br>Type: Daily Low <br>Temperature: 67","Date: 2022-03-16 <br>Type: Daily Low <br>Temperature: 68","Date: 2022-03-17 <br>Type: Daily Low <br>Temperature: 65","Date: 2022-03-18 <br>Type: Daily Low <br>Temperature: 65","Date: 2022-03-19 <br>Type: Daily Low <br>Temperature: 69","Date: 2022-03-20 <br>Type: Daily Low <br>Temperature: 65","Date: 2022-03-21 <br>Type: Daily Low <br>Temperature: 57","Date: 2022-03-22 <br>Type: Daily Low <br>Temperature: 66","Date: 2022-03-23 <br>Type: Daily Low <br>Temperature: 73","Date: 2022-03-24 <br>Type: Daily Low <br>Temperature: 63","Date: 2022-03-25 <br>Type: Daily Low <br>Temperature: 62","Date: 2022-03-26 <br>Type: Daily Low <br>Temperature: 60","Date: 2022-03-27 <br>Type: Daily Low <br>Temperature: 63","Date: 2022-03-28 <br>Type: Daily Low <br>Temperature: 67","Date: 2022-03-29 <br>Type: Daily Low <br>Temperature: 64","Date: 2022-03-30 <br>Type: Daily Low <br>Temperature: 69","Date: 2022-03-31 <br>Type: Daily Low <br>Temperature: 73","Date: 2022-04-01 <br>Type: Daily Low <br>Temperature: 68","Date: 2022-04-02 <br>Type: Daily Low <br>Temperature: 65","Date: 2022-04-03 <br>Type: Daily Low <br>Temperature: 68","Date: 2022-04-04 <br>Type: Daily Low <br>Temperature: 65","Date: 2022-04-05 <br>Type: Daily Low <br>Temperature: 72","Date: 2022-04-06 <br>Type: Daily Low <br>Temperature: 77","Date: 2022-04-07 <br>Type: Daily Low <br>Temperature: 71","Date: 2022-04-08 <br>Type: Daily Low <br>Temperature: 66","Date: 2022-04-09 <br>Type: Daily Low <br>Temperature: 58","Date: 2022-04-10 <br>Type: Daily Low <br>Temperature: 56","Date: 2022-04-11 <br>Type: Daily Low <br>Temperature: 62","Date: 2022-04-12 <br>Type: Daily Low <br>Temperature: 66","Date: 2022-04-13 <br>Type: Daily Low <br>Temperature: 70","Date: 2022-04-14 <br>Type: Daily Low <br>Temperature: 75","Date: 2022-04-15 <br>Type: Daily Low <br>Temperature: 73","Date: 2022-04-16 <br>Type: Daily Low <br>Temperature: 73","Date: 2022-04-17 <br>Type: Daily Low <br>Temperature: 75","Date: 2022-04-18 <br>Type: Daily Low <br>Temperature: 73","Date: 2022-04-19 <br>Type: Daily Low <br>Temperature: 66","Date: 2022-04-20 <br>Type: Daily Low <br>Temperature: 61","Date: 2022-04-21 <br>Type: Daily Low <br>Temperature: 67","Date: 2022-04-22 <br>Type: Daily Low <br>Temperature: 67","Date: 2022-04-23 <br>Type: Daily Low <br>Temperature: 68","Date: 2022-04-24 <br>Type: Daily Low <br>Temperature: 69","Date: 2022-04-25 <br>Type: Daily Low <br>Temperature: 71","Date: 2022-04-26 <br>Type: Daily Low <br>Temperature: 72","Date: 2022-04-27 <br>Type: Daily Low <br>Temperature: 66","Date: 2022-04-28 <br>Type: Daily Low <br>Temperature: 70","Date: 2022-04-29 <br>Type: Daily Low <br>Temperature: 69","Date: 2022-04-30 <br>Type: Daily Low <br>Temperature: 67","Date: 2022-05-01 <br>Type: Daily Low <br>Temperature: 69","Date: 2022-05-02 <br>Type: Daily Low <br>Temperature: 71","Date: 2022-05-03 <br>Type: Daily Low <br>Temperature: 71","Date: 2022-05-04 <br>Type: Daily Low <br>Temperature: 73","Date: 2022-05-05 <br>Type: Daily Low <br>Temperature: 76","Date: 2022-05-06 <br>Type: Daily Low <br>Temperature: 76","Date: 2022-05-07 <br>Type: Daily Low <br>Temperature: 74","Date: 2022-05-08 <br>Type: Daily Low <br>Temperature: 75","Date: 2022-05-09 <br>Type: Daily Low <br>Temperature: 71","Date: 2022-05-10 <br>Type: Daily Low <br>Temperature: 65","Date: 2022-05-11 <br>Type: Daily Low <br>Temperature: 64","Date: 2022-05-12 <br>Type: Daily Low <br>Temperature: 66","Date: 2022-05-13 <br>Type: Daily Low <br>Temperature: 70","Date: 2022-05-14 <br>Type: Daily Low <br>Temperature: 72","Date: 2022-05-15 <br>Type: Daily Low <br>Temperature: 72","Date: 2022-05-16 <br>Type: Daily Low <br>Temperature: 73","Date: 2022-05-17 <br>Type: Daily Low <br>Temperature: 75","Date: 2022-05-18 <br>Type: Daily Low <br>Temperature: 76","Date: 2022-05-19 <br>Type: Daily Low <br>Temperature: 77","Date: 2022-05-20 <br>Type: Daily Low <br>Temperature: 75","Date: 2022-05-21 <br>Type: Daily Low <br>Temperature: 74","Date: 2022-05-22 <br>Type: Daily Low <br>Temperature: 74","Date: 2022-05-23 <br>Type: Daily Low <br>Temperature: 79","Date: 2022-05-24 <br>Type: Daily Low <br>Temperature: 79","Date: 2022-05-25 <br>Type: Daily Low <br>Temperature: 77","Date: 2022-05-26 <br>Type: Daily Low <br>Temperature: 78","Date: 2022-05-27 <br>Type: Daily Low <br>Temperature: 78","Date: 2022-05-28 <br>Type: Daily Low <br>Temperature: 77","Date: 2022-05-29 <br>Type: Daily Low <br>Temperature: 72","Date: 2022-05-30 <br>Type: Daily Low <br>Temperature: 73","Date: 2022-05-31 <br>Type: Daily Low <br>Temperature: 73","Date: 2022-06-01 <br>Type: Daily Low <br>Temperature: 75","Date: 2022-06-02 <br>Type: Daily Low <br>Temperature: 73","Date: 2022-06-03 <br>Type: Daily Low <br>Temperature: 76","Date: 2022-06-04 <br>Type: Daily Low <br>Temperature: 75","Date: 2022-06-05 <br>Type: Daily Low <br>Temperature: 75","Date: 2022-06-06 <br>Type: Daily Low <br>Temperature: 77","Date: 2022-06-07 <br>Type: Daily Low <br>Temperature: 77","Date: 2022-06-08 <br>Type: Daily Low <br>Temperature: 81","Date: 2022-06-09 <br>Type: Daily Low <br>Temperature: 79","Date: 2022-06-10 <br>Type: Daily Low <br>Temperature: 80","Date: 2022-06-11 <br>Type: Daily Low <br>Temperature: 75","Date: 2022-06-12 <br>Type: Daily Low <br>Temperature: 77","Date: 2022-06-13 <br>Type: Daily Low <br>Temperature: 81","Date: 2022-06-14 <br>Type: Daily Low <br>Temperature: 80","Date: 2022-06-15 <br>Type: Daily Low <br>Temperature: 78","Date: 2022-06-16 <br>Type: Daily Low <br>Temperature: 79","Date: 2022-06-17 <br>Type: Daily Low <br>Temperature: 80","Date: 2022-06-18 <br>Type: Daily Low <br>Temperature: 81","Date: 2022-06-19 <br>Type: Daily Low <br>Temperature: 80","Date: 2022-06-20 <br>Type: Daily Low <br>Temperature: 78","Date: 2022-06-21 <br>Type: Daily Low <br>Temperature: 77","Date: 2022-06-22 <br>Type: Daily Low <br>Temperature: 77","Date: 2022-06-23 <br>Type: Daily Low <br>Temperature: 79","Date: 2022-06-24 <br>Type: Daily Low <br>Temperature: 79","Date: 2022-06-25 <br>Type: Daily Low <br>Temperature: 78","Date: 2022-06-26 <br>Type: Daily Low <br>Temperature: 78","Date: 2022-06-27 <br>Type: Daily Low <br>Temperature: 76","Date: 2022-06-28 <br>Type: Daily Low <br>Temperature: 77","Date: 2022-06-29 <br>Type: Daily Low <br>Temperature: 78","Date: 2022-06-30 <br>Type: Daily Low <br>Temperature: 77","Date: 2022-07-01 <br>Type: Daily Low <br>Temperature: 78","Date: 2022-07-02 <br>Type: Daily Low <br>Temperature: 80","Date: 2022-07-03 <br>Type: Daily Low <br>Temperature: 79","Date: 2022-07-04 <br>Type: Daily Low <br>Temperature: 78","Date: 2022-07-05 <br>Type: Daily Low <br>Temperature: 81","Date: 2022-07-06 <br>Type: Daily Low <br>Temperature: 80","Date: 2022-07-07 <br>Type: Daily Low <br>Temperature: 81","Date: 2022-07-08 <br>Type: Daily Low <br>Temperature: 79","Date: 2022-07-09 <br>Type: Daily Low <br>Temperature: 81","Date: 2022-07-10 <br>Type: Daily Low <br>Temperature: 78","Date: 2022-07-11 <br>Type: Daily Low <br>Temperature: 79","Date: 2022-07-12 <br>Type: Daily Low <br>Temperature: 82","Date: 2022-07-13 <br>Type: Daily Low <br>Temperature: 80","Date: 2022-07-14 <br>Type: Daily Low <br>Temperature: 73","Date: 2022-07-15 <br>Type: Daily Low <br>Temperature: 74","Date: 2022-07-16 <br>Type: Daily Low <br>Temperature: 76","Date: 2022-07-17 <br>Type: Daily Low <br>Temperature: 77","Date: 2022-07-18 <br>Type: Daily Low <br>Temperature: 81","Date: 2022-07-19 <br>Type: Daily Low <br>Temperature: 83","Date: 2022-07-20 <br>Type: Daily Low <br>Temperature: 83","Date: 2022-07-21 <br>Type: Daily Low <br>Temperature: 80","Date: 2022-07-22 <br>Type: Daily Low <br>Temperature: 77","Date: 2022-07-23 <br>Type: Daily Low <br>Temperature: 77","Date: 2022-07-24 <br>Type: Daily Low <br>Temperature: 79","Date: 2022-07-25 <br>Type: Daily Low <br>Temperature: 81","Date: 2022-07-26 <br>Type: Daily Low <br>Temperature: 76","Date: 2022-07-27 <br>Type: Daily Low <br>Temperature: 77","Date: 2022-07-28 <br>Type: Daily Low <br>Temperature: 81","Date: 2022-07-29 <br>Type: Daily Low <br>Temperature: 82","Date: 2022-07-30 <br>Type: Daily Low <br>Temperature: 81","Date: 2022-07-31 <br>Type: Daily Low <br>Temperature: 81","Date: 2022-08-01 <br>Type: Daily Low <br>Temperature: 83","Date: 2022-08-02 <br>Type: Daily Low <br>Temperature: 81","Date: 2022-08-03 <br>Type: Daily Low <br>Temperature: 76","Date: 2022-08-04 <br>Type: Daily Low <br>Temperature: 76","Date: 2022-08-05 <br>Type: Daily Low <br>Temperature: 78","Date: 2022-08-06 <br>Type: Daily Low <br>Temperature: 80","Date: 2022-08-07 <br>Type: Daily Low <br>Temperature: 78","Date: 2022-08-08 <br>Type: Daily Low <br>Temperature: 77","Date: 2022-08-09 <br>Type: Daily Low <br>Temperature: 75","Date: 2022-08-10 <br>Type: Daily Low <br>Temperature: 77","Date: 2022-08-11 <br>Type: Daily Low <br>Temperature: 79","Date: 2022-08-12 <br>Type: Daily Low <br>Temperature: 80","Date: 2022-08-13 <br>Type: Daily Low <br>Temperature: 80","Date: 2022-08-14 <br>Type: Daily Low <br>Temperature: 80","Date: 2022-08-15 <br>Type: Daily Low <br>Temperature: 79","Date: 2022-08-16 <br>Type: Daily Low <br>Temperature: 80","Date: 2022-08-17 <br>Type: Daily Low <br>Temperature: 80","Date: 2022-08-18 <br>Type: Daily Low <br>Temperature: 78","Date: 2022-08-19 <br>Type: Daily Low <br>Temperature: 76","Date: 2022-08-20 <br>Type: Daily Low <br>Temperature: 79","Date: 2022-08-21 <br>Type: Daily Low <br>Temperature: 77","Date: 2022-08-22 <br>Type: Daily Low <br>Temperature: 81","Date: 2022-08-23 <br>Type: Daily Low <br>Temperature: 80","Date: 2022-08-24 <br>Type: Daily Low <br>Temperature: 77","Date: 2022-08-25 <br>Type: Daily Low <br>Temperature: 77","Date: 2022-08-26 <br>Type: Daily Low <br>Temperature: 77","Date: 2022-08-27 <br>Type: Daily Low <br>Temperature: 79","Date: 2022-08-28 <br>Type: Daily Low <br>Temperature: 78","Date: 2022-08-29 <br>Type: Daily Low <br>Temperature: 78","Date: 2022-08-30 <br>Type: Daily Low <br>Temperature: 80","Date: 2022-08-31 <br>Type: Daily Low <br>Temperature: 80","Date: 2022-09-01 <br>Type: Daily Low <br>Temperature: 75","Date: 2022-09-02 <br>Type: Daily Low <br>Temperature: 73","Date: 2022-09-03 <br>Type: Daily Low <br>Temperature: 76","Date: 2022-09-04 <br>Type: Daily Low <br>Temperature: 80","Date: 2022-09-05 <br>Type: Daily Low <br>Temperature: 80","Date: 2022-09-06 <br>Type: Daily Low <br>Temperature: 80","Date: 2022-09-07 <br>Type: Daily Low <br>Temperature: 78","Date: 2022-09-08 <br>Type: Daily Low <br>Temperature: 74","Date: 2022-09-09 <br>Type: Daily Low <br>Temperature: 75","Date: 2022-09-10 <br>Type: Daily Low <br>Temperature: 76","Date: 2022-09-11 <br>Type: Daily Low <br>Temperature: 77","Date: 2022-09-12 <br>Type: Daily Low <br>Temperature: 77","Date: 2022-09-13 <br>Type: Daily Low <br>Temperature: 78","Date: 2022-09-14 <br>Type: Daily Low <br>Temperature: 76","Date: 2022-09-15 <br>Type: Daily Low <br>Temperature: 75","Date: 2022-09-16 <br>Type: Daily Low <br>Temperature: 74","Date: 2022-09-17 <br>Type: Daily Low <br>Temperature: 75","Date: 2022-09-18 <br>Type: Daily Low <br>Temperature: 74","Date: 2022-09-19 <br>Type: Daily Low <br>Temperature: 77","Date: 2022-09-20 <br>Type: Daily Low <br>Temperature: 76","Date: 2022-09-21 <br>Type: Daily Low <br>Temperature: 77","Date: 2022-09-22 <br>Type: Daily Low <br>Temperature: 77","Date: 2022-09-23 <br>Type: Daily Low <br>Temperature: 77","Date: 2022-09-24 <br>Type: Daily Low <br>Temperature: 76","Date: 2022-09-25 <br>Type: Daily Low <br>Temperature: 73","Date: 2022-09-26 <br>Type: Daily Low <br>Temperature: 76","Date: 2022-09-27 <br>Type: Daily Low <br>Temperature: 76","Date: 2022-09-28 <br>Type: Daily Low <br>Temperature: 73","Date: 2022-09-29 <br>Type: Daily Low <br>Temperature: 65","Date: 2022-09-30 <br>Type: Daily Low <br>Temperature: 65","Date: 2022-10-01 <br>Type: Daily Low <br>Temperature: 68","Date: 2022-10-02 <br>Type: Daily Low <br>Temperature: 67","Date: 2022-10-03 <br>Type: Daily Low <br>Temperature: 66","Date: 2022-10-04 <br>Type: Daily Low <br>Temperature: 63","Date: 2022-10-05 <br>Type: Daily Low <br>Temperature: 64","Date: 2022-10-06 <br>Type: Daily Low <br>Temperature: 67","Date: 2022-10-07 <br>Type: Daily Low <br>Temperature: 67","Date: 2022-10-08 <br>Type: Daily Low <br>Temperature: 65","Date: 2022-10-09 <br>Type: Daily Low <br>Temperature: 66","Date: 2022-10-10 <br>Type: Daily Low <br>Temperature: 74","Date: 2022-10-11 <br>Type: Daily Low <br>Temperature: 74","Date: 2022-10-12 <br>Type: Daily Low <br>Temperature: 74","Date: 2022-10-13 <br>Type: Daily Low <br>Temperature: 73","Date: 2022-10-14 <br>Type: Daily Low <br>Temperature: 71","Date: 2022-10-15 <br>Type: Daily Low <br>Temperature: 74","Date: 2022-10-16 <br>Type: Daily Low <br>Temperature: 71","Date: 2022-10-17 <br>Type: Daily Low <br>Temperature: 75","Date: 2022-10-18 <br>Type: Daily Low <br>Temperature: 64","Date: 2022-10-19 <br>Type: Daily Low <br>Temperature: 53","Date: 2022-10-20 <br>Type: Daily Low <br>Temperature: 49","Date: 2022-10-21 <br>Type: Daily Low <br>Temperature: 54","Date: 2022-10-22 <br>Type: Daily Low <br>Temperature: 55","Date: 2022-10-23 <br>Type: Daily Low <br>Temperature: 63","Date: 2022-10-24 <br>Type: Daily Low <br>Temperature: 66","Date: 2022-10-25 <br>Type: Daily Low <br>Temperature: 69","Date: 2022-10-26 <br>Type: Daily Low <br>Temperature: 69","Date: 2022-10-27 <br>Type: Daily Low <br>Temperature: 74","Date: 2022-10-28 <br>Type: Daily Low <br>Temperature: 72","Date: 2022-10-29 <br>Type: Daily Low <br>Temperature: 71","Date: 2022-10-30 <br>Type: Daily Low <br>Temperature: 71","Date: 2022-10-31 <br>Type: Daily Low <br>Temperature: 70","Date: 2022-11-01 <br>Type: Daily Low <br>Temperature: 72","Date: 2022-11-02 <br>Type: Daily Low <br>Temperature: 76","Date: 2022-11-03 <br>Type: Daily Low <br>Temperature: 71","Date: 2022-11-04 <br>Type: Daily Low <br>Temperature: 67","Date: 2022-11-05 <br>Type: Daily Low <br>Temperature: 69","Date: 2022-11-06 <br>Type: Daily Low <br>Temperature: 73","Date: 2022-11-07 <br>Type: Daily Low <br>Temperature: 70","Date: 2022-11-08 <br>Type: Daily Low <br>Temperature: 69","Date: 2022-11-09 <br>Type: Daily Low <br>Temperature: 67","Date: 2022-11-10 <br>Type: Daily Low <br>Temperature: 67","Date: 2022-11-11 <br>Type: Daily Low <br>Temperature: 71","Date: 2022-11-12 <br>Type: Daily Low <br>Temperature: 67","Date: 2022-11-13 <br>Type: Daily Low <br>Temperature: 65","Date: 2022-11-14 <br>Type: Daily Low <br>Temperature: 62","Date: 2022-11-15 <br>Type: Daily Low <br>Temperature: 68","Date: 2022-11-16 <br>Type: Daily Low <br>Temperature: 62","Date: 2022-11-17 <br>Type: Daily Low <br>Temperature: 53","Date: 2022-11-18 <br>Type: Daily Low <br>Temperature: 52","Date: 2022-11-19 <br>Type: Daily Low <br>Temperature: 53","Date: 2022-11-20 <br>Type: Daily Low <br>Temperature: 52","Date: 2022-11-21 <br>Type: Daily Low <br>Temperature: 54","Date: 2022-11-22 <br>Type: Daily Low <br>Temperature: 65","Date: 2022-11-23 <br>Type: Daily Low <br>Temperature: 65","Date: 2022-11-24 <br>Type: Daily Low <br>Temperature: 67","Date: 2022-11-25 <br>Type: Daily Low <br>Temperature: 68","Date: 2022-11-26 <br>Type: Daily Low <br>Temperature: 71","Date: 2022-11-27 <br>Type: Daily Low <br>Temperature: 67","Date: 2022-11-28 <br>Type: Daily Low <br>Temperature: 64","Date: 2022-11-29 <br>Type: Daily Low <br>Temperature: 61","Date: 2022-11-30 <br>Type: Daily Low <br>Temperature: 67","Date: 2022-12-01 <br>Type: Daily Low <br>Temperature: 63","Date: 2022-12-02 <br>Type: Daily Low <br>Temperature: 62","Date: 2022-12-03 <br>Type: Daily Low <br>Temperature: 63","Date: 2022-12-04 <br>Type: Daily Low <br>Temperature: 61","Date: 2022-12-05 <br>Type: Daily Low <br>Temperature: 63","Date: 2022-12-06 <br>Type: Daily Low <br>Temperature: 66","Date: 2022-12-07 <br>Type: Daily Low <br>Temperature: 67","Date: 2022-12-08 <br>Type: Daily Low <br>Temperature: 65","Date: 2022-12-09 <br>Type: Daily Low <br>Temperature: 63","Date: 2022-12-10 <br>Type: Daily Low <br>Temperature: 62","Date: 2022-12-11 <br>Type: Daily Low <br>Temperature: 64","Date: 2022-12-12 <br>Type: Daily Low <br>Temperature: 59","Date: 2022-12-13 <br>Type: Daily Low <br>Temperature: 60","Date: 2022-12-14 <br>Type: Daily Low <br>Temperature: 66","Date: 2022-12-15 <br>Type: Daily Low <br>Temperature: 64","Date: 2022-12-16 <br>Type: Daily Low <br>Temperature: 56","Date: 2022-12-17 <br>Type: Daily Low <br>Temperature: 54","Date: 2022-12-18 <br>Type: Daily Low <br>Temperature: 51","Date: 2022-12-19 <br>Type: Daily Low <br>Temperature: 46","Date: 2022-12-20 <br>Type: Daily Low <br>Temperature: 58","Date: 2022-12-21 <br>Type: Daily Low <br>Temperature: 60","Date: 2022-12-22 <br>Type: Daily Low <br>Temperature: 59","Date: 2022-12-23 <br>Type: Daily Low <br>Temperature: 42","Date: 2022-12-24 <br>Type: Daily Low <br>Temperature: 31","Date: 2022-12-25 <br>Type: Daily Low <br>Temperature: 31","Date: 2022-12-26 <br>Type: Daily Low <br>Temperature: 38","Date: 2022-12-27 <br>Type: Daily Low <br>Temperature: 40","Date: 2022-12-28 <br>Type: Daily Low <br>Temperature: 48","Date: 2022-12-29 <br>Type: Daily Low <br>Temperature: 56","Date: 2022-12-30 <br>Type: Daily Low <br>Temperature: 61","Date: 2022-12-31 <br>Type: Daily Low <br>Temperature: 66"],"type":"scatter","mode":"lines","line":{"width":4.1574803149606305,"color":"rgba(0,114,178,0.9)","dash":"solid"},"hoveron":"points","name":"Daily Low","legendgroup":"Daily Low","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":95,"r":40,"b":95,"l":75},"paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":17.268576172685762},"title":{"text":"Atlanta Daily Temperatures in 2019<br><sup>Daily highs and lows rise into summer, then fall back down toward winter<\/sup>","font":{"color":"rgba(0,0,0,1)","family":"","size":21.253632212536321},"x":0.050000000000000003,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[18974.799999999999,19375.200000000001],"tickmode":"array","ticktext":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan"],"tickvals":[18993,19024,19052,19083,19113,19144,19174,19205,19236,19266,19297,19327,19358],"categoryorder":"array","categoryarray":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":4.3171440431714405,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":13.814860938148611},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.78493528057662576,"zeroline":false,"anchor":"y","title":{"text":"","font":{"color":"rgba(0,0,0,1)","family":"","size":17.268576172685762}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[27.649999999999999,101.34999999999999],"tickmode":"array","ticktext":["40","60","80","100"],"tickvals":[40,60,80,100],"categoryorder":"array","categoryarray":["40","60","80","100"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":4.3171440431714414,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":13.814860938148611},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.78493528057662576,"zeroline":false,"anchor":"x","title":{"text":"Temperature","font":{"color":"rgba(0,0,0,1)","family":"","size":17.268576172685762}},"hoverformat":".2f"},"shapes":[],"showlegend":false,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":13.814860938148611},"orientation":"h","x":0.050000000000000003,"y":-0.17999999999999999},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"888c2833641e":{"x":{},"y":{},"colour":{},"text":{},"type":"scatter"}},"cur_data":"888c2833641e","visdat":{"888c2833641e":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

## Analysis of Visualization 1

The temperature chart shows a clear seasonal pattern in Atlanta during 2019. Daily high and low temperatures rise into the summer months and then drop again near winter.

The interactive part lets the reader hover over each day to see the exact date, temperature type, and temperature value. This adds detail without crowding the chart with labels for every single day.

Swap to high contrast palette for colorblind friendliness

# Visualization 2: Calendar Heatmap


``` r
calendar_weather <- weather_clean %>%
  mutate(
    month = lubridate::month(date, label = TRUE, abbr = TRUE),
    weekday = lubridate::wday(date, label = TRUE, abbr = TRUE, week_start = 1),
    week_of_month = ceiling(
      (lubridate::day(date) + lubridate::wday(lubridate::floor_date(date, "month"), week_start = 1) - 1) / 7
    )
  )

ggplot(calendar_weather, aes(x = week_of_month, y = forcats::fct_rev(weekday), fill = tmax)) +
  geom_tile(color = "white", linewidth = 0.35) +
  facet_wrap(~ month, nrow = 3) +
  scale_fill_viridis_c(
    option = "inferno",
    name = "Daily High\nTemperature",
    na.value = "gray90"
  ) +
  scale_x_continuous(
    breaks = 1:6,
    expand = c(0, 0)
  ) +
  labs(
    title = "Atlanta Daily High Temperatures in 2019",
    subtitle = "Calendar heatmap showing hotter and cooler periods across the year",
    x = "Week of Month",
    y = NULL,
    caption = "Color uses an inferno palette for stronger contrast and colorblind accessibility."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 18, color = "gray10"),
    plot.subtitle = element_text(size = 11, color = "gray25"),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 8, color = "gray30"),
    axis.text.y = element_text(size = 9, color = "gray30"),
    legend.position = "top",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    plot.caption = element_text(size = 8, color = "gray40"),
    plot.margin = margin(15, 20, 15, 20)
  )
```

<img src="Project_3_Henry_Marsh_Analysis_Data_Viz_2_files/figure-html/calendar-heatmap-1.png" alt="Calendar heatmap showing Atlanta daily high temperatures in 2019 by month, weekday, and week of month, with darker colors showing cooler days and brighter colors showing hotter days."  />

## Analysis of Visualization 2

The calendar heatmap makes the seasonal pattern easier to read day by day. The hotter days are grouped around late spring and summer, while cooler daily highs appear closer to the start and end of the year.

This chart works differently from the line chart because it shows the year as a calendar-like layout. That makes it easier to see clusters of hot days instead of only following the temperature line over time.

# Visualization 3: Weather Model Visualization


``` r
weather_model_data <- weather_clean %>%
  select(date, tmax, tmin, month) %>%
  drop_na(tmax, tmin)

weather_lm <- lm(tmax ~ tmin, data = weather_model_data)

summary(weather_lm)
```

```
## 
## Call:
## lm(formula = tmax ~ tmin, data = weather_model_data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -16.7039  -2.1508   0.2933   2.6258  10.1857 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 31.47373    1.41504   22.24   <2e-16 ***
## tmin         0.77793    0.02051   37.93   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.075 on 363 degrees of freedom
## Multiple R-squared:  0.7985,	Adjusted R-squared:  0.798 
## F-statistic:  1439 on 1 and 363 DF,  p-value: < 2.2e-16
```


``` r
weather_model_data <- weather_clean %>%
  select(date, tmax, tmin, month_num) %>%
  drop_na(tmax, tmin) %>%
  mutate(
    season = case_when(
      month_num %in% c(12, 1, 2) ~ "Winter",
      month_num %in% c(3, 4, 5) ~ "Spring",
      month_num %in% c(6, 7, 8) ~ "Summer",
      month_num %in% c(9, 10, 11) ~ "Fall"
    ),
    season = factor(season, levels = c("Winter", "Spring", "Summer", "Fall"))
  )

weather_lm <- lm(tmax ~ tmin, data = weather_model_data)

model_stats <- broom::glance(weather_lm)

model_label <- paste0(
  "R-squared: ", round(model_stats$r.squared, 3),
  "\nSlope: ", round(coef(weather_lm)[2], 2)
)

ggplot(weather_model_data, aes(x = tmin, y = tmax)) +
  geom_point(
    aes(fill = season, shape = season),
    color = "gray15",
    stroke = 0.45,
    alpha = 0.85,
    size = 3
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "black",
    fill = "gray75",
    linewidth = 1.3,
    alpha = 0.3
  ) +
  scale_fill_manual(
    values = c(
      "Winter" = "#0072B2",
      "Spring" = "#009E73",
      "Summer" = "#E69F00",
      "Fall" = "#CC79A7"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Winter" = 21,
      "Spring" = 22,
      "Summer" = 24,
      "Fall" = 23
    )
  ) +
  annotate(
    "label",
    x = min(weather_model_data$tmin, na.rm = TRUE),
    y = max(weather_model_data$tmax, na.rm = TRUE),
    label = model_label,
    hjust = 0,
    vjust = 1,
    size = 4,
    fill = "white",
    color = "gray15"
  ) +
  labs(
    title = "Daily Low Temperature vs Daily High Temperature",
    subtitle = "Linear model overlaid on a colorblind-safe scatter plot",
    x = "Daily Low Temperature (°F)",
    y = "Daily High Temperature (°F)",
    fill = "Season",
    shape = "Season",
    caption = "Black line shows the fitted linear model with confidence interval."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 11),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.caption = element_text(size = 8, color = "gray40")
  )
```

<img src="Project_3_Henry_Marsh_Analysis_Data_Viz_2_files/figure-html/weather-model-visualization-1.png" alt="Scatterplot of Atlanta daily low temperature versus daily high temperature with colorblind-safe points and a black linear model line overlaid on top."  />

## Analysis of Visualization 3

The model visualization compares daily low temperature to daily high temperature. I used a scatter plot with a fitted linear model line because the assignment required a model visualization, and this relationship is easy to interpret.

The model has an R-squared value of 0.799, meaning daily low temperature explains a large share of the variation in daily high temperature. The slope is 0.778, which means that as daily low temperature increases, daily high temperature also tends to increase.

The monthly coloring adds more context to the model. Colder months are mostly found in the lower-left part of the chart, while warmer months are mostly found in the upper-right. This connects the model back to the seasonal pattern shown in the earlier weather visualizations.

This model visualization keeps the scatter points separate from the model line by using filled seasonal colors and a black regression line. I also used different point shapes, so the chart is not relying only on color to separate the seasons.

# Section 2: Florida Lakes Spatial Data

The second part of the project uses the Florida Lakes shapefile. This dataset is different from the weather dataset because it contains geometry instead of only regular table values. Each lake is represented as a spatial feature, which makes it appropriate for the required spatial visualization.

At first, the lake map was harder to interpret because the lakes appeared without much geographic context. To improve this, I added Florida county boundaries underneath the lake layer. This made the map easier to read because the viewer can now see where the lakes are located across the state.

# Visualization 4: Spatial Visualization of Florida Lakes


``` r
library(sf)
library(tidyverse)
library(maps)

# Count lakes by county
lake_counties <- fl_lakes_clean %>%
  st_drop_geometry() %>%
  count(county, name = "lake_count") %>%
  mutate(county_join = str_to_lower(county))

# County map data
fl_counties <- map_data("county", "florida") %>%
  mutate(county_join = str_to_lower(subregion)) %>%
  left_join(lake_counties, by = "county_join") %>%
  mutate(
    lake_count = replace_na(lake_count, 0),
    lake_count_group = cut(
      lake_count,
      breaks = c(-1, 0, 10, 25, 50, 100, Inf),
      labels = c("0", "1-10", "11-25", "26-50", "51-100", "100+")
    )
  )

# State outline
fl_state <- map_data("state", "florida")

ggplot() +
  geom_polygon(
    data = fl_counties,
    aes(x = long, y = lat, group = group, fill = lake_count_group),
    color = "gray70",
    linewidth = 0.2
  ) +
  geom_sf(
    data = fl_lakes_clean,
    fill = "#0072B2",
    color = "#003B5C",
    linewidth = 0.05,
    alpha = 0.45,
    inherit.aes = FALSE
  ) +
  geom_polygon(
    data = fl_state,
    aes(x = long, y = lat, group = group),
    fill = NA,
    color = "black",
    linewidth = 0.7
  ) +
  scale_fill_viridis_d(
    option = "cividis",
    direction = 1,
    name = "Named lakes\nby county"
  ) +
  coord_sf(
    xlim = c(-87.8, -79.6),
    ylim = c(24.3, 31.2),
    expand = FALSE
  ) +
  labs(
    title = "Named Florida Lakes by County",
    subtitle = "County shading shows the number of named lakes, with lake polygons overlaid in blue",
    x = NULL,
    y = NULL,
    caption = "Source: Florida Lakes shapefile and Florida county boundaries from the maps package"
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "gray10"),
    plot.subtitle = element_text(size = 11, color = "gray25"),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8),
    plot.caption = element_text(size = 8, color = "gray40"),
    plot.margin = margin(15, 15, 15, 15)
  )
```

<img src="Project_3_Henry_Marsh_Analysis_Data_Viz_2_files/figure-html/florida-lakes-spatial-map-1.png" alt="Map of Florida counties shaded by the number of named lakes, with Florida lake polygons overlaid for spatial context."  />

## Analysis of Visualization 4

The spatial visualization shows Florida lakes layered on top of county boundaries. The lakes are shown in blue, while the counties are shown as light background outlines. This keeps the focus on the lakes while still giving the viewer enough geographic context.

The main pattern is that lakes are not evenly distributed across Florida. There are strong clusters through central Florida, with smaller groups appearing in other parts of the state. Some counties have many visible lakes, while others have fewer. This makes the map useful because it shows spatial distribution, not just individual lake shapes.

I chose to use one consistent lake color because the main goal of this plot was to show location and distribution. Using an area color scale would add another variable, but it would also make smaller lakes harder to see.

This spatial visualization adds a lake variable by showing the number of named lakes in each Florida county. The darker county shading shows where lake counts are higher, while the blue lake polygons show the actual spatial pattern of the lakes themselves.

The map shows that named lakes are not evenly distributed across Florida. Central Florida has stronger lake clustering, while several coastal and southern counties show fewer named lake features. I used a colorblind-safe cividis palette for the county counts and a high-contrast blue overlay for the lakes.

# Redesign: Before and After

### Before Temp Plot



### After Temp Plot



The redesigned chart keeps the same data, but makes the result easier to read. I changed the colors to a high-contrast colorblind-friendly pair, added clearer temperature labeling, reduced unnecessary grid clutter, and made the seasonal pattern stand out more directly.

The interactive temperature chart earlier in the report is the final version of this redesign. It adds hover text so the reader can check exact dates and temperature values without crowding the main chart.

## Before and After Heatmap

### Before Heatmap

![](Project_3_Henry_Marsh_Analysis_Data_Viz_2_files/figure-html/Before Calendar Heatmap-1.png)<!-- -->

### After Heatmap

<img src="Project_3_Henry_Marsh_Analysis_Data_Viz_2_files/figure-html/After-calendar-heatmap-1.png" alt="Calendar heatmap showing Atlanta daily high temperatures in 2019 by month, weekday, and week of month, with darker colors showing cooler days and brighter colors showing hotter days."  />

## Before and After Model plot

### Before Model Plot


```
## 
## Call:
## lm(formula = tmax ~ tmin, data = weather_model_data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -16.7039  -2.1508   0.2933   2.6258  10.1857 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 31.47373    1.41504   22.24   <2e-16 ***
## tmin         0.77793    0.02051   37.93   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.075 on 363 degrees of freedom
## Multiple R-squared:  0.7985,	Adjusted R-squared:  0.798 
## F-statistic:  1439 on 1 and 363 DF,  p-value: < 2.2e-16
```

![](Project_3_Henry_Marsh_Analysis_Data_Viz_2_files/figure-html/Before-weather-linear-model-scatter-1.png)<!-- -->

### After Model Plot

<img src="Project_3_Henry_Marsh_Analysis_Data_Viz_2_files/figure-html/After-weather-model-visualization-1.png" alt="Scatterplot of Atlanta daily low temperature versus daily high temperature with colorblind-safe points and a black linear model line overlaid on top."  />

## Before Lakes Map

### Before Lakes Map

![](Project_3_Henry_Marsh_Analysis_Data_Viz_2_files/figure-html/Before-spatial-florida-lakes-1.png)<!-- -->

### After Lakes Map

<img src="Project_3_Henry_Marsh_Analysis_Data_Viz_2_files/figure-html/after_florida_lakes_spatial_map-1.png" alt="Map of Florida counties shaded by the number of named lakes, with Florida lake polygons overlaid for spatial context."  />


# AI Use Statement

I used ChatGPT to help improve sentence structure, wording, and organization in my report. The analysis, visualizations, project direction, and final decisions were my own.

# Project 3 interactive map save

