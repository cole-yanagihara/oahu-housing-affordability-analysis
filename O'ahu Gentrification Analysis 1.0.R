library(tidycensus)
library(tidyverse)
library(sf)
library(tmap)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(stringr)
API_KEY = "902791a1553a828c6858ece11ddbd62464ac2140"

#1. Getting ACS Data on Hawaii
hawaii_spatial <- get_acs(
  geography = "tract",
  variables = c(
    total_pop = "B01003_001",
    median_income = "B19013_001",
    median_rent = "B25064_001",
    white = "B02001_002",
    asian = "B02001_005",
    hawaiian = "B02001_006",
    born_in_hi = "B05002_003",
    foreign_born = "B05002_013"
  ),
  state = "HI",
  year = 2024,
  output = "wide",
  geometry = TRUE,
  cb = TRUE
)
hawaii_name <- read_csv("data/2020_Census_Tracts.csv")
hawaii_name <- hawaii_name %>%
  select(geoid20, tractname) %>%
  rename(
    GEOID = geoid20,
    tract_name = tractname
  )
hawaii_name$GEOID <- as.character(hawaii_name$GEOID)
hawaii_spatial <- hawaii_spatial %>%
  left_join(hawaii_name, by = "GEOID")

# Transform to lat/long for filtering
oahu_spatial <- st_transform(hawaii_spatial, 4326)

# Filter to Oahu bounding box (Honolulu County includes outer islands)
oahu_spatial <- oahu_spatial %>%
  filter(
    st_coordinates(st_centroid(.))[,1] > -158.3 &
    st_coordinates(st_centroid(.))[,1] < -157.6 &
    st_coordinates(st_centroid(.))[,2] > 21.2 &
    st_coordinates(st_centroid(.))[,2] < 21.8
  )

# Project to UTM Zone 4N (meters) — required for distance-based weights
oahu_spatial <- st_transform(oahu_spatial, 32604)

#2. Change Data to Percentages
oahu_spatial <- oahu_spatial %>%
  mutate(
    pct_white = 100 * whiteE / total_popE,
    pct_asian = 100 * asianE / total_popE,
    pct_hawaiian = 100 * hawaiianE / total_popE,
    pct_born_in_hi = 100 * born_in_hiE / total_popE,
    rent_income_pct = 100 * (median_rentE * 12 / median_incomeE),
    pct_foreign_born = 100 - pct_born_in_hi #means born off the island
  ) #this shows percentage of each TRACT

#3. Plotting the Basic Graphs
  #median income graph
tmap_mode("plot")
tm_shape(oahu_spatial) +
  tm_polygons("median_incomeE",
              palette = "RdBu",
              breaks = c(0,40000,60000,80000,100000,120000,150000),
              alpha = 0.7,
              title = "Median Income in O'ahu") +
  tm_layout(main.title = "Median Income in O'ahu",
            legend.outside = TRUE)

  #hawaii locals graphs
tmap_mode("plot")
tm_shape(oahu_spatial) +
  tm_polygons("pct_born_in_hi",
              palette = "RdBu",
              style = "quantile",
              alpha = 0.7,
              title = "Percent of O'ahu Locals") +
  tm_layout(main.title = "Percent of O'ahu Locals",
            legend.outside = TRUE)

  #rent burden map
tm_shape(oahu_spatial) +
  tm_polygons(
    "rent_income_pct",
    palette = "BuRd",
    breaks = c(0,10,20,30,40,50,60,70,80),
    title = "% Income Spent on Rent",
    colorNA = "gray85",
  ) +
  tm_layout(
    main.title = "Rent Burden in O'ahu (2024)",
    legend.outside = TRUE
  )

  #rent burden dot plot graph
ggplot(oahu_spatial, aes(x = pct_born_in_hi, y = rent_income_pct)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text_repel(
    data = subset(oahu_spatial, rent_income_pct > 50),
    aes(label = tract_name),
    size = 3
  ) +
  labs(
    title = "Percent Born in Hawaiʻi vs Rent Burden (Hawai'i Census Tracts)",
    x = "Percent Born in Hawaiʻi",
    y = "Percent of Income Spent on Rent"
  ) +
  theme_minimal()

  #ggplot by race
race_long <- oahu_spatial %>%
  select(rent_income_pct, median_rentE, pct_white, pct_asian, pct_hawaiian) %>%
  pivot_longer(
    cols = starts_with("pct_"),
    names_to = "race",
    values_to = "percent"
  )
race_long$race <- recode(race_long$race,
  pct_white = "White",
  pct_asian = "Asian",
  pct_hawaiian = "Native Hawaiian"
)


ggplot(race_long, aes(x = percent, y = median_rentE, color = race)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Race Composition vs Rent Prices (Hawai'i Census Tracts)",
    x = "Percent of Population",
    y = "Rent",
    color = "Race"
  ) +
  theme_minimal() #plotted with y-axis being rent
race_long %>%
  group_by(race) %>%
  summarise(
    intercept = coef(lm(median_rentE ~ percent))[1],
    slope = coef(lm(median_rentE ~ percent))[2],
    r_squared = summary(lm(median_rentE ~ percent))$r.squared
  )

#
oahu_spatial <- oahu_spatial %>%
  mutate(
    county = str_extract(NAME, "(?<=; ).*? County")
  )
ggplot(hawaii_spatial, aes(x = county, y = rent_income_pct)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Rent Burden by County",
       x = "County",
       y = "Rent as % of Income") +
  theme_minimal()