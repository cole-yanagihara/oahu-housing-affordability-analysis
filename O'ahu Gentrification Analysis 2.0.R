library(tidycensus)
library(tidyverse)
library(sf)
library(spdep)
library(tmap)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(stringr)
API_KEY = "902791a1553a828c6858ece11ddbd62464ac2140"

#1. GET ACS DATA FOR HAWAI'I
hawaii_spatial <- get_acs(
  geography = "tract",
  variables = c(
    total_pop = "B01003_001",
    median_income = "B19013_001",
    median_rent = "B25064_001",
    born_in_hi = "B05002_003"
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

#2 BIND DATA TO O'AHU
oahu_spatial <- st_transform(hawaii_spatial, 4326)
oahu_spatial <- oahu_spatial %>%
  filter(
    st_coordinates(st_centroid(.))[,1] > -158.3 &
      st_coordinates(st_centroid(.))[,1] < -157.6 &
      st_coordinates(st_centroid(.))[,2] > 21.2 &
      st_coordinates(st_centroid(.))[,2] < 21.8
  )
oahu_spatial <- st_transform(oahu_spatial, 32604)

#3 CREATING LOCAL-BORN AND RENT-BURDEN VARIABLES
oahu_spatial <- oahu_spatial %>%
  mutate(
    local_born = 100 * born_in_hiE / total_popE,
    rent_burden = 100 * (median_rentE * 12 / median_incomeE),
    foreign_born = 100 - local_born
  )

#4 IMPORT MILITARY BASE DATA
military_bases_20 <- c(
  "15003981700","15003981802","15003981801","15003981803",
  "15003009507","15003009512","15003009511","15003009509",
  "15003009508","15003009510","15003980600","15003980700",
  "15003009000","15003007400","15003007100","15003007302",
  "15003981900","15003007002","15003006810","15003006811",
  "15003981100","15003982000","15003981300"
)
military_bases_10 <- c(
  military_bases_10 <- c(
    #Marine Corps Base Hawai’i (MCBH)
    "15003010801",
    "15003010802",
    #Schofield Barracks
    "15003009507",
    "15003009502",
    "15003009501",
    "15003009503",
    "15003009504",
    "15003980600",
    "15003980700",
    #Wheeler Army Airfield
    "15003009000",
    #Pearl Harbor and Hickam Air Force Base
    "15003007400",
    "15003007303",
    "15003007302",
    "15003007100",
    "15003007000",
    "15003007000",
    #Aliamanu Military Reservation
    "15003006804",
    #Bellows Air Force Station
    "15003981100",
    #Fort Shafter
    "15003006600"
))
oahu_spatial$military <- oahu_spatial$GEOID %in% military_bases_20
oahu_no_military <- oahu_spatial %>%
  filter(!GEOID %in% military_bases_20)
nrow(oahu_spatial)
#5 Choropleth Maps
##5.1 Rent Burden Map
tm_shape(oahu_no_military) +
  tm_polygons("rent_burden",
              palette = "BuRd",
              style = "quantile",
              alpha = 0.7,
              colorNA = "gray85",
              title = "Rent Burden in O'ahu") +
  tm_layout(main.title = "Rent Burden in O'ahu",
            legend.outside = TRUE)
##5.2 Local Born Map
tmap_mode("plot")
tm_shape(oahu_no_military) +
  tm_polygons("local_born",
              palette = "BuRd",
              style = "quantile",
              alpha = 0.7,
              colorNA = "gray85",
              title = "Percent of Residents born within Hawai'i") +
  tm_layout(main.title = "Percent of Residents born within Hawai'i",
            legend.outside = TRUE)
##5.3 Median Rent Map
tm_shape(oahu_no_military) +
  tm_polygons("median_rentE",
              palette = "BuRd",
              style = "quantile",
              alpha = 0.7,
              colorNA = "gray85",
              title = "Median Rent in O'ahu") +
  tm_layout(main.title = "Median Rent in O'ahu",
            legend.outside = TRUE)
##5.4 Military Installations Map
tm_shape(oahu_spatial) +
  tm_polygons(col = "grey85",
              border.col = "white") +
  tm_shape(oahu_spatial[oahu_spatial$military, ]) +
  tm_fill(col = "red", alpha = 0.9) +
  tm_borders(col = "black", lwd = 1.2) +
  tm_layout(main.title = "Military Installation Census Tracts on Oʻahu",
            legend.show = FALSE)

#6 SPATIAL WEIGHTS MAPS
##6.1 Queen Contiguity
nb_queen <- poly2nb(oahu_no_military, queen = TRUE)
coords <- st_coordinates(st_centroid(oahu_no_military))
crs_proj <- st_crs(oahu_no_military)$proj4string
queen_lines <- spdep::nb2lines(nb_queen, coords = coords, proj4string = crs_proj)
queen_lines <- st_as_sf(queen_lines)
##6.2 k=5 nearest
knn <- knearneigh(coords, k = 6)
nb_knn <- knn2nb(knn)
knn_lines <- spdep::nb2lines(nb_knn, coords = coords, proj4string = crs_proj)
knn_lines <- st_as_sf(knn_lines)
##6.3 Distance-based (e.g., 5 km threshold)
dists <- dnearneigh(coords, 0, 5000)
dist_lines <- spdep::nb2lines(dists, coords = coords, proj4string = crs_proj)
dist_lines <- st_as_sf(dist_lines)
##6.5 Contiguity Comparison Map
tmap_mode("plot")
p1 <- tm_shape(oahu_no_military) +
  tm_polygons(alpha = 0.6) +
  tm_shape(queen_lines) +
  tm_lines(col = "red") +
  tm_layout(main.title = "Queen Contiguity")
p2 <- tm_shape(oahu_no_military) +
  tm_polygons(alpha = 0.6) +
  tm_shape(knn_lines) +
  tm_lines(col = "blue") +
  tm_layout(main.title = "k-Nearest Neighbors (k=5)")
p3 <- tm_shape(oahu_no_military) +
  tm_polygons(alpha = 0.6) +
  tm_shape(dist_lines) +
  tm_lines(col = "green") +
  tm_layout(main.title = "Distance-Based (5km)")
tmap_arrange(p1, p2, p3, ncol = 3)
##6.6 Summary Table
summarize_weights <- function(nb, name) {
  num_neighbors <- card(nb)
  n <- length(nb)
  total_possible <- n * (n - 1)
  total_links <- sum(num_neighbors)
  pct_nonzero <- (total_links / total_possible) * 100
  data.frame(
    Weights_Type = name,
    Mean_Neighbors = mean(num_neighbors),
    Min_Neighbors = min(num_neighbors),
    Max_Neighbors = max(num_neighbors),
    Percent_Nonzero = pct_nonzero
  )
}
weights_summary <- rbind(
  summarize_weights(nb_queen, "Queen Contiguity"),
  summarize_weights(nb_knn, "KNN (k=5)"),
  summarize_weights(nb_dist, "Distance (5 km)")
)
weights_summary <- weights_summary %>%
  mutate(across(where(is.numeric), round, 2))
weights_summary

#7 LISA MAPS
##7.1 Rent-Burden LISA Map
oahu_rent_burden <- oahu_no_military %>%
  filter(!is.na(rent_burden))
coords <- st_coordinates(st_centroid(oahu_rent_burden))
knn <- knearneigh(coords, k = 5)
nb_knn <- knn2nb(knn)
listw_knn <- nb2listw(nb_knn, style = "W", zero.policy = TRUE)
local_moran <- localmoran(oahu_rent_burden$rent_burden, listw_knn)
oahu_rent_burden <- oahu_rent_burden %>%
  mutate(
    local_I = local_moran[, "Ii"],           # Local I value
    local_I_z = local_moran[, "Z.Ii"],       # Z-score
    local_p = local_moran[, "Pr(z != E(Ii))"], # P-value
    significant_05 = local_p < 0.05,         # Significant at 5%?
    significant_01 = local_p < 0.01,         # Significant at 1%?
    significant_001 = local_p < 0.001        # Significant at 0.1%?
  )
oahu_rent_burden <- oahu_rent_burden %>%
  mutate(
    rent_burden_std = as.numeric(scale(rent_burden))
  )
oahu_rent_burden <- oahu_rent_burden %>%
  mutate(
    rent_burden_lag = lag.listw(listw_knn, rent_burden_std)
  )
oahu_rent_burden <- oahu_rent_burden %>%
  mutate(
    cluster_type_all = case_when(
      rent_burden_std > 0 & rent_burden_lag > 0 ~ "High-High",
      rent_burden_std < 0 & rent_burden_lag < 0 ~ "Low-Low",
      rent_burden_std > 0 & rent_burden_lag < 0 ~ "High-Low",
      rent_burden_std < 0 & rent_burden_lag > 0 ~ "Low-High"
    )
  )
oahu_spatial_0.05 <- oahu_rent_burden %>%
  mutate(cluster_type = ifelse(significant_05, cluster_type_all, "Not Significant"))
oahu_spatial_0.01 <- oahu_rent_burden %>%
  mutate(cluster_type = ifelse(significant_01, cluster_type_all, "Not Significant"))
oahu_spatial_0.001 <- oahu_rent_burden %>%
  mutate(cluster_type = ifelse(significant_001, cluster_type_all, "Not Significant"))
rent_burden_map05 <- tm_shape(oahu_spatial_0.05) +
  tm_fill("cluster_type",
          palette = c("Not Significant" = "gray90",
                      "High-High" = "red",
                      "Low-Low" = "blue",
                      "High-Low" = "pink",
                      "Low-High" = "lightblue"),
          title = "p < 0.05") +
  tm_borders(col = "white", lwd = 0.5) +
  tm_layout(legend.show = FALSE)
rent_burden_map01 <- tm_shape(oahu_spatial_0.01) +
  tm_fill("cluster_type",
          palette = c("Not Significant" = "gray90",
                      "High-High" = "red",
                      "Low-Low" = "blue",
                      "High-Low" = "pink",
                      "Low-High" = "lightblue"),
          title = "p < 0.01") +
  tm_borders(col = "white", lwd = 0.5) +
  tm_layout(legend.show = FALSE)
rent_burden_map001 <- tm_shape(oahu_spatial_0.001) +
  tm_fill("cluster_type",
          palette = c("Not Significant" = "gray90",
                      "High-High" = "red",
                      "Low-Low" = "blue",
                      "High-Low" = "pink",
                      "Low-High" = "lightblue"),
          title = "p < 0.001") +
  tm_borders(col = "white", lwd = 0.5) +
  tm_layout(legend.show = FALSE)
tmap_arrange(rent_burden_map05, rent_burden_map01, rent_burden_map001, ncol = 3)
##7.2 Rent Burden Global Moran's I
global_moran <- moran.test(oahu_rent_burden$rent_burden, listw_knn,zero.policy = TRUE)
global_moran
##7.3 Rent Burden Local Moran's I
global_moran <- moran.test(oahu_rent_burden$local_born, listw_knn,zero.policy = TRUE)
global_moran
##7.4 Local-Born Map
oahu_local_born <- oahu_no_military %>%
  filter(!is.na(local_born))
nb_queen <- poly2nb(oahu_local_born, queen = TRUE)
listw_queen <- nb2listw(nb_queen, style = "W", zero.policy = TRUE)
local_moran <- localmoran(oahu_local_born$local_born, listw_queen)
oahu_local_born <- oahu_local_born %>%
  mutate(
    local_I = local_moran[, "Ii"],
    local_I_z = local_moran[, "Z.Ii"],
    local_p = local_moran[, "Pr(z != E(Ii))"],
    significant_05 = local_p < 0.05,
    significant_01 = local_p < 0.01,
    significant_001 = local_p < 0.001
  )
oahu_local_born <- oahu_local_born %>%
  mutate(
    local_born_std = as.numeric(scale(local_born))
  )
oahu_local_born <- oahu_local_born %>%
  mutate(
    local_born_lag = lag.listw(listw_queen, local_born_std)
  )
oahu_local_born <- oahu_local_born %>%
  mutate(
    cluster_type_all = case_when(
      local_born_std > 0 & local_born_lag > 0 ~ "High-High",
      local_born_std < 0 & local_born_lag < 0 ~ "Low-Low",
      local_born_std > 0 & local_born_lag < 0 ~ "High-Low",
      local_born_std < 0 & local_born_lag > 0 ~ "Low-High"
    )
  )
oahu_spatial_0.05 <- oahu_local_born %>%
  mutate(cluster_type = ifelse(significant_05, cluster_type_all, "Not Significant"))
oahu_spatial_0.01 <- oahu_local_born %>%
  mutate(cluster_type = ifelse(significant_01, cluster_type_all, "Not Significant"))
oahu_spatial_0.001 <- oahu_local_born %>%
  mutate(cluster_type = ifelse(significant_001, cluster_type_all, "Not Significant"))
local_born_map05 <- tm_shape(oahu_spatial_0.05) +
  tm_fill("cluster_type",
          palette = c("Not Significant" = "gray90",
                      "High-High" = "red",
                      "Low-Low" = "blue",
                      "High-Low" = "pink",
                      "Low-High" = "lightblue"),
          title = "p < 0.05") +
  tm_borders(col = "white", lwd = 0.5) +
  tm_layout(legend.show = FALSE)
local_born_map01 <- tm_shape(oahu_spatial_0.01) +
  tm_fill("cluster_type",
          palette = c("Not Significant" = "gray90",
                      "High-High" = "red",
                      "Low-Low" = "blue",
                      "High-Low" = "pink",
                      "Low-High" = "lightblue"),
          title = "p < 0.01") +
  tm_borders(col = "white", lwd = 0.5) +
  tm_layout(legend.show = FALSE)
local_born_map001 <- tm_shape(oahu_spatial_0.001) +
  tm_fill("cluster_type",
          palette = c("Not Significant" = "gray90",
                      "High-High" = "red",
                      "Low-Low" = "blue",
                      "High-Low" = "pink",
                      "Low-High" = "lightblue"),
          title = "p < 0.001") +
  tm_borders(col = "white", lwd = 0.5) +
  tm_layout(legend.show = FALSE)
tmap_arrange(local_born_map05, local_born_map01, local_born_map001, ncol = 3)

#8 TEMOPRAL LISA MAPS
##8.2 Setup
acs_years <- seq(2010, 2024, by = 2)
get_oahu_year <- function(yr) {
  # ACS 1-year not available for tracts; use 5-year
  dat <- get_acs(
    geography = "tract",
    variables = c(
      total_pop     = "B01003_001",
      median_income = "B19013_001",
      median_rent   = "B25064_001",
      born_in_hi    = "B05002_002"
    ),
    state    = "HI",
    year     = yr,
    output   = "wide",
    geometry = TRUE,
    cb       = TRUE
  )
  dat <- st_transform(dat, 4326)
  coords_wgs <- st_coordinates(st_centroid(dat))
  dat <- dat[coords_wgs[,1] > -158.3 & coords_wgs[,1] < -157.6 &
               coords_wgs[,2] >   21.2 & coords_wgs[,2] <   21.8, ]
  dat <- st_transform(dat, 32604)
  dat <- dat %>%
    mutate(
      local_born  = 100 * born_in_hiE / total_popE,
      rent_burden = 100 * (median_rentE * 12 / median_incomeE),
      year        = yr
    ) 
  if (yr < 2020) {
      dat <- dat %>% filter(!GEOID %in% military_bases_10)
    } else {
      dat <- dat %>% filter(!GEOID %in% military_bases_20)
    }
  return(dat)
}
## 7.2 Download all years (stored as a named list)
oahu_by_year <- setNames(
  lapply(acs_years, get_oahu_year),
  as.character(acs_years)
)
## 7.3 Helper: build queen weights + LISA cluster type at p < 0.05
lisa_clusters <- function(sf_obj, variable) {
  # Drop rows with NA in the target variable
  sf_clean <- sf_obj %>% filter(!is.na(.data[[variable]]))
  # Queen contiguity weights
  nb   <- poly2nb(sf_clean, queen = TRUE)
  lw   <- nb2listw(nb, style = "W", zero.policy = TRUE)
  # Local Moran's I
  lm_res <- localmoran(sf_clean[[variable]], lw, zero.policy = TRUE)
  # Standardise and compute spatial lag
  std_var <- as.numeric(scale(sf_clean[[variable]]))
  lag_var <- lag.listw(lw, std_var, zero.policy = TRUE)
  sf_clean <- sf_clean %>%
    mutate(
      local_p      = lm_res[, "Pr(z != E(Ii))"],
      var_std      = std_var,
      var_lag      = lag_var,
      cluster_raw  = case_when(
        var_std >  0 & var_lag >  0 ~ "High-High",
        var_std <  0 & var_lag <  0 ~ "Low-Low",
        var_std >  0 & var_lag <  0 ~ "High-Low",
        var_std <  0 & var_lag >  0 ~ "Low-High"
      ),
      cluster_type = ifelse(local_p < 0.05, cluster_raw, "Not Significant")
    )
  sf_clean
}
## 7.4 Build one tmap panel per year per variable
cluster_palette <- c(
  "Not Significant" = "gray90",
  "High-High"       = "red",
  "Low-Low"         = "blue",
  "High-Low"        = "pink",
  "Low-High"        = "lightblue"
)
rent_maps <- lapply(acs_years, function(yr) {
  sf_lisa <- lisa_clusters(oahu_by_year[[as.character(yr)]], "rent_burden")
  tm_shape(sf_lisa) +
    tm_fill("cluster_type",
            palette      = cluster_palette,
            title        = "Cluster",
            showNA       = FALSE) +
    tm_borders(col = "white", lwd = 0.3) +
    tm_layout(
      main.title      = as.character(yr),
      main.title.size = 0.8,
      legend.show     = FALSE
    )
})
local_born_maps <- lapply(acs_years, function(yr) {
  sf_lisa <- lisa_clusters(oahu_by_year[[as.character(yr)]], "local_born")
  tm_shape(sf_lisa) +
    tm_fill("cluster_type",
            palette  = cluster_palette,
            title    = "Cluster",
            showNA   = FALSE) +
    tm_borders(col = "white", lwd = 0.3) +
    tm_layout(
      main.title      = as.character(yr),
      main.title.size = 0.8,
      legend.show     = FALSE
    )
})
## 7.5 Interleave rows: rent_burden row first, then local_born row
# tmap_arrange fills left-to-right, top-to-bottom, so supply rent maps
# then local_born maps; ncol = number of years gives 2 clean rows.
n_years <- length(acs_years)
tmap_arrange(
  c(rent_maps, local_born_maps),
  ncol  = n_years,
  nrow  = 2,
  outer.margins = 0.01
)
## 7.6 Add a shared legend as a separate standalone map (optional but recommended)
legend_sf <- lisa_clusters(oahu_by_year[["2024"]], "rent_burden")

legend_panel <- tm_shape(legend_sf) +
  tm_fill("cluster_type",
          palette = cluster_palette,
          title   = "LISA Cluster (p < 0.01)") +
  tm_borders(col = "white", lwd = 0.3) +
  tm_layout(
    main.title      = "Legend",
    main.title.size = 0.9,
    legend.only     = TRUE   # renders just the legend box
  )
legend_panel
