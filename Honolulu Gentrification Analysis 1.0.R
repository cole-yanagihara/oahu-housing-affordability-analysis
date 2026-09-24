library(tidycensus)
library(tidyverse)
library(sf)
library(spdep)
library(tmap)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(stringr)
library(tigris)

API_KEY <- "902791a1553a828c6858ece11ddbd62464ac2140"
census_api_key(API_KEY, overwrite = TRUE)

options(tigris_use_cache = TRUE)

# ============================================================
# 1. GET ACS DATA FOR HAWAI'I
# ============================================================
hawaii_spatial <- get_acs(
  geography = "tract",
  variables = c(
    total_pop     = "B01003_001",
    median_income = "B19013_001",
    median_rent   = "B25064_001",
    born_in_hi    = "B05002_003"
  ),
  state    = "HI",
  year     = 2024,
  output   = "wide",
  geometry = TRUE,
  cb       = TRUE
)

hawaii_name <- read_csv("data/2020_Census_Tracts.csv")
hawaii_name <- hawaii_name %>%
  select(geoid20, tractname) %>%
  rename(GEOID = geoid20, tract_name = tractname)
hawaii_name$GEOID <- as.character(hawaii_name$GEOID)

hawaii_spatial <- hawaii_spatial %>%
  left_join(hawaii_name, by = "GEOID")

# ============================================================
# 2. CLIP TO URBAN HONOLULU CDP BOUNDARY
# Urban Honolulu is a Census Designated Place (CDP)
# ============================================================

# Get Urban Honolulu CDP boundary from tigris
urban_honolulu <- places(state = "HI", cb = TRUE) %>%
  filter(NAME == "Urban Honolulu") %>%
  st_transform(4326)

# Project hawaii_spatial to WGS84 for the clip
hawaii_wgs <- st_transform(hawaii_spatial, 4326) %>%
  st_make_valid()

# Clip: keep tracts whose centroid falls inside Urban Honolulu
# (using centroid avoids partial-tract edge cases along the boundary)
centroids_wgs <- st_centroid(hawaii_wgs)
in_uh <- st_within(centroids_wgs, urban_honolulu, sparse = FALSE)[, 1]
oahu_spatial <- hawaii_wgs[in_uh, ]

# Project to UTM Zone 4N (meters) for spatial analysis
oahu_spatial <- st_transform(oahu_spatial, 32604)
urban_honolulu_proj <- st_transform(urban_honolulu, 32604)

cat("Census tracts in Urban Honolulu:", nrow(oahu_spatial), "\n")

# ============================================================
# 3. CREATE LOCAL-BORN AND RENT-BURDEN VARIABLES
# ============================================================
oahu_spatial <- oahu_spatial %>%
  mutate(
    local_born   = 100 * born_in_hiE  / total_popE,
    rent_burden  = 100 * (median_rentE * 12 / median_incomeE),
    foreign_born = 100 - local_born
  )

# ============================================================
# 4. MILITARY BASE TRACTS (2020 GEOIDs)
# ============================================================
military_bases_20 <- c(
  "15003981700","15003981802","15003981801","15003981803",
  "15003009507","15003009512","15003009511","15003009509",
  "15003009508","15003009510","15003980600","15003980700",
  "15003009000","15003007400","15003007100","15003007302",
  "15003981900","15003007002","15003006810","15003006811",
  "15003981100","15003982000","15003981300"
)

military_bases_10 <- c(
  "15003010801","15003010802",
  "15003009507","15003009502","15003009501","15003009503","15003009504",
  "15003980600","15003980700",
  "15003009000",
  "15003007400","15003007303","15003007302","15003007100","15003007000",
  "15003006804",
  "15003981100",
  "15003006600"
)

oahu_spatial$military   <- oahu_spatial$GEOID %in% military_bases_20
oahu_no_military        <- oahu_spatial %>% filter(!GEOID %in% military_bases_20)

# ============================================================
# 5. CHOROPLETH MAPS
# ============================================================
tmap_mode("plot")

## 5.1 Rent Burden Map
tm_shape(oahu_no_military) +
  tm_polygons("rent_burden",
              palette  = "BuRd",
              style    = "quantile",
              alpha    = 0.7,
              colorNA  = "gray85",
              title    = "Rent Burden (%)") +
  tm_shape(urban_honolulu_proj) +
  tm_borders(col = "black", lwd = 1.5) +
  tm_layout(main.title    = "Rent Burden in Urban Honolulu",
            legend.outside = TRUE)

## 5.2 Local Born Map
tm_shape(oahu_no_military) +
  tm_polygons("local_born",
              palette  = "BuRd",
              style    = "quantile",
              alpha    = 0.7,
              colorNA  = "gray85",
              title    = "% Born in Hawai'i") +
  tm_shape(urban_honolulu_proj) +
  tm_borders(col = "black", lwd = 1.5) +
  tm_layout(main.title    = "Percent of Residents Born in Hawai'i — Urban Honolulu",
            legend.outside = TRUE)

## 5.3 Median Rent Map
tm_shape(oahu_no_military) +
  tm_polygons("median_rentE",
              palette  = "BuRd",
              style    = "quantile",
              alpha    = 0.7,
              colorNA  = "gray85",
              title    = "Median Rent ($)") +
  tm_shape(urban_honolulu_proj) +
  tm_borders(col = "black", lwd = 1.5) +
  tm_layout(main.title    = "Median Gross Rent — Urban Honolulu",
            legend.outside = TRUE)

## 5.4 Military Installations Map
tm_shape(oahu_spatial) +
  tm_polygons(col = "grey85", border.col = "white") +
  tm_shape(oahu_spatial[oahu_spatial$military, ]) +
  tm_fill(col = "red", alpha = 0.9) +
  tm_borders(col = "black", lwd = 1.2) +
  tm_shape(urban_honolulu_proj) +
  tm_borders(col = "black", lwd = 1.5) +
  tm_layout(main.title  = "Military Installation Census Tracts — Urban Honolulu",
            legend.show = FALSE)

# ============================================================
# 6. SPATIAL WEIGHTS
# ============================================================
coords <- st_coordinates(st_centroid(oahu_no_military))
crs_proj <- st_crs(oahu_no_military)$proj4string

## 6.1 Queen contiguity
nb_queen    <- poly2nb(oahu_no_military, queen = TRUE)
queen_lines <- nb2lines(nb_queen, coords = coords, proj4string = crs_proj) %>%
  st_as_sf()

## 6.2 K=5 nearest neighbors
nb_knn      <- knn2nb(knearneigh(coords, k = 5))
knn_lines   <- nb2lines(nb_knn, coords = coords, proj4string = crs_proj) %>%
  st_as_sf()

## 6.3 Distance-based (5 km threshold)
nb_dist     <- dnearneigh(coords, 0, 5000)
dist_lines  <- nb2lines(nb_dist, coords = coords, proj4string = crs_proj) %>%
  st_as_sf()

## 6.4 Contiguity comparison map
p1 <- tm_shape(oahu_no_military) +
  tm_polygons(alpha = 0.6) +
  tm_shape(queen_lines) +
  tm_lines(col = "red") +
  tm_layout(main.title = "Queen Contiguity")

p2 <- tm_shape(oahu_no_military) +
  tm_polygons(alpha = 0.6) +
  tm_shape(knn_lines) +
  tm_lines(col = "blue") +
  tm_layout(main.title = "K-Nearest Neighbors (k=5)")

p3 <- tm_shape(oahu_no_military) +
  tm_polygons(alpha = 0.6) +
  tm_shape(dist_lines) +
  tm_lines(col = "green") +
  tm_layout(main.title = "Distance-Based (5 km)")

tmap_arrange(p1, p2, p3, ncol = 3)

## 6.5 Weights summary table
summarize_weights <- function(nb, name) {
  num_neighbors  <- card(nb)
  n              <- length(nb)
  total_links    <- sum(num_neighbors)
  pct_nonzero    <- (total_links / (n * (n - 1))) * 100
  data.frame(
    Weights_Type    = name,
    Mean_Neighbors  = mean(num_neighbors),
    Min_Neighbors   = min(num_neighbors),
    Max_Neighbors   = max(num_neighbors),
    Percent_Nonzero = pct_nonzero
  )
}

weights_summary <- rbind(
  summarize_weights(nb_queen, "Queen Contiguity"),
  summarize_weights(nb_knn,   "KNN (k=5)"),
  summarize_weights(nb_dist,  "Distance (5 km)")
) %>%
  mutate(across(where(is.numeric), round, 2))

print(weights_summary)

# ============================================================
# 7. LISA MAPS (Single year — 2024)
# ============================================================

cluster_palette <- c(
  "Not Significant" = "gray90",
  "High-High"       = "red",
  "Low-Low"         = "blue",
  "High-Low"        = "pink",
  "Low-High"        = "lightblue"
)

## Helper: run LISA and return sf with cluster_type columns at all 3 thresholds
run_lisa <- function(sf_obj, variable, listw) {
  sf_clean <- sf_obj %>% filter(!is.na(.data[[variable]]))
  lm_res   <- localmoran(sf_clean[[variable]], listw, zero.policy = TRUE)
  std_var  <- as.numeric(scale(sf_clean[[variable]]))
  lag_var  <- lag.listw(listw, std_var, zero.policy = TRUE)
  
  sf_clean %>%
    mutate(
      local_I         = lm_res[, "Ii"],
      local_I_z       = lm_res[, "Z.Ii"],
      local_p         = lm_res[, "Pr(z != E(Ii))"],
      significant_05  = local_p < 0.05,
      significant_01  = local_p < 0.01,
      significant_001 = local_p < 0.001,
      var_std         = std_var,
      var_lag         = lag_var,
      cluster_raw = case_when(
        var_std >  0 & var_lag >  0 ~ "High-High",
        var_std <  0 & var_lag <  0 ~ "Low-Low",
        var_std >  0 & var_lag <  0 ~ "High-Low",
        var_std <  0 & var_lag >  0 ~ "Low-High"
      ),
      cluster_p05  = ifelse(significant_05,  cluster_raw, "Not Significant"),
      cluster_p01  = ifelse(significant_01,  cluster_raw, "Not Significant"),
      cluster_p001 = ifelse(significant_001, cluster_raw, "Not Significant")
    )
}

## Helper: build the three side-by-side threshold maps
threshold_maps <- function(lisa_sf, title_prefix) {
  make_map <- function(col, label) {
    tm_shape(lisa_sf) +
      tm_fill(col,
              palette = cluster_palette,
              title   = label) +
      tm_borders(col = "white", lwd = 0.5) +
      tm_layout(main.title      = label,
                main.title.size = 0.9,
                legend.show     = FALSE)
  }
  tmap_arrange(
    make_map("cluster_p05",  paste(title_prefix, "p < 0.05")),
    make_map("cluster_p01",  paste(title_prefix, "p < 0.01")),
    make_map("cluster_p001", paste(title_prefix, "p < 0.001")),
    ncol = 3
  )
}

## 7.1 Rent Burden LISA (KNN weights — matches your original choice)
oahu_rb <- oahu_no_military %>% filter(!is.na(rent_burden))
coords_rb  <- st_coordinates(st_centroid(oahu_rb))
nb_knn_rb  <- knn2nb(knearneigh(coords_rb, k = 6))
listw_knn_rb <- nb2listw(nb_knn_rb, style = "W", zero.policy = TRUE)

lisa_rent_burden <- run_lisa(oahu_rb, "rent_burden", listw_knn_rb)
threshold_maps(lisa_rent_burden, "Rent Burden —")

## 7.2 Rent Burden Global Moran's I
global_moran_rb <- moran.test(lisa_rent_burden$rent_burden,
                              listw_knn_rb, zero.policy = TRUE)
print(global_moran_rb)

## 7.3 Local-Born LISA (Queen weights — matches your original choice)
oahu_lb <- oahu_no_military %>% filter(!is.na(local_born))
nb_queen_lb    <- poly2nb(oahu_lb, queen = TRUE)
listw_queen_lb <- nb2listw(nb_queen_lb, style = "W", zero.policy = TRUE)

lisa_local_born <- run_lisa(oahu_lb, "local_born", listw_queen_lb)
threshold_maps(lisa_local_born, "Local-Born —")

## 7.4 Local-Born Global Moran's I
global_moran_lb <- moran.test(lisa_local_born$local_born,
                              listw_queen_lb, zero.policy = TRUE)
print(global_moran_lb)

# ============================================================
# 8. TEMPORAL LISA MAPS (2010–2024)
# ============================================================

acs_years <- seq(2012, 2024, by = 2)

urban_honolulu <- places(state = "HI", cb = TRUE) %>%
  filter(NAME == "Urban Honolulu") %>%
  st_transform(4326)
## 8.1 Function to download and clip one ACS year to Urban Honolulu
get_urban_honolulu_year <- function(yr) {
  
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
  ) %>%
    st_transform(4326) %>%
    st_make_valid()
  
  # USE SAME BOUNDARY EVERY TIME
  centroids <- st_centroid(dat)
  in_uh     <- st_within(centroids, urban_honolulu, sparse = FALSE)[,1]
  dat       <- dat[in_uh, ]
  
  dat <- st_transform(dat, 32604) %>%
    mutate(
      local_born  = 100 * born_in_hiE / total_popE,
      rent_burden = 100 * (median_rentE * 12 / median_incomeE),
      year        = yr
    )
  
  mil_tracts <- if (yr < 2020) military_bases_10 else military_bases_20
  dat <- dat %>% filter(!GEOID %in% mil_tracts)
  
  return(dat)
}

## 8.2 Download all years
message("Downloading ACS data for all years — this may take a few minutes...")
oahu_by_year <- setNames(
  lapply(acs_years, get_urban_honolulu_year),
  as.character(acs_years)
)

## 8.3 LISA helper for temporal panels (Queen, p < 0.05)
lisa_clusters_temporal <- function(sf_obj, variable) {
  sf_clean <- sf_obj %>% filter(!is.na(.data[[variable]]))
  nb   <- poly2nb(sf_clean, queen = TRUE)
  lw   <- nb2listw(nb, style = "W", zero.policy = TRUE)
  lm_res   <- localmoran(sf_clean[[variable]], lw, zero.policy = TRUE)
  std_var  <- as.numeric(scale(sf_clean[[variable]]))
  lag_var  <- lag.listw(lw, std_var, zero.policy = TRUE)
  
  sf_clean %>%
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
}

## 8.4 Build one tmap panel per year
rent_maps <- lapply(acs_years, function(yr) {
  sf_lisa <- lisa_clusters_temporal(oahu_by_year[[as.character(yr)]], "rent_burden")
  tm_shape(sf_lisa) +
    tm_fill("cluster_type",
            palette  = cluster_palette,
            showNA   = FALSE) +
    tm_borders(col = "white", lwd = 0.3) +
    tm_layout(main.title      = as.character(yr),
              main.title.size = 0.8,
              legend.show     = FALSE)
})

local_born_maps <- lapply(acs_years, function(yr) {
  sf_lisa <- lisa_clusters_temporal(oahu_by_year[[as.character(yr)]], "local_born")
  tm_shape(sf_lisa) +
    tm_fill("cluster_type",
            palette  = cluster_palette,
            showNA   = FALSE) +
    tm_borders(col = "white", lwd = 0.3) +
    tm_layout(main.title      = as.character(yr),
              main.title.size = 0.8,
              legend.show     = FALSE)
})

## 8.5 Display: rent burden row, then local born row
n_years <- length(acs_years)
tmap_arrange(
  c(rent_maps, local_born_maps),
  ncol          = n_years,
  nrow          = 2,
  outer.margins = 0.01
)

## 8.6 Standalone legend panel
legend_sf <- lisa_clusters_temporal(oahu_by_year[["2024"]], "rent_burden")
tm_shape(legend_sf) +
  tm_fill("cluster_type",
          palette = cluster_palette,
          title   = "LISA Cluster (p < 0.05)") +
  tm_borders(col = "white", lwd = 0.3) +
  tm_layout(legend.only = TRUE)