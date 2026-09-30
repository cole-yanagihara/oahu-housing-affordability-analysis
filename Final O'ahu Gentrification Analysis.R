library(tidycensus)
library(tidyverse)
library(sf)
library(tmap)
library(ggrepel)
library(spdep)
library(tigris)

# ============================================================
# 1. SETUP
# ============================================================
## 1A. Get ACS Data for Honolulu County
oahu_spatial <- get_acs(
  geography = "tract",
  variables = c(
    total_pop = "B01003_001",
    median_income = "B19013_001",
    median_rent = "B25064_001",
    born_in_hi = "B05002_003"
  ),
  state = "HI",
  county = "Honolulu",
  year = 2024,
  output = "wide",
  geometry = TRUE,
  cb = TRUE
)

## 1B. Import Tract Names and Bind to ACS Dataset
hawaii_names <- read_csv("data/2020_Census_Tracts.csv")
hawaii_names <- hawaii_names %>%
  select(geoid20, tractname) %>%
  rename(
    GEOID = geoid20,
    tract_name = tractname
  )
hawaii_names$GEOID <- as.character(hawaii_names$GEOID)
oahu_spatial <- oahu_spatial %>%
  left_join(hawaii_names, by = "GEOID")

## 1C. Bind Data to O'ahu
oahu_spatial <- st_transform(oahu_spatial, 4326)
oahu_spatial <- oahu_spatial %>%
  filter(
    st_coordinates(st_centroid(.))[,1] > -158.3 &
      st_coordinates(st_centroid(.))[,1] < -157.6 &
      st_coordinates(st_centroid(.))[,2] > 21.2 &
      st_coordinates(st_centroid(.))[,2] < 21.8
  )
oahu_spatial <- st_transform(oahu_spatial, 32604)

## 1D. Creating Local Born and Rent Burden Variables
oahu_spatial <- oahu_spatial %>%
  mutate(
    local_born = 100 * born_in_hiE / total_popE,
    rent_burden = 100 * (median_rentE * 12 / median_incomeE),
  )

## 1E. Importing Military Base Data
### military base tracts in GEOID20
military_bases_20 <- c(
  #Marine Corps Base Hawai’i (MCBH)
  "15003981700","15003981802","15003981801","15003981803",
  #Schofield Barracks
  "15003009507","15003009512","15003009511","15003009509",
  "15003009508","15003009510","15003980600","15003980700",
  #Wheeler Army Airfield
  "15003009000",
  #Pearl Harbor and Hickam Air Force Base
  "15003007400","15003007100","15003007302","15003981900",
  "15003007002","15003007001","15003011401","15003982200",
  "15003006900",
  #Aliamanu Military Reservation
  "15003006810","15003006811",
  #Bellows Air Force Station
  "15003981100",
  #Fort Shafter
  "15003982000","15003981300"
)
### military base tracts in GEOID10
military_bases_10 <- c(
  #Marine Corps Base Hawai’i (MCBH)
  "15003010801","15003010802",
  #Schofield Barracks
  "15003009507","15003009502","15003009501","15003009503",
  "15003009504","15003980600","15003980700",
  #Wheeler Army Airfield
  "15003009000",
  #Pearl Harbor and Hickam Air Force Base
  "15003007400","15003007303","15003007302","15003007100",
  "15003007000","15003007000","15003011400","15003006900",
  #Aliamanu Military Reservation
  "15003006804",
  #Bellows Air Force Station
  "15003981100",
  #Fort Shafter
  "15003006600"
)
### adding military tract indicator
oahu_spatial <- oahu_spatial %>%
  mutate(
    military_base = GEOID %in% military_bases_20
  )


# ============================================================
# 2. EXPLORATORY VISUALIZATIONS INCLUDING MILITARY DATA
# ============================================================
## 2A. Local Born vs Rent Burden Scatterplot
ggplot(oahu_spatial, aes(x = local_born, y = rent_burden)) +
  geom_point(aes(color = military_base), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  geom_text_repel(
    data = subset(oahu_spatial, rent_burden > 54),
    aes(label = tract_name),
    size = 3
  ) +
  labs(
    title = "Percent Born in Hawaiʻi vs Rent Burden 
(O'ahu Census Tracts including Military)",
    x = "Percent Born in Hawaiʻi",
    y = "Percent of Income Spent on Rent",
    color = "Military Tract"
  ) +
  theme_minimal()
cor(oahu_spatial$local_born, oahu_spatial$rent_burden, use = "complete.obs")
summary(lm(rent_burden ~ local_born, data = oahu_spatial))

## 2B. Military Installations Map
tm_shape(oahu_spatial) +
  tm_polygons(col = "grey85",
              border.col = "white") +
  tm_shape(oahu_spatial[oahu_spatial$military_base, ]) +
  tm_fill(col = "red", alpha = 0.9) +
  tm_borders(col = "black", lwd = 1.2) +
  tm_layout(main.title = "Military Installation Census Tracts on Oʻahu",
            legend.show = FALSE)

# ============================================================
# 3. EXPLORATORY VISUALIZATIONS EXCLUDING MILITARY DATA
# ============================================================
## 3A. Military Excluding Setup
oahu_no_military <- oahu_spatial %>% filter(!military_base)

## 3B. Local Born vs Rent Burden Scatterplot and Correlation Coefficient
ggplot(oahu_no_military, aes(x = local_born, y = rent_burden)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text_repel(
    data = subset(oahu_no_military, rent_burden > 40 | rent_burden < 10),
    aes(label = tract_name),
    size = 3
  ) +
  labs(
    title = "Percent Born in Hawaiʻi vs Rent Burden 
(O'ahu Census Tracts excluding Military)",
    x = "Percent Born in Hawaiʻi",
    y = "Percent of Income Spent on Rent"
  ) +
  theme_minimal()
cor(oahu_no_military$local_born, oahu_no_military$rent_burden, use = "complete.obs")
summary(lm(rent_burden ~ local_born, data = oahu_no_military))

## 3C. Rent Burden Choropleth Map
tm_shape(oahu_no_military) +
  tm_polygons("rent_burden",
              palette = "BuRd",
              style = "quantile",
              alpha = 0.7,
              colorNA = "gray85",
              title = "Rent Burden in O'ahu") +
  tm_layout(main.title = "Rent Burden in O'ahu",
            legend.outside = TRUE)

## 3D. Local Born Choropleth Map
tmap_mode("plot")
tm_shape(oahu_no_military) +
  tm_polygons("local_born",
              palette = "BuRd",
              style = "quantile",3
              alpha = 0.7,
              colorNA = "gray85",
              title = "Percent of Residents born within Hawai'i") +
  tm_layout(main.title = "Percent of Residents born within Hawai'i",
            legend.outside = TRUE)

## 3E. Median Rent Choropleth Map
tm_shape(oahu_no_military) +
  tm_polygons("median_rentE",
              palette = "BuRd",
              style = "quantile",
              alpha = 0.7,
              colorNA = "gray85",
              title = "Median Rent in O'ahu") +
  tm_layout(main.title = "Median Rent in O'ahu",
            legend.outside = TRUE)

# ============================================================
# 4. O'AHU LISA MAP
# ============================================================
## 4.1 Setup
coords <- st_coordinates(st_centroid(oahu_no_military))
crs_proj <- st_crs(oahu_no_military)$proj4string
### Queen Contiguity
nb_queen    <- poly2nb(oahu_no_military, queen = TRUE)
queen_lines <- nb2lines(nb_queen, coords = coords, proj4string = crs_proj) %>%
  st_as_sf()
### K=5 Nearest Neighbors
nb_knn      <- knn2nb(knearneigh(coords, k = 5))
knn_lines   <- nb2lines(nb_knn, coords = coords, proj4string = crs_proj) %>%
  st_as_sf()
### Distance-based (5 km threshold)
nb_dist     <- dnearneigh(coords, 0, 5000)
dist_lines  <- nb2lines(nb_dist, coords = coords, proj4string = crs_proj) %>%
  st_as_sf()

## 4.2 Contiguity comparison map
queen_map <- tm_shape(oahu_no_military) +
  tm_polygons(alpha = 0.6) +
  tm_shape(queen_lines) +
  tm_lines(col = "red") +
  tm_layout(main.title = "Queen Contiguity")
neighbors_map <- tm_shape(oahu_no_military) +
  tm_polygons(alpha = 0.6) +
  tm_shape(knn_lines) +
  tm_lines(col = "blue") +
  tm_layout(main.title = "K-Nearest Neighbors (k=5)")
distance_map <- tm_shape(oahu_no_military) +
  tm_polygons(alpha = 0.6) +
  tm_shape(dist_lines) +
  tm_lines(col = "green") +
  tm_layout(main.title = "Distance-Based (5 km)")
tmap_arrange(queen_map, neighbors_map, distance_map, ncol = 3)

## 4.3 Weights summary table
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

## 4.4 Rent Burden LISA Map O'ahu 2024
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

## 4.5 Global Moran's I Test Rent Burden
global_moran <- moran.test(oahu_rent_burden$rent_burden, listw_knn, zero.policy = TRUE)
global_moran

## 4.6 Local Born LISA Map O'ahu 2024
oahu_local_born <- oahu_no_military %>%
  filter(!is.na(local_born))
coords <- st_coordinates(st_centroid(oahu_local_born))
knn <- knearneigh(coords, k = 5)
nb_knn <- knn2nb(knn)
listw_knn <- nb2listw(nb_knn, style = "W", zero.policy = TRUE)
local_moran <- localmoran(oahu_local_born$local_born, listw_knn)
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
    local_born_lag = lag.listw(listw_knn, local_born_std)
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

## 4.7 Global Moran's I Test Local Born
global_moran <- moran.test(oahu_local_born$local_born, listw_knn, zero.policy = TRUE)
global_moran

# ============================================================
# 5. O'AHU TEMPORAL LISA MAP
# ============================================================
## 5.1 Setup
acs_years <- seq(2010, 2024, by = 2)
get_oahu_year <- function(yr) {
  oahu_data <- get_acs(
    geography = "tract",
    variables = c(
      total_pop     = "B01003_001",
      median_income = "B19013_001",
      median_rent   = "B25064_001",
      born_in_hi    = "B05002_003"
    ),
    state    = "HI",
    county = "Honolulu",
    year     = yr,
    output   = "wide",
    geometry = TRUE,
    cb       = TRUE
  )
  oahu_data <- st_transform(oahu_data, 4326)
  coords_wgs <- st_coordinates(st_centroid(oahu_data))
  oahu_data <- oahu_data[coords_wgs[,1] > -158.3 & coords_wgs[,1] < -157.6 &
               coords_wgs[,2] >   21.2 & coords_wgs[,2] <   21.8, ]
  oahu_data <- st_transform(oahu_data, 32604)
  oahu_data <- oahu_data %>%
    mutate(
      local_born  = 100 * born_in_hiE / total_popE,
      rent_burden = 100 * (median_rentE * 12 / median_incomeE),
      year        = yr
    ) 
  if (yr < 2020) {
    oahu_data <- oahu_data %>% filter(!GEOID %in% military_bases_10)
  } else {
    oahu_data <- oahu_data %>% filter(!GEOID %in% military_bases_20)
  }
  return(oahu_data)
}
oahu_by_year <- setNames(
  lapply(acs_years, get_oahu_year),
  as.character(acs_years)
)

## 5.2 Building Queen Weights and setting LISA cluster at p < 0.05
lisa_clusters <- function(sf_obj, variable) {
  sf_clean <- sf_obj %>% filter(!is.na(.data[[variable]]))
  nb   <- poly2nb(sf_clean, queen = TRUE)
  lw   <- nb2listw(nb, style = "W", zero.policy = TRUE)
### creating local moran's I
  lm_res <- localmoran(sf_clean[[variable]], lw, zero.policy = TRUE)
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

## 5.3 Plotting Temporal LISA Maps
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
n_years <- length(acs_years)
tmap_arrange(
  c(rent_maps, local_born_maps),
  ncol  = n_years,
  nrow  = 2,
  outer.margins = 0.01
)

## 5.4 Creating Separate Legend
legend_sf <- lisa_clusters(oahu_by_year[["2024"]], "rent_burden")
legend_panel <- tm_shape(legend_sf) +
  tm_fill("cluster_type",
          palette = cluster_palette,
          title   = "LISA Cluster (p < 0.05)") +
  tm_borders(col = "white", lwd = 0.3) +
  tm_layout(
    main.title      = "Legend",
    main.title.size = 0.9,
    legend.only     = TRUE 
  )
legend_panel



# ============================================================
# 6. HONOLULU TEMPORAL LISA MAP
# ============================================================
## 6.1 Setup
### creating honolulu boundary
urban_honolulu <- places(state = "HI", cb = TRUE) %>%
  filter(NAME == "Urban Honolulu") %>%
  st_transform(4326)
### years 
acs_years <- seq(2012, 2024, by = 2)
### pulling acs data
get_honolulu_year <- function(yr) {
  honolulu_data <- get_acs(
    geography = "tract",
    variables = c(
      total_pop     = "B01003_001",
      median_income = "B19013_001",
      median_rent   = "B25064_001",
      born_in_hi    = "B05002_003"
    ),
    state    = "HI",
    county = "Honolulu",
    year     = yr,
    output   = "wide",
    geometry = TRUE,
    cb       = TRUE
  )
  honolulu_data <- st_transform(honolulu_data, 4326)
  centroids <- st_centroid(honolulu_data)
  in_urban <- st_within(
    centroids,
    urban_honolulu,
    sparse = FALSE
  )[,1]
  honolulu_data <- honolulu_data %>% filter(in_urban)
  honolulu_data <- st_transform(honolulu_data, 32604)
  honolulu_data <- honolulu_data %>%
    mutate(
      local_born  = 100 * born_in_hiE / total_popE,
      rent_burden = 100 * (median_rentE * 12 / median_incomeE),
      year        = yr
    ) 
  if (yr < 2020) {
    honolulu_data <- honolulu_data %>% filter(!GEOID %in% military_bases_10)
  } else {
    honolulu_data <- honolulu_data %>% filter(!GEOID %in% military_bases_20)
  }
  return(honolulu_data)
}
honolulu_by_year <- setNames(
  lapply(acs_years, get_honolulu_year),
  as.character(acs_years)
)

## 6.2 Building Queen Weights and setting LISA cluster at p < 0.05
lisa_clusters <- function(sf_obj, variable) {
  sf_clean <- sf_obj %>% filter(!is.na(.data[[variable]]))
  nb   <- poly2nb(sf_clean, queen = TRUE)
  lw   <- nb2listw(nb, style = "W", zero.policy = TRUE)
### creating local moran's I
  lm_res <- localmoran(sf_clean[[variable]], lw, zero.policy = TRUE)
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

##6.3 Plotting Temporal LISA Maps
cluster_palette <- c(
  "Not Significant" = "gray90",
  "High-High"       = "red",
  "Low-Low"         = "blue",
  "High-Low"        = "pink",
  "Low-High"        = "lightblue"
)
rent_maps <- lapply(acs_years, function(yr) {
  sf_lisa <- lisa_clusters(honolulu_by_year[[as.character(yr)]], "rent_burden")
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
  sf_lisa <- lisa_clusters(honolulu_by_year[[as.character(yr)]], "local_born")
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
n_years <- length(acs_years)
tmap_arrange(
  c(rent_maps, local_born_maps),
  ncol  = n_years,
  nrow  = 2,
  outer.margins = 0.01
)

## 6.3 Creating Separate Legend
legend_sf <- lisa_clusters(honolulu_by_year[["2024"]], "rent_burden")
legend_panel <- tm_shape(legend_sf) +
  tm_fill("cluster_type",
          palette = cluster_palette,
          title   = "LISA Cluster (p < 0.05)") +
  tm_borders(col = "white", lwd = 0.3) +
  tm_layout(
    main.title      = "Legend",
    main.title.size = 0.9,
    legend.only     = TRUE
  )
legend_panel