################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  sf,
  units,
  tidyverse
)

# Define functions -------------------------------------------------------------
# This function assigns a "near" or "far" category to each point, depending on
# the value of x and the distance to an MPA
assign_near_far <- function(dist, x){
  case_when((dist > 0 & dist <= x) ~ "near",
            (dist > x & dist <= (2 * x)) ~ "far",
            T ~ NA_character_)
}

# Load data --------------------------------------------------------------------
data <- readRDS(here("data", "processed", "quarterly_all_rfmos.rds"))
lmpas <- st_read(here("data", "processed", "clean_lmpas.gpkg")) %>%
  st_make_valid()



## PROCESSING ##################################################################

# Create a single feature ------------------------------------------------------
lmpas_sfc <- lmpas %>%
  st_union()

# Make grid with distances and donuts ------------------------------------------
rfmo_spat_grid <- data %>%
  select(lat, lon) %>%
  distinct() %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  mutate(dist = st_distance(., lmpas_sfc),
         dist = set_units(x = dist, value = "nautical_miles"),
         dist = as.numeric(dist)) %>%
  filter(dist > 0,
         dist <= 600) %>%
  mutate(treatment_100 = assign_near_far(dist = dist, x = 100),
         treatment_200 = assign_near_far(dist = dist, x = 200),
         treatment_300 = assign_near_far(dist = dist, x = 300)) %>%
  st_join(lmpas, st_nearest_feature)


rfmo_grid <- rfmo_spat_grid %>%
  bind_cols(st_coordinates(.)) %>%
  st_drop_geometry() %>%
  rename(lon = X, lat = Y) %>%
  select(lat, lon, wdpaid, name, dist, contains("treatment"), ocean, year_enforced, month_enforced)


saveRDS(object = rfmo_grid,
        file = here("data", "processed", "distance_grid.rds"))
