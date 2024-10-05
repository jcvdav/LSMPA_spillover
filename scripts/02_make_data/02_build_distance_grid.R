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
  magrittr,
  here,
  terra,
  sf,
  units,
  tidyverse
)

# Source custom functions ------------------------------------------------------
source(here("scripts/00_set_up.R"))

# Define functions -------------------------------------------------------------
# This function assigns a "near" or "far" category to each point, depending on
# the value of x and the distance to an MPA
assign_near_far <- function(dist, x){
  case_when((dist >= 0 & dist <= x) ~ "near",
            (dist > x & dist <= (2 * x)) ~ "far",
            T ~ NA_character_)
}

# Load data --------------------------------------------------------------------
data <- readRDS(file = here("data", "processed", "rfmo_all_annual_gear_flag.rds"))
lmpas <- st_read(dsn = here("data", "processed", "clean_lmpas.gpkg"))


## PROCESSING ##################################################################

# Find cells covered by MPAs ---------------------------------------------

# First, build some rasters
cells_1x1_within <-
  rast(nrows = 180, ncols = 360,
       xmin = -180, xmax = 180,
       ymin = -90, ymax = 90,
       nlyrs = 1, crs = "EPSG: 4326",
       resolution = 1, val = 1) %>%
  # Identify which cells are within an MPA polygon
  terra::extract(lmpas,
          xy = T,
          exact = T)  %>% # The `exact = T` argument calculates the percent cover
  mutate(grid = "1x1") %>%
  select(grid, lon = x, lat = y, fraction)

dim(cells_1x1_within) # There are 914 (781 without Johsnton) cells on the 1X1 grid that fall
# within an MPA. Note that not all these cells may have data reported by
# an RFMO and that not all are FULLY within an MPA... we'll get there

cells_5x5_within <-
  rast(nrows = 180, ncols = 360,
       xmin = -180, xmax = 180,
       ymin = -90, ymax = 90,
       nlyrs = 1, crs = "EPSG: 4326",
       resolution = 5, val = 1) %>%
  # Identify which cells are within an MPA polygon
  terra::extract(lmpas,
          xy = T,
          exact = T) %>%
  mutate(grid = "5x5") %>%
  select(grid, lon = x, lat = y, fraction)

dim(cells_5x5_within) # There are 97 (81 without Johnston) cells within an MPA using the 5x5 grid


cells_within <- bind_rows(cells_1x1_within,
                          cells_5x5_within) %>%
  filter(fraction >= 1) %>%
  select(-fraction)

# How many are fully within?
# Without Johnston
# count(cells_within, grid)
# grid   n
# 1  1x1 367
# 2  5x5   1
##
# With Johnston
# count(cells_within, grid)
# grid   n
# 1  1x1 397
# 2  5x5   1

# Build spatial grid -----------------------------------------------------------
# There are a few steps here:
# - First, build a grid of unique lat-lon combinations where RFMOs actually report
# some data
# - Then, remove cells that we've identified as being FULLY covered by an MPA
# - We then calculate the distance of each point to the nearest MP

# Create a single feature to keep things tidy
lmpas_sfc <- lmpas %>%
  st_union()

# Make grid with distances and donuts ------------------------------------------
rfmo_spat_grid <- data %>%
  # Build distinct lat, lon pairs by grid type
  select(grid, lat, lon) %>%
  distinct() %>%
  # Remove cells identified as fully within an MPA
  anti_join(cells_within, by = c("grid", "lat", "lon")) %>% # This removes 110 grid cells that fall within an MPA and that have data
  # Convert into a spatial object
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  # Calculate distance to nearest MPA
  mutate(dist = st_distance(., lmpas_sfc),
         dist = set_units(x = dist, value = "nautical_miles"), # From now on, distances are in nautical mules
         dist = as.numeric(dist)) %>%
  # We'll keep only grid cells within 600 nautical miles of an MPA
  filter(dist <= 600) %>%
  # Build a series of dummies
  mutate(treatment_100 = assign_near_far(dist = dist, x = 100),
         treatment_200 = assign_near_far(dist = dist, x = 200),
         treatment_300 = assign_near_far(dist = dist, x = 300),
         near_100 = case_when(treatment_100 == "near" ~ 1,
                              treatment_100 == "far" ~ 0,
                              T ~ NA),
         near_200 = case_when(treatment_200 == "near" ~ 1,
                              treatment_200 == "far" ~ 0,
                              T ~ NA),
         near_300 = case_when(treatment_300 == "near" ~ 1,
                              treatment_300 == "far" ~ 0,
                              T ~ NA)) %>%
  # We now add information on which one is the nearest MPA
  st_join(lmpas, st_nearest_feature)


# Now convert it into a data frame
rfmo_grid <- rfmo_spat_grid %>%
  bind_cols(st_coordinates(.)) %>%
  st_drop_geometry() %>%
  rename(lon = X, lat = Y) %>%
  group_by(lat, lon) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  select(id, grid, lat, lon, wdpaid, name, dist, contains("treatment"), contains("near_"), ocean, year_enforced, month_enforced)

## EXPORT ######################################################################
saveRDS(object = rfmo_grid,
        file = here("data", "processed", "distance_grid.rds"))
