# Do a Johnston-only pipeline

pacman::p_load(
  here,
  nngeo,
  sf,
  readxl,
  janitor,
  tidyverse,
  terra,
  units
)

assign_near_far <- function(dist, x){
  case_when((dist >= 0 & dist <= x) ~ "near",
            (dist > x & dist <= (2 * x)) ~ "far",
            T ~ NA_character_)
}

# Looking to see if PRI Johnston has fishing activity near by
# The chunk below comes from the clean_lmpas script

sf_use_s2(F)
# Load data --------------------------------------------------------------------
mpas <- list.files(path =  here("data", "raw", "WDPA_WDOECM_Feb2023_Public_marine_shp"),
                   pattern = "polygons.shp",
                   full.names = T,
                   recursive = T) %>%
  map_dfr(st_read) %>%
  st_as_sf() %>%
  clean_names()

list_LSMPAs <- read_excel(path = here("data", "list_LSMPAs.xlsx"), sheet = "Table for Paper") %>%
  clean_names() %>%
  filter(!included == "No",
         !wdpa_id == "555577562", # I am manually removing this on August 16, waiting for John to send the updated excel file
         !wdpa_id == "") %>%
  mutate(wdpa_id = str_replace(wdpa_id, "\\\\textunderscore ", "_"),
         year_enforced = as.numeric(str_extract(month_year_enforced, "[:digit:]{4}")),
         month_enforced = str_extract(month_year_enforced, "[:alpha:]*")) %>%
  select(name, wdpa_id, ocean, eez, no_take_area_km2, month_enforced, year_enforced, included)

# Select relevant columns only -------------------------------------------------
clean_lmpas <- mpas %>%
  select(wdpaid, wdpa_pid, name) %>%
  mutate(wdpaid = as.character(wdpaid)) %>%
  filter((wdpa_pid %in% list_LSMPAs$wdpa_id |
            wdpaid %in% list_LSMPAs$wdpa_id)) %>%
  left_join(list_LSMPAs, by = c("wdpa_pid" = "wdpa_id")) %>%
  left_join(list_LSMPAs, by = c("wdpaid" = "wdpa_id")) %>%
  mutate(ocean = coalesce(ocean.x, ocean.y),
         year_enforced = coalesce(year_enforced.x, year_enforced.y),
         month_enforced = coalesce(month_enforced.x, month_enforced.y)) %>%
  select(wdpaid, name, ocean, year_enforced, month_enforced) %>%
  distinct() %>%
  group_by(wdpaid, name, ocean, year_enforced, month_enforced) %>%
  summarize(a = 1) %>%
  ungroup() %>%
  select(-a) %>%
  st_remove_holes()


john <- clean_lmpas %>%
  filter(wdpaid == "400011") %>%
  head(1) %>%
  st_cast("POLYGON") %>%
  mutate(area = st_area(.),
         area = units::set_units(area, "km^2")) %>%
  filter(area > units::set_units(1e5, "km^2")) %>%
  mutate(poly = LETTERS[1:nrow(.)]) %>%
  mutate(name = case_when(poly == "A" ~ "PRI (Wake)",
                          poly == "B" ~ "PRI (Jarvis)",
                          poly == "C" ~ "PRI (Johnston)"),
         wdpaid = paste(wdpaid, poly, sep = "_")) %>%
  select(-poly) %>%
  filter(name == "PRI (Johnston)") # In the original script, this condition is negated

data <- readRDS(file = here("data", "processed", "rfmo_all_annual_gear_flag.rds"))

# Visual check ++++++++++++++++++++++++++++++++++++++++
grid <- data %>%
  select(gear, lat, lon) %>%
  distinct()

ggplot() +
  geom_point(data = grid, aes(x = lon, y = lat)) +
  geom_sf(data = john, fill = "transparent", color = "red") +
  facet_wrap(~gear, ncol = 1)

# END VISUAL CHCK +++++++++++++++++++++++++++++++++++

cells_1x1_within <-
  rast(nrows = 180, ncols = 360,
       xmin = -180, xmax = 180,
       ymin = -90, ymax = 90,
       nlyrs = 1, crs = "EPSG: 4326",
       resolution = 1, val = 1) %>%
  # Identify which cells are within an MPA polygon
  terra::extract(john,
                 xy = T,
                 exact = T)  %>% # The `exact = T` argument calculates the percent cover
  mutate(grid = "1x1") %>%
  select(grid, lon = x, lat = y, fraction)

cells_5x5_within <-
  rast(nrows = 180, ncols = 360,
       xmin = -180, xmax = 180,
       ymin = -90, ymax = 90,
       nlyrs = 1, crs = "EPSG: 4326",
       resolution = 5, val = 1) %>%
  # Identify which cells are within an MPA polygon
  terra::extract(john,
                 xy = T,
                 exact = T) %>%
  mutate(grid = "5x5") %>%
  select(grid, lon = x, lat = y, fraction)


cells_within <- bind_rows(cells_1x1_within,
                          cells_5x5_within) %>%
  filter(fraction >= 1) %>%
  select(-fraction)


# Build spatial grid -----------------------------------------------------------
# THere are a few steps here:
# - First, build a grid of unique lat-lon combinations where RFMOs actually report
# some data
# - Then, remove cells that we've identified as being FULLY covered by an MPA
# - We then calculate the distance of each point to the nearest MP

# Create a single feature to keep things tidy
john_sfc <- john %>%
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
  mutate(dist = st_distance(., john_sfc),
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
  st_join(john, st_nearest_feature)

# Now convert it into a data frame
rfmo_grid <- rfmo_spat_grid %>%
  bind_cols(st_coordinates(.)) %>%
  st_drop_geometry() %>%
  rename(lon = X, lat = Y) %>%
  group_by(lat, lon) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  select(id, grid, lat, lon, wdpaid, name, dist, contains("treatment"), contains("near_"), ocean, year_enforced, month_enforced)


combined <- data %>%
  left_join(rfmo_grid, by = c("grid", "lon", "lat")) %>%
  mutate(id = paste(lat, lon, sep = "_"),
         event = year - year_enforced,
         post = 1 * (event >= 0)) %>%
  filter(cpue_tot > 0) %>%
  drop_na(name)


combined %>%
  filter(dist <= 200,
         between(event, -10, 10)) %>%
  group_by(gear, post, near_100) %>%
  count()

john_panel <- combined %>%
  filter(dist <= 200,
         between(event, -10, 10)) %>%
  rename(near = near_100)


mod <- fixest::feols(log(cpue_tot) ~ post + near + post:near | csw(0, id, flag, wdpaid ^ year),,
              panel.id = ~id + year,
              data = john_panel,
              vcov = conley(cutoff = 200))


modelsummary::modelsummary(mod, coef_omit = "^(?!.*:)",
                           stars = panelsummary:::econ_stars(),
                           gof_omit = "RMSE|IC|W")
