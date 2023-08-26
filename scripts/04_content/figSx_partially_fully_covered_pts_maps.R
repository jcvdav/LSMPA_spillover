################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
# Build a map od Nazca showing points partially covered that we keep
# and points fully covered that we drop
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------

# Load data --------------------------------------------------------------------

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
pacman::p_load(
  here,
  sf,
  tidyverse
)

data <- readRDS(file = here("data", "processed", "rfmo_all_annual_gear_flag.rds"))
lmpas <- st_read(dsn = here("data", "processed", "clean_lmpas.gpkg"))
nazca <- lmpas %>% filter(wdpaid == "555624169")

pts_near <- data %>%
  select(lat, lon) %>%
  distinct() %>%
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326") %>%
  mutate(dist = as.numeric(st_distance(., nazca))) %>%
  filter(dist < 2e5) %>%
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326")


pts <- data %>%
  select(lat, lon) %>%
  distinct() %>%
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326") %>%
  st_filter(nazca, .predicate = st_within)

ggplot() +
  geom_sf(data = nazca,
          color = "steelblue",
          linewidth = 1) +
  geom_sf(data = pts_near) +
  geom_sf(data = pts, color = "red") +
  scale_y_continuous(breaks = seq(-30, -22, 1)) +
  scale_x_continuous(breaks = seq(-85, -76, 1))
