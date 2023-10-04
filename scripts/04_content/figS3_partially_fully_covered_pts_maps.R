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
pacman::p_load(
  here,
  sf,
  tidyverse
)

# Source custom funcitons and palettes -----------------------------------------
source(here("scripts", "00_set_up.R"))

# Load data --------------------------------------------------------------------
data <- readRDS(file = here("data", "processed", "rfmo_all_annual_gear_flag.rds"))
lmpas <- st_read(dsn = here("data", "processed", "clean_lmpas.gpkg"))
nazca <- lmpas %>% filter(wdpaid == "555624169")

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
pts_near <- data %>%
  select(lat, lon) %>%
  distinct() %>%
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326") %>%
  mutate(dist = as.numeric(st_distance(., nazca))) %>%
  filter(dist < 2e5) %>%
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326")


pts_partial <- data %>%
  select(lat, lon) %>%
  distinct() %>%
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326") %>%
  st_filter(nazca, .predicate = st_within)

pt_inside <- tibble(lon = -82.5, lat = -25.5) %>%
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326")

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
p <- ggplot() +
  geom_sf(data = nazca,
          color = "steelblue",
          fill = "transparent",
          linewidth = 1) +
  geom_sf(data = pts_near, color = "gray") +
  geom_sf(data = pt_inside, color = "red") +
  geom_sf(data = pts_partial) +
  scale_y_continuous(breaks = seq(-30, -22, 1)) +
  scale_x_continuous(breaks = seq(-85, -76, 1)) +
  theme(panel.grid.major = element_line(color = "black"))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
startR::lazy_ggsave(plot = p,
                    filename = "figS3_partially_fully_covered_pts_map",
                    width = 12,
                    height = 10)





