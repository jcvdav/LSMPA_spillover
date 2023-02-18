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
  readxl,
  janitor,
  tidyverse
)

sf_use_s2(F)

# Load data --------------------------------------------------------------------
mpas <- list.files(path = "data/raw/WDPA_WDOECM_Feb2023_Public_marine_shp/",
                   pattern = "polygons.shp",
                   full.names = T,
                   recursive = T) %>%
  map_dfr(st_read) %>%
  st_as_sf() %>%
  clean_names()

list_LSMPAs <- read_excel(path = here("data", "list_LSMPAs.xlsx")) %>%
  clean_names() %>%
  select(wdpa_id, ocean, year_enforced, month_enforced)

## PROCESSING ##################################################################

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
  select(wdpaid, wdpa_pid, name, ocean, year_enforced, month_enforced) %>%
  group_by(wdpaid, name, ocean, year_enforced, month_enforced) %>%
  summarize(a = 1) %>%
  ungroup() %>%
  select(-a)

## EXPORT ######################################################################

# Save to disk -----------------------------------------------------------------
st_write(obj = clean_lmpas,
         dsn = here("data", "processed", "clean_lmpas.gpkg"),
         delete_dsn = T)
