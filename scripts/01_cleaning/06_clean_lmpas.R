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
  nngeo,
  sf,
  readxl,
  janitor,
  tidyverse
)

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
  select(wdpaid, name, ocean, year_enforced, month_enforced) %>%
  distinct() %>%
  group_by(wdpaid, name, ocean, year_enforced, month_enforced) %>%
  summarize(a = 1) %>%
  ungroup() %>%
  select(-a) %>%
  st_remove_holes()

sf_use_s2(T)
pri <- clean_lmpas %>%
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
  filter(!name == "PRI (Johnston)")

coral_sea <- clean_lmpas %>%
  filter(wdpaid == "555556875") %>%
  st_cast("POLYGON") %>%
  st_make_valid() %>%
  mutate(area = st_area(.),
         area = units::set_units(area, "km^2")) %>%
  filter(area > units::set_units(1e5, "km^2")) %>%
  mutate(poly = LETTERS[1:nrow(.)],
         wdpaid = paste(wdpaid, poly, sep = "_"),
         name = "Coral Sea - NPZ")%>%
  select(-poly)

papa <- clean_lmpas %>%
  filter(wdpaid == "220201") %>%
  st_cast("POLYGON") %>%
  mutate(area = st_area(.),
        area = units::set_units(area, "m^2")) %>%
  filter(area > units::set_units(1, "m^2")) %>%
  st_wrap_dateline(options = "WRAPDATELINE=YES") %>%
  group_by(wdpaid, name, ocean, year_enforced, month_enforced) %>%
  summarize(a = 1) %>%
  ungroup() %>%
  select(-a)

final_mpas <- clean_lmpas %>%
  filter(!wdpaid %in% c("400011", "555556875", "220201")) %>%
  bind_rows(pri, coral_sea, papa) %>%
  mutate(area = st_area(.),
         area = units::set_units(area, "km^2")) %>%
  filter(area > units::set_units(1e5, "km^2"))


## EXPORT ######################################################################

# Save to disk -----------------------------------------------------------------
st_write(obj = final_mpas,
         dsn = here("data", "processed", "clean_lmpas.gpkg"),
         delete_dsn = T)
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
  nngeo,
  sf,
  readxl,
  janitor,
  tidyverse
)

sf_use_s2(F)

# Load data --------------------------------------------------------------------
mpas <- list.files(path =  here("data", "raw", "WDPA_WDOECM_Feb2023_Public_marine_shp"),
                   pattern = "polygons.shp",
                   full.names = T,
                   recursive = T) %>%
  map_dfr(st_read) %>%
  st_as_sf() %>%
  clean_names()

list_LSMPAs <- read_excel(path = here("data", "list_LSMPAs_081523.xlsx"), sheet = "Table for Paper") %>%
  clean_names() %>%
  filter(!included == "No",
         !wdpa_id == "555577562", # I am manually removing this on August 16, waiting for John to send the updated excel file
         !wdpa_id == "") %>%
  mutate(wdpa_id = str_replace(wdpa_id, "\\\\textunderscore ", "_"),
         year_enforced = as.numeric(str_extract(month_year_enforced, "[:digit:]{4}")),
         month_enforced = str_extract(month_year_enforced, "[:alpha:]*")) %>%
  select(name, wdpa_id, ocean, eez, no_take_area_km2, month_enforced, year_enforced, included)

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
  select(wdpaid, name, ocean, year_enforced, month_enforced) %>%
  distinct() %>%
  group_by(wdpaid, name, ocean, year_enforced, month_enforced) %>%
  summarize(a = 1) %>%
  ungroup() %>%
  select(-a) %>%
  st_remove_holes()

sf_use_s2(T)
pri <- clean_lmpas %>%
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
  filter(!name == "PRI (Johnston)")

coral_sea <- clean_lmpas %>%
  filter(wdpaid == "555556875") %>%
  st_cast("POLYGON") %>%
  st_make_valid() %>%
  mutate(area = st_area(.),
         area = units::set_units(area, "km^2")) %>%
  filter(area > units::set_units(1e5, "km^2")) %>%
  mutate(poly = LETTERS[1:nrow(.)],
         wdpaid = paste(wdpaid, poly, sep = "_"),
         name = "Coral Sea - NPZ")%>%
  select(-poly)

papa <- clean_lmpas %>%
  filter(wdpaid == "220201") %>%
  st_cast("POLYGON") %>%
  mutate(area = st_area(.),
        area = units::set_units(area, "m^2")) %>%
  filter(area > units::set_units(1, "m^2")) %>%
  st_wrap_dateline(options = "WRAPDATELINE=YES") %>%
  group_by(wdpaid, name, ocean, year_enforced, month_enforced) %>%
  summarize(a = 1) %>%
  ungroup() %>%
  select(-a)

final_mpas <- clean_lmpas %>%
  filter(!wdpaid %in% c("400011", "555556875", "220201")) %>%
  bind_rows(pri, coral_sea, papa) %>%
  mutate(area = st_area(.),
         area = units::set_units(area, "km^2")) %>%
  filter(area > units::set_units(1e5, "km^2"))


## EXPORT ######################################################################

# Save to disk -----------------------------------------------------------------
st_write(obj = final_mpas,
         dsn = here("data", "processed", "clean_lmpas.gpkg"),
         delete_dsn = T)
