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
  tidyverse
)

# Source custom functions ------------------------------------------------------
source(here("scripts/00_set_up.R"))

# Load data --------------------------------------------------------------------
distance_grid <- read_rds(file = here("data", "processed", "distance_grid.rds"))
rfmo_data <- readRDS(here("data", "processed", "rfmo_all_annual_gear_flag.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
combined <- rfmo_data %>%
  left_join(distance_grid, by = c("grid", "lon", "lat")) %>%
  mutate(id = paste(lat, lon, sep = "_"),
         event = year - year_enforced,
         post = 1 * (event >= 0)) %>%
  filter(cpue_tot > 0,
         dist <= 600)

check_mt(combined, cutoff = 0)

check_effort_gear(combined)

test(combined)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = combined,
        file = here("data", "processed", "annual_panel.rds"))
