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
  kableExtra,
  panelsummary,
  fixest,
  tidyverse
)

# Source custom funcions -------------------------------------------------------
source(here("scripts/00_set_up.R"))

# Load data --------------------------------------------------------------------
most_relevant_panel <- readRDS(file = here("data", "processed", "annual_relevant_mpa_gears_estimation_panel.rds"))

## PROCESSING ##################################################################
# Purse seine models -----------------------------------------------------------
ps_mod <- feols(data = most_relevant_panel %>%
                  filter(gear == "purse_seine") %>%
                  mutate(dist_bin = (floor(dist / 25) * 25) + 12.5),
                fml = log(cpue_tot) ~ i(dist_bin, post, "187.5") | id + year + effort_measure,
                vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                  time = ~year,
                                                  lat = ~lat,
                                                  lon = ~lon,
                                                  cutoff = 200,
                                                  lag = 5))

# Purse seine models -----------------------------------------------------------
ll_mod <- feols(data = most_relevant_panel %>%
                  filter(gear == "longline") %>%
                  mutate(dist_bin = (floor(dist / 75) * 75) + 37.5),
                fml = log(cpue_tot) ~ i(dist_bin, post, "562.5") | id + year,
                vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                  time = ~year,
                                                  lat = ~lat,
                                                  lon = ~lon,
                                                  cutoff = 200,
                                                  lag = 5))

# EXPORT #######################################################################
# Purse seine models -----------------------------------------------------------
saveRDS(object = ps_mod,
        file = here("data", "output", "spatial_event_study_ps.rds"))

# Longline models --------------------------------------------------------------
saveRDS(object = ll_mod,
        file = here("data", "output", "spatial_event_study_ll.rds"))


