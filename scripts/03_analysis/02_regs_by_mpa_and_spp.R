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
  panelsummary,
  fixest,
  tidyverse
)

# Source custom functions ------------------------------------------------------
source(here("scripts/00_set_up.R"))

# Load data --------------------------------------------------------------------
most_relevant_panel <- readRDS(file = here("data", "processed", "annual_relevant_mpa_gears_estimation_panel.rds"))

## PROCESSING ##################################################################
# We will need this panel for the species-level regressions
panel_for_spp_regs <- most_relevant_panel %>%
  select(wdpaid, short_name, year, event, id, lat, lon, nice_gear, flag, effort, effort_measure, near, post, cpue_alb, cpue_bet, cpue_skj, cpue_yft, -cpue_tot) %>%
  pivot_longer(cols = contains("cpue"),
               names_to = "spp",
               values_to = "cpue_tot") %>%
  filter(cpue_tot > 0) %>% # remove explicit zeroes (i.e. a cell records catch and effort but species A but not species B, so species B's CPUE is 0 [technically NA but...])
  drop_na(cpue_tot)

# MPA-level analysis -----------------------------------------------------------
# Fit a model with fixed effects
gear_mpa_regs <- feols(log(cpue_tot) ~ post + near + i(post, near, 0) | id + flag + wdpaid^year,
                       panel.id = ~id + year,
                       vcov = conley(cutoff = 200),
                       split = ~paste(nice_gear, short_name),
                       data = most_relevant_panel)

# Species-level analysis -------------------------------------------------------
gear_spp_regs <- feols(log(cpue_tot) ~ post + near + i(post, near, 0) | id + flag + wdpaid ^ year,
                       panel.id = ~id + year,
                       vcov = conley(cutoff = 200),
                       data = panel_for_spp_regs,
                       split = ~paste(nice_gear, spp))

## Export models ###############################################################
# For gear-mpa models ----------------------------------------------------------
saveRDS(object = gear_mpa_regs,
        file = here("data", "output", "gear_mpa_regs.rds"))

# And now gear-spp models ------------------------------------------------------
saveRDS(object = gear_spp_regs,
        file = here("data", "output", "gear_spp_regs.rds"))

