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
  fixest,
  tidyverse
)

# Load data --------------------------------------------------------------------
relevant_mpa_gear_reg <- readRDS(file = here("data", "output", "relevant_mpa_gear_reg.rds"))
most_relevant_qtr_panel <-
  readRDS(file = here("data", "processed", "qtr_relevant_mpa_gears_and_distances_sensitivity_estimation_panel.rds"))

#
# test <- most_relevant_monthly_ll_panel %>%
#   group_by(year, gear, flag, grid, lat, lon, id, wdpaid, name, near_100, near_200, near_300,
#            year_enforced, ocean, event, post, short_name) %>%
#   summarize(effort = sum(effort),
#             tot = sum(tot_mt)) %>%
#   ungroup() %>%
#   mutate(cpue_tot = tot / effort) %>%
#   drop_na(cpue_tot)
#
# most_relevant_monthly_ll_panel <- test

## PROCESSING ##################################################################

# Replicate main regressions using quarterly daggregations ata -----------------
# Fit DiD
relevant_mpa_gear_reg_qtr <- feols(log(cpue_tot) ~ i(post, near, 0) | id + year + effort_measure,
                                   panel.id = ~id + year,
                                   vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                                     time = ~year,
                                                                     lat = ~lat,
                                                                     lon = ~lon,
                                                                     cutoff = 200,
                                                                     lag = 5),
                                   fsplit = ~nice_gear,
                                   data = most_relevant_qtr_panel)

# Compare to main text tables
panelsummary::panelsummary(relevant_mpa_gear_reg,
                           relevant_mpa_gear_reg_qtr,
                           stars = "econ",
                           collapse_fe = T,
                           gof_omit = "IC|With|Std.|RMSE")

# MPA-level analysis -----------------------------------------------------------
# Fit a model with fixed effects
gear_mpa_regs <- feols(log(cpue_tot) ~ i(post, near, 0) | id + year,
                       panel.id = ~id + year,
                       vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                         time = ~year,
                                                         lat = ~lat,
                                                         lon = ~lon,
                                                         cutoff = 200,
                                                         lag = 5),
                       split = ~paste(nice_gear, short_name),
                       data = most_relevant_qtr_panel)


# Now compare the spp-level regressions ----------------------------------------
# Build a panel
qtr_panel_for_spp_regs <- most_relevant_qtr_panel%>%
  mutate(cpue_skj = ifelse(gear == "longline", 0, cpue_skj)) %>%
  select(wdpaid, short_name, year, qtr, event, id, lat, lon, nice_gear, flag, effort, effort_measure, near, post, cpue_alb, cpue_bet, cpue_skj, cpue_yft, -cpue_tot) %>%
  pivot_longer(cols = contains("cpue"), names_to = "spp", values_to = "cpue_tot") %>%
  filter(cpue_tot > 0) %>%
  drop_na(cpue_tot)

# Fit DiD
gear_spp_regs <- feols(log(cpue_tot) ~ i(post, near, 0) | id + year + effort_measure,
                       panel.id = ~id + event,
                       vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                         time = ~year,
                                                         lat = ~lat,
                                                         lon = ~lon,
                                                         cutoff = 200,
                                                         lag = 5),
                       data = panel_for_spp_regs,
                       split = ~paste(nice_gear, spp))
