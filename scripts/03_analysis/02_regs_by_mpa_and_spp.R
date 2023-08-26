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
  mutate(cpue_skj = ifelse(gear == "longline", 0, cpue_skj)) %>%
  select(wdpaid, short_name, year, event, id, lat, lon, nice_gear, flag, effort, effort_measure, near, post, cpue_alb, cpue_bet, cpue_skj, cpue_yft, -cpue_tot) %>%
  pivot_longer(cols = contains("cpue"), names_to = "spp", values_to = "cpue_tot") %>%
  filter(cpue_tot > 0) %>%
  drop_na(cpue_tot)

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
                  data = most_relevant_panel)

# And a model without fixed effects
gear_mpa_regs_wo_fe <- feols(log(cpue_tot) ~ post + near + post:near,
                        panel.id = ~id + year,
                        vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                          time = ~year,
                                                          lat = ~lat,
                                                          lon = ~lon,
                                                          cutoff = 200,
                                                          lag = 5),
                        split = ~paste(nice_gear, short_name),
                        data = most_relevant_panel)

# Species-level analysis -------------------------------------------------------
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

gear_spp_regs_wo_fe <- feols(log(cpue_tot) ~ post + near + post:near | effort_measure,
                        panel.id = ~id + event,
                        vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                          time = ~year,
                                                          lat = ~lat,
                                                          lon = ~lon,
                                                          cutoff = 200,
                                                          lag = 5),
                        data = panel_for_spp_regs,
                        split = ~paste(nice_gear, spp))

## Inspect models ##############################################################
# Models for MPA analysis
mpa_model_names <- str_remove(names(gear_mpa_regs), ".+; sample: ")
panelsummary(gear_mpa_regs,
             gear_mpa_regs_wo_fe,
             colnames = c("", mpa_model_names),
             stars = "econ",
             gof_omit = c("RMSE|IC|With|Std"),
             panel_labels = c("Panel A: Models that include fixed effects",
                              "Pabel B: Models without fixed effects"),
             coef_map = (c("post::1:near" = "Post x Near",
                           "post:near" = "Post X Near"))) %>%
  kableExtra::add_header_above(c(" " = 1,
                                 "LL" = 8,
                                 "PS" = 6))
# Models for spp-level analysis
spp_model_names <- str_remove_all(names(gear_spp_regs), ".+; sample: |cpue_")
panelsummary(gear_spp_regs,
             gear_spp_regs_wo_fe,
             colnames = c("", spp_model_names),
             stars = "econ",
             gof_omit = c("RMSE|IC|With|Std|eff"),
             panel_labels = c("Panel A: Models that include fixed effects",
                              "Pabel B: Models without fixed effects"),
             coef_map = (c("post::1:near" = "Post x Near",
                           "post:near" = "Post X Near"))) %>%
  kableExtra::add_header_above(c(" " = 1,
                                 "LL" = 3,
                                 "PS" = 3))

## Export models ###############################################################
# For gear-mpa models ----------------------------------------------------------
saveRDS(object = gear_mpa_regs,
        file = here("data", "output", "gear_mpa_regs.rds"))
saveRDS(object = gear_mpa_regs_wo_fe,
        file = here("data", "output", "gear_mpa_regs_wo_fe.rds"))

# And now gear-spp models ------------------------------------------------------
saveRDS(object = gear_spp_regs,
        file = here("data", "output", "gear_spp_regs.rds"))
saveRDS(object = gear_spp_regs_wo_fe,
        file = here("data", "output", "gear_spp_regs_wo_fe.rds"))
