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

# Load data --------------------------------------------------------------------
most_relevant_panel_multiple_distances <- readRDS(here("data", "processed", "annual_relevant_mpa_gears_and_distances_sensitivity_estimation_panel.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
near_100 <- feols(log(cpue_tot) ~ i(post, near_100, 0) | id + year + effort_measure,
                  panel.id = ~id + year,
                  vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                    time = ~year,
                                                    lat = ~lat,
                                                    lon = ~lon,
                                                    cutoff = 200,
                                                    lag = 5),
                  fsplit = ~nice_gear,
                  subset = ~!is.na(near_100),
                  data = most_relevant_panel_multiple_distances)

near_200 <- feols(log(cpue_tot) ~ i(post, near_200, 0) | id + year + effort_measure,
                  panel.id = ~id + year,
                  vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                    time = ~year,
                                                    lat = ~lat,
                                                    lon = ~lon,
                                                    cutoff = 200,
                                                    lag = 5),
                  fsplit = ~nice_gear,
                  subset = ~!is.na(near_200),
                  data = most_relevant_panel_multiple_distances)

near_300 <- feols(log(cpue_tot) ~ i(post, near_300, 0) | id + year + effort_measure,
                  panel.id = ~id + year,
                  vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                    time = ~year,
                                                    lat = ~lat,
                                                    lon = ~lon,
                                                    cutoff = 200,
                                                    lag = 5),
                  fsplit = ~nice_gear,
                  subset = ~!is.na(near_300),
                  data = most_relevant_panel_multiple_distances)

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
panelsummary(near_100,
             near_200,
             near_300,
             colnames = c("", "Combined", "Purse Seine", "Longline"),
             panel_labels = c("Panel A: 0:100 - 100:200 nautical miles",
                              "Panel B: 0:200 - 100:400 nautical miles",
                              "Panel C: 0:300 - 100:500 nautical miles"),
             stars = "econ",
             collapse_fe = T,
             format = "latex",
             coef_map = c("post::1:near_100" = "Post x Distance",
                          "post::1:near_200" = "Post x Distance",
                          "post::1:near_300" = "Post x Distance"),
             pretty_num = T,
             gof_omit = "With|IC|RMSE|Std.|effort",
             hline_after = T) %>%
  cat(file = here("results", "tab", "tabSx_other_distance_specifications.tex"))
