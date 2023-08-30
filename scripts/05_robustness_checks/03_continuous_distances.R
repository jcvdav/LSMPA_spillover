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
annual_panel_raw <- readRDS(here("data", "processed", "annual_panel.rds")) %>%
  filter(between(event, -10, 10),
         cpue_tot > 0) %>%
  mutate(nice_gear = case_when(gear == "purse_seine" ~ "PS",
                               gear == "longline" ~ "LL"),
         nice_gear = fct_relevel(nice_gear, "PS", "LL"))

most_relevant_panel_multiple_distances <- readRDS(here("data", "processed", "annual_relevant_mpa_gears_and_distances_sensitivity_estimation_panel.rds"))



## PROCESSING ##################################################################

# All data ---------------------------------------------------------------------
main_cont_reg_100 <- feols(log(cpue_tot) ~ post + dist + post:dist | effort_measure,
                       panel.id = ~id + year,
                       vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                         time = ~year,
                                                         lat = ~lat,
                                                         lon = ~lon,
                                                         cutoff = 200,
                                                         lag = 5),
                       fsplit = ~nice_gear,
                       subset = ~!is.na(near_100),
                       data = annual_panel_raw %>%
                         mutate(dist = -1 * (dist / 100)))

main_cont_reg_200 <- feols(log(cpue_tot) ~ post + dist + post:dist | effort_measure,
                       panel.id = ~id + year,
                       vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                         time = ~year,
                                                         lat = ~lat,
                                                         lon = ~lon,
                                                         cutoff = 200,
                                                         lag = 5),
                       fsplit = ~nice_gear,
                       subset = ~!is.na(near_200),
                       data = annual_panel_raw %>%
                         mutate(dist = -1 * (dist / 100)))

main_cont_reg_300 <- feols(log(cpue_tot) ~ post + dist + post:dist | effort_measure,
                       panel.id = ~id + year,
                       vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                         time = ~year,
                                                         lat = ~lat,
                                                         lon = ~lon,
                                                         cutoff = 200,
                                                         lag = 5),
                       fsplit = ~nice_gear,
                       subset = ~!is.na(near_300),
                       data = annual_panel_raw %>%
                         mutate(dist = -1 * (dist / 100)))

# Relevant MPA - gear combinaitons ---------------------------------------------
relevant_main_cont_reg_100 <- feols(log(cpue_tot) ~ post + dist + post:dist | effort_measure,
                                    panel.id = ~id + year,
                                    vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                                      time = ~year,
                                                                      lat = ~lat,
                                                                      lon = ~lon,
                                                                      cutoff = 200,
                                                                      lag = 5),
                                    fsplit = ~nice_gear,
                                    subset = ~!is.na(near_100),
                                    data = most_relevant_panel_multiple_distances %>%
                                      mutate(dist = -1 * (dist / 100)))

relevant_main_cont_reg_200 <- feols(log(cpue_tot) ~ post + dist + post:dist | effort_measure,
                                    panel.id = ~id + year,
                                    vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                                      time = ~year,
                                                                      lat = ~lat,
                                                                      lon = ~lon,
                                                                      cutoff = 200,
                                                                      lag = 5),
                                    fsplit = ~nice_gear,
                                    subset = ~!is.na(near_200),
                                    data = most_relevant_panel_multiple_distances %>%
                                      mutate(dist = -1 * (dist / 100)))

relevant_main_cont_reg_300 <- feols(log(cpue_tot) ~ post + dist + post:dist | effort_measure,
                                    panel.id = ~id + year,
                                    vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                                      time = ~year,
                                                                      lat = ~lat,
                                                                      lon = ~lon,
                                                                      cutoff = 200,
                                                                      lag = 5),
                                    fsplit = ~nice_gear,
                                    subset = ~!is.na(near_300),
                                    data = most_relevant_panel_multiple_distances %>%
                                      mutate(dist = -1 * (dist / 100)))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
panelsummary(list(main_cont_reg_100, relevant_main_cont_reg_100),
             list(main_cont_reg_200, relevant_main_cont_reg_200),
             list(main_cont_reg_300, relevant_main_cont_reg_300),
             colnames = c("", "Combined", "Purse Seine", "Longline",
                          "Combined", "Purse Seine", "Longline"),
             panel_labels = c("Panel A: 0-200 nautical miles",
                              "Panel B: 0-400 nautical miles",
                              "Panel C: 0-600 nautical miles"),
             stars = "econ",
             format = "latex",
             coef_map = c("post:dist" = "Post x Distance"),
             pretty_num = T,
             gof_omit = "With|IC|RMSE|Std.|effort",
             hline_after = T,
             caption = "Effect of LMPAs on CPUE. Coefficiente estimates show the
             effect of moving 100 nautical miles closer to the border fo an LMPA.
             Columns 1-3 show coefficients estimated using the entire data set.
             Colu,ns 4-6 show coefficients estimated using a subsample of relevant
             MPA-gear combinations.") %>%
  add_header_above(c(" " = 1,
                     "All data" = 3,
                     "Relevant MPA-gear combinations" = 3)) %>%
  footnote(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                "Numbers in parenthesis are standard errors robust to heteroskedasticity",
                "and spatio-temporal autocorrelation (200 km cutoff; 5 yr lag).",
                "When the data used contains more than one effort unit",
                "(i.e. days, sets, hooks) we include fixed-effects. The number",
                "of unique MPA-gear combinations is as follows: For columns 1-3 in",
                "Panel A: 23 LMPA-gear combinations, Panel B: 25 LMPA-gear",
                "combinations, Panel C: 26 LMPA-gear combinations. For columns",
                "4-5 all panels have 14 LMPA-gear combinations.")) %>%
  cat(file = here("results", "tab", "tabS3_continuous_distance_reg_table.tex"))

