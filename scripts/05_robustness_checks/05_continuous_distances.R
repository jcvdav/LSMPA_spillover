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
  filter(gear == "purse_seine")

most_relevant_panel_multiple_distances <- readRDS(here("data", "processed", "annual_relevant_mpa_gears_and_distances_sensitivity_estimation_panel.rds")) %>%
  filter(gear == "purse_seine")



## PROCESSING ##################################################################

# All data ---------------------------------------------------------------------
main_cont_reg_100 <- feols(log(cpue_tot) ~ post + dist + post:dist | dist + flag ^ gear + wdpaid ^ gear ^ year,
                       panel.id = ~id + year,
                       vcov = conley(cutoff = 200),
                       subset = ~!is.na(near_100),
                       data = annual_panel_raw %>%
                         mutate(dist = -1 * dist / 100))

main_cont_reg_200 <- feols(log(cpue_tot) ~ post + dist + post:dist | id + flag ^ gear + wdpaid ^ gear ^ year,
                       panel.id = ~id + year,
                       vcov = conley(cutoff = 200),
                       subset = ~!is.na(near_200),
                       data = annual_panel_raw %>%
                         mutate(dist = -1 * (dist / 100)))

main_cont_reg_300 <- feols(log(cpue_tot) ~ post + dist + post:dist | id + flag ^ gear + wdpaid ^ gear ^ year,
                       panel.id = ~id + year,
                       vcov = conley(cutoff = 200),
                       subset = ~!is.na(near_300),
                       data = annual_panel_raw %>%
                         mutate(dist = -1 * (dist / 100)))

# Relevant MPA - gear combinaitons ---------------------------------------------
relevant_main_cont_reg_100 <- feols(log(cpue_tot) ~ post + dist + post:dist | id + flag ^ gear + wdpaid ^ gear ^ year,
                                    panel.id = ~id + year,
                                    vcov = conley(cutoff = 200),
                                    subset = ~!is.na(near_100),
                                    data = most_relevant_panel_multiple_distances %>%
                                      mutate(dist = -1 * (dist / 100)))

relevant_main_cont_reg_200 <- feols(log(cpue_tot) ~ post + dist + post:dist | id + flag ^ gear + wdpaid ^ gear ^ year,
                                    panel.id = ~id + year,
                                    vcov = conley(cutoff = 200),
                                    subset = ~!is.na(near_200),
                                    data = most_relevant_panel_multiple_distances %>%
                                      mutate(dist = -1 * (dist / 100)))

relevant_main_cont_reg_300 <- feols(log(cpue_tot) ~ post + dist + post:dist | id + flag ^ gear + wdpaid ^ gear ^ year,
                                    panel.id = ~id + year,
                                    vcov = conley(cutoff = 200),
                                    subset = ~!is.na(near_300),
                                    data = most_relevant_panel_multiple_distances %>%
                                      mutate(dist = -1 * (dist / 100)))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "N", 0,
              "adj.r.squared", "R2 Adj", 3,
              # "vcov.type", "SE", 0,
              "FE: id", "FE: Grid ID", 0,
              "FE: flag^gear", "FE: Flag-Gear", 0,
              "FE: wdpaid^gear^year", "FE: MPA-Gear-Year", 0
)

panelsummary(list(main_cont_reg_100, relevant_main_cont_reg_100),
             list(main_cont_reg_200, relevant_main_cont_reg_200),
             list(main_cont_reg_300, relevant_main_cont_reg_300),
             collapse_fe = T,
             colnames = c("", "All data", "Relevant MPAs"),
             panel_labels = c("Panel A: 0-200 nautical miles",
                              "Panel B: 0-400 nautical miles",
                              "Panel C: 0-600 nautical miles"),
             stars = "econ",
             format = "latex",
             coef_map = c("post:dist" = "Post x Distance"),
             pretty_num = T,
             gof_map = gm,
             hline_after = T,
             caption = "\\label{tab:cont_treat}\\textbf{Effect of LSMPAs on CPUE using distance as a continuous variable}.
             Coefficient estimates show the effect of moving 100 nautical miles closer to the border of an LSMPA.
             Columns 1-3 show coefficients estimated using the entire data set. Columns 4-6 show coefficients estimated using a subsample of relevant
             LSMPA-gear combinations. The number of unique LSMPA-gear combinations is as follows: For columns 1-3 in Panel A: 23 LSMPA-gear combinations,
             Panel B: 25 LSMPA-gear combinations, Panel C: 26 LSMPA-gear combinations. For columns 4-6 all panels have 14 LSMPA-gear combinations. All
             columns use Conley standard errors with a 200 km cutoff.") %>%
  footnote("$* p < 0.1, ** p < 0.05, *** p < 0.01$.", escape = F) %>%
  cat(file = here("results", "tab", "tabS3_continuous_distance_reg_table.tex"))

