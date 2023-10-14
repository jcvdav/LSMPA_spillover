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
annual_panel <- readRDS(file = here("data", "processed", "annual_full_estimation_panel.rds"))
most_relevant_panel <- readRDS(file = here("data", "processed", "annual_relevant_mpa_gears_estimation_panel.rds"))

# Main models
main_reg <- readRDS(file = here("data", "output", "main_reg.rds"))
# Relevant MPA-gear combinations
relevant_mpa_gear_reg <- readRDS(file = here("data", "output", "relevant_mpa_gear_reg.rds"))


## PROCESSING ##################################################################

# Fit simple difference-in-means estimations -----------------------------------
# Now without fixed effects
main_reg_wo_fe <- feols(log(cpue_tot) ~ post + near + post:near,
                        panel.id = ~id + year,
                        vcov = "iid",
                        split = ~nice_gear,
                        data = annual_panel)

# Same, but without FEs
relevant_mpa_gear_reg_wo_fe <- feols(log(cpue_tot) ~ post + near + post:near,
                                     panel.id = ~id + year,
                                     vcov = "iid",
                                     split = ~nice_gear,
                                     data = most_relevant_panel)

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "N", 0,
              "adj.r.squared", "R2 Adj", 3,
              "vcov.type", "SE", 0,
              "FE: id", "FE: Grid ID", 0,
              "FE: flag^gear", "FE: Flag-Gear", 0,
              "FE: wdpaid^gear^year", "FE: MPA-Gear-Year", 0
)

panelsummary(list(main_reg_wo_fe[[1]], main_reg[[4]][[2]],
                  main_reg_wo_fe[[2]], main_reg[[4]][[3]]),
             list(relevant_mpa_gear_reg_wo_fe[[1]], relevant_mpa_gear_reg[[4]][[2]],
                  relevant_mpa_gear_reg_wo_fe[[2]], relevant_mpa_gear_reg[[4]][[3]]),
             panel_labels = c("Panel A: All data (23 LSMPA-gear combinations; 14 LSMPAs)",
                              "Panel B: Subsample (14 LSMPA-gear combinations; 11 LSMPAs)"),
             stars = "econ",
             pretty_num = T,
             collapse_fe = T,
             coef_map = c("post::1:near" = "Post x Near",
                          "post:near" = "Post x Near"),
             gof_map = gm,
             format = "latex",
             caption = "\\label{tab:dif_in_means_reg}\\textbf{Comparison of coefficient estimates for models with and
             without fixed-effects when estimating the spillover effects of
             Large-Scale Marine Protected Areas on catch-per-unit effort of tuna
             fisheries.} Coefficients are difference-in-difference estimates for
             the change in CPUE. Columns 1 and 2 present models fit to purse
             seine data only, and columns 3 and 4 present models fit to longline
             data only. Models with fixed-effects (columns 2 and 4) are the same as the ones
             presented in the main text. Note that R$^2$ values are always higher for models with fixed-effects.") %>%
  add_header_above(c(" " = 1,
                     "Purse seine" = 2,
                     "Longline" = 2)) %>%
  footnote("$* p < 0.1, ** p < 0.05, *** p < 0.01$", escape = F) %>%
  cat(file = here("results", "tab", "tabS1_main_regression_table.tex"))
