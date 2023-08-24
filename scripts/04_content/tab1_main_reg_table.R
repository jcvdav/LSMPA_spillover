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
  kableExtra,
  panelsummary,
  here,
  tidyverse
)

# Load data --------------------------------------------------------------------
# Main models
main_reg <- readRDS(file = here("data", "output", "main_reg.rds"))
main_reg_wo_fe <- readRDS(file = here("data", "output", "main_reg_wo_fe.rds"))

# Relevant MPA-gear combinations
relevant_mpa_gear_reg <- readRDS(file = here("data", "output", "relevant_mpa_gear_reg.rds"))
relevant_mpa_gear_reg_wo_fe <- readRDS(file = here("data", "output", "relevant_mpa_gear_reg_wo_fe.rds"))

## PROCESSING ##################################################################

# Build regression tables ------------------------------------------------------

# For main text
panelsummary(main_reg,
             relevant_mpa_gear_reg,
             colnames = c(" ", "Combined gears", "Purse seine", "Longline"),
             panel_labels = c("Panel A: All data (23 LMPA-gear combinations; 14 LMPAs)",
                              "Panel B: Subsample (14 LMPA-gear combinations, 11 LMPAs)"),
             stars = "econ",
             pretty_num = T,
             collapse_fe = T,
             coef_map = c("post::1:near" = "Post x Near"),
             gof_omit = "With|IC|Std.|effort",
             hline_after = T,
             format = "latex",
             caption = "Spillover effects of Large Marine Protected Areas on
             catch-per-unit effort in the purse seine and longline tuna fleet targetting Skipjack tuna, Yellowfin
             tuna, Bigeye tuna, and Albacore tuna. Coefficients are
             difference-in-difference estimates for change in CPUE.
             Column 1 shows results for purse seine and longline data
             combined. Column 2 presents models fit to purse seine data
             only, and column 3 presents models fit to longline data only.") %>%
  footnote(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                "Numbers in parenthesis are standard errors robust to heteroskedasticity",
                "and spatio-temporal autocorrelation (200 km cutoff; 5 yr lag).",
                "When the data used contains more than one effort unit",
                "(i.e. days, sets, hooks) we include fixed-effects")) %>%
  cat(file = here("results", "tab", "tab1_main_reg_table.tex"))

# For Supplementary materials
panelsummary(list(main_reg_wo_fe[[1]], main_reg[[1]],
                  main_reg_wo_fe[[2]], main_reg[[2]],
                  main_reg_wo_fe[[3]], main_reg[[3]]),
             list(relevant_mpa_gear_reg_wo_fe[[1]], relevant_mpa_gear_reg[[1]],
                  relevant_mpa_gear_reg_wo_fe[[2]], relevant_mpa_gear_reg[[2]],
                  relevant_mpa_gear_reg_wo_fe[[3]], relevant_mpa_gear_reg[[3]]),
             panel_labels = c("Panel A: All data (23 LMPA-gear combinations; 14 LMPAs)",
                              "Panel B: Subsample (14 LMPA-gear combinations; 11 LMPAs)"),
             stars = "econ",
             pretty_num = T,
             collapse_fe = T,
             coef_map = c("post::1:near" = "Post x Near",
                          "post:near" = "Post x Near"),
             gof_omit = "With|BIC|Std.|effort",
             hline_after = T,
             format = "latex",
             caption = "Comparision of coefficinet estimates for models with and
             without fixed-effects when estimating the spillover effects of
             Large Marine Protected Areas on catch-per-unit effort of tuna
             fisheries. Coefficients are difference-in-difference estimates for
             change in CPUE. Columns 1 and 2 show results for purse seine and
             longline data combined. Columns 3 and 4 present models fit to purse
             seine data only, and columns 5 and 6 present models fit to longline
             data only. Models with fixed-effects are the same as the ones
             presented in the main text. Note that AIC values are always lower
             and R2 higher for models with fixed-effects.") %>%
  add_header_above(c(" " = 1,
                     "Combined gears" = 2,
                     "Purse seine" = 2,
                     "Longline" = 2)) %>%
  footnote(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                "Numbers in parenthesis are standard errors robust to heteroskedasticity",
                "and spatio-temporal autocorrelation (200 km cutoff; 5 yr lag).",
                "When the data used contains more than one effort unit",
                "(i.e. days, sets, hooks) we include fixed-effects")) %>%
  cat(file = here("results", "tab", "tabSx_main_regression_table.tex"))

