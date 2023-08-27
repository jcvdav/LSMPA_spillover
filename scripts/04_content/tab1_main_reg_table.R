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

# Relevant MPA-gear combinations
relevant_mpa_gear_reg <- readRDS(file = here("data", "output", "relevant_mpa_gear_reg.rds"))

## PROCESSING ##################################################################

# Build regression tables ------------------------------------------------------
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
             caption = "\\label{tab:main_reg}Spillover effects of Large Marine Protected Areas on
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


