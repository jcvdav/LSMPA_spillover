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
gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "N", 0,
              "adj.r.squared", "R2 Adj", 3,
              "vcov.type", "SE", 0,
              "FE: id", "FE: Grid ID", 0,
              "FE: flag", "FE: Flag", 0,
              "FE: wdpaid^year", "FE: MPA-Year", 0,
              "FE: wdpaid", "FE: MPA", 0
)

panelsummary(main_reg,
             relevant_mpa_gear_reg,
             stars = "econ",
             panel_labels = c("Panel A: All data (9 LSMPAs)",
                              "Panel B: Subsample of relevant LSMPAs (6 LSMPAs)"),
             gof_map = gm,
             coef_map = c("post:near" = "Post x Near"),
             collapse_fe = T,
             format = "latex",
             caption = "\\label{tab:main_reg}\\textbf{Effect of Large-Scale Marine Protected Areas on catch-per-unit-effort (CPUE) of tuna purse seine fisheries}.
             Coefficients are difference-in-difference estimates for change in CPUE. Column 1 is the simplest
             specification with no fixed effects. Column 2 includes fixed effects by grid cell. Column 3 includes fixed
             effects by grid and by flag. Column 4 shows a full specification with fixed-effects by grid, by
             flag, and by MPA-year. Column 5 then incorporates linear and quadratic terms for the Oceanic Nino Index interacted with MPA, and removes MPA-year fixed effects but retains MPA fixed effects.
             N is number of observations, R2 Adj is Adjusted R$^2$, and SE is the method/assumption used to calculate standard errors. IID stands for independently and identically distributed. Conley (200km) are Conley standard errors accounting for spatial correlation using a 200 km cutoff.") %>%
  # add_header_above(header = c(" " = 1, "Gears combined" = 4, "Purse Seine" = 1, "Longline" = 1)) %>%
  footnote("$* p < 0.1, ** p < 0.05, *** p < 0.01$", escape = F) %>%
  cat(file = here("results", "tab", "tab1_main_reg_table.tex"))

