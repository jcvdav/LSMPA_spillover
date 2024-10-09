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
  modelsummary,
  panelsummary,
  kableExtra,
  tidyverse
)

options("modelsummary_format_numeric_latex" = "plain")

# Load data --------------------------------------------------------------------
annual_panel <- readRDS(file = here("data", "processed", "annual_full_estimation_panel.rds"))
most_relevant_panel <- readRDS(file = here("data", "processed", "annual_relevant_mpa_gears_estimation_panel.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
full <- feols(log(cpue_tot) ~ post + near + post:near | id + flag + wdpaid ^ year,
              panel.id = ~id + year,
              data = annual_panel,
              subset = ~gear == "longline",
              vcov = conley(cutoff = 200))

relevant <- feols(log(cpue_tot) ~ post + near + post:near | id + flag + wdpaid ^ year,
                  panel.id = ~id + year,
                  data = most_relevant_panel,
                  subset = ~gear == "longline",
                  vcov = conley(cutoff = 200))

by_mpa <- feols(log(cpue_tot) ~ post + near + post:near | id + flag + wdpaid^year,
                panel.id = ~id + year,
                split = ~paste(nice_gear, short_name),
                subset = ~gear == "longline",
                data = most_relevant_panel,
                vcov = conley(cutoff = 200))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "N", 0,
              "adj.r.squared", "R2 Adj", 3,
              "vcov.type", "SE", 0,
              "FE: id", "FE: Grid ID", 0,
              "FE: flag", "FE: Flag", 0,
              "FE: wdpaid^year", "FE: MPA-Year", 0,
              "FE: wdpaid", "FE: MPA", 0
)

panelsummary(full,
             relevant,
             stars = "econ",
             panel_labels = c("Panel A: All data (14 LSMPAs)",
                              "Panel B: Subsample of LSMPAs with a balanced sample (2 LSMPAs)"),
             gof_map = gm,
             coef_map = c("post:near" = "Post x Near"),
             collapse_fe = T,
             format = "latex",
             caption = "\\label{tab:main_reg_ll}\\textbf{Effect of Large-Scale Marine Protected Areas on catch-per-unit-effort (CPUE) of tuna longline fisheries}.
             Coefficients are difference-in-difference estimates for change in CPUE. The top panel includes all longline data. The bottom panel
             only includes data for which longlines are the relevant fishery and ther eis enough data to conduct a BACI design.
             N is number of observations, R2 Adj is Adjusted R$^2$, and SE is the method/assumption used to calculate standard errors. Conley (200km) are Conley standard errors accounting for spatial correlation using a 200 km cutoff.") %>%
  footnote("$* p < 0.1, ** p < 0.05, *** p < 0.01$", escape = F) %>%
  cat(file = here("results", "tab", "tabS_longline_main_reg_table.tex"))

panelsummary(by_mpa,
             stars = "econ",
             gof_map = gm,
             coef_map = c("post:near" = "Post x Near"),
             format = "latex",
             caption = "\\label{tab:ll_by_mpa_reg}\\textbf{Spillover effects by Large-Scale Marine Protected Areas on longline fisheries.}
              Column 1 shows results for Chagos, column 2 shows results for Papahanaumokuakea. Coefficients are
             difference-in-difference estimates for the change in CPUE. Conley (200km) are Conley standard errors accounting for spatial correlation using a 200 km cutoff.") %>%
  footnote("$* p < 0.1, ** p < 0.05, *** p < 0.01$", escape = F) %>%
  cat(file = here("results", "tab", "tabS_longline_mpa_reg_table.tex"))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
