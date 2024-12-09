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
most_relevant_panel_multiple_distances <- readRDS(here("data", "processed", "annual_relevant_mpa_gears_and_distances_sensitivity_estimation_panel.rds")) %>%
  filter(gear == "purse_seine")

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
near_100 <- feols(log(cpue_tot) ~ post + near_100 +  post * near_100 | id + flag ^ gear + wdpaid ^ gear ^ year,
                  panel.id = ~id + year,
                  vcov = conley(cutoff = 200),
                  subset = ~!is.na(near_100),
                  data = most_relevant_panel_multiple_distances)

near_200 <- feols(log(cpue_tot) ~ post + near_200 + post * near_200 | id + flag ^ gear + wdpaid ^ gear ^ year,
                  panel.id = ~id + year,
                  vcov = conley(cutoff = 200),
                  subset = ~!is.na(near_200),
                  data = most_relevant_panel_multiple_distances)

near_300 <- feols(log(cpue_tot) ~ post + near_300 + post*near_300 | id + flag ^ gear + wdpaid ^ gear ^ year,
                  panel.id = ~id + year,
                  vcov = conley(cutoff = 200),
                  subset = ~!is.na(near_300),
                  data = most_relevant_panel_multiple_distances)

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

panelsummary(near_100,
             near_200,
             near_300,
             caption = "\\label{tab:other_near_far}\\textbf{Testing for spillover effects using other definitions of near and far from an LSMPA boundary.}
             The table shows difference-in-difference estimates using alternative definitions of the near-far areas. In Panel A near is defined as 0-100 and
             far as 100-200 NM. In Panel B near is defined as 0-200 and far as 200-400 NM. Finally, in panel B near is defined as 0-300 and far as 300-600 NM.",
             panel_labels = c("Panel A: 0:100 - 100:200 nautical miles",
                              "Panel B: 0:200 - 200:400 nautical miles",
                              "Panel C: 0:300 - 300:600 nautical miles"),
             stars = "econ",
             collapse_fe = T,
             format = "latex",
             coef_map = c("post:near_100" = "Post x Distance",
                          "post:near_200" = "Post x Distance",
                          "post:near_300" = "Post x Distance"),
             pretty_num = T,
             gof_map = gm,
             hline_after = T) %>%
  footnote("$* p < 0.1, ** p < 0.05, *** p < 0.01$", escape = F) %>%
  cat(file = here("results", "tab", "tabS2_other_distance_specifications.tex"))
