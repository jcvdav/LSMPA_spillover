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
  modelsummary,
  kableExtra,
  tidyverse
)

# Source custom functions ------------------------------------------------------
source(here("scripts/00_set_up.R"))

# Load data --------------------------------------------------------------------
# Load model objects (they are multifit or lists)
# For MPA analysis
gear_mpa_regs <- readRDS(file = here("data", "output", "gear_mpa_regs.rds"))

# And now spp models
gear_spp_regs <- readRDS(file = here("data", "output", "gear_spp_regs.rds"))


abbr_names <- function(mod, caps = F) {
  cols <- mod %>%
    names() %>%
    str_remove(pattern = ".+; sample: ") %>%
    str_remove(pattern = "cpue_") %>%
    str_remove(pattern = "PS |LL ")

  if(caps) {
    cols <- str_to_upper(cols)
  }

  names(mod) <- cols

  return(mod)
}

gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "N", 0,
              "adj.r.squared", "R2 Adj", 3,
              # "vcov.type", "SE", 0,
              "FE: id", "FE: Grid ID", 0,
              "FE: flag^gear", "FE: Flag-Gear", 0,
              "FE: flag^nice_gear", "FE: Flag-Gear", 0,
              "FE: wdpaid^gear^year", "FE: MPA-Gear-Year", 0,
              "FE: wdpaid^nice_gear^year", "FE: MPA-Gear-Year", 0
)


modelsummary(abbr_names(gear_mpa_regs),
             shape = model ~ term + statistic,
             output = here("results", "tab", "tab2_mpa_reg_table.tex"),
             stars = panelsummary:::econ_stars(),
             coef_map = c("post:near" = "Post x Near"),
             caption = "\\label{tab:mpa_reg}\\textbf{Spillover effects by Large-Scale Marine Protected Areas.} Coefficients are
             difference-in-difference estimates for the change in CPUE. Numbers in parentheses are standard errors.")

modelsummary(abbr_names(gear_spp_regs, caps = T),
             output = here("results", "tab", "tab3_spp_reg_table.tex"),
             stars = panelsummary:::econ_stars(),
             coef_map = c("post:near" = "Post x Near"),
             gof_map = gm,
             caption = "\\label{tab:spp_reg}\\textbf{Spillover effects by species.} Coefficients are
             difference-in-difference estimates for the change in CPUE. BET = Bigeye, YFT = Yellowfin, SKJ = Skipjack. Numbers in parentheses are standard errors.")
