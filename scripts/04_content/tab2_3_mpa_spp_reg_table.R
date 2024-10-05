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
  modelsummary,
  kableExtra,
  tidyverse
)
# options("modelsummary_format_numeric_latex" = "plain")
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
              "FE: flag", "FE: Flag", 0,
              "FE: wdpaid^year", "FE: MPA-Year", 0,
              "FE: wdpaid", "FE: MPA", 0
)


modelsummary(abbr_names(gear_mpa_regs),
             output = here("results", "tab", "tab2_mpa_reg_table.tex"),
             stars = panelsummary:::econ_stars(),
             coef_map = c("post:near" = "Post x Near"),
             gof_map = gm,
             escape = F,
             title = "\\label{tab:mpa_reg}\\textbf{Spillover effects by Large-Scale Marine Protected Areas.} Coefficients are
             difference-in-difference estimates for the change in CPUE. Numbers in parentheses are Conley standard errors accounting for spatial correlation using a 200 km cutoff.")

modelsummary(abbr_names(gear_spp_regs, caps = T),
             output = here("results", "tab", "tab3_spp_reg_table.tex"),
             stars = panelsummary:::econ_stars(),
             coef_map = c("post:near" = "Post x Near"),
             gof_map = gm,
             escape = F,
             title = "\\label{tab:spp_reg}\\textbf{Spillover effects by species.} Coefficients are
             difference-in-difference estimates for the change in CPUE. BET = Bigeye, YFT = Yellowfin, SKJ = Skipjack. Numbers in parentheses are Conley standard errors accounting for spatial correlation using a 200 km cutoff.")
