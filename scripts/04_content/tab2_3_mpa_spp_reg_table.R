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


abbr_names <- function(mod) {
  cols <- mod %>%
    names() %>%
    str_remove(pattern = ".+; sample: ") %>%
    str_remove(pattern = "cpue_")

  names(mod) <- cols

  return(mod)
}


modelsummary(abbr_names(gear_mpa_regs), shape = model ~ term + statistic,
             output = here("results", "tab", "tab2_mpa_reg_table.tex"),
             stars = panelsummary:::econ_stars(),
             coef_map = c("post::1:near" = "Post x Near"),
             caption = "\\label{tab:mpa_reg}Spillover effects by gear and Large Marine Protected Areas. Coefficients are
             difference-in-difference estimates for change in CPUE.")

modelsummary(abbr_names(gear_spp_regs),
             output = here("results", "tab", "tab3_spp_reg_table.tex"),
             stars = panelsummary:::econ_stars(),
             coef_map = c("post::1:near" = "Post x Near"),
             gof_omit = "With|IC|Std.|effort",
             caption = "\\label{tab:spp_reg}Spillover effects by gear and species. Coefficients are
             difference-in-difference estimates for change in CPUE.")
