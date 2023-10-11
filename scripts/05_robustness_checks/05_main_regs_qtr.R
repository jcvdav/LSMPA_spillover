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
  modelsummary,
  fixest,
  tidyverse
)

# Source custom funcions -------------------------------------------------------
source(here("scripts/00_set_up.R"))

# Load data --------------------------------------------------------------------
most_relevant_qtr_panel <-
  readRDS(file = here("data", "processed", "qtr_relevant_mpa_gears_and_distances_sensitivity_estimation_panel.rds"))


# Load models with main results
relevant_mpa_gear_reg <- readRDS(file = here("data", "output", "relevant_mpa_gear_reg.rds"))
gear_mpa_regs <- readRDS(file = here("data", "output", "gear_mpa_regs.rds"))
gear_spp_regs <- readRDS(file = here("data", "output", "gear_spp_regs.rds"))

## PROCESSING ##################################################################

# Replicate main regressions using quarterly daggregations ata -----------------
# Fit DiD
relevant_mpa_gear_reg_qtr <- feols(log(cpue_tot) ~ post + near + i(post, near, 0) | id + flag ^ gear + wdpaid ^ gear ^ year,
                                   panel.id = ~id + year,
                                   vcov = conley(cutoff = 200),
                                   fsplit = ~nice_gear,
                                   data = most_relevant_qtr_panel)

# MPA-level analysis -----------------------------------------------------------
# Fit a model with fixed effects
gear_mpa_regs_qtr <- feols(log(cpue_tot) ~ post + near + i(post, near, 0) | id + flag ^ gear + wdpaid ^ gear ^ year,
                           panel.id = ~id + year,
                           vcov = conley(cutoff = 200),
                           split = ~paste(nice_gear, short_name),
                           data = most_relevant_qtr_panel)


# Now compare the spp-level regressions ----------------------------------------
# Build a panel
qtr_panel_for_spp_regs <- most_relevant_qtr_panel%>%
  mutate(cpue_skj = ifelse(gear == "longline", 0, cpue_skj)) %>%
  select(wdpaid, short_name, year, qtr, event, id, lat, lon, nice_gear, flag, effort, effort_measure, near, post, cpue_alb, cpue_bet, cpue_skj, cpue_yft, -cpue_tot) %>%
  pivot_longer(cols = contains("cpue"), names_to = "spp", values_to = "cpue_tot") %>%
  filter(cpue_tot > 0) %>%
  drop_na(cpue_tot)

# Fit DiD
gear_spp_regs_qtr <- feols(log(cpue_tot) ~ post + near + i(post, near, 0) | id + flag ^ nice_gear + wdpaid ^ nice_gear ^ year,
                           panel.id = ~id + event,
                           vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                             time = ~year,
                                                             lat = ~lat,
                                                             lon = ~lon,
                                                             cutoff = 200,
                                                             lag = 5),
                           data = qtr_panel_for_spp_regs,
                           split = ~paste(nice_gear, spp))


## BUILD TABLES ################################################################

# Table comparing main DiD results ---------------------------------------------
abbr_names <- function(mod, caps = F) {
  cols <- mod %>%
    names() %>%
    str_remove(pattern = ".+; sample: ") %>%
    str_remove(pattern = "cpue_")

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

panelsummary(relevant_mpa_gear_reg[[4]],
             relevant_mpa_gear_reg_qtr,
             stars = "econ",
             collapse_fe = T,
             gof_map = gm,
             coef_map = (c("post::1:near" = "Post x Near",
                           "post:near" = "Post X Near")),
             colnames = c(" ", "Combined", "PS", "LL"),
             panel_labels = c("Panel A: Aggregating data to the year-flag level (form main text)",
                              "Panel B: Aggregating data to the year-quarter-flag level"),
             caption = "\\textbf{Comparison of results when aggregating data to the annual vs quarterly level using the subsample of relevant LSMPA-gear combinations}.
             Panel A uses the annual data as in the main text. Panel B uses data aggregated to the quarterly level.
             All specifications use Conley standard errors with a 200 km cutoff",
             format = "latex") %>%
  footnote("$* p < 0.1, ** p < 0.05, *** p < 0.01$.", escape = F) %>%
  cat(file = here("results", "tab", "tabS4_main_reg_quarterly.tex"))

# Table mpa-level results ------------------------------------------------------
modelsummary(abbr_names(gear_mpa_regs_qtr),
             shape = model ~ term + statistic,
             output = here("results", "tab", "tabS5_MPA_reg_quarterly.tex"),
             stars = panelsummary:::econ_stars(),
             coef_map = c("post::1:near" = "Post x Near"),
             caption = "\\label{tab:mpa_reg}\\textbf{Spillover effects by gear and Large-Scale Marine Protected Areas using quarterly data.} Coefficients are
             difference-in-difference estimates for change in CPUE. All specifications use Conley standard errors with a 200 km cutoff")



# Table comparing spp-level results --------------------------------------------
spp_model_names <- str_remove_all(names(gear_spp_regs), ".+; sample: |cpue_") %>%
  str_to_upper()
panelsummary(gear_spp_regs,
             gear_spp_regs_qtr,
             colnames = c("", spp_model_names),
             stars = "econ",
             collapse_fe =  T,
             gof_map = gm,
             panel_labels = c("Panel A: Aggregating data to the year-flag level (from main text)",
                              "Panel B: Aggregating data to the year-quarter-flag level"),
             coef_map = (c("post::1:near" = "Post x Near",
                           "post:near" = "Post X Near")),
             caption = "\\textbf{Comparison of species-level results when aggregating data to the annual vs quarterly level using the subsample of relevant LSMPA-gear combinations}. Panel A shows the same results as the main text. Panel B shows the results using quarterly data",
             format = "latex") %>%
  kableExtra::add_header_above(c(" " = 1,
                                 "LL" = 3,
                                 "PS" = 3)) %>%
  footnote("$* p < 0.1, ** p < 0.05, *** p < 0.01$.", escape = F) %>%
  cat(file = here("results", "tab", "tabS6_spp_reg_quarterly.tex"))

