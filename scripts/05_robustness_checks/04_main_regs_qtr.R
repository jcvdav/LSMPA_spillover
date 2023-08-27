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
relevant_mpa_gear_reg_qtr <- feols(log(cpue_tot) ~ i(post, near, 0) | id + year + effort_measure,
                                   panel.id = ~id + year,
                                   vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                                     time = ~year,
                                                                     lat = ~lat,
                                                                     lon = ~lon,
                                                                     cutoff = 200,
                                                                     lag = 5),
                                   fsplit = ~nice_gear,
                                   data = most_relevant_qtr_panel)

# MPA-level analysis -----------------------------------------------------------
# Fit a model with fixed effects
gear_mpa_regs_qtr <- feols(log(cpue_tot) ~ i(post, near, 0) | id + year,
                           panel.id = ~id + year,
                           vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                             time = ~year,
                                                             lat = ~lat,
                                                             lon = ~lon,
                                                             cutoff = 200,
                                                             lag = 5),
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
gear_spp_regs_qtr <- feols(log(cpue_tot) ~ i(post, near, 0) | id + year + effort_measure,
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
panelsummary(relevant_mpa_gear_reg,
             relevant_mpa_gear_reg_qtr,
             stars = "econ",
             collapse_fe = T,
             gof_omit = "IC|With|Std.|RMSE",
             colnames = c(" ", "Combined", "PS", "LL"),
             panel_labels = c("Panel A: Aggregating data to the year-flag level (form main text)",
                              "Panel B: Aggregatign data to tye year-quarter-flag level"),
             caption = "Comparison of results when aggregating our main data set to the annual level (as in the main text, Panel A) vs. aggreating it to the quarterly level (Panel B).",
             format = "latex") %>%
  cat(file = here("results", "tab", "tabSx_main_reg_quarterly.tex"))

# Table mpa-level results ------------------------------------------------------
mpa_model_names <- str_remove(names(gear_mpa_regs), ".+; sample: ")
panelsummary(gear_mpa_regs,
             gear_mpa_regs_qtr,
             colnames = c("", mpa_model_names),
             stars = "econ",
             gof_omit = c("RMSE|IC|With|Std"),
             panel_labels = c("Panel A: Aggregating data to the year-flag level (form main text)",
                              "Panel B: Aggregatign data to tye year-quarter-flag level"),
             coef_map = (c("post::1:near" = "Post x Near",
                           "post:near" = "Post X Near")),
             caption = "Comparison of results when aggregating our main data set to the annual level (as in the main text, Panel A) vs. aggreating it to the quarterly level (Panel B).",
             format = "latex") %>%
  kableExtra::add_header_above(c(" " = 1,
                                 "LL" = 8,
                                 "PS" = 6)) %>%
  cat(file = here("results", "tab", "tabSx_MPA_reg_quarterly.tex"))

# Table comparing spp-level results --------------------------------------------
spp_model_names <- str_remove_all(names(gear_spp_regs), ".+; sample: |cpue_")
panelsummary(gear_spp_regs,
             gear_spp_regs_qtr,
             colnames = c("", spp_model_names),
             stars = "econ",
             gof_omit = c("RMSE|IC|With|Std|eff"),
             panel_labels = c("Panel A: Aggregating data to the year-flag level (form main text)",
                              "Panel B: Aggregatign data to tye year-quarter-flag level"),
             coef_map = (c("post::1:near" = "Post x Near",
                           "post:near" = "Post X Near")),
             caption = "Comparison of results when aggregating our main data set to the annual level (as in the main text, Panel A) vs. aggreating it to the quarterly level (Panel B).",
             format = "latex") %>%
  kableExtra::add_header_above(c(" " = 1,
                                 "LL" = 3,
                                 "PS" = 3)) %>%
  cat(file = here("results", "tab", "tabSx_spp_reg_quarterly.tex"))

