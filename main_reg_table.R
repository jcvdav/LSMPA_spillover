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
  fixest,
  here,
  tidyverse
)

# Load data --------------------------------------------------------------------
annual_panel <- readRDS(file = here("data", "processed", "annual_full_estimation_panel.rds"))
most_relevant_panel <- readRDS(file = here("data", "processed", "annual_relevant_mpa_gears_estimation_panel.rds"))


# ## PROCESSING ##################################################################
# Estimate DiD with all the data -----------------------------------------------
# Main specification
main_reg <- feols(log(cpue_tot) ~ i(post, near, 0) | id + year + effort_measure,
                  panel.id = ~id + year,
                  vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                    time = ~year,
                                                    lat = ~lat,
                                                    lon = ~lon,
                                                    cutoff = 200,
                                                    lag = 5),
                  fsplit = ~nice_gear,
                  data = annual_panel)

# Now without fixed effects
main_reg_wo_fe <- feols(log(cpue_tot) ~ post + near + post:near | effort_measure,
                        panel.id = ~id + year,
                        vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                          time = ~year,
                                                          lat = ~lat,
                                                          lon = ~lon,
                                                          cutoff = 200,
                                                          lag = 5),
                        fsplit = ~nice_gear,
                        data = annual_panel)

# Estimate DiD with subsamples of relevant MPA-gear combinations ---------------
# Main specification
relevant_mpa_gear_reg <- feols(log(cpue_tot) ~ i(post, near, 0) | id + year + effort_measure,
                               panel.id = ~id + year,
                               vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                                 time = ~year,
                                                                 lat = ~lat,
                                                                 lon = ~lon,
                                                                 cutoff = 200,
                                                                 lag = 5),
                               fsplit = ~nice_gear,
                               data = most_relevant_panel)

# Same, but without FEs
relevant_mpa_gear_reg_wo_fe <- feols(log(cpue_tot) ~ post + near + post:near | effort_measure,
                                     panel.id = ~id + year,
                                     vcov = function(x)vcov_conley_hac(x,
                                                                       id = ~id,
                                                                       time = ~year,
                                                                       lat = ~lat,
                                                                       lon = ~lon,
                                                                       cutoff = 200,
                                                                       lag = 5),
                                     fsplit = ~nice_gear,
                                     data = most_relevant_panel)

# Build regression table -------------------------------------------------------
panelsummary(list(main_reg_wo_fe[[1]], main_reg[[1]],
                  main_reg_wo_fe[[2]], main_reg[[2]],
                  main_reg_wo_fe[[3]], main_reg[[3]]),
             list(relevant_mpa_gear_reg_wo_fe[[1]], relevant_mpa_gear_reg[[1]],
                  relevant_mpa_gear_reg_wo_fe[[2]], relevant_mpa_gear_reg[[2]],
                  relevant_mpa_gear_reg_wo_fe[[3]], relevant_mpa_gear_reg[[3]]),
             panel_labels = c("Panel A: All data", "Panel B: Relevant MPA-gear combinations"),
             stars = "econ",
             pretty_num = T,
             collapse_fe = T,
             coef_map = c("post::1:near" = "Post x Near",
                          "post:near" = "Post x Near"),
             gof_omit = "With|IC|Std.|effort",
             hline_after = T,
             format = "latex",
             caption = "Spillover effects of 13 Large Marine Protected Areas on
             catch-per-unit effort of tuna fisheries (Skipjack tuna, Yellowfin
             tuna, Bigeye tuna, and Albacore tuna). Coefficients are
             difference-in-difference estimates for change in CPUE.
             Columns 1 and 2 show results for purse seine and longline data
             combined. Column 3 and 4 present models fit to purse seine data
             only, and columns 5 and 6 present models fit to longline data only.") %>%
  add_header_above(c(" " = 1,
                     "Combined gears" = 2,
                     "Purse seine" = 2,
                     "Longline" = 2)) %>%
  footnote(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                "Numbers in parenthesis are standard errors robust to heteroskedasticity",
                "and spatio-temporal autocorrelation (200 km cutoff; 5 yr lag).",
                "When the data used contains more than one effort unit",
                "(i.e. days, sets, hooks) we include fixed-effects")) %>%
  cat(file = here("results", "tab", "main_regression_table.tex"))


# Extract gear reference points -------------------------------------------------
gear_stats <- relevant_mpa_gear_reg %>%
  map_dfr(broom::tidy, .id = "sample") %>%
  filter(str_detect(term, ":")) %>%
  mutate(sample = str_remove(sample, ".+; sample: "),
         gear = str_sub(sample, 1, 2),
         mpa = str_remove(sample, "LL |PS "),
         mpa = fct_reorder(mpa, estimate, max),
         gear = fct_relevel(gear, "PS", "LL"),
         gear = ifelse(gear == "PS", "Purse seine", "Longline"),
         gear = fct_relevel(gear, "Purse seine", "Longline")) %>%
  filter(!sample == "Full sample")

# Export
saveRDS(object = gear_stats,
        file = here("data", "output", "relevant_mpa_gear_combination_model_coefs.rds"))






########



# Continuous regression --------------------------------------------------------
main_cont_reg <- feols(log(cpue_tot) ~ post + dist + post:dist | effort_measure,
                  panel.id = ~id + year,
                  vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                    time = ~year,
                                                    lat = ~lat,
                                                    lon = ~lon,
                                                    cutoff = 200,
                                                    lag = 5),
                  fsplit = ~nice_gear,
                  data = annual_panel %>%
                    mutate(dist = -1 * (dist / 100)))

relevant_mpa_gear_cont_reg <- feols(log(cpue_tot) ~ post + dist + post:dist | effort_measure,
                                    panel.id = ~id + year,
                                    vcov = function(x)vcov_conley_hac(x, id = ~id,
                                                                      time = ~year,
                                                                      lat = ~lat,
                                                                      lon = ~lon,
                                                                      cutoff = 200,
                                                                      lag = 5),
                                    fsplit = ~nice_gear,
                                    data = most_relevant_panel %>%
                                      mutate(dist = -1 * (dist / 100)))


panelsummary(main_cont_reg,
             relevant_mpa_gear_cont_reg,
             colnames = c("", "Combined", "Purse Seine", "Longline"),
             panel_labels = c("Panel A: All data", "Panel B: Relevant MPA-gear combinations"),
             stars = "econ",
             pretty_num = T,
             collapse_fe = T,
             gof_omit = "With|IC|RMSE|Std.|effort",
             hline_after = T,
             caption = "Results using a continuous distance (in hundreds of nautical miles). The post x dist coefficient represent the effect of moving 100nm closer to the MPA border.")




