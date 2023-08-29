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
  fixest,
  here,
  tidyverse
)

# Source custom functions ------------------------------------------------------
source(here("scripts/00_set_up.R"))

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

## Extract gear-specific coefficients ------------------------------------------
gear_stats <- relevant_mpa_gear_reg %>%
  map_dfr(broom::tidy, .id = "sample", conf.int = TRUE, conf.level = 0.95) %>%
  filter(str_detect(term, ":")) %>%
  mutate(sample = str_remove(sample, ".+; sample: "),
         gear = str_sub(sample, 1, 2),
         mpa = str_remove(sample, "LL |PS "),
         mpa = fct_reorder(mpa, estimate, max),
         gear = fct_relevel(gear, "PS", "LL"),
         gear = ifelse(gear == "PS", "Purse seine", "Longline"),
         gear = fct_relevel(gear, "Purse seine", "Longline")) %>%
  filter(!sample == "Full sample")


## EXPORT ######################################################################

# Export models ----------------------------------------------------------------
# Main models
saveRDS(object = main_reg,
        file = here("data", "output", "main_reg.rds"))

# Relevant MPA-gear combinations
saveRDS(object = relevant_mpa_gear_reg,
        file = here("data", "output", "relevant_mpa_gear_reg.rds"))

# Export table of MPA and gears ------------------------------------------------
saveRDS(object = gear_stats,
        file = here("data", "output", "relevant_mpa_gear_combination_model_coefs.rds"))

