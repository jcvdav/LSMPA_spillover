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

# SOME CHECKS BEFORE PROCEEDING TO ANALYSIS
# There should be 9 LSMPAS in the main regression panel for purse seine
length(unique(annual_panel[annual_panel$gear == "purse_seine",]$name)) == 9
# There should be 6 LMPSAS int he most relevant panel for purse seine
length(unique(most_relevant_panel[most_relevant_panel$gear == "purse_seine",]$name)) == 6

# ## PROCESSING ##################################################################
# Estimate DiD with all the data -----------------------------------------------
# Step 1: no fixed effects whatsoever.
step1 <- feols(log(cpue_tot) ~ post + near + post:near,
               panel.id = ~id + year,
               data = annual_panel,
               subset = ~gear == "purse_seine",
               vcov = "IID")

# Step 2: Control for unobserved characteristics in space with grid-id fixed effects
step2 <- feols(log(cpue_tot) ~ post + near + post:near | id,
               panel.id = ~id + year,
               data = annual_panel,
               subset = ~gear == "purse_seine",
               vcov = conley(cutoff = 200))

# Step 3: Now add fixed effects by fleet to account for unobserved fleet-specific characteristics
step3 <- feols(log(cpue_tot) ~ post + near + post:near | id + flag,
               panel.id = ~id + year,
               data = annual_panel,
               subset = ~gear == "purse_seine",
               vcov = conley(cutoff = 200))

# Step4: Now add fixed effects effects by mpa-gear-year to account for time varying fishery-specific changes
step4 <- feols(log(cpue_tot) ~ post + near + post:near | id + flag + wdpaid ^ year,
               panel.id = ~id + year,
               data = annual_panel,
               subset = ~gear == "purse_seine",
               vcov = conley(cutoff = 200))

step5 <- feols(log(cpue_tot) ~ post + near + post:near + oni_avg:wdpaid + I(oni_avg ^ 2):wdpaid | id + flag + wdpaid,
               panel.id = ~id + year,
               data = annual_panel,
               subset = ~gear == "purse_seine",
               vcov = conley(cutoff = 200))

# Estimate DiD with subsamples of relevant MPA-gear combinations ---------------
# Main specification
# Step 1: no fixed effects whatsoever.
step1_r <- feols(log(cpue_tot) ~ post + near + post:near,
                 panel.id = ~id + year,
                 data = most_relevant_panel,
                 subset = ~gear == "purse_seine",
                 vcov = "iid")

# Step 2: Control for unobserved characteristics in space with grid-id fixed effects
step2_r <- feols(log(cpue_tot) ~ post + near + post:near | id,
                 panel.id = ~id + year,
                 data = most_relevant_panel,
                 subset = ~gear == "purse_seine",
                 vcov = conley(cutoff = 200))

# Step 3: Now add fixed effects by fleet to account for unobserved fleet-specific characteristics
step3_r <- feols(log(cpue_tot) ~ post + near + post:near | id + flag,
                 panel.id = ~id + year,
                 data = most_relevant_panel,
                 subset = ~gear == "purse_seine",
                 vcov = conley(cutoff = 200))

# Step4: Now add fixed effects effects by mpa-gear-year to account for time varying fishery-specific changes
step4_r <- feols(log(cpue_tot) ~ post + near + post:near | id + flag + wdpaid ^ year,
                 panel.id = ~id + year,
                 data = most_relevant_panel,
                 subset = ~gear == "purse_seine",
                 vcov = conley(cutoff = 200))

# Step 5: Add quadratic ENSO terms
step5_r <- feols(log(cpue_tot) ~ post + near + post:near + wdpaid:oni_avg + wdpaid:I(oni_avg ^ 2) | id + flag + wdpaid,
                 panel.id = ~id + year,
                 data = most_relevant_panel,
                 subset = ~gear == "purse_seine",
                 vcov = conley(cutoff = 200))

## Extract gear-specific coefficients ------------------------------------------
gear_stats <- step4_r %>%
  broom::tidy(.id = "sample", conf.int = TRUE, conf.level = 0.95) %>%
  filter(str_detect(term, ":")) %>%
  mutate(sample = "PS") %>%
  mutate(sample = str_remove(sample, ".+; sample: "),
         gear = str_sub(sample, 1, 2),
         mpa = str_remove(sample, "LL |PS "),
         mpa = fct_reorder(mpa, estimate, max),
         gear = ifelse(gear == "PS", "Purse seine", "Longline")) %>%
  filter(!sample == "Full sample")


## EXPORT ######################################################################

main_reg <- list(step1,
                 step2,
                 step3,
                 step4,
                 step5)

relevant_mpa_gear_reg <- list(step1_r,
                              step2_r,
                              step3_r,
                              step4_r,
                              step5_r)

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

