# Load packages ----------------------------------------------------------------
pacman::p_load(
  fixest,
  here,
  tidyverse,
  modelsummary,
  panelsummary
)

# Source custom functions ------------------------------------------------------
source(here("scripts/00_set_up.R"))

# Load data --------------------------------------------------------------------
annual_panel <- readRDS(file = here("data", "processed", "annual_full_estimation_panel.rds"))
most_relevant_panel <- readRDS(file = here("data", "processed", "annual_relevant_mpa_gears_estimation_panel.rds"))


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
               vcov = "IID")

# Step 3: Now add fixed effects by fleet to account for unobserved fleet-specific characteristics
step3 <- feols(log(cpue_tot) ~ post + near + post:near | id + flag,
               panel.id = ~id + year,
               data = annual_panel,
               subset = ~gear == "purse_seine",
               vcov = "IID")

# Step4: Now add fixed effects effects by mpa-gear-year to account for time varying fishery-specific changes
step4 <- feols(log(cpue_tot) ~ post + near + post:near | id + flag + wdpaid ^ year,
               panel.id = ~id + year,
               data = annual_panel,
               subset = ~gear == "purse_seine",
               vcov = "IID")

step5 <- feols(log(cpue_tot) ~ post + near + post:near + oni_avg:wdpaid + I(oni_avg ^ 2):wdpaid | id + flag + wdpaid,
               panel.id = ~id + year,
               data = annual_panel,
               subset = ~gear == "purse_seine",
               vcov = "IID")

# Estimate DiD with subsamples of relevant MPA-gear combinations ---------------
# Main specification
# Step 1: no fixed effects whatsoever.
step1_r <- feols(log(cpue_tot) ~ post + near + post:near,
                 panel.id = ~id + year,
                 data = most_relevant_panel,
                 subset = ~gear == "purse_seine",
                 vcov = "IID")

# Step 2: Control for unobserved characteristics in space with grid-id fixed effects
step2_r <- feols(log(cpue_tot) ~ post + near + post:near | id,
                 panel.id = ~id + year,
                 data = most_relevant_panel,
                 subset = ~gear == "purse_seine",
                 vcov = "IID")

# Step 3: Now add fixed effects by fleet to account for unobserved fleet-specific characteristics
step3_r <- feols(log(cpue_tot) ~ post + near + post:near | id + flag,
                 panel.id = ~id + year,
                 data = most_relevant_panel,
                 subset = ~gear == "purse_seine",
                 vcov = "IID")

# Step4: Now add fixed effects effects by mpa-gear-year to account for time varying fishery-specific changes
step4_r <- feols(log(cpue_tot) ~ post + near + post:near | id + flag + wdpaid ^ year,
                 panel.id = ~id + year,
                 data = most_relevant_panel,
                 subset = ~gear == "purse_seine",
                 vcov = "IID")

# Step 5: Add quadratic ENSO terms
step5_r <- feols(log(cpue_tot) ~ post + near + post:near + wdpaid:oni_avg + wdpaid:I(oni_avg ^ 2) | id + flag + wdpaid,
                 panel.id = ~id + year,
                 data = most_relevant_panel,
                 subset = ~gear == "purse_seine",
                 vcov = "IID")

## COMBINE #####################################################################

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

gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "N", 0,
              "adj.r.squared", "R2 Adj", 3,
              "vcov.type", "SE", 0,
              "FE: id", "FE: Grid ID", 0,
              "FE: flag", "FE: Flag", 0,
              "FE: wdpaid^year", "FE: MPA-Year", 0,
              "FE: wdpaid", "FE: MPA", 0
)

panelsummary(main_reg,
             relevant_mpa_gear_reg,
             stars = "econ",
             panel_labels = c("Panel A: All data (9 LSMPAs)",
                              "Panel B: Subsample of relevant LSMPAs (6 LSMPAs)"),
             gof_map = gm,
             coef_map = c("post:near" = "Post x Near"),
             collapse_fe = T,
             caption = "Effect of Large-Scale Marine Protected Areas on catch-per-unit-effort (CPUE) of tuna purse seine fisheries.
             Coefficients are difference-in-difference estimates for change in CPUE. Column 1 is the simplest
             specification with no fixed effects. Column 2 includes fixed effects by grid cell. Column 3 includes fixed
             effects by grid and by flag. Column 4 shows a full specification with fixed-effects by grid, by
             flag, and by MPA-year. Column 5 then incorporates linear and quadratic terms for the Oceanic Nino Index interacted with MPA, and removes MPA-year fixed effects but retains MPA fixed effects.
             N is number of observations, R2 Adj is Adjusted R$^2$, and SE is the method/assumption used to calculate standard errors. IID stands for independently and identically distributed. All standard errors are IID.") %>%
  footnote("$* p < 0.1, ** p < 0.05, *** p < 0.01$", escape = T)

