# Load packages ----------------------------------------------------------------
pacman::p_load(
  fixest,
  here,
  lme4,
  kableExtra,
  modelsummary,
  panelsummary,
  tidyverse
)

# Source custom functions ------------------------------------------------------
source(here("scripts/00_set_up.R"))

# Load data --------------------------------------------------------------------
annual_panel <- readRDS(file = here("data", "processed", "annual_full_estimation_panel.rds"))
most_relevant_panel <- readRDS(file = here("data", "processed", "annual_relevant_mpa_gears_estimation_panel.rds"))


# ## PROCESSING ##################################################################
# Estimate DiD with all the data -----------------------------------------------
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

# RANEFS
re2 <- lmer(log(cpue_tot) ~ post + near + post:near + (1 | id),
            data = annual_panel %>%
              filter(gear == "purse_seine"))

re3 <- lmer(log(cpue_tot) ~ post + near + post:near + (1 | id) + (1 | flag),
            data = annual_panel %>%
              filter(gear == "purse_seine"))

re4 <- lmer(log(cpue_tot) ~ post + near + post:near + (1 | id) + (1 | flag) + (1 | wdpaid:year),
            data = annual_panel %>%
              filter(gear == "purse_seine"))

gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "N", 0,
              "adj.r.squared", "R2 Adj", 3,
              "vcov.type", "SE", 0,
              "FE: id", "FE: Grid ID", 0,
              "FE: flag", "FE: Flag", 0,
              "FE: wdpaid^year", "FE: MPA-Year", 0,
              "FE: wdpaid", "FE: MPA", 0
)

panelsummary(list(step2, step3, step4),
             list(re2, re3, re4),
             stars = "econ",
             panel_labels = c("Panel A: Fixed effects",
                              "Panel B: Random effects"),
             gof_map = gm,
             coef_map = c("post:near" = "Post x Near"),
             collapse_fe = T,
             format = "latex",
             caption = "\\label{tab:fe_vs_re}\\textbf{Comparing coefficient estimates using fixed effects vs random effects models.}
             Coefficients are difference-in-difference estimates for change in CPUE.
             Panel A uses fixed effects and Panel B uses random effects.
             Column 1 includes fixed or random effects by grid cell. Column 2 includes fixed or random
             effects by grid and by flag. Column 3 shows a full specification with fixed and random effects by grid, by
             flag, and by MPA-year. Results in Panel A are the same as main-text Table 1, Panel A, Columns 2 to 4.") %>%
  footnote("$* p < 0.1, ** p < 0.05, *** p < 0.01$", escape = F) %>%
  cat(file = here("results", "tab", "tabSx_fe_vs_re_reg_table.tex"))
