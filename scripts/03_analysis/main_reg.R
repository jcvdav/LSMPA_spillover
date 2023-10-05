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

# Step 1: no fixed effects whatsoever.
step1 <- feols(log(cpue_tot) ~ post + near + i(post, near, 0),
               panel.id = ~id + year,
               data = annual_panel,
               vcov = "IID")

# Step 2: just MPA-fishery fixed effects, so separate dummies for PIPA longline and PIPA purse seine etc. (this allows for fundamental differences in mean values across fisheries).
step2 <- feols(log(cpue_tot) ~ post + near + i(post, near, 0) | id,
               panel.id = ~id + year,
               data = annual_panel,
               vcov = conley(cutoff = 200))

# Step 3: MPA-gear dummies plus those same dummies interacted with year dummies
step3 <- feols(log(cpue_tot) ~ post + near + i(post, near, 0) | id + flag ^ gear,
               panel.id = ~id + year,
               data = annual_panel,
               vcov = conley(cutoff = 200))

step4 <- feols(log(cpue_tot) ~ post + near + i(post, near, 0) | id + flag ^ gear + wdpaid ^ gear ^ year,
               panel.id = ~id + year,
               data = annual_panel,
               vcov = conley(cutoff = 200))

step5 <- feols(log(cpue_tot) ~ post + near + i(post, near, 0) | id + flag ^ gear + wdpaid ^ gear ^ year,
               panel.id = ~id + year,
               data = annual_panel,
               vcov = conley(cutoff = 200),
               split = ~nice_gear)

coefplot(list(step1, step2, step3, step4), keep = "::")


##############
# Step 1: no fixed effects whatsoever.
step1_r <- feols(log(cpue_tot) ~ post + near + i(post, near, 0),
                 panel.id = ~id + year,
                 data = most_relevant_panel,
                 vcov = "iid")

# Step 2: just MPA-fishery fixed effects, so separate dummies for PsIPA longline and PIPA purse seine etc. (this allows for fundamental differences in mean values across fisheries).
step2_r <- feols(log(cpue_tot) ~ post + near + i(post, near, 0) | id,
                 panel.id = ~id + year,
                 data = most_relevant_panel,
                 vcov = conley(cutoff = 200))

# Step 3: MPA-gear dummies plus those same dummies interacted with year dummies
step3_r <- feols(log(cpue_tot) ~ post + near + i(post, near, 0) | id + flag ^ gear,
                 panel.id = ~id + year,
                 data = most_relevant_panel,
                 vcov = conley(cutoff = 200))

step4_r <- feols(log(cpue_tot) ~ post + near + i(post, near, 0) | id + flag ^ gear + wdpaid ^ gear ^ year,
                 panel.id = ~id + year,
                 data = most_relevant_panel,
                 vcov = conley(cutoff = 200))

step5_r <- feols(log(cpue_tot) ~ post + near + i(post, near, 0) | id + flag ^ gear + wdpaid ^ gear ^ year,
                  panel.id = ~id + year,
                  data = most_relevant_panel,
                  vcov = conley(cutoff = 200),
                  split = ~nice_gear)

gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "N", 0,
              "adj.r.squared", "R2 Adj", 3,
              "vcov.type", "SE", 0,
              "FE: id", "FE: Grid ID", 0,
              "FE: flag^gear", "FE: Flag-Gear", 0,
              "FE: wdpaid^gear^year", "FE: MPA-Gear-Year", 0
              )

panelsummary(list(step1, step2, step3, step4, step5),
             list(step1_r, step2_r, step3_r, step4_r, step5_r),
             stars = "econ",
             panel_labels = c("Panel A: All data (23 LMPA-gear combinations; 14 LMPAs)",
                              "Panel B: Subsample (14 LMPA-gear combinations, 11 LMPAs)"),
             gof_map = gm,
             coef_map = c("post::1:near" = "Post x Near"),
             collapse_fe = T,
             format = "latex",
             caption = "Effect of Large Marine Protected Areas on catch-per-unit-effort in the worlds tuna fisheries.
             Coefficients are difference-in-difference estimates for change in CPUE. Column 1 is the simplest
             specification with no fixed effects. Column 2 includes grid-level fixed effects. Column 3 includes fixed
             effects by grid and by Flag-Gear. Column 4 shows a full specification with fixed-effects by grid, by
             Flag-Gear, and by MPA-Gear-Year. Columns 5 and 6 are the same as columnf 4, but restrict data to purseine
             and longline catch, respectively.") %>%
  footnote("* p < 0.1, ** p < 0.05, *** p < 0.01") %>%
  cat(file = here("results", "tab", "tab1_main_reg_table.tex"))

