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
  fixest,
  tidyverse,
  readxl,
  modelsummary
)

# Source custom functions ------------------------------------------------------
source(here("scripts/00_set_up.R"))

# Load data --------------------------------------------------------------------
wcpfc <- read_excel(here("data", "processed", "RAMS legacy 3 tuna species 3 regions.xlsx"),
                    sheet = 2) %>%
  set_names(c("year", "B", "B_div_Bmsy")) %>%
  mutate(rfmo = "wcpfc")
iattc <- read_excel(here("data", "processed", "RAMS legacy 3 tuna species 3 regions.xlsx"),
                    sheet = 3) %>%
  set_names(c("year", "B", "B_div_Bmsy")) %>%
  mutate(rfmo = "iattc")
iotc <- read_excel(here("data", "processed", "RAMS legacy 3 tuna species 3 regions.xlsx"),
                    sheet = 4) %>%
  set_names(c("year", "B", "B_div_Bmsy")) %>%
  mutate(rfmo = "iotc")

tuna_indices <- bind_rows(wcpfc, iattc, iotc) %>%
  mutate(B = B/1e6)


## PROCESSING ##################################################################
most_relevant_panel <- readRDS(file = here("data", "processed", "annual_relevant_mpa_gears_estimation_panel.rds")) %>%
  filter(gear == "purse_seine") %>%
  mutate(rfmo = case_when(name == "Galápagos Marine Reserve" ~ "iattc",
                          name == "Chagos Marine Protected Area" ~ "iotc",
                          name == "PRI (Jarvis)" ~ "wcpfc",
                          name == "Phoenix Islands Protected Area" ~ "wcpfc",
                          name == "Nazca-Desventuradas MP" ~ "iattc",
                          name == "Revillagigedo" ~ "iattc")) %>%
  left_join(tuna_indices, by = join_by(year, rfmo))

step4_r <- feols(log(cpue_tot) ~ post + near + post:near | id + flag + wdpaid ^ year,
                 panel.id = ~id + year,
                 data = most_relevant_panel,
                 subset = ~gear == "purse_seine",
                 vcov = conley(cutoff = 200))

step4_ra <- feols(log(cpue_tot) ~ post + near + post:near | id + flag + wdpaid ^ year,
                 panel.id = ~id + year,
                 data = most_relevant_panel %>%
                   drop_na(B),
                 subset = ~gear == "purse_seine",
                 vcov = conley(cutoff = 200))

step4_rB <- feols(log(cpue_tot) ~ post + near + post:near + B | id + flag,
                 panel.id = ~id + year,
                 data = most_relevant_panel,
                 subset = ~gear == "purse_seine",
                 vcov = conley(cutoff = 200))

step4_rb <- feols(log(cpue_tot) ~ post + near + post:near | id + flag + wdpaid ^ year,
                 panel.id = ~id + year,
                 data = most_relevant_panel %>%
                   drop_na(B_div_Bmsy),
                 subset = ~gear == "purse_seine",
                 vcov = conley(cutoff = 200))

step4_rB_div_Bmsy <- feols(log(cpue_tot) ~ post + near + post:near + B_div_Bmsy | id + flag,
                 panel.id = ~id + year,
                 data = most_relevant_panel,
                 subset = ~gear == "purse_seine",
                 vcov = conley(cutoff = 200))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "N", 0,
              "adj.r.squared", "R2 Adj", 3,
              "vcov.type", "SE", 0,
              "FE: id", "FE: Grid ID", 0,
              "FE: flag", "FE: Flag", 0,
              "FE: wdpaid^year", "FE: MPA-Year", 0,
              "FE: year", "FE: Year", 0,
              "FE: wdpaid", "FE: MPA", 0
)

modelsummary(list("Main text" = step4_r,
                  "Comp 1" = step4_ra,
                  "B index" = step4_rB,
                  "Comp 2" = step4_rb,
                  "norm B index" = step4_rB_div_Bmsy),
             coef_map = c("post:near" = "Post x Near",
                          "B" = "B",
                          "B_div_Bmsy" = "norm B"),
             stars = panelsummary:::econ_stars(),
             gof_map = gm)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
