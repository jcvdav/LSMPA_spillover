################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# 1) ESTIMATE SPECIES LEVEL (SKJ and BET) for PIPA only
# 2) ESTIMATE SPECIES LEVEL, BUT REMOVING CHAGOS
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  panelsummary,
  modelsummary,
  fixest,
  tidyverse
)

# Source custom functions ------------------------------------------------------
source(here("scripts/00_set_up.R"))

# Load data --------------------------------------------------------------------
most_relevant_panel <- readRDS(file = here("data", "processed", "annual_relevant_mpa_gears_estimation_panel.rds")) %>%
  filter(gear == "purse_seine")

## PROCESSING ##################################################################
# We will need this panel for the species-level regressions
panel_for_spp_regs <- most_relevant_panel %>%
  select(wdpaid, short_name, year, event, id, lat, lon, nice_gear, flag, effort, effort_measure, near, post, cpue_alb, cpue_bet, cpue_skj, cpue_yft, -cpue_tot) %>%
  pivot_longer(cols = contains("cpue"),
               names_to = "spp",
               values_to = "cpue_tot") %>%
  filter(cpue_tot > 0) %>% # remove explicit zeroes (i.e. a cell records catch and effort but species A but not species B, so species B's CPUE is 0 [technically NA but...])
  drop_na(cpue_tot) %>%
  mutate(spp = str_to_upper(str_remove(spp, "cpue_")))

# Species-level analysis -------------------------------------------------------
PIPA_spp_regs <- feols(log(cpue_tot) ~ post + near + post:near |  id + flag + year,
                       panel.id = ~id + year,
                       vcov = conley(cutoff = 200),
                       split = ~spp,
                       subset = ~short_name == "PIPA",
                       data = panel_for_spp_regs)

PIPA_spp_regs_simple_did <- feols(log(cpue_tot) ~ post + near + post:near,
                                  panel.id = ~id + year,
                                  vcov = conley(cutoff = 200),
                                  split = ~spp,
                                  subset = ~short_name == "PIPA",
                                  data = panel_for_spp_regs)

# Pacific MPAs only ------------------------------------------------------------
Pacific_spp_regs <- feols(log(cpue_tot) ~ post + near + post:near |  id + flag + wdpaid ^ year,
                          panel.id = ~id + year,
                          vcov = conley(cutoff = 200),
                          split = ~spp,
                          subset = ~!short_name == "Chagos",
                          data = panel_for_spp_regs)

Chagos <- feols(log(cpue_tot) ~ post + near + post:near | id + flag + year,
                panel.id = ~id + year,
                vcov = conley(cutoff = 200),
                subset = ~short_name == "Chagos",
                data = most_relevant_panel)
Chagos_2014 <- feols(log(cpue_tot) ~ post + near + post:near | id + flag + year,
                panel.id = ~id + year,
                vcov = conley(cutoff = 200),
                subset = ~(short_name == "Chagos" & year <= 2014),
                data = most_relevant_panel)
Chagos_2017 <- feols(log(cpue_tot) ~ post + near + post:near | id + flag + year,
                panel.id = ~id + year,
                vcov = conley(cutoff = 200),
                subset = ~(short_name == "Chagos" & year <= 2017),
                data = most_relevant_panel)


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


modelsummary(PIPA_spp_regs,
             stars = panelsummary:::econ_stars(),
             coef_map = c("post:near" = "Post x Near"),
             gof_map = gm,
             title = "Spillover effect for three species of tuna around the Phoenix Island Protected Area.")

modelsummary(PIPA_spp_regs_simple_did,
             stars = panelsummary:::econ_stars(),
             coef_map = c("post:near" = "Post x Near"),
             gof_map = gm,
             title = "Spillover effect for three species of tuna around the Phoenix Island Protected Area. NO CONTROLS")

modelsummary(Pacific_spp_regs,
             stars = panelsummary:::econ_stars(),
             coef_map = c("post:near" = "Post x Near"),
             gof_map = gm,
             title = "Spillover effect for three species of tuna around Pacific LSMPAs.")

modelsummary(list("Chagos (all years)" = Chagos, "Chagos (2014)" = Chagos_2014),
             stars = panelsummary:::econ_stars(),
             coef_map = c("post:near" = "Post x Near"),
             gof_map = gm,
             title = "Spillover effect for three species of tuna around Chagos.")

