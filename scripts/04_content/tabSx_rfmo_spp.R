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
  cowplot,
  here,
  kableExtra,
  tidyverse
)

# Source custom functions ------------------------------------------------------
source(here("scripts/00_set_up.R"))

# Load data --------------------------------------------------------------------
iattc <- readRDS(file = here("data", "processed", "rfmo_iattc_tuna_monthly_gear_flag.rds"))
iotc <- readRDS(file = here("data", "processed", "rfmo_iotc_tuna_monthly_gear_flag.rds"))
wcpfc <- readRDS(file = here("data", "processed", "rfmo_wcpfc_tuna_quarterly_gear_flag.rds"))
# Note that ICCAT is already annual
iccat_annual <- readRDS(file = here("data", "processed", "rfmo_iccat_tuna_annual_gear_flag.rds"))

list(iattc = iattc,
     iotc = iotc,
     wcpfc = wcpfc,
     iccat = iccat_annual) %>%
  map_dfr(check_mt, .id = "rfmo") %>%
  select(rfmo, gear, alb_mt, bet_mt, bft_mt, pbf_mt, sbf_mt, skj_mt, yft_mt) %>%
  mutate(rfmo = str_to_upper(rfmo),
         gear = str_to_sentence(str_replace(gear, "_", " "))) %>%
  mutate_at(.vars = c(3:9), ~ifelse(. == 0 | is.na(.), "-", "X")) %>%
  kbl(booktabs = T,
      linesep = F,
      col.names = c("RFMO", "Gear", "Albacore", "Bigeye", "Bluefin", "Pacific bluefin", "Southern bluefin", "Skipjack", "Yellowfin"),
      caption = "\\textbf{List of relevant tuna species contained in the data reported
      by each Regional Fisheries Management Organization (RFMO).} IATTC stands for Inter-American Tropical Tuna Commission,
      ICCAT stands for International Commission for the Conservation of Atlantic Tunas, IOTC stands for Indian Ocean Tuna
      Commission and WCPFC stands for Western and Central Pacific Fisheries Commission.
      Note that Bluefin and Pacific bluefin are not included in our main analysis as no catch is reported near the MPAs analyzed here.",
      label = "rfmo_spp",
      format = "latex"
      ) %>%
  kable_styling() %>%
  cat(file = here("results", "tab", "rfmo_spp.tex"))
