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
  tidyverse
)

# Load data --------------------------------------------------------------------
iattc <- readRDS(file = here("data", "processed", "rfmo_iattc_tuna_monthly_gear_flag.rds"))
iotc <- readRDS(file = here("data", "processed", "rfmo_iotc_tuna_monthly_gear_flag.rds"))
wcpfc <- readRDS(file = here("data", "processed", "rfmo_wcpfc_tuna_quarterly_gear_flag.rds"))
# Note that ICCAT is already annual
iccat_annual <- readRDS(file = here("data", "processed", "rfmo_iccat_tuna_annual_gear_flag.rds"))

## PROCESSING ##################################################################

# Group IATTC by year ----------------------------------------------------------
iattc_annual <- iattc %>%
  group_by(
    rfmo,
    year,
    gear,
    flag,
    lat,
    lon,
    effort_measure) %>%
  summarize_at(.vars = vars(effort, alb_mt, bet_mt, pbf_mt, skj_mt, yft_mt), .funs = sum, na.rm = T) %>%
  ungroup() %>%
  mutate(tot_mt = alb_mt + bet_mt + pbf_mt + skj_mt + yft_mt,
         cpue_alb = alb_mt / effort,
         cpue_bet = bet_mt / effort,
         cpue_pbf = pbf_mt / effort,
         cpue_skj = skj_mt / effort,
         cpue_yft = yft_mt / effort,
         cpue_tot = tot_mt / effort)

# Group IOTC by year -----------------------------------------------------------
iotc_annual <- iotc %>%
  group_by(
    rfmo,
    year,
    gear,
    flag,
    lat,
    lon,
    effort_measure) %>%
  summarize_at(.vars = vars(effort, yft_mt, bet_mt, skj_mt, alb_mt, sbf_mt), .funs = sum, na.rm = T) %>%
  ungroup() %>%
  mutate(tot_mt = yft_mt + bet_mt + skj_mt + alb_mt + sbf_mt,
         cpue_yft = yft_mt / effort,
         cpue_bet = bet_mt / effort,
         cpue_skj = skj_mt / effort,
         cpue_alb = alb_mt / effort,
         cpue_sbf = sbf_mt / effort,
         cpue_tot = tot_mt / effort)

# Group WCPFC by quarter -------------------------------------------------------
wcpfc_annual <- wcpfc %>%
  group_by(
    rfmo,
    year,
    gear,
    flag,
    lat,
    lon,
    effort_measure) %>%
  summarize_at(.vars = vars(effort, skj_mt, alb_mt, yft_mt, bet_mt), .funs = sum, na.rm = T) %>%
  ungroup() %>%
  mutate(tot_mt = yft_mt + bet_mt + skj_mt + alb_mt,
         cpue_yft = yft_mt / effort,
         cpue_bet = bet_mt / effort,
         cpue_skj = skj_mt / effort,
         cpue_alb = alb_mt / effort,
         cpue_tot = tot_mt / effort)

# Combine all ------------------------------------------------------------------
annual_all_rfmos <- bind_rows(iattc_annual,
                              iotc_annual,
                              wcpfc_annual,
                              iccat_annual) %>%
  select(
    rfmo,
    year,
    gear,
    flag,
    lat,
    lon,
    effort_measure,
    effort,
    alb_mt, bet_mt, bft_mt, pbf_mt, sbf_mt, skj_mt, tot_mt, yft_mt,
    cpue_alb, cpue_bet, cpue_bft, cpue_pbf, cpue_sbf, cpue_skj, cpue_tot, cpue_yft
  )


test(annual_all_rfmos)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = annual_all_rfmos,
        file = here("data", "processed", "rfmo_all_annual_gear_flag.rds"))
