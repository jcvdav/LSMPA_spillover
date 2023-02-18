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
iattc <- readRDS(file = here("data", "processed", "iattc_tuna_monthly_gear_flag.rds"))
iotc <- readRDS(file = here("data", "processed", "iotc_tuna_monthly_gear_flag.rds"))
wcpfc <- readRDS(file = here("data", "processed", "wcpfc_tuna_quarterly_gear_flag.rds"))
iccat <- readRDS(file = here("data", "processed", "iccat_tuna_monthly_gear_flag.rds"))

## PROCESSING ##################################################################

# Group IATTC by quarter -------------------------------------------------------
iattc_quarterly <- iattc %>%
  filter(effort > 0) %>%                                                        # Removes 9 observations where effort is 0
  mutate(qtr = ceiling(month / 3)) %>%
  group_by(
    rfmo,
    year,
    qtr,
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

# Group IOTC by quarter --------------------------------------------------------
iotc_quarterly <- iotc %>%
  filter(effort > 0) %>%                                                        # Removes 70 observations where effort is 0
  mutate(qtr = ceiling(month / 3)) %>%
  group_by(
    rfmo,
    year,
    qtr,
    gear,
    flag,
    lat,
    lon,
    effort_measure) %>%
  summarize_at(.vars = vars(effort, yft_mt , bet_mt , skj_mt , alb_mt , sbf_mt , lot_mt), .funs = sum, na.rm = T) %>%
  ungroup() %>%
  mutate(tot_mt = yft_mt + bet_mt + skj_mt + alb_mt + sbf_mt + lot_mt,
         cpue_yft = yft_mt / effort,
         cpue_bet = bet_mt / effort,
         cpue_skj = skj_mt / effort,
         cpue_alb = alb_mt / effort,
         cpue_sbf = sbf_mt / effort,
         cpue_lot = lot_mt / effort,
         cpue_tot = tot_mt / effort)

# Group WCPFC by quarter -------------------------------------------------------
wcpfc_quarterly <- wcpfc %>%
  group_by(
    rfmo,
    year,
    qtr,
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

# Group IOTC by quarter --------------------------------------------------------
iccat_quarterly <- iccat %>%
  filter(effort > 0) %>%                                                        # Removes 70 observations where effort is 0
  mutate(qtr = ceiling(month / 3)) %>%
  group_by(
    rfmo,
    year,
    qtr,
    gear,
    flag,
    lat,
    lon,
    effort_measure) %>%
  summarize_at(.vars = vars(effort, bft_mt, alb_mt, yft_mt, bet_mt, skj_mt), .funs = sum, na.rm = T) %>%
  ungroup() %>%
  mutate(tot_mt = bft_mt + alb_mt + yft_mt + bet_mt + skj_mt,
         cpue_bft = bft_mt / effort,
         cpue_alb = alb_mt / effort,
         cpue_yft = yft_mt / effort,
         cpue_bet = bet_mt / effort,
         cpue_skj = skj_mt / effort,
         cpue_tot = tot_mt / effort)

# Combine all ------------------------------------------------------------------
quarterly_all_rfmos <- bind_rows(iattc_quarterly,
                                 iotc_quarterly,
                                 wcpfc_quarterly,
                                 iccat_quarterly) %>%
  select(
    rfmo,
    year,
    qtr,
    gear,
    flag,
    lat,
    lon,
    effort_measure,
    effort,
    alb_mt, bet_mt, bft_mt, lot_mt, pbf_mt, sbf_mt, skj_mt, tot_mt, yft_mt,
    cpue_alb, cpue_bet, cpue_bft, cpue_lot, cpue_pbf, cpue_sbf, cpue_skj, cpue_tot, cpue_yft
  )

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = quarterly_all_rfmos,
        file = here("data", "processed", "quarterly_all_rfmos.rds"))
