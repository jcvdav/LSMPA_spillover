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
  sf,
  here,
  tidyverse
)

# Load data --------------------------------------------------------------------
iattc <- readRDS(file = here("data", "processed", "rfmo_iattc_tuna_monthly_gear_flag.rds"))
iotc <- readRDS(file = here("data", "processed", "rfmo_iotc_tuna_monthly_gear_flag.rds"))
iccat <- readRDS(file = here("data", "processed", "rfmo_iccat_tuna_monthly_flag.rds"))

# Already quarterly
wcpfc_qtr <- readRDS(file = here("data", "processed", "rfmo_wcpfc_tuna_quarterly_gear_flag.rds"))

distance_grid <- read_rds(file = here("data", "processed", "distance_grid.rds"))
## PROCESSING ##################################################################

# Group IATTC by year ----------------------------------------------------------
iattc_qtr <- iattc %>%
  mutate(qtr = ceiling(month / 3)) %>%
  group_by(
    rfmo,
    year,
    qtr,
    gear,
    flag,
    grid,
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
iotc_qtr <- iotc %>%
  mutate(qtr = ceiling(month / 3)) %>%
  group_by(
    rfmo,
    year,
    qtr,
    gear,
    flag,
    grid,
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

# Group ICCAT by quarter -------------------------------------------------------
iccat_qtr <- iccat %>%
  mutate(qtr = ceiling(month / 3)) %>%
  group_by(
    rfmo,
    qtr,
    year,
    gear,
    flag,
    grid,
    lat,
    lon,
    effort_measure) %>%
  summarize_at(.vars = vars(effort, skj_mt, yft_mt, bet_mt, bft_mt, alb_mt), .funs = sum, na.rm = T) %>%
  ungroup() %>%
  mutate(tot_mt = skj_mt + yft_mt + bet_mt + bft_mt + alb_mt,
         cpue_skj = skj_mt / effort,
         cpue_yft = yft_mt / effort,
         cpue_bet = bet_mt / effort,
         cpue_bft = bft_mt / effort,
         cpue_alb = alb_mt / effort,
         cpue_tot = tot_mt / effort)

# Combine all ------------------------------------------------------------------
qtr_all_rfmos <- bind_rows(iattc_qtr,
                           iotc_qtr,
                           wcpfc_qtr,
                           iccat_qtr) %>%
  select(
    rfmo,
    year,
    qtr,
    gear,
    flag,
    grid,
    lat,
    lon,
    effort_measure,
    effort,
    alb_mt, bet_mt, bft_mt, pbf_mt, sbf_mt, skj_mt, yft_mt, tot_mt
  ) %>%
  replace_na(replace = list(alb_mt = 0,
                            bet_mt = 0,
                            bft_mt = 0,
                            pbf_mt = 0,
                            sbf_mt = 0,
                            skj_mt = 0,
                            yft_mt = 0)) %>%
  # Remove points from ICCAT that overlap with points from IATTC
  filter(
    !(rfmo == "iccat" & lon == -86.5 & lat == 12.5),
    !(rfmo == "iccat" & lon == -82.5 & lat == 7.5),
    !(rfmo == "iccat" & lon == -82.5 & lat == 12.5),
    !(rfmo == "iccat" & lon == -78.5 & lat == 7.5),
    !(rfmo == "iccat" & lon == -77.5 & lat == 7.5)) %>%
  # Remove points from IATTC that overlap with points from ICCAT
  # Note that here we only remove 4, as indicated in the SM of our paper
  # We need to keep -82.5, 7.5 that is close to Galapagos (within 600 naut miles)
  filter(
    !(rfmo == "iattc" & lon == -86.5 & lat == 12.5),
    !(rfmo == "iattc" & lon == -82.5 & lat == 12.5),
    !(rfmo == "iattc" & lon == -78.5 & lat == 7.5),
    !(rfmo == "iattc" & lon == -77.5 & lat == 7.5)
  ) %>%
  group_by(year, qtr, gear, flag, grid, lat, lon, effort_measure) %>%
  summarize(effort = max(effort),
            alb_mt = max(alb_mt),
            bet_mt = max(bet_mt),
            bft_mt = max(bft_mt),
            pbf_mt = max(pbf_mt),
            sbf_mt = max(sbf_mt),
            skj_mt = max(skj_mt),
            yft_mt = max(yft_mt),
            tot_mt = max(tot_mt)) %>%
  ungroup() %>%
  mutate(tot_mt = alb_mt + bet_mt + bft_mt + pbf_mt + sbf_mt + skj_mt + yft_mt,
         cpue_alb = alb_mt / effort,
         cpue_bet = bet_mt / effort,
         cpue_bft = bft_mt / effort,
         cpue_pbf = pbf_mt / effort,
         cpue_sbf = sbf_mt / effort,
         cpue_skj = skj_mt / effort,
         cpue_yft = yft_mt / effort,
         cpue_tot = tot_mt / effort) %>%
  left_join(distance_grid, by = c("grid", "lon", "lat")) %>%
  mutate(id = paste(lat, lon, sep = "_"),
         event = year - year_enforced,
         post = 1 * (event >= 0)) %>%
  filter(cpue_tot > 0)

# How many records did we remove?
dim(qtr_all_rfmos) # [1]  633128     27

## CHECKS ######################################################################
check_mt(qtr_all_rfmos)

check_effort_gear(qtr_all_rfmos)

test(qtr_all_rfmos)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = qtr_all_rfmos,
        file = here("data", "processed", "rfmo_all_qtr_gear_flag.rds"))
