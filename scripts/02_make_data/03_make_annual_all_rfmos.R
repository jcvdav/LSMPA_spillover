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
quarterly_all_rfmos <- readRDS(file = here("data", "processed", "quarterly_all_rfmos.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
annual_all_rfmos <- quarterly_all_rfmos %>%
  select(-contains("cpue_")) %>%
  group_by(rfmo, year, gear, flag, lat, lon, effort_measure) %>%
  summarize(effort = sum(effort, na.rm = T),
            alb_mt = sum(alb_mt, na.rm = T),
            bet_mt = sum(bet_mt, na.rm = T),
            bft_mt = sum(bft_mt, na.rm = T),
            lot_mt = sum(lot_mt, na.rm = T),
            pbf_mt = sum(pbf_mt, na.rm = T),
            sbf_mt = sum(sbf_mt, na.rm = T),
            skj_mt = sum(skj_mt, na.rm = T),
            tot_mt = sum(tot_mt, na.rm = T),
            yft_mt = sum(yft_mt, na.rm = T)) %>%
  ungroup() %>%
  mutate(cpue_alb = alb_mt / effort,
         cpue_bet = bet_mt / effort,
         cpue_bft = bft_mt / effort,
         cpue_lot = lot_mt / effort,
         cpue_pbf = pbf_mt / effort,
         cpue_sbf = sbf_mt / effort,
         cpue_skj = skj_mt / effort,
         cpue_tot = tot_mt / effort,
         cpue_yft = yft_mt / effort)


## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = annual_all_rfmos,
        file = here("data", "processed", "annual_all_rfmos.rds"))
