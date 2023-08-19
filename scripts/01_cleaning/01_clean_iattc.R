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
pacman::p_load(here,
               janitor,
               tidyverse)

# Load data --------------------------------------------------------------------
ps_tuna <-
  read_csv(file = here(
    "data",
    "raw",
    "RFMO_data",
    "IATTC",
    "PublicPSTuna",
    "PublicPSTunaFlag.csv"
  )) %>%
  clean_names()

ll_tuna <-
  read_csv(
    file = here(
      "data",
      "raw",
      "RFMO_data",
      "IATTC",
      "PublicLLTunaBillfish",
      "PublicLLTunaBillfishMt.csv"
    )
  ) %>%
  clean_names()

## PROCESSING ##################################################################

# Clean purse seine data -------------------------------------------------------
ps_tuna_clean <- ps_tuna %>%
  rename(
    lat = lat_c1,
    lon = lon_c1,
    effort = num_sets,
    # Tunas
    alb_mt = alb,
    bet_mt = bet,
    pbf_mt = pbf,
    skj_mt = skj,
    yft_mt = yft
    # Not tunas
    # bkj_mt = bkj,
    # bzx_mt = bzx,
    # tun_mt = tun,
  ) %>%
  mutate(tot_mt = alb_mt + bet_mt + pbf_mt + skj_mt + yft_mt) %>%
  filter(tot_mt > 0) %>%  # Remove records that had effort and catch data for a species other than tuna
  filter(effort > 0) %>%  # And  records that report effort = 0
  mutate(
    cpue_alb = alb_mt / effort,
    cpue_bet = bet_mt / effort,
    cpue_pbf = pbf_mt / effort,
    cpue_skj = skj_mt / effort,
    cpue_yft = yft_mt / effort,
    cpue_tot = tot_mt / effort
    # cpue_bkj = bkj_mt / num_sets,
    # cpue_bzx = bzx_mt / num_sets,
    # cpue_tun = tun_mt / num_sets,
  ) %>%
  mutate(effort_measure = "sets",
         gear = "purse_seine",
         rfmo = "iattc")

# Clean longline data ----------------------------------------------------------
ll_tuna_clean <- ll_tuna %>%
  rename(lat = lat_c5, lon = lon_c5) %>%
  select(year, month, flag, lat, lon, hooks, contains("mt")) %>%
  rename(
    effort = hooks,
    # Real tunas
    alb_mt = al_bmt,
    bet_mt = be_tmt,
    pbf_mt = pb_fmt,
    skj_mt = sk_jmt,
    yft_mt = yf_tmt
  ) %>%
  mutate(tot_mt = alb_mt + bet_mt + pbf_mt + skj_mt + yft_mt) %>%
  filter(tot_mt > 0) %>%  # Remove records that had effort and catch data for a species other than tuna
  filter(effort > 0) %>%  # And  records that report effort = 0
  mutate(
    cpue_alb = alb_mt / effort,
    cpue_bet = bet_mt / effort,
    cpue_pbf = pbf_mt / effort,
    cpue_skj = skj_mt / effort,
    cpue_yft = yft_mt / effort,
    cpue_tot = tot_mt / effort
  ) %>%
  mutate(effort_measure = "hooks",
         gear = "longline",
         rfmo = "iattc")

# Combine ----------------------------------------------------------------------
iattc_tuna <-
  bind_rows(ps_tuna_clean, ll_tuna_clean) %>%
  select(
    rfmo,
    year,
    month,
    gear,
    flag,
    lat,
    lon,
    effort,
    effort_measure,
    contains("_mt"),
    contains("cpue_")
  )

test(iattc_tuna)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(
  object = iattc_tuna,
  file = here("data", "processed", "rfmo_iattc_tuna_monthly_gear_flag.rds")
)
