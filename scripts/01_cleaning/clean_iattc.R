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

pl_tuna <-
  read_csv(file = here(
    "data",
    "raw",
    "RFMO_data",
    "IATTC",
    "PublicLPTuna",
    "PublicLPTunaFlag.csv"
  )) %>%
  clean_names()

## PROCESSING ##################################################################

# Clean purse seine data -------------------------------------------------------
ps_tuna_clean <- ps_tuna %>%
  rename(
    lat = lat_c1,
    lon = lon_c1,
    alb_mt = alb,
    bet_mt = bet,
    bkj_mt = bkj,
    bzx_mt = bzx,
    pbf_mt = pbf,
    skj_mt = skj,
    tun_mt = tun,
    yft_mt = yft
  ) %>%
  mutate(
    tot_mt = alb_mt + bet_mt + pbf_mt + skj_mt + yft_mt,
    cpue_alb = alb_mt / num_sets,
    cpue_bet = bet_mt / num_sets,
    cpue_bkj = bkj_mt / num_sets,
    cpue_bzx = bzx_mt / num_sets,
    cpue_pbf = pbf_mt / num_sets,
    cpue_skj = skj_mt / num_sets,
    cpue_tun = tun_mt / num_sets,
    cpue_yft = yft_mt / num_sets,
    cpue_tot = tot_mt / num_sets
  ) %>%
  rename(effort = num_sets) %>%
  mutate(effort_measure = "sets",
         gear = "purse_seine",
         rfmo = "iattc")

# Clean longline data ----------------------------------------------------------
ll_tuna_clean <- ll_tuna %>%
  rename(lat = lat_c5, lon = lon_c5) %>%
  select(year, month, flag, lat, lon, hooks, contains("mt")) %>%
  rename(
    alb_mt = al_bmt,
    bet_mt = be_tmt,
    pbf_mt = pb_fmt,
    skj_mt = sk_jmt,
    tun_mt = tu_nmt,
    yft_mt = yf_tmt,
    bil_mt = bi_lmt,
    blm_mt = bl_mmt,
    bum_mt = bu_mmt,
    mls_mt = ml_smt,
    sfa_mt = sf_amt,
    ssp_mt = ss_pmt,
    swo_mt = sw_omt
  ) %>%
  mutate(
    tot_mt = alb_mt + bet_mt + pbf_mt + skj_mt + yft_mt,
    cpue_alb = alb_mt / hooks,
    cpue_bet = bet_mt / hooks,
    cpue_pbf = pbf_mt / hooks,
    cpue_skj = skj_mt / hooks,
    cpue_tun = tun_mt / hooks,
    cpue_yft = yft_mt / hooks,
    cpue_bil = bil_mt / hooks,
    cpue_blm = blm_mt / hooks,
    cpue_bum = bum_mt / hooks,
    cpue_mls = mls_mt / hooks,
    cpue_sfa = sfa_mt / hooks,
    cpue_ssp = ssp_mt / hooks,
    cpue_swo = swo_mt / hooks,
    cpue_tot = tot_mt / hooks
  ) %>%
  rename(effort = hooks) %>%
  mutate(effort_measure = "hooks",
         gear = "longline",
         rfmo = "iattc")

# Clean pole and line data -----------------------------------------------------
pl_tuna_clean <- pl_tuna %>%
  rename(
    lat = lat_c1,
    lon = lon_c1,
    alb_mt = alb,
    bet_mt = bet,
    bkj_mt = bkj,
    bzx_mt = bzx,
    pbf_mt = pbf,
    skj_mt = skj,
    tun_mt = tun,
    yft_mt = yft
  ) %>%
  mutate(
    tot_mt = alb_mt + bet_mt + pbf_mt + skj_mt + yft_mt,
    cpue_alb = alb_mt / num_sets,
    cpue_bet = bet_mt / num_sets,
    cpue_bkj = bkj_mt / num_sets,
    cpue_bzx = bzx_mt / num_sets,
    cpue_pbf = pbf_mt / num_sets,
    cpue_skj = skj_mt / num_sets,
    cpue_tun = tun_mt / num_sets,
    cpue_yft = yft_mt / num_sets,
    cpue_tot = tot_mt / num_sets
  ) %>%
  rename(effort = num_sets) %>%
  mutate(effort_measure = "sets",
         gear = "pole_and_line",
         rfmo = "iattc")

# Combine ----------------------------------------------------------------------
iattc_tuna <-
  bind_rows(ps_tuna_clean, ll_tuna_clean, pl_tuna_clean) %>%
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

## VISUALIZE ###################################################################

# Quick time series to check ---------------------------------------------------
# iattc_tuna %>%
#   group_by(gear) %>%
#   mutate(norm_cpue_tot = (cpue_tot - mean(cpue_tot, na.rm = T)) / sd(cpue_tot, na.rm = T)) %>%
#   ungroup() %>%
#   ggplot(aes(x = year, y = norm_cpue_tot, color = gear)) +
#   geom_smooth()

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(
  object = iattc_tuna,
  file = here("data", "processed", "iattc_tuna_monthly_gear_flag.rds")
)
