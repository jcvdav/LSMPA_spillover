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
iotc_surface <- read_csv(
  here(
    "data",
    "raw",
    "RFMO_data",
    "IOTC",
    "IOTC-DATASETS-2023-01-23-CE-ALL_1950-2021",
    "IOTC-DATASETS-2023-01-23-CE-Surface_1950-2021.csv"
  ),
  col_types = cols(
    Fleet = col_character(),
    Gear = col_character(),
    Year = col_double(),
    MonthStart = col_double(),
    MonthEnd = col_double(),
    iGrid = col_character(),
    Grid = col_double(),
    Effort = col_double(),
    EffortUnits = col_character(),
    QualityCode = col_double(),
    Source = col_character(),
    CatchUnits = col_character(),
    `YFT-FS` = col_double(),
    `YFT-LS` = col_double(),
    `YFT-UNCL` = col_double(),
    `BET-FS` = col_double(),
    `BET-LS` = col_double(),
    `BET-UNCL` = col_double(),
    `SKJ-FS` = col_double(),
    `SKJ-LS` = col_double(),
    `SKJ-UNCL` = col_double(),
    `ALB-FS` = col_double(),
    `ALB-LS` = col_double(),
    `ALB-UNCL` = col_double(),
    `SBF-FS` = col_double(),
    `SBF-LS` = col_double(),
    `SBF-UNCL` = col_double(),
    `LOT-FS` = col_double(),
    `LOT-LS` = col_double(),
    `LOT-UNCL` = col_double(),
    `FRZ-FS` = col_double(),
    `FRZ-LS` = col_double(),
    `FRZ-UNCL` = col_double(),
    `KAW-FS` = col_double(),
    `KAW-LS` = col_double(),
    `KAW-UNCL` = col_double(),
    `COM-FS` = col_double(),
    `COM-LS` = col_double(),
    `COM-UNCL` = col_double(),
    `TUX-FS` = col_double(),
    `TUX-LS` = col_double(),
    `TUX-UNCL` = col_double(),
    `FAL-FS` = col_double(),
    `FAL-LS` = col_double(),
    `FAL-UNCL` = col_double(),
    `OCS-FS` = col_double(),
    `OCS-LS` = col_double(),
    `OCS-UNCL` = col_double(),
    `SKH-FS` = col_double(),
    `SKH-LS` = col_double(),
    `SKH-UNCL` = col_double(),
    `NTAD-FS` = col_double(),
    `NTAD-LS` = col_double(),
    `NTAD-UNCL` = col_double()
  )
) %>%
  clean_names()

iotc_longline <- read_csv(
  here(
    "data",
    "raw",
    "RFMO_data",
    "IOTC",
    "IOTC-DATASETS-2023-01-23-CE-ALL_1950-2021",
    "IOTC-DATASETS-2023-01-23-CE-Longline_1950-2021.csv"
  ),
  col_types = cols(
    Fleet = col_character(),
    Gear = col_character(),
    Year = col_double(),
    MonthStart = col_double(),
    MonthEnd = col_double(),
    iGrid = col_character(),
    Grid = col_double(),
    Effort = col_double(),
    EffortUnits = col_character(),
    QualityCode = col_double(),
    Source = col_character(),
    `YFT-NO` = col_double(),
    `YFT-MT` = col_double(),
    `BET-NO` = col_double(),
    `BET-MT` = col_double(),
    `SKJ-NO` = col_double(),
    `SKJ-MT` = col_double(),
    `ALB-NO` = col_double(),
    `ALB-MT` = col_double(),
    `SBF-NO` = col_double(),
    `SBF-MT` = col_double(),
    `SWO-NO` = col_double(),
    `SWO-MT` = col_double(),
    `BLM-NO` = col_double(),
    `BLM-MT` = col_double(),
    `BUM-NO` = col_double(),
    `BUM-MT` = col_double(),
    `MLS-NO` = col_double(),
    `MLS-MT` = col_double(),
    `SFA-NO` = col_double(),
    `SFA-MT` = col_double(),
    `SSP-NO` = col_double(),
    `SSP-MT` = col_double(),
    `BILL-NO` = col_double(),
    `BILL-MT` = col_double(),
    `TUX-NO` = col_double(),
    `TUX-MT` = col_double(),
    `BSH-NO` = col_double(),
    `BSH-MT` = col_double(),
    `FAL-NO` = col_double(),
    `FAL-MT` = col_double(),
    `MSK-NO` = col_double(),
    `MSK-MT` = col_double(),
    `OCS-NO` = col_double(),
    `OCS-MT` = col_double(),
    `POR-NO` = col_double(),
    `POR-MT` = col_double(),
    `RSK-NO` = col_double(),
    `RSK-MT` = col_double(),
    `SPY-NO` = col_double(),
    `SPY-MT` = col_double(),
    `THR-NO` = col_double(),
    `THR-MT` = col_double(),
    `SKH-NO` = col_double(),
    `SKH-MT` = col_double(),
    `NTAD-NO` = col_double(),
    `NTAD-MT` = col_double()
  )
) %>%
  clean_names()

## PROCESSING ##################################################################

# Define a function
grid_to_coords <- function(x) {
  size <- as.numeric(str_sub(x, start = 0, end = 1))
  quadrant <- as.numeric(str_sub(x, start = 2, end = 2))
  lat <- as.numeric(str_sub(x, start = 3, end = 4))
  lon <- as.numeric(str_sub(x, start = 5, end = 7))

  lat <- ifelse(quadrant %in% c(1, 4), lat, -1 * lat)
  lon <- ifelse(quadrant %in% c(1, 2), lon, -1 * lon)

  data.frame(lat, lon)
}

# Get lat and long -------------------------------------------------------------
unique_coords <- tibble(grid = unique(c(iotc_surface$grid, iotc_longline$grid))) %>%
  mutate(coords = map(grid, grid_to_coords)) %>%
  unnest(coords)

iotc_surface_clean <- iotc_surface %>%
  filter(gear %in% c("PS"),
         !is.na(catch_units)) %>%
  left_join(unique_coords, by = "grid") %>%
  replace_na(replace = list(
    yft_ls = 0, yft_fs = 0, yft_uncl = 0,
    bet_ls = 0, bet_fs = 0, bet_uncl = 0,
    skj_ls = 0, skj_fs = 0, skj_uncl = 0,
    alb_ls = 0, alb_fs = 0, alb_uncl = 0,
    sbf_ls = 0, sbf_fs = 0, sbf_uncl = 0,
    lot_ls = 0, lot_fs = 0, lot_uncl = 0,
    frz_ls = 0, frz_fs = 0, frz_uncl = 0,
    kaw_ls = 0, kaw_fs = 0, kaw_uncl = 0,
    com_ls = 0, com_fs = 0, com_uncl = 0,
    tux_ls = 0, tux_fs = 0, tux_uncl = 0,
    fal_ls = 0, fal_fs = 0, fal_uncl = 0,
    ocs_ls = 0, ocs_fs = 0, ocs_uncl = 0,
    skh_ls = 0, skh_fs = 0, skh_uncl = 0,
    ntad_ls = 0, ntad_fs = 0, ntad_uncl = 0
  )) %>%
  mutate(yft_mt = yft_fs + yft_ls + yft_uncl,
         bet_mt = bet_fs + bet_ls + bet_uncl,
         skj_mt = skj_fs + skj_ls + skj_uncl,
         alb_mt = alb_fs + alb_ls + alb_uncl,
         sbf_mt = sbf_fs + sbf_ls + sbf_uncl,
         lot_mt = lot_fs + lot_ls + lot_uncl,
         frz_mt = frz_fs + frz_ls + frz_uncl,
         kaw_mt = kaw_fs + kaw_ls + kaw_uncl,
         com_mt = com_fs + com_ls + com_uncl,
         tux_mt = tux_fs + tux_ls + tux_uncl,
         fal_mt = fal_fs + fal_ls + fal_uncl,
         ocs_mt = ocs_fs + ocs_ls + ocs_uncl,
         skh_mt = skh_fs + skh_ls + skh_uncl,
         ntad_mt = ntad_fs + ntad_ls + ntad_uncl) %>%
  select(year, month = month_start, fleet, gear, lat, lon, effort, effort_units, contains("_mt")) %>%
  mutate(tot_mt = yft_mt + bet_mt + skj_mt + alb_mt + sbf_mt + lot_mt) %>%
  mutate(
    cpue_yft_mt = yft_mt / effort,
    cpue_bet_mt = bet_mt / effort,
    cpue_skj_mt = skj_mt / effort,
    cpue_alb_mt = alb_mt / effort,
    cpue_sbf_mt = sbf_mt / effort,
    cpue_lot_mt = lot_mt / effort,
    cpue_frz_mt = frz_mt / effort,
    cpue_kaw_mt = kaw_mt / effort,
    cpue_com_mt = com_mt / effort,
    cpue_tux_mt = tux_mt / effort,
    cpue_fal_mt = fal_mt / effort,
    cpue_ocs_mt = ocs_mt / effort,
    cpue_skh_mt = skh_mt / effort,
    cpue_ntad_mt = ntad_mt/ effort,
    cpue_tot = tot_mt / effort) %>%
  mutate(gear = "purse_seine",
         rfmo = "iotc")


iotc_longline_clean <- iotc_longline %>%
  filter(gear %in% c("LL", "FLL")) %>%
  left_join(unique_coords, by = "grid") %>%
  select(year, month = month_start, flag = fleet, gear, lat, lon, effort, effort_units, contains("_mt")) %>%
  replace_na(replace = list(
    yft_mt = 0,
    bet_mt = 0,
    skj_mt = 0,
    alb_mt = 0,
    sbf_mt = 0,
    swo_mt = 0,
    blm_mt = 0,
    bum_mt = 0,
    mls_mt = 0,
    sfa_mt = 0,
    ssp_mt = 0,
    bill_mt = 0,
    tux_mt = 0,
    bsh_mt = 0,
    fal_mt = 0,
    msk_mt = 0,
    ocs_mt = 0,
    por_mt = 0,
    rsk_mt = 0,
    spy_mt = 0,
    thr_mt = 0,
    skh_mt = 0,
    ntad_mt = 0)) %>%
  mutate(tot_mt = yft_mt + bet_mt + skj_mt + alb_mt + sbf_mt) %>%
  mutate(
    cpue_yft_mt = yft_mt / effort,
    cpue_bet_mt = bet_mt / effort,
    cpue_skj_mt = skj_mt / effort,
    cpue_alb_mt = alb_mt / effort,
    cpue_sbf_mt = sbf_mt / effort,
    cpue_swo_mt = swo_mt / effort,
    cpue_blm_mt = blm_mt / effort,
    cpue_bum_mt = bum_mt / effort,
    cpue_mls_mt = mls_mt / effort,
    cpue_sfa_mt = sfa_mt / effort,
    cpue_ssp_mt = ssp_mt / effort,
    cpue_bill_mt = bill_mt / effort,
    cpue_tux_mt = tux_mt / effort,
    cpue_bsh_mt = bsh_mt / effort,
    cpue_fal_mt = fal_mt / effort,
    cpue_msk_mt = msk_mt / effort,
    cpue_ocs_mt = ocs_mt / effort,
    cpue_por_mt = por_mt / effort,
    cpue_rsk_mt = rsk_mt / effort,
    cpue_spy_mt = spy_mt / effort,
    cpue_thr_mt = thr_mt / effort,
    cpue_skh_mt = skh_mt / effort,
    cpue_ntad_mt = ntad_mt/ effort,
    cpue_tot = tot_mt / effort) %>%
  mutate(rfmo = "iotc")

iotc_tuna <- bind_rows(iotc_surface_clean,
                       iotc_longline_clean) %>%
  mutate(gear = ifelse(gear == "LL", "longline", gear),
         effort_units = str_to_lower(effort_units)) %>%
  select(
    rfmo,
    year,
    month,
    gear,
    flag,
    lat,
    lon,
    effort,
    effort_measure = effort_units,
    contains("_mt"),
    contains("cpue_")
  )

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(
  object = iotc_tuna,
  file = here("data", "processed", "iotc_tuna_monthly_gear_flag.rds")
)
