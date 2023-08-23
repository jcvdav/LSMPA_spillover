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
# For PS data
# ------------------------------------------------------------------------------
# Field Name  | Picture  | Description
# YY N( 4 ) Year
# QTR N( 2 ) Quarter
# FLAG_ID C( 2 ) Flag – Fishing Nation (ISO 2-letter country code)
# LAT_short C( 3 ) Latitude. It represents the latitude of the
# south-west corner of 1° square for these data.
# LON_short C( 4 ) Longitude. It represents the longitude of
# the south-west corner of 1° square for these data.
# CWP_GRID N( 11 ) Coordinating Working Party No
# DAYS N( 6 ) Days fishing and searching (effort).
# SETS_UNA N( 6 ) Number of Sets (Unassociated schools).
# SETS_LOG N( 6 ) Number of Sets (Natural Log/debris).
# SETS_DFAD N( 6 ) Number of Sets (Drifting FAD).
# SETS_AFAD N( 6 ) Number of Sets (Anchored FAD).
# SETS_OTH N( 6 ) Number of Sets (Other set types combined).
# SKJ_C_UNA N( 8, 3) Skipjack catch in metric tonnes (Unassociated schools).
# YFT_C_UNA N( 8, 3) Yellowfin catch (metric tonnes) (Unassociated schools).
# BET_C_UNA N( 8, 3) Bigeye catch (metric tonnes) (Unassociated schools).
# OTH_C_UNA N( 8, 3) Other species catch (metric tonnes) (Unassociated schools).
# SKJ_C_LOG N( 8, 3) Skipjack catch in metric tonnes (Natural-Log schools).
# YFT_C_LOG N( 8, 3) Yellowfin catch (metric tonnes) (Natural-Log schools).
# BET_C_LOG N( 8, 3) Bigeye catch (metric tonnes) (Natural-Log schools).
# OTH_C_LOG N( 8, 3) Other species catch (metric tonnes) (Natural-Log schools).
# SKJ_C_DFAD N( 8, 3) Skipjack catch in metric tonnes (Drifting FAD schools).
# YFT_C_DFAD N( 8, 3) Yellowfin catch (metric tonnes) (Drifting FAD schools).
# BET_C_DFAD N( 8, 3) Bigeye catch (metric tonnes) (Drifting FAD schools).
# OTH_C_DFAD N( 8, 3) Other species catch (metric tonnes) (Drifting FAD schools).
# SKJ_C_AFAD N( 8, 3) Skipjack catch in metric tonnes (Anchored FAD schools).
# YFT_C_AFAD N( 8, 3) Yellowfin catch (metric tonnes) (Anchored FAD schools).
# BET_C_AFAD N( 8, 3) Bigeye catch (metric tonnes) (Anchored FAD schools).
# OTH_C_AFAD N( 8, 3) Other species catch (metric tonnes) (Anchored FAD schools).
# SKJ_C_OTH N( 8, 3) Skipjack catch in metric tonnes (Schools from other set types).
# YFT_C_OTH N( 8, 3) Yellowfin catch (metric tonnes) (Schools from other set types).
# BET_C_OTH N( 8, 3) Bigeye catch (metric tonnes) (Schools from other set types).
# OTH_C_OTH N( 8, 3) Other species catch (metric tonnes) (Schools from other set types).
# ------------------------------------------------------------------------------
#
# For LL data
# ------------------------------------------------------------------------------
# Field Name  | Picture  | Description
# YY N( 4 ) Year
# FLAG_ID C( 2 ) Flag codes (when this field is blank, the record is a cell representing
#                            activities of less than three vessels and so the EFFORT (hooks)
#                            and CATCH by SPECIES fields have not been provided.
# LAT5 C( 3 ) Latitude. It represents the latitude of the
# south-west corner of 5° square for these data.
# LON5 C( 4 ) Longitude. It represents the longitude of
# the south-west corner of 5° square for these data.
# HHOOKS N( 6 ) Hundreds of hooks (longline effort).
# ALB_C N( 8, 3) Albacore catch in metric tonnes.
# ALB_N N( 6 ) Albacore catch in numbers.
# YFT_C N( 8, 3) Yellowfin catch (metric tonnes)
# YFT_N N( 6 ) Yellowfin catch in numbers.
# BET_C N( 8, 3) Bigeye catch (metric tonnes).
# BET_N N( 6 ) Bigeye catch in numbers.
# MLS_C N( 8, 3) Striped Marlin catch (metric tonnes).
# MLS_N N( 6 ) Striped Marlin catch (number).
# BLM_C N( 8, 3) Black marlin catch (metric tonnes).
# BLM_N N( 6 ) Black marlin catch (number).
# BLZ_C N( 8, 3) Blue marlin catch (metric tonnes).
# BLZ_N N( 6 ) Blue marlin catch (number).
# SWO_C N( 8, 3) Swordfish catch (metric tonnes).
# SWO_N N( 6 ) Swordfish catch (number).
# OTH_C N( 8, 3) Other species catch (metric tonnes)
# OTH_N N( 6 ) The total of all other species catch (in numbers).
# ------------------------------------------------------------------------------
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  countrycode,
  janitor,
  tidyverse)

# Load data --------------------------------------------------------------------

# Country dictionaries
iso_codes <- countrycode::codelist %>%
  select(iso2c, iso3c) %>%
  distinct()

ps_tuna <-
  read_csv(
    file = here(
      "data",
      "raw",
      "RFMO_data",
      "WCPFC",
      "WCPFC_S_PUBLIC_BY_1x1_QTR_FLAG_3",
      "WCPFC_S_PUBLIC_BY_1x1_QTR_FLAG.csv"
    )
  ) %>%
  clean_names()

ll_tuna <-
  read_csv(
    file = here(
      "data",
      "raw",
      "RFMO_data",
      "WCPFC",
      "WCPFC_L_PUBLIC_BY_FLAG_YR_7",
      "WCPFC_L_PUBLIC_BY_FLAG_YR.csv"
    )
  ) %>%
  clean_names()

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
ps_tuna_clean <- ps_tuna %>%
  rename(year = yy,
         lat = lat_short,
         lon = lon_short) %>%
  mutate(
    num_sets = sets_una + sets_log + sets_dfad + sets_afad + sets_oth,
    skj_mt = skj_c_una + skj_c_log + skj_c_dfad + skj_c_afad + skj_c_oth,
    yft_mt = yft_c_una + yft_c_log + yft_c_dfad + yft_c_afad + yft_c_oth,
    bet_mt = bet_c_una + bet_c_log + bet_c_dfad + bet_c_afad + bet_c_oth,
  ) %>%
  select(year, qtr, flag_id, lat, lon, num_sets, contains("_mt")) %>%
  mutate(tot_mt = skj_mt + yft_mt + bet_mt) %>%
  filter(num_sets > 0,
         tot_mt > 0) %>%
  mutate(
    cpue_skj = skj_mt / num_sets,
    cpue_yft = yft_mt / num_sets,
    cpue_bet = bet_mt / num_sets,
    cpue_tot = tot_mt / num_sets
  ) %>%
  rename(effort = num_sets) %>%
  mutate(effort_measure = "sets",
         gear = "purse_seine",
         rfmo = "wcpfc")

ll_tuna_clean <- ll_tuna %>%
  # Since "(when this field is blank, the record is a cell representing
  # activities of less than three vessels and so the EFFORT (hooks)
  # and CATCH by SPECIES fields have not been provided."
  drop_na(flag_id) %>% #
  mutate(hooks = hhooks * 100) %>%
  rename(
    year = yy,
    lat = lat_short,
    lon = lon_short,
    alb_mt = alb_c,
    yft_mt = yft_c,
    bet_mt = bet_c
  ) %>%
  select(year, flag_id, lat, lon, hooks, contains("_mt")) %>%
  mutate(tot_mt = alb_mt + yft_mt + bet_mt) %>%
  filter(hooks > 0,
         tot_mt > 0) %>%
  mutate(
    cpue_alb = alb_mt / hooks,
    cpue_yft = yft_mt / hooks,
    cpue_bet = bet_mt / hooks,
    cpue_tot = tot_mt / hooks
  ) %>%
  rename(effort = hooks) %>%
  mutate(effort_measure = "hooks",
         gear = "longline",
         rfmo = "wcpfc")

wcpfc_tuna <-
  bind_rows(ps_tuna_clean, ll_tuna_clean) %>%
  left_join(iso_codes, by = c("flag_id" = "iso2c")) %>%
  select(
    rfmo,
    year,
    qtr,
    gear,
    flag = iso3c,
    lat,
    lon,
    effort,
    effort_measure,
    contains("_mt"),
    contains("cpue_")
  ) %>%
  mutate(
    lat_mult = ifelse(str_detect(lat, "N"), 1, -1),
    lon_mult = ifelse(str_detect(lon, "E"), 1, -1)) %>%
  mutate(
    lat = lat_mult * as.numeric(str_remove_all(lat, "[:alpha:]")),
    lon = lon_mult * as.numeric(str_remove_all(lon, "[:alpha:]"))
  ) %>%
  mutate(
    # WCPFC reports "the latitude of the south-west corner"
    lat = ifelse(gear == "longline", lat + 2.5, lat + 0.5),
    lon = ifelse(gear == "longline", lon + 2.5, lon + 0.5)
  ) %>%
  select(-lat_mult, -lon_mult)

# Check species we have
check_mt(wcpfc_tuna)

# Check right combinations of effort and gear
check_effort_gear(wcpfc_tuna)

# Check 0s and NAs
test(wcpfc_tuna)


## EXPORT ######################################################################
# X ----------------------------------------------------------------------------
saveRDS(object = wcpfc_tuna,
        file = here("data", "processed", "rfmo_wcpfc_tuna_quarterly_gear_flag.rds"))
