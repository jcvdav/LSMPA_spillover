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

test(wcpfc_tuna)


## EXPORT ######################################################################
# X ----------------------------------------------------------------------------
saveRDS(object = wcpfc_tuna,
        file = here("data", "processed", "rfmo_wcpfc_tuna_quarterly_gear_flag.rds"))
