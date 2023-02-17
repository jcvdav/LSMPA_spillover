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
      "WCPFC_L_PUBLIC_BY_FLAG_QTR_8",
      "WCPFC_L_PUBLIC_BY_FLAG_QTR.csv"
    )
  ) %>%
  clean_names()


pl_tuna <-
  read_csv(
    file = here(
      "data",
      "raw",
      "RFMO_data",
      "WCPFC",
      "WCPFC_P_PUBLIC_BY_1x1_QTR_FLAG_1",
      "WCPFC_P_PUBLIC_BY_1x1_QTR_FLAG.csv"
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
    oth_mt = oth_c_una + oth_c_log + oth_c_dfad + oth_c_afad + oth_c_oth
  ) %>%
  select(year, qtr, flag_id, lat, lon, num_sets, contains("_mt")) %>%
  mutate(tot_mt = skj_mt + yft_mt + bet_mt + oth_mt) %>%
  mutate(
    cpue_skj = skj_mt / num_sets,
    cpue_yft = yft_mt / num_sets,
    cpue_bet = bet_mt / num_sets,
    cpue_oth = oth_mt / num_sets,
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
    bet_mt = bet_c,
    mls_mt = mls_c,
    blm_mt = blm_c,
    bum_mt = bum_c,
    swo_mt = swo_c,
    oth_mt = oth_c
  ) %>%
  select(year, qtr, flag_id, lat, lon, hooks, contains("_mt")) %>%
  rename() %>%
  mutate(tot_mt = alb_mt + yft_mt + bet_mt + mls_mt + blm_mt + bum_mt + swo_mt + oth_mt) %>%
  mutate(
    cpue_alb = alb_mt / hooks,
    cpue_yft = yft_mt / hooks,
    cpue_bet = bet_mt / hooks,
    cpue_mls = mls_mt / hooks,
    cpue_blm = blm_mt / hooks,
    cpue_bum = bum_mt / hooks,
    cpue_swo = swo_mt / hooks,
    cpue_oth = oth_mt / hooks,
    cpue_tot = tot_mt / hooks
  ) %>%
  rename(effort = hooks) %>%
  mutate(effort_measure = "hooks",
         gear = "longline",
         rfmo = "wcpfc")

pl_tuna_clean <- pl_tuna %>%
  rename(
    year = yy,
    lat = lat_short,
    lon = lon_short,
    skj_mt = skj_c,
    yft_mt = yft_c,
    bet_mt = bet_c,
    oth_mt = oth_c) %>%
  mutate(tot_mt = skj_mt + yft_mt + bet_mt + oth_mt) %>%
  mutate(
    cpue_skj = skj_mt / days,
    cpue_yft = yft_mt / days,
    cpue_bet = bet_mt / days,
    cpue_oth = oth_mt / days,
    cpue_tot = tot_mt / days
  ) %>%
  rename(effort = days) %>%
  mutate(effort_measure = "days",
         gear = "pole_and_line",
         rfmo = "wcpfc")

wcpfc_tuna <-
  bind_rows(ps_tuna_clean, ll_tuna_clean, pl_tuna_clean) %>%
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
    lat = as.numeric(str_remove_all(lat, "[:alpha:]")),
    lon = as.numeric(str_remove_all(lon, "[:alpha:]")),
    flag = ifelse(is.na(flag), "Other", flag)
  )

## VISUALIZE ###################################################################

# Quick time series to check ---------------------------------------------------
wcpfc_tuna %>%
  group_by(gear) %>%
  mutate(norm_cpue_tot = (cpue_tot - mean(cpue_tot, na.rm = T)) / sd(cpue_tot, na.rm = T)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = norm_cpue_tot, color = gear)) +
  geom_smooth()


## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = wcpfc_tuna,
        file = here("data", "processed", "wcpfc_tuna_quarterly_gear_flag.rds"))
