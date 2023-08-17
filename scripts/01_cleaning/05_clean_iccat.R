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
  janitor,
  tidyverse
)

# Load data --------------------------------------------------------------------
# Read the Microsoft access database
con <- readRDS(file = here("data", "raw", "RFMO_data", "ICCAT", "ICCAT_database.rds"))

# Extract the data
data <- con$t2ce %>%
  clean_names()

# Extract flag codes
fleet_flag <- con$Flags %>%
  clean_names() %>%
  mutate(flag = str_remove(flag_code, pattern = "EU-|UK-|FR-")) %>%
  select(fleet_id, flag_code, flag) %>%
  distinct()

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------

iccat_tuna <- data %>%
  filter(gear_code %in% c(
    "LL",     #Longline
    "LL-B",     #Longline: Bottom or Deep longliners
    # "LL-Shrk",     #Longline: Targetting sharks (BSH & SMA)
    "LL-surf",     #Longline: Surface
    "LLALB",     #Longline: Targetting ALB
    "LLAMS",     #Longline: American style
    "LLBFT",     #Longline: Targetting BFT
    "LLJAP",     #Longline: Japanese type (Spain)
    "LLMB",     #Longline: With mother boat
    "LLMESO",     #Longline: Mesopelagic
    "LLPB",     #Longline: "Stone-ball" (Spain)
    # "LLSWO",     #Longline: Targetting SWO
    "PS",     #Purse seine
    "PSD",     #Purse seine: Double-boats
    "PSFB",     #Purse seine: Catching large fish
    # "PSFS",     #Purse seine: Catching small fish
    "PSG",     #Purse seine: Large scale (over 200 MT capacity)
    "PSLB",     #Purse seine: Using live bait
    "PSM",     #Purse seine: Medium scale (between 50 and 200 MT capacity)
    "PSS"     #Purse seine: Small scale (less than 50 MT capacity)
  )) %>%
  filter(d_set_type_id == ".w",
         catch_unit == "kg",
         eff1 > 0,
         time_period_id <= 12,
         !(eff1 == 0 & eff1type == "-none-"),
         square_type_code %in% c("1x1", "5x5")) %>%
  mutate(
    bft_mt = bft / 1e3,
    alb_mt = alb / 1e3,
    yft_mt = yft / 1e3,
    bet_mt = bet / 1e3,
    skj_mt = skj / 1e3,
    tot_mt = bft_mt + alb_mt + yft_mt + bet_mt + skj_mt) %>%
  filter(tot_mt > 0) %>%
  mutate(lat_mult = ifelse(quad_id %in% c(1, 4), 1, -1),
         lon_mult = ifelse(quad_id %in% c(1, 2), 1, -1),
         centering = ifelse(square_type_code == "1x1", 0.5, 2.5),
         lat = (lat * lat_mult) + centering,
         lon = (lon * lon_mult) + centering) %>%
  select(-contains("mult")) %>%
  left_join(fleet_flag, by = "fleet_id") %>%
  group_by(year_c, lat, lon, time_period_id, flag, gear_grp_code, eff1type) %>%
  summarize(effort = sum(eff1, na.rm = T),
            bft_mt = sum(bft_mt, na.rm = T),
            alb_mt = sum(alb_mt, na.rm = T),
            yft_mt = sum(yft_mt, na.rm = T),
            bet_mt = sum(bet_mt, na.rm = T),
            skj_mt = sum(skj_mt, na.rm = T),
            tot_mt = sum(tot_mt, na.rm = T)) %>%
  ungroup() %>%
  mutate(cpue_bft = bft_mt / effort,
         cpue_alb = alb_mt / effort,
         cpue_yft = yft_mt / effort,
         cpue_bet = bet_mt / effort,
         cpue_skj = skj_mt / effort,
         cpue_tot = tot_mt / effort,
         rfmo = "iccat") %>%
  mutate(gear_grp_code = case_when(
    gear_grp_code == "LL" ~ "longline",
    gear_grp_code == "PS" ~ "purse_seine"),
    eff1type = case_when(eff1type == "NO.HOOKS" ~ "hooks",
                         eff1type == "NO.SETS" ~ "sets",
                         eff1type == "NO.TRIPS" ~ "trips",
                         T ~ eff1type),
    eff1type = str_to_lower(eff1type)) %>%
  select(rfmo,
         year = year_c,
         month = time_period_id,
         gear = gear_grp_code,
         flag,
         lat,
         lon,
         effort,
         effort_measure = eff1type,
         contains("_mt"),
         contains("cpue_")) %>%
  filter(!(effort_measure == "hooks" & effort < 600))



## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
# iccat_tuna %>%
#   group_by(gear) %>%
#   mutate(norm_cpue_tot = (cpue_tot - mean(cpue_tot, na.rm = T)) / sd(cpue_tot, na.rm = T)) %>%
#   ungroup() %>%
#   ggplot(aes(x = year, y = norm_cpue_tot, color = gear)) +
#   geom_smooth()

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(
  object = iccat_tuna,
  file = here("data", "processed", "iccat_tuna_monthly_gear_flag.rds")
)


