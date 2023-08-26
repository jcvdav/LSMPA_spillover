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

# Source custom functions ------------------------------------------------------
source(here("scripts/00_set_up.R"))

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
  filter(d_set_type_id == ".w", # Keep data reported as weight
         catch_unit == "kg",    #
         eff1 > 0,
         quad_id > 0,
         eff1type %in% c("NO.HOOKS", "NO.SETS") | eff2type %in% c("NO.HOOKS", "NO.SETS"),
         # time_period_id <= 12, #This would keep only data that are reported monthly, but we're using it all to keep up to annual
         !(eff1 == 0 & eff1type == "-none-"),
         square_type_code %in% c("1x1", "5x5")) %>%
  mutate(effort_measure = case_when(gear_grp_code == "LL" & eff1type == "NO.HOOKS" ~ eff1type,
                                  gear_grp_code == "LL" & eff2type == "NO.HOOKS" ~ eff2type,
                                  gear_grp_code == "PS" & eff1type == "NO.SETS" ~ eff1type,
                                  gear_grp_code == "PS" & eff2type == "NO.SETS" ~ eff2type,
                                  T ~ NA_character_),
         effort = case_when(gear_grp_code == "LL" & eff1type == "NO.HOOKS" ~ eff1,
                            gear_grp_code == "LL" & eff2type == "NO.HOOKS" ~ eff2,
                            gear_grp_code == "PS" & eff1type == "NO.SETS" ~ eff1,
                            gear_grp_code == "PS" & eff2type == "NO.SETS" ~ eff2,
                            T ~ NA)) %>%
  # Drops 27 observations that had only been kept because "Sets" appeard as
  # effort unit on longline data, but that are not available in hooks
  drop_na(effort_measure) %>%
  drop_na(effort) %>%
  filter(effort > 0) %>%
  select(-c(eff1, eff1type, eff2, eff2type)) %>% mutate(
    # Data are reported in Kg so we convert to MT
    bft_mt = bft / 1e3,
    alb_mt = alb / 1e3,
    yft_mt = yft / 1e3,
    bet_mt = bet / 1e3,
    skj_mt = skj / 1e3) %>%
  #This PDF has information on how to handle ICCAT's geographical data
  # https://www.iccat.int/Data/ICCAT_maps.pdf
  # It also uses "of corner of the Rectangle closest to the equator"
  mutate(lat_mult = ifelse(quad_id %in% c(1, 4), 1, -1),
         lon_mult = ifelse(quad_id %in% c(1, 2), 1, -1),
         size = as.numeric(str_extract(square_type_code, "[:digit:]")),
         # Assign sign to proper hemisphere
         lat = (lat * lat_mult),
         lon = (lon * lon_mult),
         # Move the coordinate to the center of the grid
         lat = case_when(quad_id == 1 ~ lat + (size / 2),
                         quad_id == 2 ~ lat - (size / 2),
                         quad_id == 3 ~ lat - (size / 2),
                         quad_id == 4 ~ lat + (size / 2)),
         lon = case_when(quad_id == 1 ~ lon + (size / 2),
                         quad_id == 2 ~ lon + (size / 2),
                         quad_id == 3 ~ lon - (size / 2),
                         quad_id == 4 ~ lon - (size / 2))) %>%
  select(-contains("mult")) %>%
  left_join(fleet_flag, by = "fleet_id")

iccat_ps_clean <- iccat_tuna %>%
  filter(gear_grp_code == "PS") %>%
  select(year = year_c,
         gear = gear_grp_code,
         flag, grid = square_type_code,
         lat, lon, effort, effort_measure,
         # Ignoring albacore because its les than 0.2%
         skj_mt, yft_mt, bet_mt, bft_mt) %>%
  mutate(tot_mt = skj_mt + yft_mt + bet_mt + bft_mt) %>%
  group_by(year, lat, lon, flag, grid, gear, effort_measure) %>%
  summarize(skj_mt = sum(skj_mt, na.rm = T),
            yft_mt = sum(yft_mt, na.rm = T),
            bet_mt = sum(bet_mt, na.rm = T),
            bft_mt = sum(bft_mt, na.rm = T),
            effort = sum(effort, na.rm = T),
            tot_mt = sum(tot_mt, na.rm = T)) %>%
  ungroup() %>%
  filter(tot_mt > 0) %>%
  mutate(cpue_skj = skj_mt / effort,
         cpue_yft = yft_mt / effort,
         cpue_bet = bet_mt / effort,
         cpue_bft = bft_mt / effort,
         cpue_tot = tot_mt / effort,
         rfmo = "iccat") %>%
  mutate(gear = "purse_seine",
         effort_measure = "sets")

iccat_ll_clean <- iccat_tuna %>%
  filter(gear_grp_code == "LL") %>%
  select(year = year_c,
         gear = gear_grp_code,
         flag,
         grid = square_type_code,
         lat, lon, effort, effort_measure,
         # Ignoring skipjack because its les than 0.5%
         bet_mt, alb_mt, yft_mt, bft_mt) %>%
  mutate(tot_mt = bet_mt + alb_mt + yft_mt + bft_mt) %>%
  group_by(year, lat, lon, flag, grid, gear, effort_measure) %>%
  summarize(
    bet_mt = sum(bet_mt, na.rm = T),
    alb_mt = sum(alb_mt, na.rm = T),
    yft_mt = sum(yft_mt, na.rm = T),
    bft_mt = sum(bft_mt, na.rm = T),
    effort = sum(effort, na.rm = T),
    tot_mt = sum(tot_mt, na.rm = T)) %>%
  ungroup() %>%
  filter(tot_mt > 0) %>%
  mutate(
    cpue_bet = bet_mt / effort,
    cpue_alb = alb_mt / effort,
    cpue_yft = yft_mt / effort,
    cpue_bft = bft_mt / effort,
    cpue_tot = tot_mt / effort,
    rfmo = "iccat") %>%
  mutate(gear = "longline",
         effort_measure = "hooks") %>%
  filter(!(effort_measure == "hooks" & effort < 600))


iccat_tuna_clean <- bind_rows(iccat_ps_clean, iccat_ll_clean) %>%
  select(rfmo,
         year,
         gear,
         flag,
         grid,
         lat,
         lon,
         effort,
         effort_measure,
         contains("_mt"),
         contains("cpue_"))

## EXPORT ######################################################################
check_mt(iccat_tuna_clean)

check_effort_gear(iccat_tuna_clean)

test(iccat_tuna_clean)

# X ----------------------------------------------------------------------------
saveRDS(
  object = iccat_tuna_clean,
  file = here("data", "processed", "rfmo_iccat_tuna_annual_gear_flag.rds")
)


