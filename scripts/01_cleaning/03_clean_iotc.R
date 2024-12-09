################################################################################
# Clean IOTC data
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
#
# Description
#
# From website: https://iotc.org/data/datasets
# This dataset corresponds to the latest version of the ‘best scientific
# estimates’ of retained catches in live weight equivalent for the 16 IOTC
# species, aggregated by year, IOTC statistical area, and reporting country
# flag, and fully disaggregated by species and fishing gear. Data are reported
# by calendar year and extend back to the 1950s when industrial longlining
# started in the Indian Ocean.
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  janitor,
  tidyverse
)

# Source custom funcions -------------------------------------------------------
source(here("scripts/00_set_up.R"))

# Load data --------------------------------------------------------------------
iotc_surface <- read_csv(
  here(
    "data",
    "raw",
    "RFMO_data",
    "IOTC",
    "IOTC-DATASETS-2023-04-24-CE-ALL_1950-2021",
    "IOTC-DATASETS-2023-04-24-CE-Surface_1950-2021.csv"
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
  clean_names() %>%
  rename(grid_code = grid)

iotc_longline <- read_csv(
  here(
    "data",
    "raw",
    "RFMO_data",
    "IOTC",
    "IOTC-DATASETS-2023-04-24-CE-ALL_1950-2021",
    "IOTC-DATASETS-2023-04-24-CE-Longline_1950-2021.csv"
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
  clean_names() %>%
  rename(grid_code = grid)

## PROCESSING ##################################################################

# Define a function to make grid codes into coordinates
grid_to_coords <- function(x) {
  # Extract pieces of the grid codes
  # Size of the rectangle (5 indicates 1 degree and 6 indicates 5 degree)
  size <- str_sub(x, start = 0, end = 1)
  size <- case_when(size == 5 ~ 1,
                    size == 6 ~ 5)
  # Quadrant indicate shemispere
  quadrant <- as.numeric(str_sub(x, start = 2, end = 2))

  # Now extract the lat and long
  lat <- as.numeric(str_sub(x, start = 3, end = 4))
  lon <- as.numeric(str_sub(x, start = 5, end = 7))

  # Position in the hemisphere
  lat <- ifelse(quadrant %in% c(1, 4), lat, -1 * lat)
  lon <- ifelse(quadrant %in% c(1, 2), lon, -1 * lon)

  # Adjust the coordinate to be in the center of the cell
  # The IOTC grids are assumed to occur in the Indicate the corner of the square
  # closest to 0o latitude and 0o longitude, which is annoying...
  lat <- case_when(quadrant == 1 ~ lat + (size / 2),
                   quadrant == 2 ~ lat - (size / 2),
                   quadrant == 3 ~ lat - (size / 2),
                   quadrant == 4 ~ lat + (size / 2))

  lon <- case_when(quadrant == 1 ~ lon + (size / 2),
                   quadrant == 2 ~ lon + (size / 2),
                   quadrant == 3 ~ lon - (size / 2),
                   quadrant == 4 ~ lon - (size / 2))

  grid <- paste0(size, "x", size)


  return(data.frame(lat, lon, grid))
}


# Get lat and long -------------------------------------------------------------
unique_coords <- tibble(grid_code = unique(c(iotc_surface$grid_code,
                                             iotc_longline$grid_code))) %>%
  filter(str_sub(grid_code, 1, 1) %in% c("5", "6")) %>%
  mutate(coords = map(grid_code, grid_to_coords)) %>%
  unnest(coords)

iotc_surface_clean <- iotc_surface %>%
  filter(gear %in% c("PS", "PSS"),
         !is.na(catch_units),
         quality_code >= 2,
         effort_units == "FHOURS") %>%
  left_join(unique_coords, by = "grid_code") %>%
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
         sbf_mt = sbf_fs + sbf_ls + sbf_uncl
         ) %>%
  select(year, month = month_start, flag = fleet, gear, grid, lat, lon, effort, effort_units,
         # Ordere tuna spp by importance to catch, also ignoring SBF and SKJ that account for ~1%
         skj_mt, yft_mt, bet_mt) %>%
  mutate(tot_mt = skj_mt + yft_mt + bet_mt,
         gear = "purse_seine") %>%
  filter(tot_mt > 0,
         effort > 0) %>%
  group_by(year, month, gear, flag, grid, lat, lon, effort_units) %>%
  summarize(skj_mt = sum(skj_mt),
            yft_mt = sum(yft_mt),
            bet_mt = sum(bet_mt),
            tot_mt = sum(tot_mt),
            effort = sum(effort)) %>%
  ungroup() %>%
  mutate(
    cpue_skj = skj_mt / effort,
    cpue_yft = yft_mt / effort,
    cpue_bet = bet_mt / effort,
    cpue_tot = tot_mt / effort) %>%
  mutate(rfmo = "iotc")


iotc_longline_clean <- iotc_longline %>%
  filter(gear %in% c("LL", "FLL"),
         effort_units == "HOOKS",
         quality_code >= 2) %>%
  left_join(unique_coords, by = "grid_code") %>%
  select(year, month = month_start, flag = fleet, gear, grid, lat, lon, effort, effort_units,
         # Ignoring SKJ that account for ~1%
         bet_mt, alb_mt, yft_mt, sbf_mt) %>%
  replace_na(replace = list(
    bet_mt = 0,
    alb_mt = 0,
    yft_mt = 0,
    sbf_mt = 0)) %>%
  mutate(tot_mt = bet_mt + alb_mt + yft_mt + sbf_mt,
         gear = "longline") %>%
  filter(tot_mt > 0,
         effort > 0) %>%
  # We filter records where they indicate that less than 200 hooks were used because these are unlikely
  filter(!effort < 200) %>%
  group_by(year, month, gear, flag, grid, lat, lon, effort_units) %>%
  summarize(bet_mt = sum(bet_mt),
            alb_mt = sum(alb_mt),
            yft_mt = sum(yft_mt),
            sbf_mt = sum(sbf_mt),
            tot_mt = sum(tot_mt),
            effort = sum(effort)) %>%
  ungroup() %>%
  mutate(
    cpue_bet = bet_mt / effort,
    cpue_alb = alb_mt / effort,
    cpue_yft = yft_mt / effort,
    cpue_sbf = sbf_mt / effort,
    cpue_tot = tot_mt / effort) %>%
  mutate(rfmo = "iotc")

iotc_tuna <- bind_rows(iotc_surface_clean,
                       iotc_longline_clean) %>%
  mutate(effort_units = str_to_lower(effort_units),
         flag = str_sub(flag, -3, -1)) %>%
  select(
    rfmo,
    year,
    month,
    gear,
    flag,
    grid,
    lat,
    lon,
    effort,
    effort_measure = effort_units,
    contains("_mt"),
    contains("cpue_")
  )


check_mt(iotc_tuna, cutoff = 2000)

check_effort_gear(iotc_tuna)

test(iotc_tuna)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(
  object = iotc_tuna,
  file = here("data", "processed", "rfmo_iotc_tuna_monthly_gear_flag.rds")
)
