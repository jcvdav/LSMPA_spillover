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
distance_grid <- read_rds(file = here("data", "processed", "distance_grid.rds"))
rfmo_data <- readRDS(here("data", "processed", "quarterly_all_rfmos.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
combined <- rfmo_data %>%
  left_join(distance_grid, by = c("lon", "lat"))

# I define "main players" as those with the most EFFORT since MPA implemented
main_players <- combined %>%
  filter(year > year_enforced) %>%
  filter(cpue_tot > 0) %>%
  drop_na(flag) %>%
  filter(gear %in% c("purse_seine", "longline")) %>%
  group_by(rfmo, name, gear, flag) %>%
  summarize(effort = sum(effort)) %>%
  ungroup() %>%
  group_by(rfmo, name, gear) %>%
  filter(effort == max(effort)) %>%
  ungroup() %>%
  select(rfmo, name, gear, flag)

# Keeping only the "main players", retains about 11% of the data (77488 / 653522)
restricted <- combined %>%
  inner_join(main_players, by = c("rfmo", "name", "gear", "flag")) %>%
  mutate(bin = factor(ceiling(dist / 100)))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = restricted,
        file = here("data", "processed", "restricted_quarterly_panel.rds"))
saveRDS(object = combined,
        file = here("data", "processed", "quarterly_panel.rds"))
