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
  kableExtra,
  tidyverse
)

# Source custom functions ------------------------------------------------------
source(here("scripts/00_set_up.R"))

# Load data --------------------------------------------------------------------
# ENSO index to add
oni <- readRDS(file = here("data", "raw", "oni", "annual_oni.rds"))
oni_qtr <- readRDS(file = here("data", "raw", "oni", "quarterly_oni.rds"))

# Annual data
# Filter to retain only daya within 200 nautical miles of an MPA,
# and within 10 years of implementation
annual_panel_raw <- readRDS(here("data", "processed", "annual_panel.rds")) %>%
  filter(between(event, -10, 10),
         dist <= 200)
# Keep a separate version with all post-implementation data
annual_panel_full_post_raw <-
  readRDS(here("data", "processed", "annual_panel.rds")) %>%
  filter(event >= -10,
         dist <= 200)

# Quarterly data
qtr_panel_raw <- readRDS(file = here("data", "processed", "rfmo_all_qtr_gear_flag.rds")) %>%
  filter(between(event, -10, 10),
         dist <= 200)

## PROCESSING ##################################################################

# Now, find MPA-gear combinations that conform (or dont) to a BACI design ------
# Count the number of pixels that appear in each mpa-pre-dist combination
# For purse seine
ns_per_period_ps <- annual_panel_raw %>%
  filter((gear == "purse_seine" & !is.na(near_100))) %>% # gear must be PS
  group_by(wdpaid, name, gear, post, near_100) %>%
  summarize(n_pixels = n_distinct(id),
            year = min(year_enforced)) %>%
  group_by(wdpaid, name, gear, year) %>%
  summarize(n_baci = n(),
            n_pixels = min(n_pixels)) %>%
  ungroup()

# For longline
ns_per_period_ll <- annual_panel_raw %>%
  filter((gear == "longline" & !is.na(near_100))) %>% # Data must be longline
  group_by(wdpaid, name, gear, post, near_100) %>%
  summarize(n_pixels = n_distinct(id),
            year = min(year_enforced)) %>%
  group_by(wdpaid, name, gear, year) %>%
  summarize(n_baci = n(),
            n_pixels = min(n_pixels)) %>%
  ungroup()

# Combine them
ns_per_period <- bind_rows(ns_per_period_ps,
                           ns_per_period_ll)

# These don't conform
# They have less than three "B, A, C, I" groups
# Have less than four pixels per group
# Were implemented on or after 2020
not_enough <- ns_per_period %>%
  filter(n_baci <= 3 | n_pixels <= 4 | year >= 2020) %>%
  select(wdpaid, name, gear) %>%
  distinct()

# These do
# They have 4 BACI groups
# They have more than 5 pixels per group
# They were implemented on or before 2019
enough <- filter(ns_per_period, n_baci == 4, n_pixels >= 5, year <= 2019) %>%
  select(wdpaid, name, gear) %>%
  distinct()

# Extract the combinations
keep <- enough %>%
  select(wdpaid, gear) %>%
  distinct() %>%
  mutate(keep = paste(wdpaid, gear)) %>%
  pull(keep)

# Now find the most relevant gear for each MPA ---------------------------------
# First, find the relative contribution of each gear to all catch around 200 nm
# of each MPA. # This doesn't mean we have enough for a BACI design, so we will
# filter for that too.
gear_with_most_landings_by_mpa <- annual_panel_raw %>%
  filter(dist <= 200) %>%
  group_by(wdpaid, name, gear) %>%
  summarize(tot_mt = sum(tot_mt),
            n = n()) %>%
  ungroup() %>%
  group_by(wdpaid, name) %>%
  mutate(pct_mt = tot_mt / sum(tot_mt) * 100) %>%
  arrange(pct_mt) %>%
  ungroup() %>%
  mutate(
    enough_for_baci = paste(wdpaid, gear) %in% keep,
    short_name = case_when(wdpaid == "11753" ~ "Galápagos",
                           wdpaid == "309888" ~ "PIPA",
                           wdpaid == "555512151" ~ "Chagos",
                           wdpaid == "555543712" ~ "Motu Motiro Hiva",
                           wdpaid == "555556875_A" ~ "Coral Sea",
                           wdpaid == "555624169" ~ "Nazca-Desventuradas",
                           wdpaid == "555705568" ~ "Niue Moana Mahu",
                           wdpaid == "220201" ~ "Papahānaumokuākea",
                           wdpaid == "555624172" ~ "Pitcairn",
                           T ~ name)) %>%
  select(short_name, wdpaid, gear, tot_mt, pct_mt, n, enough_for_baci) %>%
  arrange(enough_for_baci, n, pct_mt, short_name)

# # Build the panels -----------------------------------------------------------
# Panel with ALL the data (PS is 0-200, LL is 0-200)
annual_panel <- annual_panel_raw %>%
  mutate(near = near_100,
         nice_gear = case_when(gear == "purse_seine" ~ "PS",
                               gear == "longline" ~ "LL"),
         nice_gear = fct_relevel(nice_gear, "PS", "LL")) %>%
  drop_na(near) %>%
  filter(cpue_tot > 0) %>%
  replace_na(replace = list(flag = "Missing")) %>%
  left_join(oni, by = "year")

# Panel with relevant MPA-gear combinations determined by full data,
most_relevant_panel <- annual_panel %>%
  inner_join(gear_with_most_landings_by_mpa %>%
               filter(pct_mt > 5,
                      enough_for_baci) %>%
               select(short_name, wdpaid, gear),
             by = join_by(gear, wdpaid))

# Panels for sensitivity analysis ----------------------------------------------
# This panel is to test for distance as continuous with relevant MPA-gear combinations determined by the full data set
most_relevant_panel_multiple_distances <- readRDS(here("data", "processed", "annual_panel.rds")) %>%
  filter(between(event, -10, 10)) %>%
  mutate(nice_gear = case_when(gear == "purse_seine" ~ "PS",
                               gear == "longline" ~ "LL"),
         nice_gear = fct_relevel(nice_gear, "PS", "LL")) %>%
  filter(cpue_tot > 0) %>%
  replace_na(replace = list(flag = "Missing")) %>%
  inner_join(gear_with_most_landings_by_mpa %>%
               filter(pct_mt > 5,
                      enough_for_baci) %>%
               select(short_name, wdpaid, gear),
             by = join_by(gear, wdpaid))

# This panel is to test with quarterly data
most_relevant_qtr_panel <- qtr_panel_raw  %>%
  mutate(near = near_100,
         nice_gear = case_when(gear == "purse_seine" ~ "PS",
                               gear == "longline" ~ "LL"),
         nice_gear = fct_relevel(nice_gear, "PS", "LL")) %>%
  drop_na(near) %>%
  filter(cpue_tot > 0) %>%
  replace_na(replace = list(flag = "Missing")) %>%
  inner_join(gear_with_most_landings_by_mpa %>%
               filter(pct_mt > 5,
                      enough_for_baci) %>%
               select(short_name, wdpaid, gear),
             by = join_by(gear, wdpaid)) %>%
  left_join(oni_qtr, by = c("year", "qtr"))

# And this panel is to test with all post-implementation data (for all MPAs and for the subset)
# For all data
annual_panel_full_post <- annual_panel_full_post_raw %>%
  mutate(near = near_100,
         nice_gear = case_when(gear == "purse_seine" ~ "PS",
                               gear == "longline" ~ "LL"),
         nice_gear = fct_relevel(nice_gear, "PS", "LL")) %>%
  drop_na(near) %>%
  filter(cpue_tot > 0) %>%
  replace_na(replace = list(flag = "Missing")) %>%
  left_join(oni, by = "year")

# And for relevant MPA-gear combinations determined by full data,
most_relevant_panel_full_post <- annual_panel_full_post %>%
inner_join(gear_with_most_landings_by_mpa %>%
             filter(pct_mt > 5,
                    enough_for_baci) %>%
             select(short_name, wdpaid, gear),
           by = join_by(gear, wdpaid))


## EXPORT ######################################################################
# Main panels ------------------------------------------------------------------
saveRDS(object = annual_panel,
        file = here("data", "processed", "annual_full_estimation_panel.rds"))

saveRDS(object = most_relevant_panel,
        file = here("data", "processed", "annual_relevant_mpa_gears_estimation_panel.rds"))

# Robustness test panels -------------------------------------------------------
saveRDS(object = most_relevant_panel_multiple_distances,
        file = here("data", "processed", "annual_relevant_mpa_gears_and_distances_sensitivity_estimation_panel.rds"))

saveRDS(object = most_relevant_qtr_panel,
        file = here("data", "processed", "qtr_relevant_mpa_gears_and_distances_sensitivity_estimation_panel.rds"))

saveRDS(object = annual_panel_full_post,
        file = here("data", "processed", "annual_full_estimation_panel_full_post.rds"))

saveRDS(object = most_relevant_panel_full_post,
        file = here("data", "processed", "annual_relevant_mpa_gears_estimation_panel_full_post.rds"))

# Table for SM
# NOTE THAT PAPAHANAOMOKUAKEA IS MANUALLY MOVED TO NON-BACI COMPLIANCE DUE TO OVERLAPS WITH JOHNSTON, WHICH ARE NOT CONSIDERED ABOVE
# JOHNSTON IS NOT INCLUDED THUS FAR, BUT WILL BE ADDED MANUALLY IN THE TEXT
gear_with_most_landings_by_mpa %>%
  mutate(enough_for_baci = ifelse(wdpaid == "220201", FALSE, enough_for_baci)) %>%
  arrange(-enough_for_baci, desc(gear), short_name) %>%
  mutate(gear = str_to_sentence(str_replace(gear, "_", " ")),
         enough_for_baci = ifelse(enough_for_baci, "Yes", "No")) %>%
  select(enough_for_baci, everything()) %>%
  kable(col.names = c("BACI", "MPA", "WDPAID", "Gear", "Catch (mt)", "% of total catch", "N. Obs."),
        caption = "\\label{tab:relevant_mpa_gear_combinations}\\textbf{Before-After-Control-Impact (BACI) compliance indicator, MPA and fishing gear combinations, contribution of each gear's catch to total catch around each MPA, and total number of observations.} The column WDPA ID shows the unique shapefile identifier from the World Database on Protected Areas.",
        label = "relevant_mpa_gear_combinations",
        digits = 4,
        booktabs = T,
        linesep = "",
        format = "latex") %>%
  kable_styling() %>%
  collapse_rows(columns = c(1, 3, 7)) %>%
  save_kable(here("results", "tab", "relevant_mpa_gear_combinations.tex"))
