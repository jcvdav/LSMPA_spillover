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

# Load data --------------------------------------------------------------------
annual_panel_raw <- readRDS(here("data", "processed", "annual_panel.rds")) %>%
  filter(between(event, -10, 10))

## PROCESSING ##################################################################

# Now, find MPA-gear combinations that conform (or dont) to a BACI design ------
# Count the number of pixels that apear in each mpa-pre-dist combination
# For purse seine
ns_per_period_ps <- annual_panel_raw %>%
  filter((gear == "purse_seine" & !is.na(near_100))) %>%
  group_by(wdpaid, name, gear, post, near_100) %>%
  summarize(n_pixels = n_distinct(id),
            year = min(year_enforced)) %>%
  group_by(wdpaid, name, gear, year) %>%
  summarize(n_baci = n(),
            n_pixels = min(n_pixels)) %>%
  ungroup()

# For longline
ns_per_period_ll <- annual_panel_raw %>%
  filter((gear == "longline" & !is.na(near_300))) %>%
  group_by(wdpaid, name, gear, post, near_300) %>%
  summarize(n_pixels = n_distinct(id),
            year = min(year_enforced)) %>%
  group_by(wdpaid, name, gear, year) %>%
  summarize(n_baci = n(),
            n_pixels = min(n_pixels)) %>%
  ungroup()

# Combine them
ns_per_period <- bind_rows(ns_per_period_ps,
                           ns_per_period_ll)

# These dont confiorm
not_enough <- ns_per_period %>%
  filter(n_baci <= 3 | n_pixels <= 4 | year >= 2020) %>%
  select(wdpaid, name, gear) %>%
  distinct()

# These do
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
# First, find the relative contributiuon of each gear to all catch around 600 nm
# of each MPA. # This doesn't mean we have enough for a BACI design, so we will
# filter for that too.
gear_with_most_landings_by_mpa <- annual_panel_raw %>%
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
# Panel with ALL the data
annual_panel <- annual_panel_raw %>%
  mutate(near = ifelse(gear == "purse_seine", near_100, near_300),
         nice_gear = case_when(gear == "purse_seine" ~ "PS",
                               gear == "longline" ~ "LL"),
         nice_gear = fct_relevel(nice_gear, "PS", "LL")) %>%
  drop_na(near) %>%
  filter(cpue_tot > 0) %>%
  replace_na(replace = list(flag = "Missing"))

# Panel with relevant MPA-gear combinations
most_relevant_panel <- annual_panel %>%
  inner_join(gear_with_most_landings_by_mpa %>%
               filter(pct_mt > 5,
                      enough_for_baci) %>%
               select(short_name, wdpaid, gear),
             by = join_by(gear, wdpaid))

## EXPORT ######################################################################
saveRDS(object = annual_panel,
        file = here("data", "processed", "annual_full_estimation_panel.rds"))

saveRDS(object = most_relevant_panel,
        file = here("data", "processed", "annual_relevant_mpa_gears_estimation_panel.rds"))

# Tables for text
gear_with_most_landings_by_mpa %>%
  arrange(short_name) %>%
  mutate(gear = str_to_sentence(str_replace(gear, "_", " "))) %>%
  kable(col.names = c("MPA", "wdpaid", "Gear", "Catch (mt)", "% of total catch", "N. Obs.", "BACI?"),
        digits = 2,
        booktabs = T,
        format = "latex") %>%
  kable_styling() %>%
  save_kable(here("results", "tab", "relevant_mpa_gear_combinations.tex"))
