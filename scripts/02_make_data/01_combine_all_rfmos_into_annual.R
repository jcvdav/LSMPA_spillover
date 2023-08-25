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
  cowplot,
  sf,
  here,
  tidyverse
)

# Load data --------------------------------------------------------------------
iattc <- readRDS(file = here("data", "processed", "rfmo_iattc_tuna_monthly_gear_flag.rds"))
iotc <- readRDS(file = here("data", "processed", "rfmo_iotc_tuna_monthly_gear_flag.rds"))
wcpfc <- readRDS(file = here("data", "processed", "rfmo_wcpfc_tuna_quarterly_gear_flag.rds"))
# Note that ICCAT is already annual
iccat_annual <- readRDS(file = here("data", "processed", "rfmo_iccat_tuna_annual_gear_flag.rds"))

## PROCESSING ##################################################################

# Group IATTC by year ----------------------------------------------------------
iattc_annual <- iattc %>%
  group_by(
    rfmo,
    year,
    gear,
    flag,
    lat,
    lon,
    effort_measure) %>%
  summarize_at(.vars = vars(effort, alb_mt, bet_mt, pbf_mt, skj_mt, yft_mt), .funs = sum, na.rm = T) %>%
  ungroup() %>%
  mutate(tot_mt = alb_mt + bet_mt + pbf_mt + skj_mt + yft_mt,
         cpue_alb = alb_mt / effort,
         cpue_bet = bet_mt / effort,
         cpue_pbf = pbf_mt / effort,
         cpue_skj = skj_mt / effort,
         cpue_yft = yft_mt / effort,
         cpue_tot = tot_mt / effort)

# Group IOTC by year -----------------------------------------------------------
iotc_annual <- iotc %>%
  group_by(
    rfmo,
    year,
    gear,
    flag,
    lat,
    lon,
    effort_measure) %>%
  summarize_at(.vars = vars(effort, yft_mt, bet_mt, skj_mt, alb_mt, sbf_mt), .funs = sum, na.rm = T) %>%
  ungroup() %>%
  mutate(tot_mt = yft_mt + bet_mt + skj_mt + alb_mt + sbf_mt,
         cpue_yft = yft_mt / effort,
         cpue_bet = bet_mt / effort,
         cpue_skj = skj_mt / effort,
         cpue_alb = alb_mt / effort,
         cpue_sbf = sbf_mt / effort,
         cpue_tot = tot_mt / effort)

# Group WCPFC by quarter -------------------------------------------------------
wcpfc_annual <- wcpfc %>%
  group_by(
    rfmo,
    year,
    gear,
    flag,
    lat,
    lon,
    effort_measure) %>%
  summarize_at(.vars = vars(effort, skj_mt, alb_mt, yft_mt, bet_mt), .funs = sum, na.rm = T) %>%
  ungroup() %>%
  mutate(tot_mt = yft_mt + bet_mt + skj_mt + alb_mt,
         cpue_yft = yft_mt / effort,
         cpue_bet = bet_mt / effort,
         cpue_skj = skj_mt / effort,
         cpue_alb = alb_mt / effort,
         cpue_tot = tot_mt / effort)

# Combine all ------------------------------------------------------------------
annual_all_rfmos <- bind_rows(iattc_annual,
                              iotc_annual,
                              wcpfc_annual,
                              iccat_annual) %>%
  select(
    rfmo,
    year,
    gear,
    flag,
    lat,
    lon,
    effort_measure,
    effort,
    alb_mt, bet_mt, bft_mt, pbf_mt, sbf_mt, skj_mt, tot_mt, yft_mt,
    cpue_alb, cpue_bet, cpue_bft, cpue_pbf, cpue_sbf, cpue_skj, cpue_tot, cpue_yft
  ) %>%
  replace_na(replace = list( alb_mt = 0,
                             bet_mt = 0,
                             bft_mt = 0,
                             pbf_mt = 0,
                             sbf_mt = 0,
                             skj_mt = 0,
                             yft_mt = 0))

## BREIF PARENTHRESES TO LOOK AT DUPLICATE DATA ################################

cells_with_dupes <- annual_all_rfmos %>%
  group_by(lat, lon) %>%
  mutate(n = n_distinct(rfmo)) %>%
  filter(n == 2) %>%
  count(rfmo, lat, lon) %>%
  group_by(lat, lon) %>%
  summarize(rfmo = paste(rfmo, collapse = ", ")) %>%
  ungroup()

cells_with_dupes %>%
  count(rfmo)

annual_all_rfmos %>%
  group_by(lat, lon) %>%
  mutate(n = n_distinct(rfmo)) %>%
  filter(n == 2) %>%
  count(rfmo, lat, lon) %>%
  inner_join(cells_with_dupes %>%
               filter(rfmo == "iattc, iccat") %>%
               select(lat, lon),
             by = c("lat", "lon"))

mpas <- sf::st_read(here("data/processed/clean_lmpas.gpkg"))
gal <- mpas %>%
  filter(wdpaid == "11753")
gal_300 <- st_buffer(gal, dist = (3e5 * 1.854)) %>%
  st_cast("LINESTRING")
gal_600 <- st_buffer(gal, dist = (6e5 * 1.854)) %>%
  st_cast("LINESTRING")
coast <- rnaturalearth::ne_coastline(returnclass = "sf")
central_america <- rnaturalearth::ne_countries(
  country = c("Costa Rica",
              "Panama",
              "Nicaragua",
              "Colombia",
              "Ecuador",
              "Honduras"),
  returnclass = "sf")

map_of_all_overlaps <- cells_with_dupes %>%
  ggplot(aes(x = lon, y = lat)) +
  geom_sf(data = coast,
          inherit.aes = F) +
  geom_sf(data = mpas,
          inherit.aes = F, fill = "blue") +
  geom_point(size = 1, color = "red")

map_of_overlaps_IATTC_IATC <-
  cells_with_dupes %>%
  filter(rfmo == "iattc, iccat") %>%
  select(lat, lon) %>%
  ggplot() +
  geom_sf(data = central_america) +
  geom_sf(data = gal, fill = "blue") +
  geom_sf(data = gal_300, fill = "blue", linetype = "dashed") +
  geom_sf(data = gal_600, fill = "blue") +
  geom_point(aes(x = lon, y = lat), color = "red") +
  geom_point(x = -82.5, y = 7.5, shape = 21, size = 5, color = "gray")

map_of_overlaps <- plot_grid(map_of_all_overlaps,
                             map_of_overlaps_IATTC_IATC,
                             ncol = 1,
                             labels = "AUTO")


startR::lazy_ggsave(
  plot = map_of_overlaps,
  filename = "figSx_map_of_overlaps",
  width = 15,
  height = 15
)

# Number of unique overlapping grid cells
yr <- 2006
unique_cells <- annual_all_rfmos %>%
  filter(year > yr,
         rfmo %in% c("wcpfc", "iattc")) %>%
  group_by(lat, lon) %>%
  summarize(n = n_distinct(rfmo),
            .groups = "drop")

ggplot(unique_cells, aes(x = lon, y = lat, fill = n)) +
  geom_tile() +
  coord_equal()

unique_cells %>%
  filter(n > 1) %>%
  dim()

# SW corner
unique_cells %>%
  filter(n > 1) %>%
  summarize_all(min)
# NE corner
unique_cells %>%
  filter(n > 1) %>%
  summarize_all(max)

# Overlaping df
overlaps <- annual_all_rfmos %>%
  filter(year > yr,
         rfmo %in% c("wcpfc", "iattc")) %>%
  group_by(lat, lon, flag, gear, year) %>%
  mutate(n = n_distinct(rfmo)) %>%
  ungroup()

overlaps %>%
  filter(n > 1) %>%
  dim()

overlaps %>%
  filter(n > 1) %>%
  count(flag) %>%
  arrange(desc(n)) %>%
  janitor::adorn_percentages(denominator = "col") %>%
  janitor::adorn_pct_formatting() %>%
  knitr::kable(format = "latex",
               caption = "Overlapping records by flag",
               col.names = c("Flag", "% Records"),
               booktabs = T)

mt_ts_overlaps <- overlaps %>%
  filter(n > 1) %>%
  ggplot(aes(x = year, y = tot_mt, color = rfmo)) +
  stat_summary(geom = "line",
               fun = "sum", na.rm = T) +
  facet_wrap(~flag, ncol = 2, scales = "free_y") +
  labs(x = "Year",
       y = "Catch (MT)") +
  theme(legend.position = "bottom")

rfmo_overlaps <- overlaps %>%
  filter(n > 1) %>%
  group_by(rfmo, flag, year) %>%
  summarize(tot_mt = sum(tot_mt, na.rm = T)) %>%
  pivot_wider(names_from = rfmo,
              values_from = tot_mt,
              values_fill = 0) %>%
  ggplot(aes(x = iattc, y = wcpfc)) +
  geom_abline(intercept = 0,
              slope = 1) +
  geom_point(aes(color = year)) +
  facet_wrap(~flag, ncol = 2) +
  coord_equal() +
  labs(x = "IATTC catch (MT)",
       y = "WCPFC catch (MT)") +
  theme(legend.position = "bottom")

overlap_plot <- cowplot::plot_grid(mt_ts_overlaps,
                                   rfmo_overlaps,
                                   labels = "AUTO",
                                   rel_widths = c(2, 1))

startR::lazy_ggsave(overlap_plot,
                    "figSX_mt_ts_overlaps",
                    height = 15,
                    width = 20)

## BACK TO COMBINING DATA ######################################################
annual_all_rfmos_without_overlaps <- annual_all_rfmos %>%
  # Remove points from ICCAT that overlap with points from IATTC
  filter(
    !(rfmo == "iccat" & lon == -86.5 & lat == 12.5),
    !(rfmo == "iccat" & lon == -82.5 & lat == 7.5),
    !(rfmo == "iccat" & lon == -82.5 & lat == 12.5),
    !(rfmo == "iccat" & lon == -78.5 & lat == 7.5),
    !(rfmo == "iccat" & lon == -77.5 & lat == 7.5)) %>%
  # Remove points from IATTC that overlap with points from ICCAT
  # Note that here we only remove 4, as indicated in the SM of our paper
  # We need to keep -82.5, 7.5 that is close to Galapagos (within 600 naut miles)
  filter(
    !(rfmo == "iattc" & lon == -86.5 & lat == 12.5),
    !(rfmo == "iattc" & lon == -82.5 & lat == 12.5),
    !(rfmo == "iattc" & lon == -78.5 & lat == 7.5),
    !(rfmo == "iattc" & lon == -77.5 & lat == 7.5)
  ) %>%
  group_by(year, gear, flag, lat, lon, effort_measure) %>%
  summarize(effort = max(effort),
            alb_mt = max(alb_mt),
            bet_mt = max(bet_mt),
            bft_mt = max(bft_mt),
            pbf_mt = max(pbf_mt),
            sbf_mt = max(sbf_mt),
            skj_mt = max(skj_mt),
            yft_mt = max(yft_mt),
            tot_mt = max(tot_mt)) %>%
  ungroup() %>%
  mutate(tot_mt = alb_mt + bet_mt + bft_mt + pbf_mt + sbf_mt + skj_mt + yft_mt,
         cpue_alb = alb_mt / effort,
         cpue_bet = bet_mt / effort,
         cpue_bft = bft_mt / effort,
         cpue_pbf = pbf_mt / effort,
         cpue_sbf = sbf_mt / effort,
         cpue_skj = skj_mt / effort,
         cpue_yft = yft_mt / effort,
         cpue_tot = tot_mt / effort)

# How many records did we remove?
dim(annual_all_rfmos)[1] - dim(annual_all_rfmos_without_overlaps)[1]


## CHECKS ######################################################################
check_mt(annual_all_rfmos_without_overlaps, cutoff = 0)

check_effort_gear(annual_all_rfmos_without_overlaps)

test(annual_all_rfmos_without_overlaps)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = annual_all_rfmos_without_overlaps,
        file = here("data", "processed", "rfmo_all_annual_gear_flag.rds"))
