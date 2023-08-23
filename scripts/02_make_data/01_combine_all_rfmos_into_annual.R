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

overlap_plot <- cowplot::plot_grid(mt_ts_overlaps, rfmo_overlaps)

startR::lazy_ggsave(overlap_plot,
                    "figSX_mt_ts_overlaps",
                    height = 15,
                    width = 20)

## BACK TO COMBINING DATA ######################################################
annual_all_rfmos_without_overlaps <- annual_all_rfmos %>%
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
