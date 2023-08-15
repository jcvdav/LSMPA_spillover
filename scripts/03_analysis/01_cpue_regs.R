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
  fixest,
  modelsummary,
  broom,
  tidyverse
)

# Load data --------------------------------------------------------------------
annual_panel <- readRDS(here("data", "processed", "annual_panel.rds")) %>%
  mutate(event = ifelse(wdpaid == "11753", year - 1998, event),
         post = 1 * (event >= 0)) %>%
  filter(effort_measure %in% c("sets", "hooks"))

## PROCESSING ##################################################################
mixed <- annual_panel %>%
  filter(
    (gear == "purse_seine" & wdpaid == "11753") |
      (gear == "purse_seine" & wdpaid == "555629385") |
      (gear == "purse_seine" & wdpaid == "309888") |
      (gear == "purse_seine" & wdpaid == "555624169") |
      (gear == "longline" & wdpaid == "555512151") |
      (gear == "longline" & wdpaid == "555624172") |
      (gear == "longline" & wdpaid == "555635929") |
      (gear == "purse_seine" & wdpaid == "555635928") |
      (gear == "longline" & wdpaid == "220201")) %>%
  filter(cpue_tot > 0) %>%
  filter(event >= -10,
         event < 10) %>%
  mutate(group = paste(name, gear),
         group = fct_reorder(group, id, function(x){-1 * n_distinct(x)}))


feols(log(cpue_tot) ~ i(post, near_100, 0) | id + year,
      panel.id = ~id + event,
      data = mixed,
      vcov = function(x)vcov_conley_hac(x,
                                        id = ~id,
                                        time = ~event,
                                        lat = ~lat,
                                        lon = ~lon,
                                        cutoff = 200,
                                        lag = 5),
      subset = ~!is.na(treatment_100),
      fsplit = ~group) %>%
  modelsummary::modelsummary(stars = T, gof_omit = "R|IC")


## MPA-level analysis ----------------------------------------------------------

fit <- function(data, title) {
  if(length(unique(data$effort_measure)) > 1) {warning("More than one effort measure!")}
  n_flags <- length(unique(data$flag))

  n_years <- length(unique(data$event))
  n_years_pre <- sum(between(unique(data$event), -10, -1), na.rm = T)
  n_years_post <- sum(between(unique(data$event), 1, 10), na.rm = T)
  notes <- paste("Total years:", n_years, "; Pre-years in window = ", n_years_pre, ": Post-years in window:", n_years_post)

  # Build formula
  fml <- "c(log(cpue), cpue) ~ i(post, near_100, 0) | id + event"
  if(n_flags > 1) fml <- paste(fml, "+ flag")

  feols(as.formula(fml),
        panel.id = ~id + year,
        data = data,
        vcov = function(x)vcov_conley_hac(x,
                                          id = ~id,
                                          time = ~event,
                                          lat = ~lat,
                                          lon = ~lon,
                                          cutoff = 200,
                                          lag = 5),
        fsplit = ~window10,
        split.drop = "other") %>%
    modelsummary::modelsummary(stars = T, gof_omit = "R|IC",
                               title = title,
                               notes = notes)
}

john_plot <- function(data, window = 10) {
  data %>%
    filter(between(event, -1 * window, window)) %>%
    group_by(id, post, lat, lon, dist, near_100) %>%
    summarize(cpue = mean(cpue, na.rm = T),
              cpue = log(cpue)) %>%
    ungroup() %>%
    pivot_wider(names_from = post,
                values_from = cpue, names_prefix = "cpue_") %>%
    mutate(delta = cpue_1 - cpue_0) %>%
    drop_na(delta) %>%
    ggplot(aes(x = 1, y = dist, fill = delta, shape = factor(near_100))) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = 100, linetype = "dashed") +
    geom_hline(yintercept = 200, linetype = "dashed") +
    geom_jitter(height = 0, width = 0.1, size = 4) +
    # geom_point(size = 4) +
    coord_polar() +
    scale_y_continuous(limits = c(-100, 200)) +
    scale_x_continuous(breaks = NULL) +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue") +
    scale_shape_manual(values = c(24, 21)) +
    theme_void()
}

# Galapagos
# Ecuadorian flags
# Purse seine only
# Speciess: Yellowfin tuna (Thunnus albacares), Skipjack (Katwonus pelamis) and Bigeye (Thunnus obesus)
gal <- annual_panel %>%
  filter(wdpaid == "11753",
         flag == "ECU",
         gear == "purse_seine") %>%
  filter(!is.na(treatment_100)) %>%
  mutate(cpue = cpue_yft + cpue_skj + cpue_bet,
         window10 = ifelse((event >= -10 &  event < 10), "Within 10", "other")) %>%
  filter(cpue > 0)

fit(data = gal,
    title = "MPA: Galapagos; Flag(s): ECU, gear: purse seines, spp: Yellowfin, Skipjack, and Bigeye")

john_plot(gal)


# Revillagigedo
# Mexican vessels
# Purse seines
# Species: Yellowfin tuna (Thunnus albacares)
revilla <- annual_panel %>%
  filter(wdpaid == "555629385",
         flag == "MEX",
         gear == "purse_seine") %>%
  filter(!is.na(treatment_100)) %>%
  mutate(cpue = cpue_yft,
         window10 = ifelse((event >= -10 &  event < 10), "Within 10", "other")) %>%
  filter(cpue > 0)

fit(data = revilla,
    title = "MPA: Revillagigedo; Flag(s): MEX, gear: purse seines, spp: Yellowfin")

john_plot(revilla)

# PIPA
# Purse seines
# Species: Skipjack (Katwonus pelamis)
pipa_ps <- annual_panel %>%
  filter(wdpaid == "309888",
         gear == "purse_seine") %>%
  filter(!is.na(treatment_100),
         !is.na(flag)) %>%
  mutate(cpue = cpue_skj,
         window10 = ifelse((event >= -10 &  event < 10), "Within 10", "other")) %>%
  filter(cpue > 0)

fit(data = pipa_ps,
    title = "MPA: PIPA; Flag(s): Many, gear: purse seines, spp: Skipjack")

john_plot(pipa_ps)

pipa_ll <- annual_panel %>%
  filter(wdpaid == "309888",
         gear == "longline") %>%
  filter(!is.na(treatment_100),
         !is.na(flag)) %>%
  mutate(cpue = cpue_yft + cpue_bet,
         window10 = ifelse((event >= -10 &  event < 10), "Within 10", "other")) %>%
  filter(cpue > 0)

fit(data = pipa_ll,
    title = "MPA: PIPA; Flag(s): Many, gear: longline, spp: Yellowfin and Bigeye")

john_plot(pipa_ll)

# Nazca desventuradas
nazca <- annual_panel %>%
  filter(wdpaid == "555624169",
         gear == "purse_seine") %>%
  filter(!is.na(treatment_100),
         !is.na(flag)) %>%
  mutate(cpue = cpue_skj,
         window10 = ifelse((event >= -10 &  event < 10), "Within 10", "other")) %>%
  filter(cpue > 0)

fit(data = nazca,
    title = "MPA: Nazca; Flag(s): Many, gear: purse seines, spp: Skipjack")


john_plot(nazca)

# Chagos
chagos_ll <- annual_panel %>%
  filter(wdpaid == "555512151",
         gear == "longline") %>%
  filter(!is.na(treatment_100),
         !is.na(flag)) %>%
  mutate(cpue = cpue_bet, # But YFT is a close second
         window10 = ifelse((event >= -10 &  event < 10), "Within 10", "other")) %>%
  filter(cpue > 0)

fit(data = chagos_ll,
    title = "MPA: Chagos; Flag(s): Many, gear: longline, spp: Bigeye")

john_plot(chagos_ll)

chagos_ps <- annual_panel %>%
  filter(wdpaid == "555512151",
         gear == "purse_seine") %>%
  filter(!is.na(treatment_100)) %>%
  mutate(cpue = cpue_skj + cpue_yft,
         window10 = ifelse((event >= -10 &  event < 10), "Within 10", "other")) %>%
  filter(cpue > 0)

fit(data = chagos_ps,
    title = "MPA: Chagos; Flag(s): Missing, gear: purse seines, spp: Skipjack and Yellowfin")

john_plot(chagos_ps)


# Pitcairn islands
pitcairn <- annual_panel %>%
  filter(wdpaid == "555624172",
         gear == "longline") %>%
  filter(!is.na(treatment_100),
         !is.na(flag)) %>%
  mutate(cpue = cpue_alb,
         window10 = ifelse((event >= -10 &  event < 10), "Within 10", "other")) %>%
  filter(cpue > 0)

fit(data = pitcairn,
    title = "MPA: Pitcairn; Flag(s): Many, gear: longline, spp: Albacore")

john_plot(data = pitcairn)


# Ilhas de Trinidade
trinidade <- annual_panel %>%
  filter(wdpaid == "555635929",
         gear == "longline") %>%
  filter(!is.na(treatment_100),
         !is.na(flag)) %>%
  mutate(cpue = cpue_alb,
         window10 = ifelse((event >= -10 &  event < 10), "Within 10", "other")) %>%
  filter(cpue > 0)


fit(trinidade, title = "MPA: Ilhas de Trinidade; Flag(s): Mmany, gear: longline, spp: Albacore")

john_plot(trinidade)

# Arquipelago de Sao Pedro e Sao Paulo
sao_pedro <- annual_panel %>%
  filter(wdpaid == "555635928",
         gear == "longline") %>%
  filter(!is.na(treatment_100),
         !is.na(flag)) %>%
  mutate(cpue = cpue_skj + cpue_yft,
         window10 = ifelse((event >= -10 &  event < 10), "Within 10", "other")) %>%
  filter(cpue > 0)

fit(sao_pedro, title = "MPA: Arquipelago de Sao Pedro; Flag(s): Mmany, gear: purse seine, spp: Yellowfin and Skipjack")

john_plot(sao_pedro)

# Papahanaomokakea
papa <- annual_panel %>%
  filter(wdpaid == "220201",
         gear == "longline") %>%
  filter(!is.na(treatment_100),
         !is.na(flag)) %>%
  mutate(cpue = cpue_bet + cpue_yft,
         window10 = ifelse((event >= -10 &  event < 10), "Within 10", "other")) %>%
  filter(cpue > 0)

fit(papa, title = "MPA: Ilhas de Trinidade; Flag(s): Mmany, gear: longline, spp: Albacore")

john_plot(papa)


# Niue
niue <- annual_panel %>%
  filter(wdpaid == "555705568",
         gear == "longline") %>%
  filter(!is.na(treatment_100),
         !is.na(flag)) %>%
  mutate(cpue = cpue_alb + cpue_bet + cpue_yft,
         window10 = ifelse((event >= -10 &  event < 10), "Within 10", "other")) %>%
  filter(cpue > 0)

fit(niue, title = "MPA: Niue; Flag(s): Mmany, gear: longline, spp: Albacore, Bigeye, Yellowfin")

john_plot(niue)

# Marianas
marianas <- annual_panel %>%
  filter(wdpaid == "400010",
         gear == "longline") %>%
  filter(!is.na(treatment_100),
         !is.na(flag)) %>%
  mutate(cpue = cpue_alb + cpue_bet + cpue_yft,
         window10 = ifelse((event >= -10 &  event < 10), "Within 10", "other")) %>%
  filter(cpue > 0)

fit(marianas, title = "MPA: Marianas; Flag(s): Mmany, gear: longline, spp: Albacore, Bigeye, Yellowfin")

john_plot(marianas)

# Coral sea
coral<- annual_panel %>%
  filter(wdpaid == "555556875",
         gear == "longline") %>%
  filter(!is.na(treatment_100),
         !is.na(flag)) %>%
  mutate(cpue = cpue_alb + cpue_bet + cpue_yft,
         window10 = ifelse((event >= -10 &  event < 10), "Within 10", "other")) %>%
  filter(cpue > 0)

fit(coral, title = "MPA: Coral sea, Flag(s): Mmany, gear: longline, spp: Albacore, Bigeye, Yellowfin")

john_plot(coral)

# Plau
palau <- annual_panel %>%
  filter(wdpaid == "555622118",
         gear == "longline") %>%
  filter(!is.na(treatment_100),
         !is.na(flag)) %>%
  mutate(cpue = cpue_alb + cpue_bet + cpue_yft,
         window10 = ifelse((event >= -10 &  event < 10), "Within 10", "other")) %>%
  filter(cpue > 0)

fit(palau, title = "MPA: Palau Flag(s): Mmany, gear: longline, spp: Albacore, Bigeye, Yellowfin")

john_plot(palau)


# Cordillera de Coiba
coiba <- annual_panel %>%
  filter(wdpaid == "555705293",
         gear == "purse_seine") %>%
  filter(!is.na(treatment_100),
         !is.na(flag)) %>%
  mutate(cpue = cpue_yft,
         window10 = ifelse((event >= -10 &  event < 10), "Within 10", "other")) %>%
  filter(cpue > 0)

fit(coiba, title = "MPA: Cordilleras d Coiba, Flag(s): Mmany, gear: longline, spp: Yellowfin")

john_plot(coiba)

# Combined ---------------------------------------------------------------------
all_ps <- bind_rows(gal, revilla, pipa_ps, nazca, chagos_ps, coiba) %>%
  drop_na()
fit(all_ps, title = "All PS")
john_plot(all_ps)

all_ll <- bind_rows(pipa_ll, chagos_ll, pitcairn, trinidade, sao_pedro, papa, niue, marianas, palau, coral) %>%
  drop_na()

fit(all_ll, title = "All LL")
john_plot(all_ll)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
