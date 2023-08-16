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
  mutate(event = ifelse(wdpaid == "11753", year - 2002, event),
         post = 1 * (event >= 0)) %>%
  filter(effort_measure %in% c("sets", "hooks"),
         gear %in% c("longline", "purse_seine"))

## MPA-level analysis ----------------------------------------------------------

fit <- function(data, window = T) {

  if(window){
    data <- filter(data, between(event, -10, 10))
  }

  # Build formula
  fml <- "log(cpue) ~ i(post, near_100, 0) | id + event"

  model <- feols(as.formula(fml),
                 panel.id = ~id + year,
                 data = data,
                 vcov = function(x)vcov_conley_hac(x,
                                                   id = ~id,
                                                   time = ~event,
                                                   lat = ~lat,
                                                   lon = ~lon,
                                                   cutoff = 200,
                                                   lag = 5),
                 fsplit = ~name)

  return(model)
}

# Model summary table

ms <- function(model) {
  modelsummary(model,
               stars = T,
               gof_omit = "^(?!.*R2 Adj|Num.Obs.)",
               coef_rename = c("post::1:near_100" = "Post X Near"),
               notes = "All specifications include year and gridcel fixed effects and use Coley HAC standard errors (cuttoff = 200 km; lag = 5 yrs)")
}

john_plot <- function(data, window = T) {

  if(window) {
    data <- data %>%
      filter(between(event, -1 * window, window))
  }

  data %>%
    group_by(id, post, lat, lon, dist, near_100) %>%
    summarize(cpue = mean(cpue, na.rm = T),
              cpue = log(cpue)) %>%
    ungroup() %>%
    pivot_wider(names_from = post,
                values_from = cpue, names_prefix = "cpue_") %>%
    mutate(delta = cpue_1 - cpue_0) %>%
    drop_na(delta) %>%
    ggplot(aes(x = 1, y = dist, fill = delta > 0, shape = factor(near_100))) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = 100, linetype = "dashed") +
    geom_hline(yintercept = 200, linetype = "dashed") +
    geom_jitter(height = 0, width = 0.1, size = 4) +
    # geom_point(size = 4) +
    coord_polar() +
    scale_y_continuous(limits = c(-100, 200)) +
    scale_x_continuous(breaks = NULL) +
    # scale_fill_binned(type = "viridis") +
    scale_shape_manual(values = c(24, 21)) +
    theme_void()
}

# Build individual estimation panels for each MPA, based on the target species,
# gear used, and flag

# Purse seine first ============================================================

# Galapagos --------------------------------------------------------------------
gal <- annual_panel %>%
  filter(wdpaid == "11753",
         flag == "ECU",
         gear == "purse_seine") %>%
  filter(!is.na(treatment_100)) %>%
  mutate(tot_mt = yft_mt + skj_mt + bet_mt) %>%
  filter(tot_mt > 0) %>%
  group_by(wdpaid, name, id, lat, lon, year, event, post, near_100) %>%
  summarize(tot_mt = sum(tot_mt, na.rm = T),
            tot_effort = sum(effort, na.rm = T)) %>%
  ungroup() %>%
  mutate(cpue = tot_mt / tot_effort) %>%
  filter(cpue > 0)

# Revillagigedo ----------------------------------------------------------------
revilla <- annual_panel %>%
  filter(wdpaid == "555629385",
         flag == "MEX",
         gear == "purse_seine") %>%
  filter(!is.na(treatment_100)) %>%
  mutate(cpue = cpue_yft) %>%
  mutate(tot_mt = yft_mt) %>%
  filter(tot_mt > 0) %>%
  group_by(wdpaid, name, id, lat, lon, year, event, post, near_100) %>%
  summarize(tot_mt = sum(tot_mt, na.rm = T),
            tot_effort = sum(effort, na.rm = T)) %>%
  ungroup() %>%
  mutate(cpue = tot_mt / tot_effort) %>%
  filter(cpue > 0)

# PIPA -------------------------------------------------------------------------
# Purse seines
# Species: Skipjack (Katwonus pelamis)
pipa_ps <- annual_panel %>%
  filter(wdpaid == "309888",
         gear == "purse_seine") %>%
  filter(!is.na(treatment_100)) %>%
  mutate(tot_mt = skj_mt) %>%
  filter(tot_mt > 0) %>%
  group_by(wdpaid,  name, id, lat, lon, year, event, post, near_100) %>%
  summarize(tot_mt = sum(tot_mt, na.rm = T),
            tot_effort = sum(effort, na.rm = T)) %>%
  ungroup() %>%
  mutate(cpue = tot_mt / tot_effort) %>%
  filter(cpue > 0)


# Nazca desventuradas ----------------------------------------------------------
nazca <- annual_panel %>%
  filter(wdpaid == "555624169",
         gear == "purse_seine") %>%
  filter(!is.na(treatment_100)) %>%
  mutate(tot_mt = skj_mt) %>%
  filter(tot_mt > 0) %>%
  group_by(wdpaid,  name, id, lat, lon, year, event, post, near_100) %>%
  summarize(tot_mt = sum(tot_mt, na.rm = T),
            tot_effort = sum(effort, na.rm = T)) %>%
  ungroup() %>%
  mutate(cpue = tot_mt / tot_effort) %>%
  filter(cpue > 0)


# Chagos -----------------------------------------------------------------------
chagos_ps <- annual_panel %>%
  filter(wdpaid == "555512151",
         gear == "purse_seine") %>%
  filter(!is.na(treatment_100)) %>%
  mutate(tot_mt = yft_mt + skj_mt) %>%
  filter(tot_mt > 0) %>%
  group_by(wdpaid,  name, id, lat, lon, year, event, post, near_100) %>%
  summarize(tot_mt = sum(tot_mt, na.rm = T),
            tot_effort = sum(effort, na.rm = T)) %>%
  ungroup() %>%
  mutate(cpue = tot_mt / tot_effort) %>%
  filter(cpue > 0)

# Cordillera de Coiba ----------------------------------------------------------
coiba <- annual_panel %>%
  filter(wdpaid == "555705293",
         gear == "purse_seine") %>%
  filter(!is.na(treatment_100)) %>%
  mutate(tot_mt = yft_mt) %>%
  filter(tot_mt > 0) %>%
  group_by(wdpaid,  name, id, lat, lon, year, event, post, near_100) %>%
  summarize(tot_mt = sum(tot_mt, na.rm = T),
            tot_effort = sum(effort, na.rm = T)) %>%
  ungroup() %>%
  mutate(cpue = tot_mt / tot_effort) %>%
  filter(cpue > 0)

# Now longline =================================================================
# PIPA -------------------------------------------------------------------------
pipa_ll <- annual_panel %>%
  filter(wdpaid == "309888",
         gear == "longline") %>%
  filter(!is.na(treatment_100)) %>%
  mutate(tot_mt = yft_mt + bet_mt) %>%
  filter(tot_mt > 0) %>%
  group_by(wdpaid,  name, id, lat, lon, year, event, post, near_100) %>%
  summarize(tot_mt = sum(tot_mt, na.rm = T),
            tot_effort = sum(effort, na.rm = T)) %>%
  ungroup() %>%
  mutate(cpue = tot_mt / tot_effort) %>%
  filter(cpue > 0)

# Chagos -----------------------------------------------------------------------
chagos_ll <- annual_panel %>%
  filter(wdpaid == "555512151",
         gear == "longline") %>%
  filter(!is.na(treatment_100)) %>%
  mutate(tot_mt = bet_mt) %>%
  filter(tot_mt > 0) %>%
  group_by(wdpaid,  name, id, lat, lon, year, event, post, near_100) %>%
  summarize(tot_mt = sum(tot_mt, na.rm = T),
            tot_effort = sum(effort, na.rm = T)) %>%
  ungroup() %>%
  mutate(cpue = tot_mt / tot_effort) %>%
  filter(cpue > 0)


# Pitcairn islands -------------------------------------------------------------
pitcairn <- annual_panel %>%
  filter(wdpaid == "555624172",
         gear == "longline") %>%
  filter(!is.na(treatment_100)) %>%
  mutate(tot_mt = alb_mt) %>%
  filter(tot_mt > 0) %>%
  group_by(wdpaid,  name, id, lat, lon, year, event, post, near_100) %>%
  summarize(tot_mt = sum(tot_mt, na.rm = T),
            tot_effort = sum(effort, na.rm = T)) %>%
  ungroup() %>%
  mutate(cpue = tot_mt / tot_effort) %>%
  filter(cpue > 0)

# Ilhas de Trinidade -----------------------------------------------------------
trinidade <- annual_panel %>%
  filter(wdpaid == "555635929",
         gear == "longline") %>%
  filter(!is.na(treatment_100)) %>%
  mutate(tot_mt = alb_mt) %>%
  filter(tot_mt > 0) %>%
  group_by(wdpaid,  name, id, lat, lon, year, event, post, near_100) %>%
  summarize(tot_mt = sum(tot_mt, na.rm = T),
            tot_effort = sum(effort, na.rm = T)) %>%
  ungroup() %>%
  mutate(cpue = tot_mt / tot_effort) %>%
  filter(cpue > 0)

# Arquipelago de Sao Pedro e Sao Paulo -----------------------------------------
sao_pedro <- annual_panel %>%
  filter(wdpaid == "555635928",
         gear == "longline") %>%
  filter(!is.na(treatment_100)) %>%
  mutate(tot_mt = yft_mt + skj_mt) %>%
  filter(tot_mt > 0) %>%
  group_by(wdpaid,  name, id, lat, lon, year, event, post, near_100) %>%
  summarize(tot_mt = sum(tot_mt, na.rm = T),
            tot_effort = sum(effort, na.rm = T)) %>%
  ungroup() %>%
  mutate(cpue = tot_mt / tot_effort) %>%
  filter(cpue > 0)


# Papahanaomokakea -------------------------------------------------------------
papa <- annual_panel %>%
  filter(wdpaid == "220201",
         gear == "longline") %>%
  filter(!is.na(treatment_100)) %>%
  mutate(tot_mt = yft_mt + bet_mt) %>%
  filter(tot_mt > 0) %>%
  group_by(wdpaid,  name, id, lat, lon, year, event, post, near_100) %>%
  summarize(tot_mt = sum(tot_mt, na.rm = T),
            tot_effort = sum(effort, na.rm = T)) %>%
  ungroup() %>%
  mutate(cpue = tot_mt / tot_effort) %>%
  filter(cpue > 0)

# Niue -------------------------------------------------------------------------
niue <- annual_panel %>%
  filter(wdpaid == "555705568",
         gear == "longline") %>%
  filter(!is.na(treatment_100)) %>%
  mutate(tot_mt = alb_mt, bet_mt, yft_mt) %>%
  filter(tot_mt > 0) %>%
  group_by(wdpaid,  name, id, lat, lon, year, event, post, near_100) %>%
  summarize(tot_mt = sum(tot_mt, na.rm = T),
            tot_effort = sum(effort, na.rm = T)) %>%
  ungroup() %>%
  mutate(cpue = tot_mt / tot_effort) %>%
  filter(cpue > 0)

# Marianas ---------------------------------------------------------------------
marianas <- annual_panel %>%
  filter(wdpaid == "400010",
         gear == "longline") %>%
  filter(!is.na(treatment_100)) %>%
  mutate(tot_mt = alb_mt, bet_mt, yft_mt) %>%
  filter(tot_mt > 0) %>%
  group_by(wdpaid,  name, id, lat, lon, year, event, post, near_100) %>%
  summarize(tot_mt = sum(tot_mt, na.rm = T),
            tot_effort = sum(effort, na.rm = T)) %>%
  ungroup() %>%
  mutate(cpue = tot_mt / tot_effort) %>%
  filter(cpue > 0)

# Coral sea --------------------------------------------------------------------
coral<- annual_panel %>%
  filter(wdpaid == "555556875",
         gear == "longline") %>%
  filter(!is.na(treatment_100)) %>%
  mutate(tot_mt = alb_mt, bet_mt, yft_mt) %>%
  filter(tot_mt > 0) %>%
  group_by(wdpaid,  name, id, lat, lon, year, event, post, near_100) %>%
  summarize(tot_mt = sum(tot_mt, na.rm = T),
            tot_effort = sum(effort, na.rm = T)) %>%
  ungroup() %>%
  mutate(cpue = tot_mt / tot_effort) %>%
  filter(cpue > 0)

# Plau -------------------------------------------------------------------------
palau <- annual_panel %>%
  filter(wdpaid == "555622118",
         gear == "longline") %>%
  filter(!is.na(treatment_100)) %>%
  mutate(tot_mt = alb_mt, bet_mt, yft_mt) %>%
  filter(tot_mt > 0) %>%
  group_by(wdpaid,  name, id, lat, lon, year, event, post, near_100) %>%
  summarize(tot_mt = sum(tot_mt, na.rm = T),
            tot_effort = sum(effort, na.rm = T)) %>%
  ungroup() %>%
  mutate(cpue = tot_mt / tot_effort) %>%
  filter(cpue > 0)

# Now we cmbine the panels -----------------------------------------------------
all_ps <- bind_rows(
  gal,
  revilla,
  pipa_ps,
  nazca,
  chagos_ps
  )

ps_models <- fit(all_ps, window = T)
ms(ps_models)


all_ll <- bind_rows(pipa_ll, chagos_ll, pitcairn, trinidade, sao_pedro, papa, niue, marianas, coral, palau)
ll_models <- fit(all_ll)
ms(ll_models)
