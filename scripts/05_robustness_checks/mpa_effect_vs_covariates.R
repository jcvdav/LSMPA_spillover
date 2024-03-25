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
  sf,
  cowplot,
  broom,
  ggrepel,
  tidyverse
)

# Load data --------------------------------------------------------------------
mpas_sf <- st_read(here("data", "processed", "clean_lmpas.gpkg")) %>%
  mutate(name = case_when(wdpaid == "11753" ~ "Galápagos",
                          wdpaid == "309888" ~ "PIPA",
                          wdpaid == "555512151" ~ "Chagos",
                          wdpaid == "555543712" ~ "Motu Motiro Hiva",
                          wdpaid == "555556875_A" ~ "Coral Sea",
                          wdpaid == "555624169" ~ "Nazca-Desventuradas",
                          wdpaid == "555705568" ~ "Niue Moana Mahu",
                          wdpaid == "220201" ~ "Papahānaumokuākea",
                          wdpaid == "555624172" ~ "Pitcairn",
                          T ~ name))
annual_panel <- readRDS(here("data", "processed", "annual_panel.rds")) %>%
  filter(gear == "purse_seine") %>%
  mutate(name = case_when(wdpaid == "11753" ~ "Galápagos",
                          wdpaid == "309888" ~ "PIPA",
                          wdpaid == "555512151" ~ "Chagos",
                          wdpaid == "555543712" ~ "Motu Motiro Hiva",
                          wdpaid == "555556875_A" ~ "Coral Sea",
                          wdpaid == "555624169" ~ "Nazca-Desventuradas",
                          wdpaid == "555705568" ~ "Niue Moana Mahu",
                          wdpaid == "220201" ~ "Papahānaumokuākea",
                          wdpaid == "555624172" ~ "Pitcairn",
                          T ~ name))
model <- readRDS(here("data", "output", "gear_mpa_regs.rds"))


## PROCESSING ##################################################################
# X ----------------------------------------------------------------------------
mpas <- mpas_sf %>%
  st_drop_geometry() %>%
  select(name, area, year_enforced)


# X ----------------------------------------------------------------------------
effort_100 <- annual_panel %>%
  filter(dist <= 100) %>%
  filter(between(year, year_enforced - 10, year_enforced -1)) %>%
  group_by(year, name, effort_measure) %>%
  summarize(effort = sum(effort, na.rm = T)) %>%
  group_by(name, effort_measure) %>%
  summarize(effort_100 = mean(effort, na.rm = T))

effort <- annual_panel %>%
  filter(is.na(dist)) %>%
  select(year, lon, lat, effort, effort_measure) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_join(mpas_sf) %>%
  drop_na(name) %>%
  filter(between(year, year_enforced - 10, year_enforced -1)) %>%
  st_drop_geometry() %>%
  group_by(year, name, effort_measure) %>%
  summarize(effort = sum(effort, na.rm = T)) %>%
  group_by(name, effort_measure) %>%
  summarize(effort = mean(effort, na.rm = T)) %>%
  left_join(effort_100, by = c("name", "effort_measure")) %>%
  mutate(pct = (effort / (effort + effort_100)) * 100)



# X ----------------------------------------------------------------------------
coef_data <- map_dfr(.x = model, .f = tidy, .id = "name") %>%
  filter(str_detect(term, "post:near")) %>%
  mutate(name = str_remove(string = name, pattern = ".+PS ")) %>%
  left_join(mpas, by = "name") %>%
  left_join(effort, by = "name") %>%
  mutate(age = 2024 - year_enforced,
         displaced = ifelse(name %in% c("Chagos", "PRI (Jarvis)", "Nazca-Desventuradas"), "Paper parks", "MPAs"))

coef_data

## VISUALIZE ###################################################################

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
p1 <- ggplot(data = coef_data,
       mapping = aes(x = age, y = estimate)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_pointrange(mapping = aes(ymin = estimate - std.error,
                                ymax = estimate + std.error),
                  size = 4,
                  fill = gear_palette["PS"],
                  fatten = 1,
                  shape = 21) +
  geom_text_repel(aes(label = name), nudge_x =  4, nudge_y = -0.1) +
  geom_smooth(method = "lm", se = F, linetype = "dashed", color = "black") +
  facet_wrap(~displaced, scales = "free_y") +
  labs(fill = "LSMPA",
       x = "MPA Age (years)",
       y = "DiD Estimate",
       title = "MPA Age")

# X ----------------------------------------------------------------------------
p2 <- ggplot(data = coef_data,
       mapping = aes(x = area, y = estimate)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_pointrange(mapping = aes(ymin = estimate - std.error,
                                ymax = estimate + std.error),
                  size = 4,
                  fill = gear_palette["PS"],
                  fatten = 1,
                  shape = 21) +
  geom_text_repel(aes(label = name), nudge_x =  4, nudge_y = -0.1) +
  geom_smooth(method = "lm", se = F, linetype = "dashed", color = "black") +
  facet_wrap(~displaced, scales = "free_y") +
  labs(fill = "LSMPA",
       x = "MPA Area (km2)",
       y = "DiD Estimate",
       title = "MPA size")

# X ----------------------------------------------------------------------------
p3 <- ggplot(data = coef_data,
       mapping = aes(x = pct, y = estimate)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_pointrange(mapping = aes(ymin = estimate - std.error,
                                ymax = estimate + std.error),
                  size = 4,
                  fill = gear_palette["PS"],
                  fatten = 1,
                  shape = 21) +
  geom_text_repel(aes(label = name), nudge_x =  4, nudge_y = -0.1) +
  geom_smooth(method = "lm", se = F, linetype = "dashed", color = "black") +
  facet_wrap(~displaced, scales = "free_y") +
  labs(fill = "LSMPA",
       x = "Effort displaced",
       y = "DiD Estimate",
       title = "Displaced effort")

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
plot_grid(p1, p2, p3, ncol = 1)

