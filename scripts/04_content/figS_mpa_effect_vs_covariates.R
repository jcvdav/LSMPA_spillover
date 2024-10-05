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
model_full_post <- readRDS(here("data", "output", "gear_mpa_regs_full_post.rds"))


## PROCESSING ##################################################################
# X ----------------------------------------------------------------------------
mpas <- mpas_sf %>%
  st_drop_geometry() %>%
  select(name, area, year_enforced)


# X ----------------------------------------------------------------------------
coef_data_full_post <- map_dfr(.x = model_full_post, .f = tidy, .id = "name") %>%
  filter(str_detect(term, "post:near")) %>%
  mutate(name = str_remove(string = name, pattern = ".+PS ")) %>%
  left_join(mpas, by = "name") %>%
  mutate(age = 2024 - year_enforced,
         displaced = ifelse(name %in% c("Chagos", "PRI (Jarvis)", "Nazca-Desventuradas"),
                            "Low effort displacement\nor not well enforced", "Displaced fishing effort\nand well enforced"))


coef_data <- map_dfr(.x = model, .f = tidy, .id = "name") %>%
  filter(str_detect(term, "post:near")) %>%
  mutate(name = str_remove(string = name, pattern = ".+PS ")) %>%
  left_join(mpas, by = "name") %>%
  mutate(age = 2024 - year_enforced,
         displaced = ifelse(name %in% c("Chagos", "PRI (Jarvis)", "Nazca-Desventuradas"),
                            "Low effort displacement\nor not well enforced", "Displaced fishing effort\nand well enforced"))
## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
p1 <- ggplot(data = coef_data_full_post,
       mapping = aes(x = age, y = estimate)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_smooth(method = "lm", formula = y~0 + log10(x), linetype = "dashed", color = "black", fullrange = T) +
  geom_pointrange(mapping = aes(ymin = estimate - std.error,
                                ymax = estimate + std.error),
                  size = 2,
                  fill = gear_palette["PS"],
                  fatten = 1,
                  shape = 21) +
  geom_text_repel(aes(label = name), size = 2, nudge_x =  4, nudge_y = -0.1) +
  facet_wrap(~displaced, scales = "free_y") +
  scale_x_continuous(limits = c(1, NA)) +
  labs(fill = "LSMPA",
       x = "MPA Age (years)",
       y = "DiD Estimate",
       title = "MPA Age")


p2 <- ggplot(data = coef_data,
       mapping = aes(x = area, y = estimate)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_smooth(method = "lm", formula = y~0 +x, linetype = "dashed", color = "black", fullrange = T) +

  geom_pointrange(mapping = aes(ymin = estimate - std.error,
                                ymax = estimate + std.error),
                  size = 2,
                  fill = gear_palette["PS"],
                  fatten = 1,
                  shape = 21) +
  geom_text_repel(aes(label = name), size = 2, nudge_x =  4, nudge_y = -0.1) +
  facet_wrap(~displaced, scales = "free_y") +
  scale_x_continuous(limits = c(1, NA)) +
  labs(fill = "LSMPA",
       x = "MPA Area (km2)",
       y = "DiD Estimate",
       title = "MPA size")

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
p <- plot_grid(p1, p2, ncol = 1, labels = "auto")

startR::lazy_ggsave(
  plot = p,
  filename = "figS_mpa_effect_vs_covariates",
  width = 9,
  height = 9
)

