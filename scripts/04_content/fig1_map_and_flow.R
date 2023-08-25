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
  cowplot,
  terra,
  rnaturalearth,
  sf,
  tidyverse
)

# Load data --------------------------------------------------------------------
annual_panel <- readRDS(here("data", "processed", "annual_panel.rds")) %>%
  filter(gear %in% c("purse_seine", "longline"))

coast <- ne_countries(returnclass = "sf")

coastline <- ne_coastline(returnclass = "sf") %>%
  st_wrap_dateline(option = "WRAPDATELINE=YES")

mpas <- st_read("data/processed/clean_lmpas.gpkg")

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
map_data <- annual_panel %>%
  filter(between(year, 2011, 2021)) %>%
  group_by(year, lat, lon) %>%
  summarize(tot_mt = sum(tot_mt, na.rm = T)) %>%
  ungroup() %>%
  group_by(lat, lon) %>%
  summarize(tot_mt = mean(tot_mt, na.rm = T)) %>%
  ungroup() %>%
  st_as_sf(coords = c("lon", "lat"),
           crs = "EPSG:4326")

r <- rast(nrows = 180, ncols = 360, nlyrs = 1,
          xmin = -180, xmax = 180,
          ymin = -90, ymax = 90,
          crs = "EPSG:4326")

interpolated_catch <- rasterize(x = vect(map_data),
                                y = r,
                                field = "tot_mt",
                                fun = "mean")

# # X ----------------------------------------------------------------------------
# alluvial_data <- annual_panel %>%
#   filter(dist <= 200) %>%
#   select(name, gear, flag, contains("mt"), -tot_mt) %>%
#   replace_na(replace = list(name = "No MPA",
#                             flag = "No flag")) %>%
#   group_by(name, gear, flag) %>%
#   summarize_all(sum, na.rm = T) %>%
#   ungroup() %>%
#   pivot_longer(cols = contains("mt"), names_to = "spp", values_to = "mt") %>%
#   ungroup() %>%
#   group_by(name, gear, flag, spp) %>%
#   summarize(mt = sum(mt, na.rm = T)) %>%
#   ungroup() %>%
#   mutate(spp = str_remove(spp, "_mt"),
#          pct_mt = mt / sum(mt, na.rm = T)) %>%
#   mutate(name = fct_reorder(name, pct_mt, sum),
#          spp = fct_reorder(spp, pct_mt, sum),
#          gear = fct_reorder(gear, pct_mt, sum),
#          flag = fct_reorder(flag, pct_mt, sum))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

map <- ggplot() +
  geom_spatraster(data = log(interpolated_catch),
                  aes(fill = mean)) +
  geom_sf(data = coast,
          fill = "#DCE1E5",
          color = "#DCE1E5",
          linewidth = 0.1) +
  geom_sf(data = coastline,
          color = "#111517",
          linewidth = 0.25) +
  geom_sf(data = mpas,
          fill = "#F47321",
          color = "black",
          linewidth = 1) +
  geom_sf(data = mpas,
          fill = "#F47321",
          color = "transparent") +
  scale_fill_gradient(low = "#DCE1E5",
                      high = "#003660", na.value = "transparent") +
  labs(fill = "log(mt)") +
  guides(fill = guide_colorbar(ticks.colour = "black",
                               frame.colour = "black")) +
  theme(axis.title = element_blank()) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

map

# alluvial <- ggplot(data = alluvial_data,
#        mapping = aes(y = pct_mt,
#                      axis1 = name,
#                      axis2 = spp,
#                      axis3 = gear,
#                      axis4 = flag)) +
#   geom_alluvium() +
#   geom_stratum(width = 0.3, size = 0.3) +
#   geom_text(stat = "stratum",
#             aes(label = after_stat(stratum)),
#             size = 2) +
#   scale_x_discrete(limits = c("MPA",
#                               "Species",
#                               "Gear",
#                               "Flag"),
#                    expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
#   labs(x = "",
#        y = "% Total tuna caught") +
#   theme_bw() +
#   theme(legend.position = "None")


# p <- plot_grid(map, alluvial, ncol = 1, labels = "AUTO")

## EXPORT ######################################################################

# X -----------------------------------------------------------------------------
startR::lazy_ggsave(
  plot = map,
  filename = "fig_1_map",
  width = 11,
  height = 5
)

# Globally
alluvial_data2 <- annual_panel %>%
  filter(gear %in% c("purse_seine", "longline"),
         year >= 2018) %>%
  mutate(name = ifelse(dist <= 600, name, "No MPA")) %>%
  replace_na(replace = list(name = "No MPA",
                            flag = "No flag")) %>%
  select(name, gear, flag, contains("mt"), -tot_mt) %>%
  group_by(name, gear, flag) %>%
  summarize_all(sum, na.rm = T) %>%
  ungroup() %>%
  pivot_longer(cols = contains("mt"), names_to = "spp", values_to = "mt") %>%
  ungroup() %>%
  group_by(name, gear, flag, spp) %>%
  summarize(mt = sum(mt, na.rm = T)) %>%
  ungroup() %>%
  mutate(spp = str_remove(spp, "_mt"),
         pct_mt = mt / sum(mt, na.rm = T)) %>%
  mutate(name = fct_reorder(name, pct_mt, sum),
         spp = fct_reorder(spp, pct_mt, sum),
         gear = fct_reorder(gear, pct_mt, sum),
         flag = fct_reorder(flag, pct_mt, sum))

ggplot(data = alluvial_data2,
       mapping = aes(y = pct_mt,
                     axis1 = name,
                     axis2 = spp,
                     axis3 = gear,
                     axis4 = flag)) +
  geom_alluvium() +
  geom_stratum(width = 0.3, size = 0.3) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            size = 2) +
  scale_x_discrete(limits = c("MPA",
                              "Species",
                              "Flag"),
                   expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
  labs(x = "",
       y = "% Total tuna caught") +
  theme_bw()
