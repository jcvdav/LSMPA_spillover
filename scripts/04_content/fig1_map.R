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
  magrittr,
  cowplot,
  tidyterra,
  terra,
  rnaturalearth,
  sf,
  tidyverse
)

# Source custom funcions -------------------------------------------------------
source(here("scripts/00_set_up.R"))

# Load data --------------------------------------------------------------------
annual_panel <- readRDS(here("data", "processed", "annual_panel.rds"))

coast <- ne_countries(returnclass = "sf")

coastline <- ne_coastline(returnclass = "sf") %>%
  st_wrap_dateline(option = "WRAPDATELINE=YES")

mpas <- st_read(here("data", "processed", "clean_lmpas.gpkg"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
map_data_1x1 <- annual_panel %>%
  filter(grid == "1x1") %>%
  filter(between(year, 2011, 2021)) %>%
  group_by(grid, year, lat, lon) %>%
  summarize(tot_mt = sum(tot_mt, na.rm = T)) %>%
  ungroup() %>%
  group_by(grid, lat, lon) %>%
  summarize(tot_mt = mean(tot_mt, na.rm = T)) %>%
  ungroup() %>%
  st_as_sf(coords = c("lon", "lat"),
           crs = "EPSG:4326")

map_data_5x5 <- annual_panel %>%
  filter(grid == "5x5") %>%
  filter(between(year, 2011, 2021)) %>%
  group_by(grid, year, lat, lon) %>%
  summarize(tot_mt = sum(tot_mt, na.rm = T)) %>%
  ungroup() %>%
  group_by(grid, lat, lon) %>%
  summarize(tot_mt = mean(tot_mt, na.rm = T)) %>%
  ungroup() %>%
  st_as_sf(coords = c("lon", "lat"),
           crs = "EPSG:4326")

map_data <- bind_rows(map_data_1x1, map_data_5x5)

r1 <- rast(nrows = 180, ncols = 360, nlyrs = 1,
           xmin = -180, xmax = 180,
           ymin = -90, ymax = 90, res = 1,
           crs = "EPSG:4326")

r5 <- rast(nrows = 180, ncols = 360, nlyrs = 1,
           xmin = -180, xmax = 180,
           ymin = -90, ymax = 90, res = 5,
           crs = "EPSG:4326")

rast_1x1 <- rasterize(x = vect(map_data_1x1),
                      y = r1,
                      field = "tot_mt",
                      fun = "mean")

rast_5x5 <- rasterize(x = vect(map_data_5x5),
                      y = r5,
                      field = "tot_mt",
                      fun = "mean")

rast_5x5 <- (rast_5x5 / cellSize(rast_5x5))
rast_5x5 <- resample(rast_5x5, r1, method = "cubicspline") * cellSize(r1)

interpolated_catch <- c(rast_1x1, rast_5x5) %>%
  sum(na.rm = T)

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
mpa_col <- "gray50"#"#047C91"

map <- ggplot() +
  geom_spatraster(data = log(interpolated_catch),
                  aes(fill = sum)) +
  # geom_spatraster_contour(data = log(interpolated_catch),
  #                         aes(z = sum), color = "black") +
  geom_sf(data = coast,
          fill = "#DCE1E5",
          color = "#DCE1E5",
          linewidth = 0.1) +
  geom_sf(data = coastline,
          color = "#111517",
          linewidth = 0.25) +
  geom_sf(data = mpas,
          fill = mpa_col,
          color = "#111517",
          linewidth = 1) +
  geom_sf(data = mpas,
          fill = mpa_col,
          color = "transparent") +
  scale_fill_gradientn(colors = blues,
                       na.value = "transparent") +
  guides(fill = guide_colorbar(title = "Tuna catch, log-transformed (MT)",
                               title.position = "top",
                               title.vjust = 0.5,
                               barwidth = 9,
                               barheight = 0.5,
                               ticks.colour = "black",
                               frame.colour = "black")) +
  theme(axis.title = element_blank(),
        panel.border = element_blank(),
        legend.position = "top",
        legend.box.spacing = unit(0, "pt")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_sf(crs = "ESRI:54009")

subplot <- function(mpa, wdpaid, dist = 150000) {

  if(wdpaid == "220201") {
    mpa <- st_transform(mpa, crs = "EPSG:2782") %>%
      group_by(name) %>%
      summarize(a = 1)
    interpolated_catch <- project(interpolated_catch, "EPSG:2782")
  }

  # browser()
  dist_km <- dist * 1.854
  buffered_mpa <- st_buffer(mpa, dist = dist_km)

  interpolated_catch <- crop(interpolated_catch, buffered_mpa, touches = T)
  buffered_mpa <- st_buffer(mpa, dist = 100000 * 1.854)
  # coast <- st_crop(coast, buffered_mpa)
  # coastline <- st_crop(coastline, buffered_mpa)

  plot <- ggplot() +
    geom_spatraster(data = log(interpolated_catch),
                    aes(fill = sum)) +
    geom_sf(data = mpa,
            fill = mpa_col,
            color = "black",
            linewidth = 1) +
    geom_sf(data = mpa,
            fill = mpa_col,
            color = mpa_col) +
    geom_sf(data = buffered_mpa,
            fill = "transparent",
            color = "black") +
    scale_fill_gradientn(colors = blues,
                         na.value = "transparent")  +
    theme_void() +
    theme(legend.position = "None")

  return(plot)
}

subplots <- mpas %>%
  group_by(wdpaid) %>%
  nest() %$%
  map2(data, wdpaid, subplot)


left <- plot_grid(plotlist = subplots[1:2], ncol = 1, align = "hv", labels = c("b", "d"))
right <- plot_grid(plotlist = subplots[3:4], ncol = 1, align = "hv", labels = c("c", "e"))
bottom <- plot_grid(plotlist = subplots[5:14], nrow = 2, align = "hv", labels = letters[(5:14) + 1])

panel <- plot_grid(
  plot_grid(map, left, right, ncol = 3,
            rel_widths = c(4, 1, 1),
            align = "hv", axis = "t",
            labels = "a", label_y = 0.9),
  bottom,
  rel_heights = c(3, 2),
  ncol = 1,
  align = "hv",
  axis = "t"
)



## EXPORT ######################################################################

# X -----------------------------------------------------------------------------
startR::lazy_ggsave(
  plot = panel,
  filename = "fig1_map",
  width = 18,
  height = 15
)

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

# Globally
# alluvial_data2 <- annual_panel %>%
#   filter(gear %in% c("purse_seine", "longline"),
#          year >= 2018) %>%
#   mutate(name = ifelse(dist <= 600, name, "No MPA")) %>%
#   replace_na(replace = list(name = "No MPA",
#                             flag = "No flag")) %>%
#   select(name, gear, flag, contains("mt"), -tot_mt) %>%
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
#
# ggplot(data = alluvial_data2,
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
#                               "Flag"),
#                    expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
#   labs(x = "",
#        y = "% Total tuna caught") +
#   theme_bw()
