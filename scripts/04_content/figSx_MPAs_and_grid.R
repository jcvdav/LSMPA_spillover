
rfmo_grid <- readRDS(file = here("data", "processed", "distance_grid.rds"))

map_100 <- ggplot() +
  geom_point(data = rfmo_grid %>% drop_na(near_100),
             aes(x = lon, y = lat, color = factor(near_100))) +
  geom_sf(data = lmpas, fill = "blue") +
  theme(legend.position = "None")
map_300 <- ggplot() +
  geom_point(data = rfmo_grid %>%
               drop_na(near_300),
             aes(x = lon, y = lat, color = factor(near_300))) +
  geom_sf(data = lmpas, fill = "blue") +
  theme(legend.position = "None")

maps <- cowplot::plot_grid(map_100, map_300, ncol = 1)

startR::lazy_ggsave(
  plot = maps,
  filename = "MPAs_and_grid",
  width = 10,
  height = 10
)
