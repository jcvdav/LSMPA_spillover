

rfmo <- readRDS(file = "data/processed/quarterly_all_rfmos.rds")

coast <- rnaturalearth::ne_coastline(returnclass = "sf")

rfmo %>%
  group_by(lat, lon) %>%
  summarize(tot_mt = sum(tot_mt, na.rm = T)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  ggplot() +
  geom_sf(aes(color = tot_mt), size = 0.5) +
  geom_sf(data = coast) +
  theme_void()

