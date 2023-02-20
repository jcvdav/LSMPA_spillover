################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# # replicating broeder et al: https://www.int-res.com/articles/feature/m585p001.pdf
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  sf,
  units,
  tidyverse,
  fixest
)

theme_set(theme_minimal())

# Load data --------------------------------------------------------------------
# LMPAs
mpas <- st_read(dsn = here("data", "processed", "clean_lmpas.gpkg"))

# CPUE data
data <- readRDS(here("data", "processed", "quarterly_all_rfmos.rds"))

grid <- readRDS(here("data", "processed", "distance_grid.rds")) %>%
  mutate(year_enforced = ifelse(wdpaid == "11753", 1998, year_enforced)) %>%
  filter(year_enforced <= 2018) %>%
  drop_na(year_enforced)

# Build a hotspot gtid matching Fig 1 of Broeder et al (hashed blue squares)
hotspot_grid_cells <- tibble(
  lat = c(1.5, 1.5, 1.5,
          0.5, 0.5, 0.5, 0.5, 0.5,
          -0.5, -0.5, -0.5, -0.5, -0.5,
          -1.5, -1.5, -1.5, -1.5, -1.5,
          -2.5, -2.5, -2.5),
  lon = c(-95.5, -94.5, -93.5,
          -96.5, -95.5, -94.5, -93.5, -92.5,
          -96.5, -95.5, -94.5, -93.5, -92.5,
          -94.5, -93.5, -92.5, -91.5, -90.5,
          -93.5, -92.5, -91.5),
  group = "hotspot",
  n_cel = 21
)

## PROCESSING ##################################################################

# Galapagos boundary -----------------------------------------------------------
gal <- mpas %>%
  filter(wdpaid == "11753") %>%
  select(wdpaid) %>%
  st_union()


# Galapagos CPUE data ----------------------------------------------------------
gal_data <- data %>%
  filter(rfmo == "iattc",
         flag == "ECU",
         gear == "purse_seine",
         between(lat, -6, 6),
         between(lon, -97, -85))

replication_data <- gal_data %>%
  left_join(hotspot_grid_cells, by = c("lat", "lon")) %>%
  replace_na(replace = list(group = "others",
                            n_cel = (12 * 12) - 21))

pts <- replication_data %>%
  filter(year <= 1989) %>%
  group_by(lat, lon) %>%
  summarize(effort = sum(effort, na.rm = T),
            catch = sum(tot_mt), na.rm = T) %>%
  ungroup() %>%
  complete(lon, lat, fill = list(catch = 0, effort = 0)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

ggplot() +
  geom_sf(data = gal, fill = "steelblue", color = "black") +
  geom_tile(data = hotspot_grid_cells, aes(x = lon, y = lat), fill = "lightblue", alpha = 0.5) +
  geom_sf(data = pts, aes(size = effort, color = catch)) +
  theme_bw()


ts <- replication_data %>%
  filter(between(year, 1990, 2015)) %>%
  group_by(year, group, n_cel) %>%
  summarize(effort = sum(effort, na.rm = T),
            tot_mt = sum(tot_mt, na.rm = T)) %>%
  ungroup() %>%
  mutate(cpue = tot_mt / effort)

d <- ggplot(data = ts, aes(x = year, y = log(tot_mt / n_cel), color = group)) +
  geom_line() +
  geom_vline(xintercept = 1998) +
  geom_vline(xintercept = 2003) +
  geom_smooth(linetype = "dashed") +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  theme(legend.position = "None")

e <- ggplot(data = ts, aes(x = year, y = log(effort / n_cel), color = group)) +
  geom_line() +
  geom_vline(xintercept = 1998) +
  geom_vline(xintercept = 2003) +
  geom_smooth(linetype = "dashed") +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  theme(legend.position = "None")

f <- ggplot(data = ts, aes(x = year, y = log(cpue), color = group)) +
  geom_line() +
  geom_vline(xintercept = 1998) +
  geom_vline(xintercept = 2003) +
  geom_smooth(linetype = "dashed") +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  theme(legend.position = c(1, 0),
        legend.justification = c(1, 0),
        legend.background = element_blank())


cowplot::plot_grid(d, e, f, ncol = 1)

# Noe extend it
pts <- replication_data %>%
  select(lat, lon) %>%
  distinct() %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  mutate(dist = st_distance(geometry, gal),
         dist = set_units(x = dist, value = "nautical_miles"),
         dist = as.numeric(dist),
         dist_bin = case_when(dist <= 100 ~ "0-100",
                              (dist > 100 & dist <= 200) ~ "100-200",
                              (dist > 200 & dist <= 300) ~ "200-300")) %>%
  filter(as.numeric(dist) > 0) %>%
  bind_cols(st_coordinates(.)) %>%
  st_drop_geometry() %>%
  rename(lon = X, lat = Y) %>%
  drop_na()

year_imp <- 1998

panel <- replication_data %>%
  filter(between(year, 1988, 2020)) %>%
  group_by(
    year,
    qtr,
    lat,
    lon) %>%
  summarize(effort = sum(effort, na.rm = T),
            tot_mt = sum(tot_mt, na.rm = T)) %>%
  ungroup() %>%
  mutate(cpue = tot_mt / effort) %>%
  group_by(lat, lon) %>%
  mutate(norm_cpue = (cpue - mean(cpue[year < year_imp])) / sd(cpue[year < year_imp])) %>%
  ungroup() %>%
  inner_join(pts, by = c("lon", "lat")) %>%
  filter(dist <= 200,
         dist > 0) %>%
  mutate(dist = -1 * (dist / 100),
         post = 1 *(year >= year_imp),
         treated = 1 * (dist_bin == "0-100"),
         id = paste(lat, lon, sep = "_"))

list(feols(norm_cpue ~ treated + post + post:treated, data = panel, cluster = ~id),
     feols(norm_cpue ~ dist * post, data = panel, cluster = ~id)) %>%
  modelsummary::modelsummary(stars =T,
                             coef_rename = c("post" = "Post",
                                             "treated" = "Treated",
                                             "dist" = "Treated"))


### Now properly

range <- 10

our_data <- data %>%
  filter(rfmo == "iattc",
         flag == "ECU",
         gear == "purse_seine") %>%
  select(year, qtr, lat, lon, cpue_tot) %>%
  left_join(grid, by = c("lat", "lon")) %>%
  # filter(dist <= 200) %>%
  filter(wdpaid == "11753") %>%
  filter(year >= (year_enforced - range)) %>%
  group_by(lat, lon) %>%
  mutate(norm_cpue = (cpue_tot - mean(cpue_tot[year < year_enforced])) / sd(cpue_tot[year < year_enforced])) %>%
  ungroup() %>%
  mutate(dist = -1 * (dist / 100),
         dist_bin = as.character(trunc(dist) - 1),
         dist_bin = fct_reorder(dist_bin, dist),
         post = 1 *(year >= year_enforced),
         id = paste(lat, lon, sep = "_"),
         event = year - year_enforced) %>%
  drop_na(norm_cpue)


feols(norm_cpue ~ treatment_100*post | qtr, data = our_data, cluster = ~id, subset = ~!is.na(treatment_100)) %>%
  broom::tidy() %>%
  filter(str_detect(term, ":post")) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error)) +
  coord_flip()



