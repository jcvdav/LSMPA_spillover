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

# Load data --------------------------------------------------------------------
panel <- readRDS(file = here("data", "processed", "annual_relevant_mpa_gears_estimation_panel.rds")) %>%
  filter(gear == "purse_seine")


imp_panel <- panel %>%
  select(wdpaid, name, year_enforced, month_enforced) %>%
  distinct() %>%
  mutate(year_enforced = lubridate::ym(paste(year_enforced, month_enforced)))

monthly_oni <- readRDS(file = here("data", "raw", "oni", "monthly_oni.rds")) %>%
  mutate(ym = lubridate::ym(paste(year, month))) %>%
  filter(ym > min(imp_panel$year_enforced))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
ggplot() +
  geom_line(data = monthly_oni, aes(x = ym, y = oni, color = oni), linewidth = 2) +
  geom_vline(data = imp_panel,
             mapping = aes(xintercept = year_enforced),
             linetype = "dashed",
             linewidth = 1) +
  scale_color_viridis_c(option = "A")

# X ----------------------------------------------------------------------------
panel %>%
  select(wdpaid, name, year_enforced, event, post, contains("oni")) %>%
  distinct() %>%
  mutate(name = fct_reorder(name, year_enforced)) %>%
  ggplot(aes(x = event, y = oni_avg)) +
  geom_smooth(aes(group = post),
              method = "lm") +
  geom_line() +
  geom_ribbon(aes(ymin = oni_m, ymax = oni_M),
              fill = "transparent",
              linetype = "dashed",
              color = "black") +
  facet_wrap(~name) +
  geom_vline(xintercept = 0) +
  ylim(c(-3, 3)) +
  scale_color_viridis_c(option = "A")

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
