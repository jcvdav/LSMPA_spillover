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
  tidyverse,
  fixest,
  ggfixest
)

# Load data --------------------------------------------------------------------
most_relevant_panel <- readRDS(file = here("data", "processed", "annual_relevant_mpa_gears_estimation_panel.rds"))

## PROCESSING ##################################################################
# Base event-study with no FE's
base <- feols(log(cpue_tot) ~ event_fct + near + i(event, near, -1),
              data = most_relevant_panel %>%
                mutate(event_fct = as.factor(event),
                       event_fct = fct_relevel(event_fct, "-1")),
              vcov = "iid",
              subset = ~gear == "purse_seine")

# Stepwise adding FEs
mod_sw <- feols(log(cpue_tot) ~ i(event, near, -1) | csw(id + event, flag,  year ^ wdpaid),
                data = most_relevant_panel,
                vcov = vcov_conley(cutoff = 200),
                subset = ~gear == "purse_seine")

# Manually build a data frame of coefficients-----------------------------------
# First for the base approach
base_coefs <- base %>%
  ggfixest:::iplot_data(object = ., .aggr_es = "both") %>%
  mutate(fixef = "none")

# Now for the ones we build step-wise
sw_coefs <- mod_sw %>%
  map_dfr(~ggfixest:::iplot_data(object = ., .aggr_es = "both"), .id = "fixef") %>%
  mutate(fixef = str_remove_all(fixef, "fixef: "))

# combine them all
all_coefs <- bind_rows(base_coefs,
                       sw_coefs) %>%
  mutate(fixef = str_replace(fixef, "\\^", "-by-"),
         fixef = str_replace(fixef, "(wdpaid)", "mpa"),
         fixef = as_factor(fixef),
         fixef = fct_relevel(fixef,
                             "none"))

## VISUALIZE ###################################################################
# Build the figure -------------------------------------------------------------
p <- ggplot(data = all_coefs,
       aes(x = x,
           y = y,
           group = fixef,
           color = fixef)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = -1, linetype = "dashed") +
  geom_pointrange(aes(ymin = ci_low,
                      ymax = ci_high,
                      shape = fixef),
                  fatten = 1,
                  size = 2,
                  position = position_dodge(width = 0.75),
                  linewidth = 0.5,
                  alpha = 0.75) +
  geom_line(#data = . %>% filter(fixef %in% c("none", "id + event + flag + year-by-mpa")),
            aes(x = x, y = aggr_eff),
            linewidth = 1) +
  guides(color = guide_legend(override.aes = list(fatten = 0,
                                                  size = 1),
                              nrow = 2)) +
  scale_color_brewer(palette = "Set2") +
  scale_shape_manual(values = c(16, 1, 2, 15)) +
  theme_linedraw() +
  theme(legend.position = "bottom") +
  labs(x = "Time-to-treatment (0 is year of enforcement)",
       y = "Estimate ± 95% CI",
       color = "Specification",
       shape = "Specification")


startR::lazy_ggsave(plot = p,
                    filename = "figS_event_study",
                    width = 15,
                    height = 10)


