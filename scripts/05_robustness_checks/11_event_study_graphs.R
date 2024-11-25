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

# Source custom functions ------------------------------------------------------
source(here("scripts/00_set_up.R"))

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
mod_fe <- feols(log(cpue_tot) ~ i(event, near, -1) | id + event + flag + year ^ wdpaid,
                data = most_relevant_panel,
                vcov = vcov_conley(cutoff = 200),
                subset = ~gear == "purse_seine")

# Manually build a data frame of coefficients-----------------------------------
# Fundction to add the reference category
add_ref <- function(x) {
  x %>%
    select(model) %>%
    distinct() %>%
    mutate(x = -1,
           y = 0,
           std.error = 0,
           aggr_eff = 0)
}

# First, get aggregate effects for each model
base_agg <- base %>%
  ggfixest:::aggr_es(period = "both") %>%
  select(period = term, aggr_eff = estimate)

fe_agg <- mod_fe %>%
  ggfixest:::aggr_es(period = "both") %>%
  select(period = term, aggr_eff = estimate)

# Now the coefficients
base_coefs <- base %>%
  broom::tidy() %>%
  filter(str_detect(term, ":")) %>%
  mutate(x = as.numeric(str_extract(term, "-?[:digit:]+")),
         period = ifelse(x < 0, "pre-treatment (mean)", "post-treatment (mean)"),
         y = estimate,
         model = "Without FEs") %>%
  left_join(base_agg, by = join_by(period)) %>% # add the agg effects
  bind_rows(add_ref(.))

# Now for the full FE specification

fe_coefs <- mod_fe %>%
  broom::tidy() %>%
  filter(str_detect(term, ":")) %>%
  mutate(x = as.numeric(str_extract(term, "-?[:digit:]+")),
         period = ifelse(x < 0, "pre-treatment (mean)", "post-treatment (mean)"),
         y = estimate,
         model = "With FEs") %>%
  left_join(fe_agg, by = join_by(period)) %>% # add the gg effects
  bind_rows(add_ref(.))

# combine them all
all_coefs <- bind_rows(base_coefs,
                       fe_coefs)

# ## VISUALIZE ###################################################################
# # Build the figure -------------------------------------------------------------
p <- ggplot(data = all_coefs,
             aes(x = x,
                 y = y,
                 group = model,
                 color = model)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = -1, linetype = "dashed") +
  geom_line(aes(y = aggr_eff),
            linewidth = 1) +
  geom_pointrange(aes(ymin = y - std.error,
                      ymax = y + std.error,
                      fill = model),
                  color = "black",
                  position = position_dodge(width = 0.25),
                  shape = 21) +
  guides(color = guide_legend(override.aes = list(fatten = 0,
                                                  size = 1))) +
  scale_fill_manual(values = fe_palette) +
  scale_color_manual(values = fe_palette) +
  guides(fill = guide_legend(override.aes = list(shape = 21))) +
  theme(legend.position = "bottom") +
  labs(x = "Time-to-treatment (0 is year of enforcement)",
       y = "Effect on CPUE",
       fill = "Specification",
       color = "Specification",
       shape = "Specification")


startR::lazy_ggsave(plot = p,
                    filename = "figS_event_study",
                    width = 15,
                    height = 10)


