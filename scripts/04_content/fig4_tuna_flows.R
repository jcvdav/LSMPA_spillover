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
  ggalluvial,
  tidyverse
)

# Load data --------------------------------------------------------------------
most_relevant_panel <- readRDS(here("data", "processed", "annual_relevant_mpa_gears_estimation_panel.rds")) %>%
  mutate(name = short_name) %>%
  filter(gear == "purse_seine")

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
alluvial_data <- most_relevant_panel %>%
  filter(dist <= 200,
         event > 0) %>%
  select(name, gear, flag, year, contains("mt"), -tot_mt) %>%
  replace_na(replace = list(name = "No MPA",
                            flag = "No flag")) %>%
  group_by(name, gear, flag, year) %>%
  summarize_all(sum, na.rm = T) %>%
  group_by(name, gear, flag) %>%
  summarize_all(mean, na.rm = T) %>%
  ungroup() %>%
  pivot_longer(cols = contains("mt"), names_to = "spp", values_to = "mt") %>%
  ungroup() %>%
  filter(spp %in% c("skj_mt",
                    "yft_mt",
                    "bet_mt",
                    "alb_mt")) %>%
  group_by(name, gear, flag, spp) %>%
  summarize(mt = sum(mt, na.rm = T)) %>%
  ungroup() %>%
  mutate(spp = str_remove(spp, "_mt"),
         pct_mt = mt / sum(mt, na.rm = T))

pct_mpas <- alluvial_data %>%
  group_by(name) %>%
  summarize(pct = sum(pct_mt)) %>%
  arrange(desc(pct)) %>%
  mutate(cumsum_pct = cumsum(pct))

pct_flags <- alluvial_data %>%
  group_by(flag) %>%
  summarize(pct = sum(pct_mt)) %>%
  arrange(desc(pct)) %>%
  mutate(cumsum_pct = cumsum(pct))

grouped_alluvial_data <- alluvial_data %>%
  mutate(name = ifelse(name %in% head(pct_mpas$name, 4), name, "Other MPAs (N = 2)"),
         flag = ifelse(flag %in% head(pct_flags$flag, 10), flag, "Other flags (N = 16)"),
         spp = case_when(spp == "skj" ~ "Skipjack",
                         spp == "yft" ~ "Yellowfin",
                         spp == "bet" ~ "Bigeye",
                         spp == "alb" ~ "Albacore")) %>%
  mutate(name = fct_reorder(name, pct_mt, sum),
         spp = fct_reorder(spp, pct_mt, sum),
         gear = fct_reorder(gear, pct_mt, sum),
         flag = fct_reorder(flag, pct_mt, sum),
         name = fct_relevel(name, "Other MPAs (N = 2)"),
         flag = fct_relevel(flag, "Other flags (N = 16)"),
         gear = str_to_sentence(str_replace(gear, "_", " "))) %>%
  group_by(name, gear, flag, spp) %>%
  summarize(mt = sum(mt),
            pct_mt = sum(pct_mt))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
p <- ggplot(data = grouped_alluvial_data,
            mapping = aes(y = pct_mt,
                          axis1 = name,
                          axis2 = flag,
                          axis3 = spp,
                          fill = gear)) +
  geom_alluvium(alpha = 0.75, color = "black", linewidth = 0.1) +
  geom_stratum(fill = "white", width = 0.3, size = 0.3) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            size = 2) +
  scale_x_discrete(limits = c("LSMPA",
                              "Fishing nation",
                              "Species"),
                   expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
  scale_fill_manual(values = gear_palette) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  labs(x = "",
       y = "% Total tuna caught",
       fill = "Gear")+
  theme(legend.position = "None")

# Stats for text
grouped_alluvial_data %>%
  filter(name%in% c("Galápagos", "Revillagigedo"),
         gear == "Purse seine",
         spp %in% c("Skipjack", "Yellowfin")) %>%
  arrange(spp) %>%
  group_by(name, flag) %>%
  summarize(mt = sum(mt)) %>%
  group_by(name) %>% mutate(mpa_mt = sum(mt)) %>%
  ungroup() %>%
  mutate(pct_mt = mt / mpa_mt)

# X ----------------------------------------------------------------------------
startR::lazy_ggsave(p,
                    filename = "fig5_tuna_flows",
                    width = 18,
                    height = 12)
