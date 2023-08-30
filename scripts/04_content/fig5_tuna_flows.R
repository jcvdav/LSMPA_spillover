library(ggalluvial)

alluvial_data <- annual_panel %>%
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
  group_by(name, gear, flag, spp) %>%
  summarize(mt = sum(mt, na.rm = T)) %>%
  ungroup() %>%
  mutate(spp = str_remove(spp, "_mt"),
         pct_mt = mt / sum(mt, na.rm = T)) %>%
  mutate(name = fct_reorder(name, pct_mt, sum),
         spp = fct_reorder(spp, pct_mt, sum),
         gear = fct_reorder(gear, pct_mt, sum),
         flag = fct_reorder(flag, pct_mt, sum))

p <- ggplot(data = alluvial_data,
       mapping = aes(y = pct_mt,
                     axis1 = name,
                     axis2 = flag,
                     axis3 = spp,
                     fill = gear)) +
  geom_alluvium() +
  geom_stratum(fill = "white", width = 0.3, size = 0.3) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            size = 2) +
  scale_x_discrete(limits = c("MPA",
                              "Flag",
                              "Species"),
                   expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
  scale_fill_manual(values = gear_palette) +
  labs(x = "",
       y = "% Total tuna caught") +
  theme_bw() +
  theme(legend.position = "None")


startR::lazy_ggsave(p,
                    filename = "fig_5_tuna_flows",
                    width = 18)
