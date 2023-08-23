

wcpfc <- annual_panel_raw %>% filter(wdpaid == "555624172", gear == "longline", rfmo == "wcpfc") %>% select(rfmo, year, flag, lat, lon, effort, tot_mt) %>% mutate(effort = trunc(effort), tot_mt = trunc(tot_mt))
iattc <- annual_panel_raw %>% filter(wdpaid == "555624172", gear == "longline", rfmo == "iattc") %>%  select(rfmo, year, flag, lat, lon, effort, tot_mt) %>% mutate(effort = trunc(effort), tot_mt = trunc(tot_mt))
a <- inner_join(wcpfc, iattc, by = c("year", "flag", "lat", "lon")) %>% arrange(year, lat, lon)
b <- bind_rows(wcpfc, iattc)
