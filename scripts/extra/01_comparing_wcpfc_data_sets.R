
ps_tuna <-
  read_csv(file = here("data","raw","RFMO_data","WCPFC","WCPFC_S_PUBLIC_BY_FLAG_YEAR_7",
                       "WCPFC_S_PUBLIC_BY_FLAG_YEAR.csv")) %>%
  clean_names() %>%
  filter(yy >= 1992) %>%
  mutate(year = yy,
         gear = "purse_seine",
         tot_mt = 1,
         effort = sets_una + sets_log + sets_dfad + sets_afad + sets_oth,
         skj_mt = skj_c_una + skj_c_log + skj_c_dfad + skj_c_afad + skj_c_oth,
         yft_mt = yft_c_una + yft_c_log + yft_c_dfad + yft_c_afad + yft_c_oth,
         bet_mt = bet_c_una + bet_c_log + bet_c_dfad + bet_c_afad + bet_c_oth) %>%
  check_mt_e() %>%
  mutate(frequency = "year")


q_ps_tuna <-
  read_csv(file = here("data","raw","RFMO_data","WCPFC","WCPFC_S_PUBLIC_BY_1x1_QTR_FLAG_3",
                       "WCPFC_S_PUBLIC_BY_1x1_QTR_FLAG.csv")) %>%
  clean_names() %>%
  filter(yy >= 1992) %>%
  mutate(year = yy,
         gear = "purse_seine",
         tot_mt = 1,
         effort = sets_una + sets_log + sets_dfad + sets_afad + sets_oth,
         skj_mt = skj_c_una + skj_c_log + skj_c_dfad + skj_c_afad + skj_c_oth,
         yft_mt = yft_c_una + yft_c_log + yft_c_dfad + yft_c_afad + yft_c_oth,
         bet_mt = bet_c_una + bet_c_log + bet_c_dfad + bet_c_afad + bet_c_oth) %>%
  check_mt_e()  %>%
  mutate(frequency = "qtr")


ll_tuna <-
  read_csv(file = here("data","raw","RFMO_data","WCPFC","WCPFC_L_PUBLIC_BY_FLAG_YR_7",
                       "WCPFC_L_PUBLIC_BY_FLAG_YR.csv")) %>%
  clean_names() %>%
  filter(yy >= 1992) %>%
  mutate(year = yy,
         gear = "longline",
         effort = hhooks * 100,
         tot_mt = 1,
         alb_mt = alb_c,
         yft_mt = yft_c,
         bet_mt = bet_c) %>%
  check_mt_e()  %>%
  mutate(frequency = "year")


q_ll_tuna <-
  read_csv(file = here("data","raw","RFMO_data","WCPFC","WCPFC_L_PUBLIC_BY_FLAG_QTR_8",
                       "WCPFC_L_PUBLIC_BY_FLAG_QTR.csv")) %>%
  clean_names() %>%
  filter(yy >= 1992) %>%
  mutate(year = yy,
         gear = "longline",
         effort = hhooks * 100,
         tot_mt = 1,
         alb_mt = alb_c,
         yft_mt = yft_c,
         bet_mt = bet_c) %>%
  check_mt_e()  %>%
  mutate(frequency = "qtr")

m_ll_tuna <-
  read_csv(file = here("data","raw","RFMO_data","WCPFC","WCPFC_L_PUBLIC_BY_FLAG_MON_5",
                       "WCPFC_L_PUBLIC_BY_FLAG_MON.csv")) %>%
  clean_names() %>%
  filter(yy >= 1992) %>%
  mutate(year = yy,
         gear = "longline",
         effort = hhooks * 100,
         tot_mt = 1,
         alb_mt = alb_c,
         yft_mt = yft_c,
         bet_mt = bet_c) %>%
  check_mt_e() %>%
  mutate(frequency = "month")

bind_rows(ps_tuna,
          q_ps_tuna,
          ll_tuna,
          q_ll_tuna,
          m_ll_tuna) %>%
  janitor::adorn_totals("col") %>%
  mutate(Total = Total - effort,
         effort = effort / 1000) %>%
  select(gear, frequency, effort, contains("mt"), Total) %>%
  kableExtra::kable(format = "latex", booktabs = T)
