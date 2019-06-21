#Metabolism calculations

library(tidyverse);library(lubridate);library(LakeMetabolizer)
source("metabolisme calc func.R")
Sys.setenv(TZ="GMT")

raw_data_2018 <- readRDS(paste0(getwd(), "/Output/meta_2018.rds"))

ilt_sens_depth <- 0.7

prep_data_2018 <- raw_data_2018 %>% 
  mutate(dosat = o2.at.sat.base(wtr),
         #zmix = 1,
         wnd_10 = wind.scale.base(wnd, 2.5),
         k600 = k.vachon.base(wnd_10, 9.15*10^6),
         #k600 = k.cole.base(wnd_10),
         kgas = k600.2.kGAS.base(k600, wtr, "O2")/24/6,
         dummy = ifelse(zmix < ilt_sens_depth, 0, 1),
         date = as_date(DateTime_UTC)) %>% 
  select(date, DateTime_UTC, doobs, dosat, kgas, zmix, lux = par, wtr, dummy)

prep_data_2018 %>% 
  gather(variable, value, -DateTime_UTC) %>% 
  ggplot(aes(DateTime_UTC, value))+
  geom_line()+
  facet_grid(variable~., scales = "free")+
  theme_bw()

metab_2018 <- prep_data_2018 %>% 
  #mutate(doobs = rollmean(doobs, 12, align = "center", na.pad = TRUE)) %>% 
  mutate(date_round = round_date(date, "3 days")) %>% 
  select(-date) %>% 
  nest(-date_round) %>% 
  mutate(metab_mle = map(data, ~metab_calc(.x))) %>% 
  mutate(metab_result = map(metab_mle, ~ as.data.frame(.x[1])),
         metab_pred = map(metab_mle, ~ as.data.frame(.x[2])))

metab_2018 %>% 
  unnest(metab_pred) %>% 
  ggplot(aes(DateTime_UTC))+
  geom_line(aes(y = doobs), col = "red")+
  geom_line(aes(y=dopred), col = "blue")

metab_2018 %>% 
  unnest(metab_result) %>% View()
