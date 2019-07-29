#Metabolism calculations

library(tidyverse);library(lubridate);library(LakeMetabolizer)
source("Metabolism func.R")
Sys.setenv(TZ="GMT")

raw_data_2018 <- readRDS(paste0(getwd(), "/Output/meta_2018.rds"))

ilt_sens_depth <- 0.7

event <- ymd_hm("2018-07-28 00:00")

prep_data_2018 <- raw_data_2018 %>% 
  mutate(dosat = o2.at.sat.base(wtr),
         #zmix = ifelse(zmix == 2.7, 1.1, zmix),
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

prep_data_2018_after <- prep_data_2018 %>% 
  filter(DateTime_UTC > event)

prep_data_2018_before <- prep_data_2018 %>% 
  filter(DateTime_UTC < event, DateTime_UTC > ymd_hm("2018-07-02 00:00"))

metab_2018 <- prep_data_2018_before %>% 
  #mutate(doobs = rollmean(doobs, 12, align = "center", na.pad = TRUE)) %>% 
  mutate(date_round = round_date(date, "1 days")) %>% 
  select(-date) %>% 
  nest(-date_round) %>% 
  mutate(metab_mle = map(data, ~metab_calc(.x))) %>% 
  mutate(metab_result = map(metab_mle, ~ as.data.frame(.x[1])),
         metab_pred = map(metab_mle, ~ as.data.frame(.x[2])))

metab_2018 %>% 
  unnest(metab_pred) %>% 
  ggplot(aes(DateTime_UTC))+
  geom_line(aes(y = doobs), col = "red")+
  geom_line(aes(y = dopred), col = "blue")+
  geom_vline(xintercept = event)

metab_2018 %>% 
  unnest(metab_result) %>% View()

event_doinit <- prep_data_2018_after$doobs[1]

forecast_df <- metab_2018 %>% 
  mutate(forecast = map(metab_result, ~doforecast(.x, doinit=event_doinit, datain=prep_data_2018_after))) %>% 
  unnest(forecast)

forecast_df_mean <- forecast_df %>% 
  group_by(DateTime_UTC) %>% 
  summarise(mean = mean(dopred))
#write.csv(forecast_df_mean, paste0(getwd(), "/Output/filso_ilt_mean_scenario.csv"), row.names = FALSE)

dopred_before <- metab_2018 %>% 
  unnest(metab_pred)

filso_ilt_plot <- ggplot()+
  geom_line(data=forecast_df, aes(DateTime_UTC, dopred, group=(date_round)), alpha = 0.25, col = "red")+
  geom_point(data=dopred_before, aes(DateTime_UTC, doobs), shape = 1)+
  geom_point(data=prep_data_2018_after, aes(DateTime_UTC, doobs), shape = 1)+
  geom_line(data=dopred_before, aes(DateTime_UTC, dopred), col = "red", size = 1.5)+
  geom_line(data=forecast_df_mean, aes(DateTime_UTC, mean), col = "red", size = 1.5)+
  ylab(expression("Dissolved oxygen (mg L"^{-1}*")"))+
  xlab("Dato")+
  geom_vline(xintercept = event, linetype = 2)+
  theme_classic()

ggsave(paste0(getwd(), "/Output/filso_ilt_plot.png"), filso_ilt_plot, width = 7, height = 5)
