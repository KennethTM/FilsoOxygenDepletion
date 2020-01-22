#Metabolism calculations

library(tidyverse);library(lubridate);library(LakeMetabolizer)
source("Metabolism func.R")
Sys.setenv(TZ="GMT")
Sys.setlocale("LC_TIME", "US")

theme_pub <- theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text = element_text(colour = "black"), 
        strip.background = element_rect(fill = "white"))
theme_set(theme_pub)
  
raw_data_2018 <- readRDS(paste0(getwd(), "/Output/meta_2018.rds"))

ilt_sens_depth <- 0.7

event <- ymd_hm("2018-07-28 00:00")
event_end <- ymd_hm("2018-08-12 23:50")

prep_data_2018 <- raw_data_2018 %>% 
  mutate(dosat = o2.at.sat.base(wtr),
         #zmix = ifelse(zmix == 2.7, 1.1, zmix),
         wnd_10 = wind.scale.base(wnd, 2.5),
         k600 = k.vachon.base(wnd_10, 4223445),
         #k600 = k.cole.base(wnd_10),
         kgas = k600.2.kGAS.base(k600, wtr, "O2")/24/6,
         #dummy = ifelse(zmix < ilt_sens_depth, 0, 1),
         dummy = 1,
         date = as_date(DateTime_UTC)) %>% 
  dplyr::select(date, DateTime_UTC, doobs, dosat, kgas, zmix = zmean, lux = par, wtr, dummy)

prep_data_2018 %>% 
  gather(variable, value, -DateTime_UTC) %>% 
  ggplot(aes(DateTime_UTC, value))+
  geom_line()+
  facet_grid(variable~., scales = "free")+
  theme_bw()

# prep_data_2018_after <- prep_data_2018 %>% 
#   filter(DateTime_UTC > event)
# 
# prep_data_2018_before <- prep_data_2018 %>% 
#   filter(DateTime_UTC < event, DateTime_UTC > ymd_hm("2018-07-02 00:00"))

prep_data_2018_event <- prep_data_2018 %>% 
  filter(between(DateTime_UTC, event, event_end))

prep_data_2018_not_event <- prep_data_2018 %>% 
  filter(!between(DateTime_UTC, event, event_end), 
         DateTime_UTC >= ymd_hm("2018-06-07 00:00"),
         !(DateTime_UTC == ymd_hm("2018-09-01 00:00")))

#library(zoo)
metab_2018 <- prep_data_2018_not_event %>% 
  #mutate(doobs = rollmean(doobs, 12, align = "right", na.pad = TRUE)) %>% 
  mutate(date_round = round_date(date, "1 days")) %>% 
  dplyr::select(-date) %>% 
  nest(data = c(DateTime_UTC, doobs, dosat, kgas, zmix, lux, wtr, dummy)) %>% 
  mutate(metab_mle = map(data, ~metab_calc(.x))) %>% 
  mutate(metab_result = map(metab_mle, ~ as.data.frame(.x[1])),
         metab_pred = map(metab_mle, ~ as.data.frame(.x[2])))

metab_2018 %>% 
  unnest(metab_pred) %>% 
  ggplot(aes(DateTime_UTC))+
  geom_line(aes(y = doobs), col = "red")+
  geom_line(aes(y = dopred), col = "blue")+
  geom_vline(xintercept = event)

#metab_2018 %>% 
#  unnest(metab_result) %>% View()

# metab_coefs <- metab_2018 %>% 
#   unnest(metab_result) %>% 
#   dplyr::select(date_round, gppcoef, rcoef, r_spear) %>% 
#   filter(r_spear > 0.5)
# 
# metab_coefs %>% 
#   gather(variable, value, -date_round, -r_spear) %>% 
#   ggplot(aes(x=value,y=..density..))+
#   geom_histogram()+
#   geom_density()+
#   facet_wrap(variable~., scales = "free")

#Fit distribution to coefs og træk 100 random tal ud som scenarier, lav mean og plot det hele
# library(fitdistrplus)
# fit_gppcoef <- fitdist(metab_coefs$gppcoef, "gamma")
# denscomp(list(fit_gppcoef))
# 
# fit_rcoef <- fitdist(metab_coefs$rcoef, "gamma")
# denscomp(list(fit_rcoef))

# #simuler ilt
# sim_values <- data.frame(sim_n = 1:100,
#                          gppcoef = rgamma(100, shape = fit_gppcoef$estimate[["shape"]], rate = fit_gppcoef$estimate[["rate"]]),
#                          rcoef = rgamma(100, shape = fit_rcoef$estimate[["shape"]], rate = fit_rcoef$estimate[["rate"]]))
# 
# 
# sim_scenarios <- sim_values %>% 
#   nest(data = c(gppcoef, rcoef)) %>% 
#   mutate(sim_oxygen = map(data, ~doforecast(.x, doinit=event_doinit, datain=prep_data_2018_event))) %>% 
#   unnest(sim_oxygen)
#   
# sim_mean_scenario <- sim_scenarios %>% 
#   group_by(DateTime_UTC) %>% 
#   summarise(dopred_mean = mean(dopred))

event_doinit <- prep_data_2018 %>%
  filter(DateTime_UTC == event) %>%
  pull(doobs)

forecast_df <- metab_2018 %>%
  mutate(forecast = map(metab_result, ~doforecast(.x, doinit=event_doinit, datain=prep_data_2018_event, gpp_scale = 1, r_scale = 1))) %>%
  unnest(forecast)

forecast_df_quantile <- forecast_df %>% 
  select(DateTime_UTC, dopred) %>% 
  group_by(DateTime_UTC) %>% 
  summarise(low = quantile(dopred, 0.025),
            high = quantile(dopred, 0.975),
            mean = mean(dopred))
  # summarise(mean = mean(dopred),
  #           t_error = qt(0.975, df = 70)*sd(dopred)/sqrt(70)) %>% 
  # mutate(low = mean - t_error,
  #        high = mean +  t_error)


# forecast_df_mean <- forecast_df %>%
#   group_by(DateTime_UTC) %>%
#   summarise(mean = mean(dopred))

prep_data_2018_event_minus_visit <- prep_data_2018_event %>% 
  mutate(doobs = ifelse(between(DateTime_UTC, ymd_hm("2018-08-06 12:00"), ymd_hm("2018-08-07 12:00")), NA, doobs))

metab_2018 %>% 
  unnest(metab_pred) %>% 
  right_join(data.frame(DateTime_UTC = seq(ymd_hm("2018-06-07 00:00"),
                                           ymd_hm("2018-09-01 00:00"),
                                           "10 mins"))) %>% 
  ggplot(aes(DateTime_UTC))+
  geom_line(aes(y = doobs, col = "Observed"))+
  geom_line(aes(y = dopred, col = "Modelled"))+
  scale_color_manual(values = c("orange", "black"))+
  #geom_line(data = forecast_df, aes(DateTime_UTC, dopred, group=date_round), alpha = 0.2, col = "grey")+
  geom_ribbon(data = forecast_df_quantile, aes(x=DateTime_UTC, ymin=low, ymax=high), fill = "grey",  alpha = 0.4)+
  geom_line(data = prep_data_2018_event_minus_visit, aes(DateTime_UTC, doobs))+
  geom_line(data = forecast_df_quantile, aes(DateTime_UTC, mean), col = "orange")+
  ylab(expression("Dissolved oxygen (mg L"^{-1}*")"))+
  xlab(NULL)+
  geom_vline(xintercept = event, linetype = 2)+
  scale_y_continuous(limits = c(-0.5, 19), expand = c(0, 0.2))+
  scale_x_datetime(date_labels = "%b", limits = c(ymd_hm("2018-06-01 00:00"), ymd_hm("2018-09-01 00:00")))+
  theme(legend.title = element_blank(),
        legend.position = c(0.1, 0.9))

ggsave(paste0(getwd(), "/Output/filso_ilt_obs_pred.png"), height = 120, width = 174, units = "mm")
