#Rawdata processing. Read from /Rawdata folder and write processed output to /Output folder. 

library(tidyverse);library(lubridate);library(zoo)
Sys.setenv(TZ="GMT")

#Read raw data files. Clean files initially and save .rds file.
#Sensorer ved Filsø
raw_files <- list.files(paste0(getwd(), "/Rawdata"), full.names = TRUE)

oxygen_list <- lapply(raw_files[grep("*ilt", raw_files)], function(file){read.delim(file, sep = ";") %>% 
    tbl_df() %>% 
    set_names(c("DateTime_UTC", "wtr", "doobs", "dosat_perc")) %>% 
    mutate(DateTime_UTC = ymd_hms(DateTime_UTC))})

oxygen_df <- bind_rows("ilt_2017" = oxygen_list[[1]], "ilt_2018" = oxygen_list[[2]], .id = "source") %>% 
  mutate(DateTime_UTC = round_date(DateTime_UTC, "10 mins"))

vejrst_df <- read.csv(raw_files[grep("*vejrst.csv", raw_files)], skip = 2, header = FALSE,
         col.names = c("row", "DateTime_GMT2", "wind_dir", "par", "wnd", "wnd_gust")) %>% 
  tbl_df() %>% 
  mutate(DateTime_UTC = dmy_hms(DateTime_GMT2)-2*60*60-1) %>% 
  select(-row, -DateTime_GMT2) %>% 
  mutate(DateTime_UTC = round_date(DateTime_UTC, "10 mins"))

vejrst_2018_df <- read.csv(raw_files[grep("Vejrstation_Filsoe_18-09-12", raw_files)]) %>% 
  tbl_df() %>% 
  set_names(c("DateTime_GMT2", "atmpres", "wnd", "wnd_gust", "wnd_dir", "par", "rain", "airt", "rh")) %>% 
  mutate(DateTime_UTC = dmy_hms(DateTime_GMT2)-2*60*60,
         par = par-1.2)

par_uw_list <- lapply(raw_files[grep("par*", raw_files)], function(file){read.csv(file, skip = 9, header = FALSE) %>% 
    tbl_df() %>% 
    set_names(c("rownumber", "Date", "Time", "raw_value", "cal_value")) %>% #tidszone??
    mutate(DateTime_UTC = dmy_hms(paste(Date, Time))) %>% 
    select(DateTime_UTC, par_uw = cal_value)})

par_uw_df <- bind_rows("st2_2017_bot" = par_uw_list[[1]], 
                       "st2_2018_bot" = par_uw_list[[2]],
                       "st2_2017_top" = par_uw_list[[3]], 
                       "st2_2018_top" = par_uw_list[[4]], .id = "source")

#Tidligere filer med wtr temp data og indices fra filsø
wtr_list <- lapply(raw_files[grep("*rds", raw_files)], readRDS)
names(wtr_list) <- c("schmidt", "dif", "wtr", "zmix")

# #Vejr fra skjern enge
# #Kalibrer til Filsø data
# #Hvad er tidszone??
# vejr_skjern_raw_df <- read.delim2(raw_files[grep("*skjernenge.txt", raw_files)]) %>% 
#   tbl_df() %>% 
#   mutate(DateTime_UTC = dmy_hm(paste(date, time))-2*60*60) %>% 
#   select(DateTime_UTC, wind_speed, wind_dir) %>%
#   mutate(DateTime_UTC = round_date(DateTime_UTC, "10 mins")) %>% 
#   set_names("DateTime_UTC", "wnd_skjern", "wind_dir_skjern") %>% 
#   mutate(wnd_dir_skjern_cat = cut(wind_dir_skjern, c(0, 45, 135, 225, 315, 360), labels = c("n1", "e", "s", "v", "n2")),
#          wnd_dir_skjern_cat = ifelse(wnd_dir_skjern_cat %in% c("n1", "n2"), "n", wnd_dir_skjern_cat)) %>% 
#   na.omit()
# 
# vejr_skjern_filso <- left_join(vejr_skjern_raw_df, vejrst_df) %>% 
#   na.omit()
# 
# m1 <- lm(wnd ~ wnd_skjern, data = vejr_skjern_filso)
# m2 <- lm(wnd ~ wnd_skjern + wnd_dir_skjern_cat, data = vejr_skjern_filso)
# m3 <- lm(wnd ~ wnd_skjern + I(wnd_skjern^2), data = vejr_skjern_filso)
# m4 <- lm(wnd ~ wnd_skjern + I(wnd_skjern^2) + wnd_dir_skjern_cat, data = vejr_skjern_filso)
# AIC(m1, m2, m3, m4)
# summary(m2)
# 
# ggplot(vejr_skjern_filso, aes(wnd_skjern, wnd, col = wnd_dir_skjern_cat))+
#   geom_point()+
#   geom_smooth(method = "lm", formula = y~x-1)
# 
# filso_wnd_cor <- vejr_skjern_raw_df %>% 
#   mutate(wnd_cor = predict(m2, newdata = vejr_skjern_raw_df)) %>% 
#   select(DateTime_UTC, wnd_cor) %>% 
#   right_join(data.frame(DateTime_UTC = seq(min(vejr_skjern_raw_df$DateTime_UTC),
#                                            max(vejr_skjern_raw_df$DateTime_UTC),
#                                            "10 mins"))) %>% 
#   mutate(wnd_cor = na.approx(wnd_cor),
#          wnd_cor = ifelse(wnd_cor < 0, 0, wnd_cor))

#samle data til metabolisme
#DateTime_UTC, par, wtr, zmix, hvand, doobs, wnd

#zmax, zmix, lux fra hobo loggere ved st1
#Ingen lys målt ved vejrstation??
# st_1_lux <- wtr_list$wtr %>% 
#   filter(station == "st1", label == "wtr_0.7") %>% 
#   mutate(DateTime_UTC = datetime-2*60*60) %>% 
#   select(DateTime_UTC, lux, zmax)
# 
# wtr_list$wtr %>% 
#   filter(station == "st1") %>% 
#   ggplot(aes(datetime, lux, col = factor(depth)))+
#   geom_line()+
#   ylim(0, 1000)

st_1_zmix <- wtr_list$zmix %>% 
  filter(station == "st1") %>%
  mutate(DateTime_UTC = datetime-2*60*60) %>% 
  select(DateTime_UTC, zmix, zmax)
  
meta_data_2018 <- oxygen_df %>% 
  filter(source == "ilt_2018") %>% 
  right_join(data.frame(DateTime_UTC = seq(ymd_hm("2018-07-01 00:10"),
                                           ymd_hm("2018-08-05 21:50"),
                                           "10 mins"))) %>%
  mutate_at(vars(wtr, doobs), list(na.approx)) %>% 
  left_join(vejrst_2018_df) %>% 
  #left_join(filso_wnd_cor) %>% 
  #left_join(st_1_lux) %>% 
  left_join(st_1_zmix) %>% 
  select(DateTime_UTC, wtr, doobs, wnd, par, zmax, zmix)

saveRDS(meta_data_2018, 
        paste0(getwd(), "/Output/meta_2018.rds"))
