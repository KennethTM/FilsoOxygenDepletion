#Rawdata processing
#Read from Rawdata folder and write to Output folder. 

source("libs_and_funcs.R")

#Read raw data files, clean initially and save to .rds file.
raw_files <- list.files(paste0(getwd(), "/Rawdata"), full.names = TRUE)

#Prepare oxygen/minidot data
#All oxygen data from station 1 with minidot in top and bottom

#Prepare a 2013 file which is in different format
raw_df_2013 <- read.delim2(paste0(getwd(), "/Rawdata/Sommer_ilt_raw/2013 data/22012014_50_excel.TXT"),
                           header = FALSE, stringsAsFactors = FALSE,
                           col.names = c("timestamp", "DateTime_UTC", "DateTime_CET", "wtr_doobs", "doobs", "dosat_perc")) %>%
  tbl_df() %>%
  mutate(DateTime_UTC = dmy_hm(DateTime_UTC),
         station = "st1",
         hob = 150) %>%
  select(station, hob, DateTime_UTC, wtr_doobs, doobs, dosat_perc)

#Prepare all other files, bind and save
minidot_path <- paste0(getwd(), "/Rawdata/Sommer_ilt_lvl1/")

raw_minidot_files <- list.files(minidot_path, full.names = TRUE, pattern = "*.TXT")

raw_list <- lapply(raw_minidot_files, read.csv, 
                   skip = 9, header = FALSE, stringsAsFactors = FALSE,
                   col.names = c("timestamp", "DateTime_UTC", "DateTime_CET", "wtr_doobs", "doobs", "dosat_perc", "q"))

names(raw_list) <- basename(raw_minidot_files)

raw_df <- bind_rows(raw_list, .id = "file") %>% 
  tbl_df() %>% 
  mutate(DateTime_UTC = ymd_hms(DateTime_UTC),
         hob = parse_number(str_sub(file, start = 10, end = -5)),
         station = "st1") %>% 
  select(station, hob, DateTime_UTC, wtr_doobs, doobs, dosat_perc) %>% 
  bind_rows(raw_df_2013) %>% 
  distinct()

saveRDS(raw_df, paste0(getwd(), "/Output/", "raw_df.rds"))

#Prepare data for metabolism modeling during 2018 summer
oxygen_2018_df <- raw_df %>% 
  filter(year(DateTime_UTC) == 2018 & hob == 150) %>% 
  mutate(DateTime_UTC = round_date(DateTime_UTC, "10 mins"))

#tidszone er korrekt her
vejrst_2018_df <- read.csv(raw_files[grep("Vejrstation_Filsoe_18-09-12", raw_files)]) %>% 
  tbl_df() %>% 
  set_names(c("DateTime_GMT2", "atmpres", "wnd", "wnd_gust", "wnd_dir", "par", "rain", "airt", "rh")) %>% 
  mutate(DateTime_UTC = dmy_hms(DateTime_GMT2)-2*60*60,
         par = par-1.2) %>% 
  select(-DateTime_GMT2)

par_uw_list <- lapply(raw_files[grep("par*", raw_files)], function(file){read.csv(file, skip = 9, header = FALSE) %>% 
    tbl_df() %>% 
    set_names(c("rownumber", "Date", "Time", "raw_value", "cal_value")) %>% #tidszone??
    mutate(DateTime_UTC = dmy_hms(paste(Date, Time))) %>% 
    select(DateTime_UTC, par_uw = cal_value)})

par_uw_df <- bind_rows("st2_2017_bot" = par_uw_list[[1]], 
                       "st2_2018_bot" = par_uw_list[[2]],
                       "st2_2017_top" = par_uw_list[[3]], 
                       "st2_2018_top" = par_uw_list[[4]], 
                      .id = "source")

par_uw_2018_df <- par_uw_df %>% 
  filter(source == "st2_2018_top")

depths_2018 <- read_xls(paste0(getwd(), "/Rawdata/filso_depths.xls"), sheet = 1, skip = 8) %>% 
  select(datetime = Tid...1, zmean = middeldybde, zmax = maksdybde) %>% 
  na.omit() %>% 
  mutate(DateTime_UTC = datetime - 2*60*60) %>% 
  select(-datetime)

saveRDS(depths_2018, paste0(getwd(), "/Rawdata/filso_depths.rds"))

depths_2018_interp <- depths_2018 %>% 
  right_join(data.frame(DateTime_UTC = seq(min(depths_2018$DateTime_UTC),
                                           max(depths_2018$DateTime_UTC),
                                           "10 mins"))) %>% 
  mutate_at(vars(zmean, zmax), funs(na.approx(., na.rm = FALSE)))

#Previously prepared files describing water temperature, schmidt stability, water temp. diff and zmix
###INDSÆT MARKDOWN DOKUMENT OG DATA FOR NEDENSTÅENDE DATA###
wtr_list <- lapply(raw_files[grep("*rds", raw_files)], readRDS)
names(wtr_list) <- c("depths", "schmidt", "dif", "wtr", "zmix")

zmix_2018 <- wtr_list$zmix %>% 
  filter(station == "st1") %>%
  mutate(DateTime_UTC = datetime-2*60*60) %>% 
  select(DateTime_UTC, zmix)

meta_data_2018 <- oxygen_2018_df %>% 
  right_join(data.frame(DateTime_UTC = seq(ymd_hm("2018-06-01 00:00"),
                                           ymd_hm("2018-09-01 00:00"),
                                           "10 mins"))) %>%
  mutate_at(vars(wtr_doobs, doobs), list(na.approx)) %>% 
  left_join(vejrst_2018_df) %>% 
  left_join(zmix_2018) %>% 
  left_join(depths_2018_interp) %>% 
  select(DateTime_UTC, wtr = wtr_doobs, doobs, wnd, par, zmax, zmean) %>% 
  na.omit()

saveRDS(meta_data_2018, paste0(getwd(), "/Output/meta_2018.rds"))


# #hvorfor lux fra overfladen er dårlig:
# wtr_list$wtr %>%
#   ggplot(aes(DateTime, lux))+
#   geom_line()+
#   facet_grid(factor(hob)~., scales = "free")+
#   theme_bw()
# 
# par_uw_2018_df %>% 
#   ggplot(aes(DateTime_UTC, par_uw))+
#   geom_line()+
#   theme_bw()
#
# meta_data_2018 %>%
#   ggplot(aes(DateTime_UTC, par))+
#   geom_line()+
#   theme_bw()
