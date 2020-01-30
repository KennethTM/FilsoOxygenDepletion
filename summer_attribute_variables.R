#Plot of attribute vars during 2018 summer

source("libs_and_funcs.R")

#Read data for plotting
vejrst_2018_df <- read.csv(paste0(getwd(), "/Rawdata/Vejrstation_Filsoe_18-09-12.csv")) %>% 
  tbl_df() %>% 
  set_names(c("DateTime_GMT2", "atmpres", "wnd", "wnd_gust", "wnd_dir", "par", "rain", "airt", "rh")) %>% 
  mutate(DateTime_UTC = dmy_hms(DateTime_GMT2)-2*60*60,
         par = par-1.2)

df_wnd_rain_airt <- vejrst_2018_df %>% 
  select(DateTime_UTC, rain, wnd, airt) %>% 
  filter(month(DateTime_UTC) %in% c(6, 7, 8)) %>% 
  #mutate(DateTime_UTC = round_date(DateTime_UTC, "24 hour")) %>% 
  mutate(date = as_date(DateTime_UTC)) %>% 
  na.omit() %>% 
  select(-DateTime_UTC) %>% 
  group_by(date) %>% 
  summarise_all(funs(min, mean, max, sum))

df_doc <- read_xlsx(paste0(getwd(), "/Rawdata/filso_doc.xlsx"), sheet = 2) %>% 
  mutate(date = as_date(Date)) %>%
  na.omit() %>% 
  group_by(date) %>% 
  summarise(doc = mean(konc_mg_l1))

df_chla <- read_xlsx(paste0(getwd(), "/Rawdata/filso_chla.xlsx"), sheet = 1) %>% 
  select(chla = `Daglig middel klorofyl µg/l`) %>% 
  na.omit() %>% 
  mutate(date = seq(ymd("2018-04-17"), by = "1 day", length.out = n()))

df_zmean <- readRDS(paste0(getwd(), "/Rawdata/filso_depths.rds")) %>% 
  mutate(date = as_date(DateTime_UTC)) %>% 
  group_by(date) %>% 
  summarise(zmean_day_mean = mean(zmean))

#Create individual plots
airt <- df_wnd_rain_airt %>% 
  ggplot(aes(x=date, y=airt_mean, ymin=airt_min, ymax=airt_max))+
  geom_vline(xintercept = event_date, linetype = 2)+
  geom_ribbon(fill=grey(0.7))+
  geom_line()+
  ylab(expression("Air temperature ("*degree*C*")"))+
  xlab(NULL) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_x_date(limits = c(ymd("2018-06-01"), ymd("2018-09-01")))

wnd <- df_wnd_rain_airt %>% 
  ggplot(aes(x=date, y=wnd_mean, ymin=wnd_min, ymax=wnd_max))+
  geom_vline(xintercept = event_date, linetype = 2)+
  geom_ribbon(fill=grey(0.7))+
  geom_line()+
  ylab(expression("Wind speed ("*m~s^{-1}*")"))+
  xlab(NULL) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_x_date(limits = c(ymd("2018-06-01"), ymd("2018-09-01")))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10), breaks = seq(0, 8, 2))

rain <- df_wnd_rain_airt %>% 
  ggplot(aes(date, rain_sum))+
  geom_vline(xintercept = event_date, linetype = 2)+
  geom_col(fill = "black", col = "black")+
  ylab("Precipitation (mm)")+
  xlab(NULL) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_x_date(limits = c(ymd("2018-06-01"), ymd("2018-09-01")))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40))

doc <- df_doc %>% 
  filter(month(date) %in% c(6, 7, 8)) %>% 
  ggplot(aes(date, doc))+
  geom_vline(xintercept = event_date, linetype = 2)+
  geom_line()+
  geom_point()+
  ylab(expression("DOC (mg C"~L^{-1}*")"))+
  xlab(NULL) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

chla <- df_chla %>% 
  filter(month(date) %in% c(6, 7, 8)) %>% 
  ggplot(aes(date, chla))+
  geom_vline(xintercept = event_date, linetype = 2)+
  geom_line()+
  ylab(expression("Chlorphyll A ("*mu*g~L^{-1}*")"))+
  xlab(NULL)+
  scale_x_date(date_labels = "%b", limits = c(ymd("2018-06-01"), ymd("2018-09-01")))

zmean <- df_zmean %>% 
  filter(month(date) %in% c(6, 7, 8)) %>% 
  ggplot(aes(date, zmean_day_mean))+
  geom_vline(xintercept = event_date, linetype = 2)+
  geom_line()+
  ylab(expression(z[mean]~"(m)"))+
  xlab(NULL)+
  scale_x_date(date_labels = "%b", limits = c(ymd("2018-06-01"), ymd("2018-09-01")))

#Collect plots and save
all_plots_2col <- airt+wnd+rain+zmean+doc+chla+plot_layout(ncol=2)+plot_annotation(tag_levels = "A")
all_plots_2col

ggsave(paste0(getwd(), "/Output/", "fig_attr_vars.png"), all_plots_2col, height = 160, width = 174, units = "mm")

#Plot of CDOM through all years during summer
df_cdom <- read_xlsx(paste0(getwd(), "/Rawdata/filso_cdom_300_750.xlsx"), sheet = 2) %>% 
  gather(year, value, -jday) %>% 
  na.omit() %>% 
  mutate(origin = ymd(paste0(year, "-01-01")),
         date = origin+jday) %>% 
  filter(month(date) %in% c(6, 7, 8)) 

#Labels for 1. juni, juli og august
jday_pos <- c(151, 181, 211, 242)
jday_labs <- c("Jun", "Jul", "Aug", "Sep")

df_cdom %>% 
  ggplot(aes(jday, value, col = year))+
  geom_line()+
  geom_point()+
  scale_color_brewer(palette = "Dark2")+
  scale_x_continuous(breaks = jday_pos, labels = jday_labs)+
  ylab(expression("CDOM"[300-750]*" (m"^{-1}*")"))+
  xlab("Month")+
  theme(legend.title = element_blank())
  
ggsave(paste0(getwd(), "/Output/", "fig_cdom.png"), height = 84, width = 129, units = "mm")
