library(tidyverse);library(lubridate);library(zoo);library(patchwork);library(readxl)
Sys.setenv(TZ="GMT")
Sys.setlocale("LC_TIME", "US")

#ggplot theme
theme_pub <- theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text = element_text(colour = "black"), 
        strip.background = element_rect(fill = "white"))
theme_set(theme_pub)

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

airt <- df_wnd_rain_airt %>% 
  ggplot(aes(x=date, y=airt_mean, ymin=airt_min, ymax=airt_max))+
  geom_ribbon(fill=grey(0.7))+
  geom_line()+
  ylab(expression("Air temperature ("*degree*C*")"))+
  xlab(NULL) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_x_date(limits = c(ymd("2018-06-01"), ymd("2018-09-01")))

wnd <- df_wnd_rain_airt %>% 
  ggplot(aes(x=date, y=wnd_mean, ymin=wnd_min, ymax=wnd_max))+
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
  geom_col(fill = "black", col = "white")+
  ylab("Precipitation (mm)")+
  xlab(NULL) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_x_date(limits = c(ymd("2018-06-01"), ymd("2018-09-01")))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40))

doc <- df_doc %>% 
  filter(month(date) %in% c(6, 7, 8)) %>% 
  ggplot(aes(date, doc))+
  geom_line()+
  geom_point()+
  ylab(expression("DOC (mg"~L^{-1}*")"))+
  xlab(NULL)+
  scale_x_date(date_labels = "%B", limits = c(ymd("2018-06-01"), ymd("2018-09-01")))

all_plots <- airt+wnd+rain+doc+plot_layout(ncol=1)
all_plots

ggsave(paste0(getwd(), "/Output/", "attr_vars_plot.png"), all_plots, height = 170, width = 129, units = "mm")
