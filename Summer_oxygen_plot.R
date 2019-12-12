library(tidyverse);library(lubridate);library(zoo);library(patchwork)
Sys.setenv(TZ="GMT")

#Aq. Sci.:For most journals the figures should be 39 mm, 84 mm, 129 mm, or 174 mm wide and not higher than 234 mm.

#ggplot theme
theme_pub <- theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text = element_text(colour = "black"), 
        strip.background = element_rect(fill = "white"))
theme_set(theme_pub)

event <- ymd_hm("2018-07-28 00:00")

#all oxygen data from station 1 with minidot in top and bottom
#summer oxygen plot

minidot_path <- paste0(getwd(), "/Rawdata/Sommer_ilt_lvl1/")

raw_files <- list.files(minidot_path, full.names = TRUE, pattern = "*.TXT")

raw_list <- lapply(raw_files, read.csv, 
                   skip = 9, header = FALSE, stringsAsFactors = FALSE,
                   col.names = c("timestamp", "DateTime_UTC", "DateTime_CET", "wtr_doobs", "doobs", "dosat_perc", "q"))

names(raw_list) <- basename(raw_files)

#prepare 2013 data which is in a different format and save
# data_2013 <- read.delim2(paste0(getwd(), "/Rawdata/Sommer_ilt_raw/2013 data/22012014_50_excel.TXT"), 
#                                           header = FALSE, stringsAsFactors = FALSE,
#                                           col.names = c("timestamp", "DateTime_UTC", "DateTime_CET", "wtr_doobs", "doobs", "dosat_perc")) %>% 
#   tbl_df() %>% 
#   mutate(DateTime_UTC = dmy_hm(DateTime_UTC),
#          station = "st1",
#          hob = 150) %>% 
#   select(station, hob, DateTime_UTC, wtr_doobs, doobs, dosat_perc)
# saveRDS(data_2013, paste0(getwd(), "/Rawdata/Sommer_ilt_lvl1/2013_oxygen_data.rds"))

data_2013 <- readRDS(paste0(getwd(), "/Rawdata/Sommer_ilt_lvl1/2013_oxygen_data.rds"))

raw_df <- bind_rows(raw_list, .id = "file") %>% 
  tbl_df() %>% 
  mutate(DateTime_UTC = ymd_hms(DateTime_UTC),
         hob = parse_number(str_sub(file, start = 10, end = -5)),
         station = "st1") %>% 
  select(station, hob, DateTime_UTC, wtr_doobs, doobs, dosat_perc) %>% 
  bind_rows(data_2013) %>% 
  distinct()

#save intermediary .rds file
#saveRDS(raw_df, paste0(getwd(), "/Output/", "ilt_raw_df.rds"))

year_labels <- data.frame(year = seq(2013, 2018, 1), labels = c("2013", "2014", "2015", "2016*", "2017", "2018"), x = -Inf, y = Inf)

plot_data <- raw_df %>% 
  mutate(DateTime_UTC = round_date(DateTime_UTC, "10 mins"), #hvad sker der her ved genstart?!?!
         month = month(DateTime_UTC),
         year = year(DateTime_UTC),
         min_of_year = as.numeric(DateTime_UTC - floor_date(DateTime_UTC, "year")),
         hob_label = factor(ifelse(hob >= 100, "Top", "Bottom"), levels = c("Top", "Bottom")),
         day_of_month = mday(DateTime_UTC)) %>% 
  filter(month %in% c(6, 7, 8)) #%>% 
  #mutate_at(vars(wtr_doobs, doobs, dosat_perc), funs(rollmean(., 12, align="left", fill = NA)))

plot_data_clean <- plot_data %>% 
  filter(DateTime_UTC > ymd_hm("2013-06-20 00:00"),
         !between(DateTime_UTC, ymd_hm("2018-06-01 00:00"),ymd_hm("2018-06-08 00:00"))) %>% 
  group_by(year, hob) %>% 
  mutate(do_diff = c(NA, diff(dosat_perc))) %>% 
  filter(between(do_diff, -2, 2)) %>% 
  mutate(dosat_perc = ifelse(between(DateTime_UTC, ymd_hm("2018-08-06 12:00"), ymd_hm("2018-08-07 12:00")), NA, dosat_perc),
         wtr_doobs = ifelse(between(DateTime_UTC, ymd_hm("2018-08-06 12:00"), ymd_hm("2018-08-07 12:00")), NA, wtr_doobs)) %>% 
  ungroup()

month_pos <- plot_data %>% 
  filter(day_of_month == 1) %>% 
  distinct(month, day_of_month, .keep_all = TRUE) %>% 
  pull(min_of_year) %>% 
  c(., 20995200)

month_labels <- c("June", "July", "August", "September")

plot_data_avg_dif <- plot_data_clean %>% 
  group_by(year, min_of_year) %>% 
  summarise(wtr_doobs_mean = mean(wtr_doobs, na.rm = TRUE)) %>% 
  mutate(wtr_avg_dif = wtr_doobs_mean - mean(wtr_doobs_mean, na.rm = TRUE)) #avg summer temp 18.51

plot_data_clean %>% 
  ggplot(aes(min_of_year, dosat_perc, col = hob_label))+
  #plot geom_vline
  geom_line()+
  scale_x_continuous(breaks = month_pos, labels = month_labels)+
  ylab("Dissolved oxygen saturation (%)")+
  xlab(NULL)+
  scale_color_manual(values = c("grey", "black"), name = "Sensor position")+
  geom_text(data = year_labels, aes(x = x, y = y, label = labels), inherit.aes = FALSE, hjust = -0.2, vjust = 1.2)+
  facet_grid(year~.)+
  theme(strip.text = element_blank(),
        legend.position = "bottom")+
  guides(color = guide_legend(title.position="top"))

ggsave(paste0(getwd(), "/Output/", "ilt_plot.png"), height = 234, width = 174, units = "mm")

plot_data_avg_dif %>% 
  ggplot(aes(min_of_year, wtr_doobs_mean, col = wtr_avg_dif))+
  #plot geom_vline
  geom_line()+
  scale_x_continuous(breaks = month_pos, labels = month_labels)+
  ylab(expression("Mean watercolumn temperature ("*degree*C*")"))+
  xlab(NULL)+
  #scale_colour_distiller(type = "div", palette = "RdYlBu")+
  scale_colour_gradient2(low = "cornflowerblue", high = "brown1", mid = "grey", breaks = c(-6, -3, 0, 3, 6), limits = c(-8, 8))+
  geom_text(data = year_labels, aes(x = x, y = y, label = labels), inherit.aes = FALSE, hjust = -0.2, vjust = 1.2)+
  facet_grid(year~.)+
  theme(strip.text = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal")+
  guides(color = guide_colourbar(title.position="top", title = expression(Temperature~anomaly~"("*degree*C*")"), barwidth = unit(45, "mm")))

ggsave(paste0(getwd(), "/Output/", "wtr_plot.png"), height = 234, width = 174, units = "mm")


