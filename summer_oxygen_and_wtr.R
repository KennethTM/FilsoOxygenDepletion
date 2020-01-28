#Plot of summer oxygen and temperature in all years

source("libs_and_funcs.R")

#Read data
raw_df <- readRDS(paste0(getwd(), "/Output/", "raw_df.rds"))

#Prepare data for plotting
year_labels <- data.frame(year = seq(2013, 2018, 1), labels = c("2013", "2014", "2015", "2016*", "2017", "2018"), x = -Inf, y = Inf)

plot_data <- raw_df %>% 
  mutate(DateTime_UTC = round_date(DateTime_UTC, "10 mins"),
         month = month(DateTime_UTC),
         year = year(DateTime_UTC),
         min_of_year = as.numeric(DateTime_UTC - floor_date(DateTime_UTC, "year")),
         hob_label = factor(ifelse(hob >= 100, "Top", "Bottom"), levels = c("Top", "Bottom")),
         day_of_month = mday(DateTime_UTC)) %>% 
  filter(month %in% c(6, 7, 8)) 

#Remove some outliers
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

event_vline <- plot_data_clean %>% 
  filter(DateTime_UTC == event) %>% 
  slice(1) 

#Plot and save
#Oxygen
plot_data_clean %>% 
  ggplot(aes(min_of_year, dosat_perc, col = hob_label))+
  geom_vline(data = event_vline, aes(xintercept = min_of_year), linetype = 2)+
  geom_line()+
  scale_x_continuous(breaks = month_pos, labels = month_labels)+
  ylab("Dissolved oxygen saturation (%)")+
  xlab(NULL)+
  scale_color_manual(values = c("cornflowerblue", "coral1"), name = "Sensor position")+
  geom_text(data = year_labels, aes(x = x, y = y, label = labels), inherit.aes = FALSE, hjust = -0.2, vjust = 1.2)+
  facet_grid(year~.)+
  theme(strip.text = element_blank(),
        legend.position = "bottom")+
  guides(color = guide_legend(title.position="top"))

ggsave(paste0(getwd(), "/Output/", "fig_summer_oxygen.png"), height = 234, width = 174, units = "mm")

#Water temperature colored by magnitude of anomaly
plot_data_avg_dif %>% 
  ggplot(aes(min_of_year, wtr_doobs_mean, col = wtr_avg_dif))+
  geom_vline(data = event_vline, aes(xintercept = min_of_year), linetype = 2)+
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

ggsave(paste0(getwd(), "/Output/", "fig_summer_wtr.png"), height = 234, width = 174, units = "mm")
