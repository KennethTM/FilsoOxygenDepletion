library(tidyverse);library(lubridate);library(zoo)
Sys.setenv(TZ="GMT")

#Aq. Sci.:For most journals the figures should be 39 mm, 84 mm, 129 mm, or 174 mm wide and not higher than 234 mm.

#ggplot theme
theme_pub <- theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text = element_text(colour = "black"), 
        strip.background = element_rect(fill = "white"))
theme_set(theme_pub)

#all oxygen data from station 1 with minidot in top and bottom
#summer oxygen plot

minidot_path <- paste0(getwd(), "/Rawdata/Sommer_ilt/")

raw_files <- list.files(minidot_path, full.names = TRUE, pattern = "*.TXT")

raw_list <- lapply(raw_files, read.csv, 
                   skip = 9, header = FALSE, stringsAsFactors = FALSE,
                   col.names = c("timestamp", "DateTime_UTC", "DateTime_CET", "wtr_doobs", "doobs", "dosat_perc", "q"))

names(raw_list) <- basename(raw_files)

data_2013 <- read.delim2(paste0(minidot_path, "2013 data/22012014_50_excel.TXT"), 
                                          header = FALSE, stringsAsFactors = FALSE,
                                          col.names = c("timestamp", "DateTime_UTC", "DateTime_CET", "wtr_doobs", "doobs", "dosat_perc")) %>% 
  tbl_df() %>% 
  mutate(DateTime_UTC = dmy_hm(DateTime_UTC),
         station = "st1",
         hob = 150) %>% 
  select(station, hob, DateTime_UTC, wtr_doobs, doobs, dosat_perc)

raw_df <- bind_rows(raw_list, .id = "file") %>% 
  tbl_df() %>% 
  mutate(DateTime_UTC = ymd_hms(DateTime_UTC),
         hob = parse_number(str_sub(file, start = 10, end = -5)),
         station = "st1") %>% 
  select(station, hob, DateTime_UTC, wtr_doobs, doobs, dosat_perc) %>% 
  bind_rows(data_2013) %>% 
  distinct()

#save intermediary .rds file
#saveRDS(raw_df, paste0(minidot_path, "ilt_raw_df.rds"))

month_pos <- c(5, 6, 7, 8, 9)*c(31, 30, 31, 31, 30)*24*60
month_labels <- c("May", "June", "July", "August", "September")

year_labels <- data.frame(year = seq(2013, 2018, 1), labels = seq(2013, 2018, 1), x = -Inf, y = Inf)

raw_df %>% 
  mutate(month = month(DateTime_UTC),
         year = year(DateTime_UTC),
         min_of_year = as.numeric(DateTime_UTC - floor_date(DateTime_UTC, "year")),
         hob_label = factor(ifelse(hob > 100, "Top", "Bottom"), levels = c("Top", "Bottom"))) %>% 
  filter(month %in% c(6, 7, 8, 9)) %>% 
  ggplot(aes(min_of_year, dosat_perc, col = hob_label))+
  #ggplot(aes(min_of_year, dosat_perc, group = hob_label, col = dosat_perc))+
  geom_line()+
  scale_x_continuous(breaks = month_pos, labels = month_labels)+
  ylab("Dissolved oxygen saturation (%)")+
  xlab(NULL)+
  #scale_color_gradient2(low = "#A50026", high = "#006837", mid = "cornsilk2", midpoint = 100, limits = c(-20, 200))+
  #sc+
  scale_color_manual(values = c("grey", "black"))+
  geom_text(data = year_labels, aes(x = x, y = y, label = labels), inherit.aes = FALSE, hjust = -0.2, vjust = 1.2)+
  facet_grid(year~.)+
  theme(strip.text = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")

ggsave(paste0(minidot_path, "ilt_plot.png"), height = 234, width = 174, units = "mm")

library(RColorBrewer)
myPalette <- colorRampPalette(c("#A50026", "#F46D43", "#FEE08B", "#92C5DE", "#D9EF8B", "#66BD63", "#006837"))
sc <- scale_colour_gradientn(colours = myPalette(100))
display.brewer.pal(10, "Spectral")
brewer.pal(10, "RdYlGn")
