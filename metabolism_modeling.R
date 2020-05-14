#Metabolism modeling and plot

source("libs_and_funcs.R")

raw_data_2018 <- readRDS(paste0(getwd(), "/Output/meta_2018.rds"))

#Because of the shallow water column zmean is used instead of zmix for modeling
prep_data_2018 <- raw_data_2018 %>% 
  mutate(dosat = o2.at.sat.base(wtr),
         wnd_10 = wind.scale.base(wnd, 2),
         k600 = k.vachon.base(wnd_10, 4223445),
         kgas = k600.2.kGAS.base(k600, wtr, "O2")/24/6,
         dummy = 1,
         date = as_date(DateTime_UTC)) %>% 
  dplyr::select(date, DateTime_UTC, doobs, dosat, kgas, zmix = zmean, lux = par, wtr, dummy)

#Data til beregning af nat slopes (resp under iltsvind)
# library(openxlsx)
# prep_data_2018 %>%
#   mutate(doobs_diff = c(0, diff(doobs)),
#          o2_flux =  -kgas * (doobs - dosat) / zmix,
#          nep = doobs_diff - o2_flux) %>%
#   write.xlsx(paste0(getwd(), "/Output/filso_nep_raw.xlsx"))

#Plot of input variables
prep_data_2018 %>% 
  gather(variable, value, -DateTime_UTC) %>% 
  ggplot(aes(DateTime_UTC, value))+
  geom_line()+
  facet_grid(variable~., scales = "free")+
  theme_bw()

#Filter data part of/not part of the event
prep_data_2018_event <- prep_data_2018 %>% 
  filter(between(DateTime_UTC, event, event_end))

prep_data_2018_not_event <- prep_data_2018 %>% 
  filter(!between(DateTime_UTC, event, event_end), 
         DateTime_UTC >= ymd_hm("2018-06-07 00:00"),
         !(DateTime_UTC == ymd_hm("2018-09-01 00:00")))

#Estimate metabolism parameters (describing R and GPP) for each day not part of the event
metab_2018 <- prep_data_2018_not_event %>% 
  mutate(date_round = round_date(date, "1 days")) %>% 
  dplyr::select(-date) %>% 
  nest(data = c(DateTime_UTC, doobs, dosat, kgas, zmix, lux, wtr, dummy)) %>% 
  mutate(metab_mle = map(data, ~metab_calc(.x))) %>% 
  mutate(metab_result = map(metab_mle, ~ as.data.frame(.x[1])),
         metab_pred = map(metab_mle, ~ as.data.frame(.x[2])))

#Plot fits
metab_2018 %>% 
  unnest(metab_pred) %>% 
  ggplot(aes(DateTime_UTC))+
  geom_line(aes(y = doobs), col = "black")+
  geom_line(aes(y = dopred), col = "orange")+
  geom_vline(xintercept = event)

#Initial oxygen concentration at onset of event
event_doinit <- prep_data_2018 %>%
  filter(DateTime_UTC == event) %>%
  pull(doobs)

#Use estimated metabolism parameters to predict oxygen during the depletion event
#That is, all daily estimated parameters (~70) are used to calculate and oxygen trajectory from water temp. and par during event
#Wrapped in function so that the influence on scaling gpp and/or r at each time step can be investigated

metabolism_scaling <- function(gpp_scale = 1, r_scale = 1, f_scale = 1, label = FALSE, show_f = TRUE){
  
  forecast_df <- metab_2018 %>%
    mutate(forecast = map(metab_result, ~doforecast(.x, doinit=event_doinit, datain=prep_data_2018_event, gpp_scale = gpp_scale, r_scale = r_scale, f_scale = f_scale))) %>%
    unnest(forecast)
  
  #Calculate mean and quantiles which include 95 % of observations at each time point during event
  forecast_df_quantile <- forecast_df %>% 
    select(DateTime_UTC, dopred) %>% 
    group_by(DateTime_UTC) %>% 
    summarise(low = quantile(dopred, 0.025),
              high = quantile(dopred, 0.975),
              mean = mean(dopred))
  
  #Set values during maintainance visit to NA
  prep_data_2018_event_minus_visit <- prep_data_2018_event %>% 
    mutate(doobs = ifelse(between(DateTime_UTC, ymd_hm("2018-08-06 12:00"), ymd_hm("2018-08-07 12:00")), NA, doobs))
  
  #Plot observed and modelled oxygen outside event and predicted trajectory during event
  metab_plot <- metab_2018 %>% 
    unnest(metab_pred) %>% 
    right_join(data.frame(DateTime_UTC = seq(ymd_hm("2018-06-07 00:00"),
                                             ymd_hm("2018-09-01 00:00"),
                                             "10 mins"))) %>% 
    ggplot(aes(DateTime_UTC))+
    geom_line(aes(y = doobs, col = "Observed"))+
    geom_line(aes(y = dopred, col = "Modelled"))+
    scale_color_manual(values = c("orange", "black"))+
    geom_ribbon(data = forecast_df_quantile, aes(x=DateTime_UTC, ymin=low, ymax=high), fill = "grey", alpha = 0.5)+
    geom_line(data = prep_data_2018_event_minus_visit, aes(DateTime_UTC, doobs))+
    geom_line(data = forecast_df_quantile, aes(DateTime_UTC, mean), col = "orange")+
    ylab(expression("Dissolved oxygen (mg L"^{-1}*")"))+
    xlab(NULL)+
    geom_vline(xintercept = event, linetype = 2)+
    coord_cartesian(ylim = c(-0.2, 19), expand = TRUE)+
    scale_x_datetime(date_labels = "%b", limits = c(ymd_hm("2018-06-01 00:00"), ymd_hm("2018-09-01 00:00")))+
    theme(legend.title = element_blank(),
          legend.position = c(0.1, 0.75))
  
    if(label){
      
      metab_scale_label <- function(scale){
        if(scale == 1){
          label <- "unchanged"
        }else if(scale > 1){
          label <- paste0("increased ", round((scale-1)*100, 0), "%")
        }else{
          label <- paste0("decreased ", round((1-scale)*100, 0), "%")
        }
        return(label)
      }
      
      if(show_f){
        metab_plot <- metab_plot +
          ggtitle(paste0("GPP ", metab_scale_label(gpp_scale),
                         ", R ", metab_scale_label(r_scale),
                         " and F ", metab_scale_label(f_scale)))
      }else{
        metab_plot <- metab_plot +
          ggtitle(paste0("GPP ", metab_scale_label(gpp_scale),
                         " and R ", metab_scale_label(r_scale)))
      }
      
    }
  
    return(metab_plot)
}

metabolism_scaling(label = TRUE, show_f = FALSE)/metabolism_scaling(1, 3, 1, label = TRUE, show_f = FALSE)+plot_annotation(tag_levels = "A")

ggsave(paste0(getwd(), "/Output/fig_metabolism.png"), height = 200, width = 174, units = "mm")
ggsave(paste0(getwd(), "/Output/fig_metabolism.pdf"), height = 200, width = 174, units = "mm")

#Investigate how altering gpp and r changes oxygen trajectory during event
plot_grid <- expand.grid(GPP = c(0.5, 1, 2, 3), R = c(0.5, 1, 2, 3), F = c(0.5, 1, 2)) %>% 
  arrange(GPP, F) %>% 
  mutate(plots = pmap(list(GPP, R, F), ~metabolism_scaling(..1, ..2, ..3, label = TRUE)))

pdf(paste0(getwd(), "/Output/fig_supp_metabolism.pdf"), paper = "a4", width = 7, height = 12)
for(i in seq(1, nrow(plot_grid), 4)){
  print(wrap_plots(plot_grid$plots[i:(i+3)], ncol = 1))
}
dev.off()
