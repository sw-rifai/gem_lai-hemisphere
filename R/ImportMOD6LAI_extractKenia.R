library(tidyverse); library(lubridate)

mod <- read_csv("../El_Niño_pantropical_forest_climate_anomalies/data/MODc6_LAI_Pantropics/MODc6_LAI_GEMplotLocations_2003_2016.csv")

mod <- mod %>% filter(mean>-999) %>% 
  mutate(date=parse_date_time(substr(`system:index`,3,13), "ymd")) %>%
  rename(lai=mean) %>% select(-.geo) %>% mutate(year=year(date), month=month(date)) %>% 
  group_by(year,month, plot_name) %>% 
  summarize(mod_lai=mean(lai)*0.1) %>% 
  rename(plot_code=plot_name) %>% 
  ungroup() %>% 
  mutate(mod_lai = ifelse(mod_lai<3, 3, mod_lai))

out <- mod %>% filter(plot_code=="Kenia_A" | plot_code=="Kenia_B") %>% 
  mutate(date=parse_date_time(paste(year,month,15), "ymd")) %>% 
  arrange(plot_code,date)

out_A <- out %>% filter(plot_code=="Kenia_A")
out_B <- out %>% filter(plot_code=="Kenia_B")
gam_A <- mgcv::gam(mod_lai~s(as.numeric(date), bs="cc",fx=T), data=out_A)
gam_B <- mgcv::gam(mod_lai~s(as.numeric(date), bs="cc",fx=T), data=out_B)
lo_A <- loess(mod_lai~as.numeric(date), data=out_A, degree=2, enp.target = (length(out_A$date))**0.5)
lo_B <- loess(mod_lai~as.numeric(date), data=out_B, degree=2, enp.target = (length(out_A$date))**0.5)

out_A$mod_lai_gam <- predict(gam_A, newdata=data.frame(date=as.numeric(out_A$date)))
out_B$mod_lai_gam <- predict(gam_B, newdata=data.frame(date=as.numeric(out_B$date)))
out_A$mod_lai_lo <- predict(lo_A, newdata=data.frame(date=as.numeric(out_A$date)))
out_B$mod_lai_lo <- predict(lo_B, newdata=data.frame(date=as.numeric(out_B$date)))

out <- bind_rows(out_A, out_B)

out %>% ggplot(data=., aes(date, mod_lai_lo, color=plot_code))+geom_path()+
  geom_point(aes(date,mod_lai))
  
write_csv(out, path = "outputs/MODc6_Kenia_A_B_monthlyLAI.csv")
