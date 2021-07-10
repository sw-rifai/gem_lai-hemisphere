library(tidyverse); library(lubridate); library(mgcv)

mod <- read_csv("../El_Niño_pantropical_forest_climate_anomalies/data/MODc6_LAI_Pantropics/MODc6_LAI_GEMplotLocations_2003_2016.csv")

mod <- mod %>% filter(mean>-999) %>% 
  mutate(date=parse_date_time(substr(`system:index`,3,13), "ymd")) %>%
  rename(lai=mean, plot_code=GEM_plot_c) %>% 
  mutate(year=year(date), month=month(date), doy=yday(date), lai=lai*0.1, site=(substr(plot_code,1,3))) %>% 
  filter(is.na(plot_code)==F)
mod <- mod %>% filter(lai>=2.5)

ctrl <- gam.control(nthreads = 4) 
fit_all <- bam(lai ~ s(doy,bs="gp",by=plot_code) + te(month, year, bs = c("cc", "gp"),by=site)+s(plot_code,bs="re"),
               # knots = list(month = c(0.5, 12.5)),
               # family = tw, 
               family=scat(link="log",min.df = 2),
               method = "REML", 
               select = TRUE, 
               control = ctrl, 
               data=mod %>% mutate(site=as.factor(site), plot_code=as.factor(plot_code)))
summary(fit_all)
plot(fit_ken)
tmp_ken$lai_pred <- predict(fit_ken, type="response")
tmp_ken %>% filter(plot_code=="KEN-01") %>% 
  ggplot(data=., aes(date,lai))+geom_path(color="darkgreen")+
  geom_path(aes(date, lai_pred),lwd=3)+theme_bw()

