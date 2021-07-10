library(tidyverse); library(lubridate); library(mgcv)
mod2 <- read_csv("../data_general/MODc6_LAI_Pantropics/MODc6_LAI_GEMplots_2003_2016_highestQual_20180201.csv")
mod2 <- mod2 %>% filter(mean>-999) %>% 
  mutate(date=parse_date_time(substr(`system:index`,3,13), "ymd")) %>%
  rename(lai=mean, plot_code=GEM_plot_code) %>% 
  mutate(year=year(date), month=month(date), doy=yday(date), lai=lai*0.1, site=(substr(plot_code,1,3))) %>% 
  filter(is.na(plot_code)==F & lai>=1.5)

vec_plot_codes <- mod2$plot_code %>% unique() %>% sort()

df_preds <- expand.grid(plot_code=vec_plot_codes,year=seq(2005,2017,by=1),month=seq(1,12,by=1)) %>% 
  mutate(site=substr(plot_code,1,3)) %>% tbl_df() %>% 
  mutate(date=parse_date_time(paste(year,month,15),"ymd"))


fit_1 <- bam(lai~s(month,bs="cc")+
               s(year,bs="gp", m=c(3,0.175)),
             # family=scat(link="inverse", min.df=2),
             family=Gamma(link="log"),
             method="fREML", 
             data= mod2 %>% 
               filter(plot_code==vec_plot_codes[1]) %>% 
               mutate(site=as.factor(site), 
                      plot_code=as.factor(plot_code), 
                      date=as.numeric(date)))
tmp_preds0 <- df_preds %>% 
  filter(plot_code==vec_plot_codes[1]) %>% 
  mutate(site=as.factor(site), 
         plot_code=as.factor(plot_code), 
         date=as.numeric(date))

tmp_preds0$lai_pred <- predict(fit_1, 
                             type="response", 
                             newdata=tmp_preds0)

for(i in 2:length(vec_plot_codes)){
  fit_1 <- bam(lai~s(month,bs="cc")+
                 s(year,bs="gp", m=c(3,0.175)),
               # family=scat(link="inverse", min.df=2),
               family=Gamma(link="log"),
               method="fREML", 
               data= mod2 %>% 
                 filter(plot_code==vec_plot_codes[i]) %>% 
                 mutate(site=as.factor(site), 
                        plot_code=as.factor(plot_code), 
                        date=as.numeric(date)))
  tmp_preds1 <- df_preds %>% 
    filter(plot_code==vec_plot_codes[i]) %>% 
    mutate(site=as.factor(site), 
           plot_code=as.factor(plot_code), 
           date=as.numeric(date))
  
  tmp_preds1$lai_pred <- predict(fit_1, 
                                 type="response", 
                                 newdata=tmp_preds1)
  tmp_preds0 <- bind_rows(tmp_preds0, tmp_preds1); 
 
  rm(tmp_preds1); rm(fit_1)
 print(i)
}
tmp_preds0 <- tmp_preds0 %>% mutate(date=as.POSIXct(date, origin="1970-01-01")) 
  
write_csv(tmp_preds0, path = "outputs/MODc6_GEM_plots_individual_GAM_GP_preds.csv")
tmp_preds <- read_csv("outputs/MODc6_GEM_plots_individual_GAM_GP_preds.csv")


tmp_preds %>% 
  mutate(date=as.POSIXct(date, origin="1970-01-01")) %>%
  mutate(year=year(date),month=month(date)) %>% 
  group_by(site,plot_code,year,month) %>% 
  summarize(lai_pred=mean(lai_pred,na.rm=T)) %>%
  mutate(date=parse_date_time(paste(year,month,15),'ymd')) %>%
  arrange(date) %>% 
  ggplot(data=., aes(date, lai_pred))+geom_path()+
  facet_wrap(~plot_code)


tmp_preds %>% mutate(date=as.POSIXct(date, origin="1970-01-01")) %>%
  arrange(date) %>%  filter(year<=2006) %>% ggplot(data=., aes(date, lai_pred))+geom_path()+
  facet_wrap(~plot_code)



write_csv(tmp_preds0, path = "outputs/MODc6_GEM_plots_individual_GAM_GP_preds.csv")
tmp_preds <- read_csv("outputs/MODc6_GEM_plots_individual_GAM_GP_preds.csv")

tmp_preds0 %>% arrange(date) %>% ggplot(data=., aes(date, lai_pred))+geom_path()

tmp_preds %>% filter(site=="CAX") %>% 
  mutate(date=as.POSIXct(date, origin="1970-01-01")) %>%
  arrange(date) %>% 
  filter(year<=2006) %>% 
  ggplot(data=., aes(date, doy))+geom_path()

mod2 %>% filter(site=="TAM") %>% 
  ggplot(data=., aes(date,lai))+geom_point()+
  geom_smooth(se=F, method="gam", formula = y~s(x,bs="gp"), method.args=list(method="REML"))+
  facet_wrap(~plot_code)

mod2 %>% filter(site=="TAM") %>% 
  ggplot(data=., aes(date,lai,color=lai))+
  geom_hex()+
  viridis::scale_color_viridis(discrete = F)
  geom_point()

  geom_smooth(se=F, method="gam", formula = y~s(x,bs="gp"), method.args=list(method="REML"))+
  facet_wrap(~plot_code)

junk_fit <- bam(lai~s(month,bs="cc",by=plot_code)+
                  s(year,bs="gp", m=c(3,0.175)),
                  # s(month,week,
                  #      # bs=c("cc","gp"),
                  #      bs=c("gp"),
                  #      # k=c(4,6),
                  #      m=c(1,0.5), 
                  #      by=plot_code),
    # family=scat(link="inverse", min.df=2),
    family=Gamma(link="log"),
    method="fREML",
    select=T,
    data= mod2 %>% 
      # filter(plot_code=="TAM-06") %>%
      filter(site=="TAM") %>% 
      mutate(site=as.factor(site), 
             plot_code=as.factor(plot_code),
             week=week(date),
             date=as.numeric(date)))

junk <- df_preds %>% 
  filter(site=="TAM") %>% 
  mutate(site=as.factor(site), 
         plot_code=as.factor(plot_code), 
         date=as.numeric(date),
         month=month(as.POSIXct(date,origin="1970-01-01"))) %>% 
  arrange(date,plot_code)
junk$lai_pred <- predict(junk_fit, type="response", newdata=junk)
junk %>% mutate(date=as.POSIXct(date, origin="1970-01-01")) %>% 
  arrange(date) %>% 
  group_by(site,plot_code,year,month) %>%
  summarize(lai_pred=mean(lai_pred,na.rm=T), date=mean(date)) %>%
  ggplot(data=., aes(date,lai_pred))+geom_path()+
  facet_wrap(~plot_code,nrow = 3)



# tmp_preds0 %>% filter(site=="ALP") %>% 
#   ggplot(data=., aes(date, lai_pred))+geom_path()
# 
# 
# 
# 
# library(parallel);
# nc <- 8; 
# if (detectCores()>1) { ## no point otherwise
#   cl <- makeCluster(nc) 
#   ## could also use makeForkCluster, but read warnings first!
# } else cl <- NULL
# 
# 
# 
# precip <- read_csv("../El_Niño_pantropical_forest_climate_anomalies/data/data_climate/CWD_RS_ensemble20171030.csv")
# mod0 <- read_csv("../El_Niño_pantropical_forest_climate_anomalies/data/MODc6_LAI_Pantropics/MODc6_LAI_GEMplotLocations_2003_2016.csv")
# mod1 <- read_csv("../El_Niño_pantropical_forest_climate_anomalies/data/MODc6_LAI_Pantropics/MODc6_LAI_GEMplots_2003_2016_highestQual_20180201.csv")
# 
# precip <- precip %>% select(site, GEM_plot_code,year,month,date,u_precip,cwd) %>% 
#   distinct() %>% 
#   rename(plot_code=GEM_plot_code) %>% 
#   group_by(plot_code) %>% 
#   arrange(date) %>% 
#   mutate(precip_l1 = lag(u_precip, order_by = date), 
#          precip_l2 = lag(u_precip, order_by = date, n = 2))
# df_map <- precip %>% distinct() %>% filter(date<="2016-12-31 UTC") %>% group_by(site, plot_code,year) %>% 
#   summarize(ap=sum(u_precip,na.rm=T)) %>% group_by(site, plot_code) %>% summarize(map=mean(ap,na.rm=T))
# 
# mod0 <- mod0 %>% filter(mean>-999) %>% 
#   mutate(date=parse_date_time(substr(`system:index`,3,13), "ymd")) %>%
#   rename(lai=mean, plot_code=GEM_plot_c) %>% 
#   mutate(year=year(date), month=month(date), doy=yday(date), lai=lai*0.1, site=(substr(plot_code,1,3))) %>% 
#   filter(is.na(plot_code)==F)
# mod1 <- mod1 %>% filter(mean>-999) %>% 
#   mutate(date=parse_date_time(substr(`system:index`,3,13), "ymd")) %>%
#   rename(lai=mean, plot_code=GEM_plot_code) %>% 
#   mutate(year=year(date), month=month(date), doy=yday(date), lai=lai*0.1, site=(substr(plot_code,1,3))) %>% 
#   filter(is.na(plot_code)==F)
# mod2 <- mod2 %>% filter(mean>-999) %>% 
#   mutate(date=parse_date_time(substr(`system:index`,3,13), "ymd")) %>%
#   rename(lai=mean, plot_code=GEM_plot_code) %>% 
#   mutate(year=year(date), month=month(date), doy=yday(date), lai=lai*0.1, site=(substr(plot_code,1,3))) %>% 
#   filter(is.na(plot_code)==F)
# 
# 
# # mod2 %>% filter(site=="ALP") %>% ggplot(data=., aes(date,lai))+geom_path()
# mod2 <- left_join(mod2, precip %>% select(-date), by=c("site","plot_code","year","month"))
# mod2 <- left_join(mod2,df_map %>% select(-plot_code),by="site")
# mod2 <- mod2 %>% filter(lai>=(map*0.001))
# 
# 
# 
# tmp_mod %>% mutate(date=as.POSIXct(date, origin="1970-01-01")) %>% 
#   ggplot(data=., aes(date, lai))+geom_path()+
#   geom_path(aes(date, lai_pred),color="darkgreen",lwd=2)
# # geom_path(aes(date, lai_pred2), color="blue", lwd=1)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# fit_2 <- bam(lai~ s(doy,bs="gp")+ 
#                s(date, bs="gp") + 
#                s(month,bs="cc")+
#                s(year)+
#                s(u_precip,bs="gp")+
#                s(precip_l1,bs="gp")+
#                s(precip_l2,bs="gp"),
#              family=scat(link="inverse", min.df=2),
#              data=tmp_mod)
# summary(fit_2)
# # plot(fit_2)
# tmp_mod$lai_pred2 <- predict(fit_2, type="response", newdata = tmp_mod)
# 
# 
# 
# 
# 
# fit_1 <- bam(lai~ s(date, by=site) + s(year,month,by=site)+s(plot_code,bs = "re"),
#              family=Gamma(link="log"),
#              data=mod %>% mutate(site=as.factor(site), 
#                                  plot_code=as.factor(plot_code), 
#                                  date=as.numeric(date)))
# plot(fit_1)
# mod$lai_pred <- predict(fit_1, type="response")
# 
# mod %>% ggplot(data=., aes(lai,lai_pred,color=site))+geom_point()+
#   geom_abline(aes(intercept=0,slope=1),color="red")
# 
# mod %>% filter(plot_code=="KEN-01") %>% 
#   ggplot(data=., aes(date,lai))+geom_point()+
#   geom_path(aes(date, lai_pred))
# 
# 
# mod %>% filter(site=="KEN") %>% pull(lai) %>% quantile(0.975)
# 
# tmp_ken <- mod %>% filter(site=="KEN" & lai>=3) %>% 
#   mutate(site=as.factor(site), 
#          plot_code=as.factor(plot_code), 
#          date=as.numeric(date))
# 
# ctrl <- gam.control(nthreads = 4) 
# fit_ken <- bam(lai ~ s(doy,bs="gp",by=plot_code) + te(month, year, bs = c("cc", "gp"))+s(plot_code,bs="re"),
#                data = tmp_ken, 
#                # knots = list(month = c(0.5, 12.5)),
#                # family = tw, 
#                family=scat(link="log",min.df = 2),
#                method = "REML", 
#                select = TRUE, 
#                control = ctrl)
# summary(fit_ken)
# plot(fit_ken)
# tmp_ken$lai_pred <- predict(fit_ken, type="response")
# tmp_ken %>% filter(plot_code=="KEN-01") %>% 
#   ggplot(data=., aes(date,lai))+geom_path(color="darkgreen")+
#   geom_path(aes(date, lai_pred),lwd=3)+theme_bw()
# 
