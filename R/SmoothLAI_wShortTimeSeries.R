library(tidyverse); library(lubridate)

#import data
dat <- read_csv("data/LAI_all_23Oct.csv")

# date cleaning and assignment
dat$day[is.na(dat$day)==T] <- 15 # assigning 15 to the day
dat$day[dat$day>28 & dat$month==2] <- 28 # removing the Feb 30s 
dat <- dat %>% mutate(date=parse_date_time(paste(year,month,day),"ymd"))

# Removing values that are too high and too low
dat$LAI[dat$LAI>8] <- NA
dat$LAI[dat$LAI<1] <- NA

# short_ts <- 
tsLte10dates_vec <- dat %>% group_by(plot_code) %>% summarize(n_dates = length(unique(date))) %>% 
  filter(n_dates<10) %>% pull(plot_code)
short_dat <- dat %>% filter(plot_code %in% tsLte10dates_vec)
long_dat <- dat %>% filter(!plot_code %in% tsLte10dates_vec)
##################################################################################################  
tmp_u <- long_dat %>% group_by(plot_code,date) %>% 
  summarize(u_LAI=mean(LAI,na.rm=T), sd_LAI=sd(LAI,na.rm=T)) %>% 
  ungroup()
long_plot_code_vec <- unique(tmp_u$plot_code)
tmp_u$gam_LAI <- NA_real_
tmp_u$loess_LAI <- NA_real_

# process longer time series with more than 10 dates ------------------------------------------------------
i <- 1
for(i in 1:length(long_plot_code_vec)){
  tmp <- dat %>% filter(plot_code==long_plot_code_vec[i]) %>% arrange(date)
  if(length(unique(tmp$date))<5) next
  unique_dates <- tmp %>% pull(date) %>% unique
  len_dates <- unique_dates %>% length
  tmp_fit <- NULL
  preds <- NULL
  try(tmp_fit <- loess(LAI~as.numeric(date), data=tmp, degree=2, 
                       enp.target = (length(tmp$date))**0.5),
      silent = T)
  loess_preds <- try(predict(tmp_fit, newdata=data.frame(date=as.numeric(unique(tmp$date)))), silent = T)
  if(class(preds)=="try-error"){
    try(tmp_fit <- loess(LAI~as.numeric(date), data=tmp, degree=2))
    loess_preds <- predict(tmp_fit, newdata=data.frame(date=as.numeric(unique(tmp$date))))
    print(paste(plot_code_vec[i],"using default loess"))}
  gam_fit <- mgcv::gam(LAI~s(as.numeric(date), bs="cc",fx=T), data=tmp)
  gam_preds <- predict(gam_fit, newdata=data.frame(date=as.numeric(unique(tmp$date))))
  
  tmp_u[tmp_u$date%in%unique_dates & tmp_u$plot_code==long_plot_code_vec[i],]$gam_LAI <- gam_preds
  tmp_u[tmp_u$date%in%unique_dates & tmp_u$plot_code==long_plot_code_vec[i],]$loess_LAI <- loess_preds
  
  rm(tmp_fit,gam_preds,loess_preds,unique_dates,len_dates)
}

tmp_u_long <- tmp_u




# process short time series ---------------------------------------------------------------------------
short_plot_code_vec <- unique(short_dat$plot_code)
i <- 1
for(i in 1:length(short_plot_code_vec)){
  tmp <- dat %>% filter(plot_code==long_plot_code_vec[i]) %>% arrange(date)
  if(length(unique(tmp$date))==1) next
  unique_dates <- tmp %>% pull(date) %>% unique
  len_dates <- unique_dates %>% length
  tmp_fit <- NULL
  preds <- NULL
  try(tmp_fit <- loess(LAI~as.numeric(date), data=tmp, degree=2, 
                       enp.target = (length(tmp$date))**0.5),
      silent = T)
  loess_preds <- try(predict(tmp_fit, newdata=data.frame(date=as.numeric(unique(tmp$date)))), silent = T)
  if(class(preds)=="try-error"){
    try(tmp_fit <- loess(LAI~as.numeric(date), data=tmp, degree=2))
    loess_preds <- predict(tmp_fit, newdata=data.frame(date=as.numeric(unique(tmp$date))))
    print(paste(plot_code_vec[i],"using default loess"))}
  gam_fit <- mgcv::gam(LAI~s(as.numeric(date), bs="cc",fx=T), data=tmp)
  gam_preds <- predict(gam_fit, newdata=data.frame(date=as.numeric(unique(tmp$date))))
  
  tmp_u[tmp_u$date%in%unique_dates & tmp_u$plot_code==long_plot_code_vec[i],]$gam_LAI <- gam_preds
  tmp_u[tmp_u$date%in%unique_dates & tmp_u$plot_code==long_plot_code_vec[i],]$loess_LAI <- loess_preds
  
  rm(tmp_fit,gam_preds,loess_preds,unique_dates,len_dates)
}

tmp_u_short <- tmp_u

tmp_u <- bind_rows(tmp_u_long, tmp_u_short) %>% arrange(plot_code, date)
tmp_u$loess_LAI <- as.numeric(tmp_u$loess_LAI)
# stack and write to file 
write_csv(tmp_u, "outputs/LAI_u_GAM_LOESS_smoothed.csv")


tmp_u_long %>% filter(plot_code=="BOB-01") %>% 
  ggplot(data=., aes(date, u_LAI))+geom_path()+
  geom_path(aes(date,gam_LAI))
tmp_u_long %>% filter(plot_code=="BOB-01") %>% 
  ggplot(data=., aes(date, loess_LAI))+geom_path()
  
