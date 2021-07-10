library(tidyverse); library(lubridate)

#import data
dat <- read_csv("data/LAI_all.csv")

# date cleaning and assignment
dat$day[is.na(dat$day)==T] <- 15 # assigning 15 to the day
dat$day[dat$day>28 & dat$month==2] <- 28 # removing the Feb 30s 
dat <- dat %>% mutate(date=parse_date_time(paste(year,month,day),"ymd"))
plot_code_vec <- unique(dat$plot_code)

# Removing values that are too high and too low
dat$LAI[dat$LAI>8] <- NA
dat$LAI[dat$LAI<1] <- NA

##################################################################################################  
tmp_u <- dat %>% group_by(plot_code,date) %>% 
  summarize(u_LAI=mean(LAI,na.rm=T), sd_LAI=sd(LAI,na.rm=T))
tmp_u$gam_LAI <- NA
tmp_u$loess_LAI <- NA
  
i <- 1

for(i in 1:length(plot_code_vec)){
  tmp <- dat %>% filter(plot_code==plot_code_vec[i]) %>% arrange(date)
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
  
  tmp_u[tmp_u$date%in%unique_dates & tmp_u$plot_code==plot_code_vec[i],]$gam_LAI <- gam_preds
  tmp_u[tmp_u$date%in%unique_dates & tmp_u$plot_code==plot_code_vec[i],]$loess_LAI <- loess_preds
  
  rm(tmp_fit,gam_preds,loess_preds,unique_dates,len_dates)
}
write_csv(tmp_u, "outputs/LAI_u_GAM_LOESS_smoothed.csv")

##################################################################################################  




##################################################################################################
# PLOTTING THE DIFFERENCES BETWEEN THE MEAN AND THE LOESS & GAM SMOOTHERs

tmp_u <- dat %>% group_by(plot_code,date) %>% 
  summarize(u_LAI=mean(LAI,na.rm=T), sd_LAI=sd(LAI,na.rm=T))

for(i in 1:length(plot_code_vec)){
  tmp <- dat %>% filter(plot_code==plot_code_vec[i]) %>% arrange(date)
  if(length(unique(tmp$date))<5) next
  tmp_fit <- NULL
  preds <- NULL
  try(tmp_fit <- loess(LAI~as.numeric(date), data=tmp, degree=2, 
                       enp.target = (length(tmp$date))**0.5),
      silent = T)
  preds <- try(predict(tmp_fit, newdata=data.frame(date=as.numeric(tmp$date))), silent = T)
  if(class(preds)=="try-error"){
    try(tmp_fit <- loess(LAI~as.numeric(date), data=tmp, degree=2))
    print(paste(plot_code_vec[i],"using default loess"))}
  preds <- predict(tmp_fit, newdata=data.frame(date=as.numeric(tmp$date)))
  
  png(paste0("figures/loess_mean_compare/",plot_code_vec[i]),
      width = 1200, height=650, units = "px",pointsize = 18)
  plot(LAI~date, data=tmp, cex=0.5, 
       main=plot_code_vec[i])
  lines(preds~tmp$date, col="blue",lwd=3)
  lines(u_LAI~date, data=tmp_u %>% filter(plot_code==plot_code_vec[i]),col="red",lwd=3)  
  legend("topleft",lwd=c(3,3),col=c('blue',"red"),legend=c("LOESS","Mean"))
  dev.off()
  rm(tmp_fit,preds)
}

for(i in 1:length(plot_code_vec)){
  tmp_u <- dat %>% group_by(plot_code,date) %>% 
    summarize(u_LAI=mean(LAI,na.rm=T), sd_LAI=sd(LAI,na.rm=T))
  tmp <- dat %>% filter(plot_code==plot_code_vec[i]) %>% arrange(date)
  if(length(unique(tmp$date))<5) next
  tmp_fit <- NULL
  preds <- NULL
  try(tmp_fit <- loess(LAI~as.numeric(date), data=tmp, degree=2, 
                       enp.target = (length(tmp$date))**0.5),
      silent = T)
  preds <- try(predict(tmp_fit, newdata=data.frame(date=as.numeric(tmp$date))), silent = T)
  if(class(preds)=="try-error"){
    try(tmp_fit <- loess(LAI~as.numeric(date), data=tmp, degree=2))
    print(paste(plot_code_vec[i],"using default loess"))}
  preds <- predict(tmp_fit, newdata=data.frame(date=as.numeric(tmp$date)))
  gam_fit <- mgcv::gam(LAI~s(as.numeric(date), bs="cc",fx=T), data=tmp)
  gam_preds <- predict(gam_fit, newdata=data.frame(date=as.numeric(tmp$date)))
  png(paste0("figures/gam_v_loess/",plot_code_vec[i]),
      width = 1200, height=650, units = "px",pointsize = 18)
  plot(LAI~date, data=tmp, cex=0.5, 
       main=plot_code_vec[i])
  lines(preds~tmp$date, col="blue",lwd=3)
  lines(u_LAI~date, data=tmp_u %>% filter(plot_code==plot_code_vec[i]),col="red",lwd=3)  
  lines(gam_preds~tmp$date, col="darkgreen",lwd=3)
  legend("topleft",lwd=c(3,3),col=c('blue',"red"),legend=c("LOESS","Mean"))
  dev.off()
  rm(tmp_fit,preds)
}


for(i in 1:length(plot_code_vec)){
  dat %>% filter(plot_code==plot_code_vec[i]) %>% arrange(date) %>% 
  ggplot(data=., aes(date, LAI)) + geom_point() + 
  geom_smooth(method="gam", formula = y~s(x, fx=T),col="blue",se=F) +
  geom_smooth(method="gam", formula = y~s(x, fx=T, bs="so"),col="darkgreen",se=F) +
  geom_smooth(method="loess", span=0.15, col="red", se=F)+
  ggtitle(paste(plot_code_vec[i], "GAM vs LOESS_span0.15")) 
  ggsave(paste0("figures/gam_v_loess/",plot_code_vec[i],".png"))
}



for(i in 1:length(plot_code_vec)){
dat %>% filter(plot_code==plot_code_vec[i]) %>% arrange(date) %>% 
  ggplot(data=., aes(date, LAI)) + geom_point() + 
  geom_smooth(method="gam") + 
  ggtitle(paste(plot_code_vec[i], "LOESS Span 0.15"))
ggsave(paste0("figures/span_0p15/",plot_code_vec[i],".png"))
}

for(i in 1:length(plot_code_vec)){
  dat %>% filter(plot_code==plot_code_vec[i]) %>% arrange(date) %>% 
    ggplot(data=., aes(date, LAI)) + geom_point() + 
    geom_smooth(method="loess", span=0.2) + 
    ggtitle(paste(plot_code_vec[i], "LOESS Span 0.25"))
  ggsave(paste0("figures/span_0p2/",plot_code_vec[i],".png"))
}

for(i in 1:length(plot_code_vec)){
  dat %>% filter(plot_code==plot_code_vec[i]) %>% arrange(date) %>% 
    ggplot(data=., aes(date, LAI)) + geom_point() + 
    geom_smooth(method="loess", span=0.3) + 
    ggtitle(paste(plot_code_vec[i], "LOESS Span 0.35"))
  ggsave(paste0("figures/span_0p3/",plot_code_vec[i],".png"))
}

for(i in 1:length(plot_code_vec)){
  dat %>% filter(plot_code==plot_code_vec[i]) %>% arrange(date) %>% 
    group_by(date) %>% 
    summarize(u_LAI=mean(LAI,na.rm=T), sd_LAI=sd(LAI,na.rm=T)) %>% 
    ggplot(data=., aes(date, u_LAI)) + geom_point() + geom_path()+
    geom_ribbon(aes(date, ymin=(u_LAI-sd_LAI), ymax=(u_LAI+sd_LAI)), 
                fill="grey70",alpha=0.5)+
    theme_bw()+
    ggtitle(paste(plot_code_vec[i], "Plot Mean + SD"))
    ggsave(paste0("figures/plotMean/",plot_code_vec[i],".png"))
}
