# Graphs 20210202_fnc_time but reorganized. 
# Now plot time 2

library(reshape2)
library(dplyr)
library(lubridate)
library(tsibble) #Can use function yearweek()
library(ggplot2)
#https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
library(gridExtra)
library(dplyr)
setwd("~/Box/Sarah and Molly's Box/FHL data/plots")

timing <- 2
if(timing == 1){
  setwd("~/Box/Sarah and Molly's Box/FHL data/code/Desktop")
  #load("~/Box/Sarah and Molly's Box/FHL data/code/Desktop/my_work_space_time1_20210409.RData")
}
if(timing == 2){
  setwd("~/Box/Sarah and Molly's Box/FHL data/code/Desktop")
  #load("~/Box/Sarah and Molly's Box/FHL data/code/Desktop/my_work_space_time2_20210409.RData")
}

mean(as.numeric(Iter.len.2[1,])) #0.23
mean(as.numeric(Iter.len.1[1,])) #0.22

Iter.len.1[1,Iter.len.1[1,]>=.215&Iter.len.1[1,]<=.235] #V27 is .222, V110 is .220, V143 is .229
Iter.len.1[1, Elevation.list[[1]]=="Mid"]#108-143 
Iter.len.1[1, Elevation.list[[1]]=="Low"]#


Iter.len.2[1,Iter.len.2[1,]>=.205&Iter.len.2[1,]<=.235]
Iter.len.2[1, Elevation.list[[2]]=="Upper"]#1-92 - V1 is 0.205, V32 is 0.221
Iter.len.2[1, Elevation.list[[2]]=="Mid"]#93-108 - V94 is 0.20, V100 is 0.23
Iter.len.2[1, Elevation.list[[2]]=="Low"]#109-138 - V124 is 0.209, V121 is 0.225


Iter.len.2[1,Iter.len.2[1,Elevation.list[[2]]=="Mid"]>=.20&Iter.len.2[1,Elevation.list[[2]]=="Mid"]<=.225] #V114 is 0.222
Iter.len.2[1,Iter.len.2[1,Elevation.list[[2]]=="Low"]>=.225&Iter.len.2[1,Elevation.list[[2]]=="Low"]<=.23]


Iter.len.2[1,Elevation.list[[2]]=="Mid"]
Iter.len.2[1,Elevation.list[[2]]=="Low"]


par(mar = c(1,1,1,1)+.1)
par(oma = c(1,4,1,1))

# Set representative barnacles ####
if(timing == 1){
  elev <- c(4,110,147)
  elev.1 <- elev
  datetime_int <- datetimes[[1]]
  Iter.len.1[1,elev] #0.21, 0.22, 0.20
  cutoff <- 189 #This should be 188 or 189 but maybe I'm cutting off points twice? 
  length(Iter.len.1[,elev[1]])/96
}

if(timing ==2){
  elev <- c(36,100,121)
  elev.2 <- elev
  datetime_int <- datetimes[[2]]
  length(Iter.len.2[,elev[1]])/96
  cutoff <- 186 #This should be 198 - we are missing 8 days?
  #cutoff <- 196
  head(Iter.len.2[,elev]) #V95 (mid) was a little small, but changed rep barnacles in beginning of April
}

Elev.abbrev.list <- list(elev.1, elev.2) #Need to run timing 1 and 2

# Temp graphs, all ####
setwd("~/Box/Sarah and Molly's Box/FHL data/plots/Interval 1 and 2")
pdf("temps_15_min_int1and2.pdf", width=6.5, height=1.5)
par(mfrow = c(1,3))
par(mar = c(1,4,1,1)+.1)
par(oma = c(1,1,1,1))
par(mfrow = c(1,3))
plot(air.temp[,4], pch = ".", col = "blue", ylim =c(0,35), ylab = "Temperature deg C")
points(water.temp[,4],  pch = ".")
title("Low")

head(air.temp)
plot(air.temp[,3], pch = ".", col = "blue", ylim =c(0,35), ylab = "")
points(water.temp[,3],  pch = ".")
title("Mid")
#points(intake_15min_scaled[,2], col = "green", pch = ".")

plot(air.temp[,2], pch = ".", col = "blue", ylim =c(0,35), ylab = "")
points(water.temp[,2],  pch = ".")
#points(intake_15min_scaled[,3], col = "green", pch = ".")
title("Upper")
dev.off()
setwd("~/Box/Sarah and Molly's Box/FHL data/plots")

# Physiological rates over 1 day, start ####
# This isn't computing for both beginning and end separately I don't think. 
ts
#i_seq <- 100
dim(Iter.air)
#And then again last day (repeat)
end
i_seq <- 2
i <- ts*(i_seq-1)+1
j <- i+ts-1
f <- j+1
temp_i.w <- Iter.water[i:j,]
temp_i.a <- Iter.air[i:j,]
food_i <- Iter.food[i:j]

cost_15min_dat <- as.data.frame(cost_15min[i:j,])
intake_15min_scaled_dat <- as.data.frame(intake_15min_scaled[i:j,])
pred_expose_30_dat <- as.data.frame(pred_expose_30[i:j,])
Hour <- seq(1, to = 96*15, by = 15)/60

if(timing == 2){
setwd("~/Box/Sarah and Molly's Box/FHL data/plots/Interval 2")
pdf("within_day_rates_15_min_int2.pdf", width=6.5, height=1.5)
par(mfrow = c(1,3))
par(mar = c(1,4,1,1)+.1)
par(oma = c(1,1,1,1))
par(mfrow = c(1,3))
par(mfrow = c(1,3))
plot(Hour,cost_15min_dat[,elev[3]], ylim = c(0,.15), type = "l", ylab = "Physiological Rate (J per 15min)",bty = "n")
points(Hour,intake_15min_scaled_dat[,elev[3]], col = "green", type = "l",bty = "n")
points(Hour,pred_expose_30_dat[,elev[3]], col = "blue", type = "l",bty = "n")

plot(Hour,cost_15min_dat[,elev[2]], ylim = c(0,.15), ylab = "", type = "l",bty = "n")
points(Hour,intake_15min_scaled_dat[,elev[2]], col = "green", type = "l",bty = "n")
points(Hour,pred_expose_30_dat[,elev[2]], col = "blue", type = "l",bty = "n")

plot(Hour,cost_15min_dat[,elev[1]], ylim = c(0,.15),
     ylab = "", type = "l",bty = "n")
points(Hour,intake_15min_scaled_dat[,elev[1]], col = "green", type = "l")
points(Hour,pred_expose_30_dat[, elev[1]], col = "blue", type = "l")
dev.off()
}

if(timing == 1){
  setwd("~/Box/Sarah and Molly's Box/FHL data/plots/Interval 1")
  pdf("within_day_rates_15_min_int1.pdf", width=6.5, height=1.5)
  par(mfrow = c(1,3))
  par(mar = c(1,4,1,1)+.1)
  par(oma = c(1,1,1,1))
  par(mfrow = c(1,3))
  par(mfrow = c(1,3))
  plot(Hour,cost_15min_dat[,elev[3]], ylim = c(0,.15), type = "l", ylab = "Physiological Rate (J per 15min)",bty = "n")
  points(Hour,intake_15min_scaled_dat[,elev[3]], col = "green", type = "l",bty = "n")
  points(Hour,pred_expose_30_dat[,elev[3]], col = "blue", type = "l",bty = "n")
  
  plot(Hour,cost_15min_dat[,elev[2]], ylim = c(0,.15), ylab = "", type = "l",bty = "n")
  points(Hour,intake_15min_scaled_dat[,elev[2]], col = "green", type = "l",bty = "n")
  points(Hour,pred_expose_30_dat[,elev[2]], col = "blue", type = "l",bty = "n")
  
  plot(Hour,cost_15min_dat[,elev[1]], ylim = c(0,.15),
       ylab = "", type = "l",bty = "n")
  points(Hour,intake_15min_scaled_dat[,elev[1]], col = "green", type = "l")
  points(Hour,pred_expose_30_dat[, elev[1]], col = "blue", type = "l")
  dev.off()
}

par(mfrow = c(1,3))
#

if(timing ==1){
  setwd("~/Box/Sarah and Molly's Box/FHL data/plots/Interval 1")
  pdf("within_day_temps_15min_int1.pdf", width=6.5, height=1.5)
  par(mfrow = c(1,3))
  par(mar = c(1,4,1,1)+.1)
  par(oma = c(1,1,1,1))
  par(mfrow = c(1,3))
  plot(Hour,temp_i.a[,elev[3]],ylim = c(2,30),type = "l",bty = "n",ylab = "Temp", col = "grey")
  points(Hour,temp_i.w[,elev[3]], col = "blue",type = "l",bty = "n",ylab = "")
  plot(Hour,temp_i.a[,elev[2]],ylim = c(2,30),type = "l",bty = "n",ylab = "", col = "grey")
  points(Hour,temp_i.w[,elev[2]], col = "blue",type = "l",bty = "n",ylab = "")
  plot(Hour,temp_i.a[,elev[1]], ylim = c(2,30),type = "l",bty = "n",ylab = "", col = "grey")
  points(Hour,temp_i.w[,elev[1]], col = "blue",type = "l",bty = "n",ylab = "")
  dev.off()
}

if(timing ==2){
  setwd("~/Box/Sarah and Molly's Box/FHL data/plots/Interval 2")
  pdf("within_day_temps_15min_int2.pdf", width=6.5, height=1.5)
  par(mfrow = c(1,3))
  par(mar = c(1,4,1,1)+.1)
  par(oma = c(1,1,1,1))
  par(mfrow = c(1,3))
  plot(Hour,temp_i.a[,elev[3]],ylim = c(2,30),type = "l",bty = "n",ylab = "Temp", col = "grey")
  points(Hour,temp_i.w[,elev[3]], col = "blue",type = "l",bty = "n",ylab = "")
  plot(Hour,temp_i.a[,elev[2]],ylim = c(2,30),type = "l",bty = "n",ylab = "", col = "grey")
  points(Hour,temp_i.w[,elev[2]], col = "blue",type = "l",bty = "n",ylab = "")
  plot(Hour,temp_i.a[,elev[1]], ylim = c(2,30),type = "l",bty = "n",ylab = "", col = "grey")
  points(Hour,temp_i.w[,elev[1]], col = "blue",type = "l",bty = "n",ylab = "")
  dev.off()
}


# Physiological rates over 1 day, end ####
# This isn't computing for both beginning and end separately I don't think. 
ts
#i_seq <- 100
dim(Iter.air)
#And then again last day (repeat)
end
i_seq <- end
i <- ts*(i_seq-1)+1
j <- i+ts-1
f <- j+1
temp_i.w <- Iter.water[i:j,]
temp_i.a <- Iter.air[i:j,]
food_i <- Iter.food[i:j]

cost_15min_dat <- as.data.frame(cost_15min[i:j,])
intake_15min_scaled_dat <- as.data.frame(intake_15min_scaled[i:j,])
pred_expose_30_dat <- as.data.frame(pred_expose_30[i:j,])
Hour <- seq(1, to = 96*15, by = 15)/60

if(timing == 1){
  setwd("~/Box/Sarah and Molly's Box/FHL data/plots/Interval 1")
  pdf("within_day_rates_15_min_int1_end.pdf", width=6.5, height=1.5)
  par(mfrow = c(1,3))
  par(mar = c(1,4,1,1)+.1)
  par(oma = c(1,1,1,1))
  par(mfrow = c(1,3))
  par(mfrow = c(1,3))
  plot(Hour,cost_15min_dat[,elev[3]], ylim = c(0,.15), type = "l", ylab = "Physiological Rate (J per 15min)",bty = "n")
  points(Hour,intake_15min_scaled_dat[,elev[3]], col = "green", type = "l",bty = "n")
  points(Hour,pred_expose_30_dat[,elev[3]], col = "blue", type = "l",bty = "n")
  
  plot(Hour,cost_15min_dat[,elev[2]], ylim = c(0,.15), ylab = "", type = "l",bty = "n")
  points(Hour,intake_15min_scaled_dat[,elev[2]], col = "green", type = "l",bty = "n")
  points(Hour,pred_expose_30_dat[,elev[2]], col = "blue", type = "l",bty = "n")
  
  plot(Hour,cost_15min_dat[,elev[1]], ylim = c(0,.15),
       ylab = "", type = "l",bty = "n")
  points(Hour,intake_15min_scaled_dat[,elev[1]], col = "green", type = "l")
  points(Hour,pred_expose_30_dat[, elev[1]], col = "blue", type = "l")
  dev.off()
}

if(timing == 2){
  setwd("~/Box/Sarah and Molly's Box/FHL data/plots/Interval 2")
  pdf("within_day_rates_15_min_int2_end.pdf", width=6.5, height=1.5)
  par(mfrow = c(1,3))
  par(mar = c(1,4,1,1)+.1)
  par(oma = c(1,1,1,1))
  par(mfrow = c(1,3))
  par(mfrow = c(1,3))
  plot(Hour,cost_15min_dat[,elev[3]], ylim = c(0,.15), type = "l", ylab = "Physiological Rate (J per 15min)",bty = "n")
  points(Hour,intake_15min_scaled_dat[,elev[3]], col = "green", type = "l",bty = "n")
  points(Hour,pred_expose_30_dat[,elev[3]], col = "blue", type = "l",bty = "n")
  
  plot(Hour,cost_15min_dat[,elev[2]], ylim = c(0,.15), ylab = "", type = "l",bty = "n")
  points(Hour,intake_15min_scaled_dat[,elev[2]], col = "green", type = "l",bty = "n")
  points(Hour,pred_expose_30_dat[,elev[2]], col = "blue", type = "l",bty = "n")
  
  plot(Hour,cost_15min_dat[,elev[1]], ylim = c(0,.15),
       ylab = "", type = "l",bty = "n")
  points(Hour,intake_15min_scaled_dat[,elev[1]], col = "green", type = "l")
  points(Hour,pred_expose_30_dat[, elev[1]], col = "blue", type = "l")
  dev.off()
}

if(timing == 1){
  setwd("~/Box/Sarah and Molly's Box/FHL data/plots/Interval 1")
  pdf("within_day_temps_15min_int1_end.pdf", width=6.5, height=1.5)
  par(mfrow = c(1,3))
  par(mar = c(1,4,1,1)+.1)
  par(oma = c(1,1,1,1))
  par(mfrow = c(1,3))
  plot(Hour,temp_i.a[,elev[3]],ylim = c(2,30),type = "l",bty = "n",ylab = "Temp", col = "grey")
  points(Hour,temp_i.w[,elev[3]], col = "blue",type = "l",bty = "n",ylab = "")
  plot(Hour,temp_i.a[,elev[2]],ylim = c(2,30),type = "l",bty = "n",ylab = "", col = "grey")
  points(Hour,temp_i.w[,elev[2]], col = "blue",type = "l",bty = "n",ylab = "")
  plot(Hour,temp_i.a[,elev[1]], ylim = c(2,30),type = "l",bty = "n",ylab = "", col = "grey")
  points(Hour,temp_i.w[,elev[1]], col = "blue",type = "l",bty = "n",ylab = "")
  dev.off()
}
if(timing == 2){
  setwd("~/Box/Sarah and Molly's Box/FHL data/plots/Interval 2")
  pdf("within_day_temps_15min_int2_end.pdf", width=6.5, height=1.5)
  par(mfrow = c(1,3))
  par(mar = c(1,4,1,1)+.1)
  par(oma = c(1,1,1,1))
  par(mfrow = c(1,3))
  plot(Hour,temp_i.a[,elev[3]],ylim = c(2,30),type = "l",bty = "n",ylab = "Temp", col = "grey")
  points(Hour,temp_i.w[,elev[3]], col = "blue",type = "l",bty = "n",ylab = "")
  plot(Hour,temp_i.a[,elev[2]],ylim = c(2,30),type = "l",bty = "n",ylab = "", col = "grey")
  points(Hour,temp_i.w[,elev[2]], col = "blue",type = "l",bty = "n",ylab = "")
  plot(Hour,temp_i.a[,elev[1]], ylim = c(2,30),type = "l",bty = "n",ylab = "", col = "grey")
  points(Hour,temp_i.w[,elev[1]], col = "blue",type = "l",bty = "n",ylab = "")
  dev.off()
}

# Growth curve ####
if(timing == 1){
  setwd("~/Box/Sarah and Molly's Box/FHL data/plots/Interval 1")
  pdf("Length_pred_int1.pdf", width=4, height=4)
  par(mfrow = c(1,1))
  #plot(matrix(seq(1, length(Iter.len.day), by = 1), nrow = length(Iter.len.day), ncol = Iter.len.day[,names(temp_i.a)=="Upper"]),Iter.len.day[,names(temp_i.a)=="Upper"], ylim = c(0,5), pch = ".", ylab = "Length (mm)", xlab = "Day") #Upper
  plot(Iter.len.day[,elev[1]], col = "black", type = "l", ylim = c(1,4), ylab = "Length (mm)", xlab = "Day") #Upper
  lines(Iter.len.day[,elev[2]], col = "purple") #Mid
  lines(Iter.len.day[,elev[3]], col = "orange") #Low
  dev.off()
  
  mass <- 10^(0.4864*(Iter.len.day[,elev])-0.7912)
  joule_mass <- mass * ED_J_p_mg
  
  pdf("Joule_pred_int1.pdf", width=4, height=4)
  par(mfrow = c(1,1))
  #plot(matrix(seq(1, length(Iter.len.day), by = 1), nrow = length(Iter.len.day), ncol = Iter.len.day[,names(temp_i.a)=="Upper"]),Iter.len.day[,names(temp_i.a)=="Upper"], ylim = c(0,5), pch = ".", ylab = "Length (mm)", xlab = "Day") #Upper
  plot(joule_mass[,1], col = "black", type = "l", 
       ylim = c(0,120), 
       ylab = "Energy content (J)", xlab = "Day") #Upper
  lines(joule_mass[,2], col = "purple") #Mid
  lines(joule_mass[,3], col = "orange") #Low
  dev.off()
}
if(timing == 2){
  setwd("~/Box/Sarah and Molly's Box/FHL data/plots/Interval 2")
  pdf("Length_pred_int2.pdf", width=4, height=4)
  par(mfrow = c(1,1))
  #plot(matrix(seq(1, length(Iter.len.day), by = 1), nrow = length(Iter.len.day), ncol = Iter.len.day[,names(temp_i.a)=="Upper"]),Iter.len.day[,names(temp_i.a)=="Upper"], ylim = c(0,5), pch = ".", ylab = "Length (mm)", xlab = "Day") #Upper
  plot(Iter.len.day[,elev[1]], col = "black", type = "l", ylim = c(1,4), ylab = "Length (mm)", xlab = "Day") #Upper
  lines(Iter.len.day[,elev[2]], col = "purple") #Mid
  lines(Iter.len.day[,elev[3]], col = "orange") #Low
  dev.off()
  
  mass <- 10^(0.4864*(Iter.len.day[,elev])-0.7912)
  joule_mass <- mass * ED_J_p_mg
  
  pdf("Joule_pred_int2.pdf", width=4, height=4)
  par(mfrow = c(1,1))
  #plot(matrix(seq(1, length(Iter.len.day), by = 1), nrow = length(Iter.len.day), ncol = Iter.len.day[,names(temp_i.a)=="Upper"]),Iter.len.day[,names(temp_i.a)=="Upper"], ylim = c(0,5), pch = ".", ylab = "Length (mm)", xlab = "Day") #Upper
  plot(joule_mass[,1], col = "black", type = "l", 
       ylim = c(0,120), 
       ylab = "Energy content (J)", xlab = "Day") #Upper
  lines(joule_mass[,2], col = "purple") #Mid
  lines(joule_mass[,3], col = "orange") #Low
  dev.off()
}

# Obs vs. predicted growth ####
if(timing == 1){
  setwd("~/Box/Sarah and Molly's Box/FHL data/plots/Interval 1")
  pdf("Pred_vs_Obs_int1.pdf", width=4, height=4)
  means_Obslen <- tapply(obs.growth.len, as.factor(Elevation), mean)
  means_Predlen <- tapply(pred.len, as.factor(Elevation), mean)
  sd_Obslen <- tapply(obs.growth.len, as.factor(Elevation), sd)
  sd_Predlen <- tapply(pred.len, as.factor(Elevation), sd)
  
  par(mfrow = c(1,1))
  plot(means_Predlen ~ means_Obslen, xlim = c(0,1.5), ylim = c(0,1.5), 
       col = c("orange","purple","black"),
       #pch = c(15,16,17), for time 2
       pch = c(0,1,2), #for time 1
       ylab = "Predicted growth (mm)",
       xlab = "Observed growth (mm)")
  lines(x = c(-1,2.5),y = c(-1,2.5))
  arrows(x0=means_Obslen, y0=means_Predlen-sd_Predlen, x1=means_Obslen, 
         y1=means_Predlen+sd_Predlen, code=0)
  arrows(x0=means_Obslen-sd_Obslen, y0=means_Predlen, x1=means_Obslen+sd_Obslen, 
         y1=means_Predlen, code=0)
  dev.off()
}
if(timing == 2){
  setwd("~/Box/Sarah and Molly's Box/FHL data/plots/Interval 2")
  pdf("Pred_vs_Obs_int2.pdf", width=4, height=4)
  means_Obslen <- tapply(obs.growth.len, as.factor(Elevation), mean)
  means_Predlen <- tapply(pred.len, as.factor(Elevation), mean)
  sd_Obslen <- tapply(obs.growth.len, as.factor(Elevation), sd)
  sd_Predlen <- tapply(pred.len, as.factor(Elevation), sd)
  
  par(mfrow = c(1,1))
  plot(means_Predlen ~ means_Obslen, xlim = c(0,1.5), ylim = c(0,1.5), 
       col = c("orange","purple","black"),
       pch = c(15,16,17),
       #pch = c(0,1,2), #for time 1
       ylab = "Predicted growth (mm)",
       xlab = "Observed growth (mm)")
  lines(x = c(-1,2.5),y = c(-1,2.5))
  arrows(x0=means_Obslen, y0=means_Predlen-sd_Predlen, x1=means_Obslen, 
         y1=means_Predlen+sd_Predlen, code=0)
  arrows(x0=means_Obslen-sd_Obslen, y0=means_Predlen, x1=means_Obslen+sd_Obslen, 
         y1=means_Predlen, code=0)
  dev.off()
}

# Physiological rates in terms of day ####
# new_intake in terms of date ####
length(datetime_int)/96
pred_expose_30_dat2 <- cbind(as.data.frame(pred_expose_30)[,elev], datetime = datetime_int[1:nrow(pred_expose_30)])
intake_dat2 <- cbind(as.data.frame(intake_15min_scaled)[elev], datetime = datetime_int[1:nrow(intake_15min_scaled)])
cost_15min_dat2 <- cbind(as.data.frame(cost_15min)[elev], datetime = datetime_int[1:nrow(cost_15min)])
head(pred_expose_30_dat2)
if(timing ==1){
new_expose <- pred_expose_30_dat2 %>%
  group_by(date = as.Date(ymd_hms(datetime))) %>%
  dplyr::summarise(V1 = sum(V4),
                   V2 = sum(V110),
                   V3 = sum(V147))

new_intake <- intake_dat2 %>%
  group_by(date = as.Date(ymd_hms(datetime))) %>%
  dplyr::summarise(V1 = sum(V4),
                   V2 = sum(V110),
                   V3 = sum(V147))

new_aq_cost <- cost_15min_dat2 %>%
  group_by(date = as.Date(ymd_hms(datetime))) %>%
  dplyr::summarise(V1 = sum(V4),
                   V2 = sum(V110),
                   V3 = sum(V147))
}
if(timing == 2){
  new_expose <- pred_expose_30_dat2 %>%
    group_by(date = as.Date(ymd_hms(datetime))) %>%
    dplyr::summarise(V1 = sum(V36),
                     V2 = sum(V100),
                     V3 = sum(V121))
  
  new_intake <- intake_dat2 %>%
    group_by(date = as.Date(ymd_hms(datetime))) %>%
    dplyr::summarise(V1 = sum(V36),
                     V2 = sum(V100),
                     V3 = sum(V121))
  
  new_aq_cost <- cost_15min_dat2 %>%
    group_by(date = as.Date(ymd_hms(datetime))) %>%
    dplyr::summarise(V1 = sum(V36),
                     V2 = sum(V100),
                     V3 = sum(V121))
}
daily_SFG <- new_intake[,2:4] - new_expose[,2:4] - new_aq_cost[,2:4]

names(new_intake) <- c("date","Upper", "Mid", "Low")
names(new_expose) <- c("date","Upper", "Mid", "Low")
names(new_aq_cost) <- c("date","Upper", "Mid", "Low")
names(daily_SFG) <- c("Upper", "Mid", "Low")

#Don't plot initial and final timepoint, because not a complete timepoint
new_intake <- new_intake[2:cutoff,]
new_expose <- new_expose[2:cutoff,]
new_aq_cost <- new_aq_cost[2:cutoff,]
daily_SFG <- daily_SFG[2:cutoff,]
tail(new_intake)
head(new_intake)
dim(new_intake)

nrow(new_intake)
# Plot all rates, pretty version LMU, 6.5X1.5 dim ####
dev.off()
if(timing == 1){
  pdf("daily_physiological_rates_int1.pdf", width=6.5, height=1.5)
  par(mfrow = c(1,3))
  par(mar = c(1,1,1,1)+.1)
  par(oma = c(1,4,1,1))
  
  plot(new_intake$date, new_intake$Low, type = "l", ylim = c(0,4.5),yaxs = "i",
       col = "darkslategray", xlab = "", ylab = "Physiological rate (J / day)", bty = "n", lty = 1)
  points(new_intake$date, new_aq_cost$Low, type = "l", col = "blue", lty = 1)
  points(new_intake$date, new_expose$Low, type = "l", col = "darkorchid4", lty = 1, lwd = 1.5)
  
  legend(x = "topleft",
         legend = c("Intake", "Aquatic cost", "Aerial cost"),
         col =c("darkslategray", "blue", "darkorchid4"), lty=1:1, box.lty=0, bg="transparent", bty = "n")
  
  plot(new_intake$date, new_intake$Mid, type = "l", ylim = c(0,4.5),yaxs = "i",
       col = "darkslategray", xlab = "", ylab = "Physiological rate\n (J / day)", bty = "n")
  points(new_intake$date, new_aq_cost$Mid, type = "l", col = "blue")
  points(new_intake$date, new_expose$Mid, type = "l", col = "darkorchid4", lwd = 1.5)
  
  plot(new_intake$date, new_intake$Upper, type = "l", ylim = c(0,4.5), yaxs = "i",
       col = "darkslategray", xlab = "", ylab = "Physiological rate (J / day)", bty = "n")
  points(new_intake$date, new_aq_cost$Upper, type = "l", col = "blue")
  points(new_intake$date, new_expose$Upper, type = "l", col = "darkorchid4", lwd = 1.5)
  dev.off()
}
if(timing == 2){
  pdf("daily_physiological_rates_int2.pdf", width=6.5, height=1.5)
  par(mfrow = c(1,3))
  par(mar = c(1,1,1,1)+.1)
  par(oma = c(1,4,1,1))
  
  plot(new_intake$date, new_intake$Low, type = "l", ylim = c(0,4.5),yaxs = "i",
       col = "darkslategray", xlab = "", ylab = "Physiological rate (J / day)", bty = "n", lty = 1)
  points(new_intake$date, new_aq_cost$Low, type = "l", col = "blue", lty = 1)
  points(new_intake$date, new_expose$Low, type = "l", col = "darkorchid4", lty = 1, lwd = 1.5)
  
  legend(x = "topleft",
         legend = c("Intake", "Aquatic cost", "Aerial cost"),
         col =c("darkslategray", "blue", "darkorchid4"), lty=1:1, box.lty=0, bg="transparent", bty = "n")
  
  plot(new_intake$date, new_intake$Mid, type = "l", ylim = c(0,4.5),yaxs = "i",
       col = "darkslategray", xlab = "", ylab = "Physiological rate\n (J / day)", bty = "n")
  points(new_intake$date, new_aq_cost$Mid, type = "l", col = "blue")
  points(new_intake$date, new_expose$Mid, type = "l", col = "darkorchid4", lwd = 1.5)
  
  plot(new_intake$date, new_intake$Upper, type = "l", ylim = c(0,4.5), yaxs = "i",
       col = "darkslategray", xlab = "", ylab = "Physiological rate (J / day)", bty = "n")
  points(new_intake$date, new_aq_cost$Upper, type = "l", col = "blue")
  points(new_intake$date, new_expose$Upper, type = "l", col = "darkorchid4", lwd = 1.5)
  dev.off()
}

# Plot SFG, pretty version LMU, 6.5X1.5 dim ####
if(timing == 1){
  pdf("daily_SFG_int1.pdf", width=6.5, height=1.5)
  par(mfrow = c(1,3))
  par(mar = c(1,1,1,1)+.1)
  par(oma = c(1,4,1,1))
  par(mfrow = c(1,3))
  
  dim(daily_SFG)
  plot(new_intake$date, daily_SFG$Low,type = "l", ylim = c(-.9,4.5), yaxs = "i",
       col = "orange", xlab = "", ylab = "Physiological rate (J / day)", bty = "n")
  
  legend(x = "topleft",
         legend = c("Scope for Growth"),
         col =c("orange"), lty=1:1, box.lty=0, bg="transparent", bty = "n")
  
  plot(new_intake$date, daily_SFG$Mid,type = "l", ylim = c(-.9,4.5), yaxs = "i",
       col = "orange", xlab = "", ylab = "Physiological rate (J / day)", bty = "n")
  plot(new_intake$date, daily_SFG$Upper,type = "l", ylim = c(-.9,4.5), yaxs = "i",
       col = "orange", xlab = "", ylab = "Physiological rate (J / day)", bty = "n")
  dev.off()
}
if(timing == 2){
  pdf("daily_SFG_int2.pdf", width=6.5, height=1.5)
  par(mfrow = c(1,3))
  par(mar = c(1,1,1,1)+.1)
  par(oma = c(1,4,1,1))
  par(mfrow = c(1,3))
  
  dim(daily_SFG)
  plot(new_intake$date, daily_SFG$Low,type = "l", ylim = c(-.9,4.5), yaxs = "i",
       col = "orange", xlab = "", ylab = "Physiological rate (J / day)", bty = "n")
  
  legend(x = "topleft",
         legend = c("Scope for Growth"),
         col =c("orange"), lty=1:1, box.lty=0, bg="transparent", bty = "n")
  
  plot(new_intake$date, daily_SFG$Mid,type = "l", ylim = c(-.9,4.5), yaxs = "i",
       col = "orange", xlab = "", ylab = "Physiological rate (J / day)", bty = "n")
  plot(new_intake$date, daily_SFG$Upper,type = "l", ylim = c(-.9,4.5), yaxs = "i",
       col = "orange", xlab = "", ylab = "Physiological rate (J / day)", bty = "n")
  dev.off()
}
# Calc daily air and water temp ####

# Look to see if/where data is missing
plot(air.temp.2$datetime,air.temp.2$Low, type = "p", pch = ".", xlim = c(as.POSIXct("2018-08-10"),as.POSIXct("2018-12-01")))
points(water.temp.2$datetime,water.temp.2$Low, type = "p", pch = ".", xlim = c(as.POSIXct("2018-08-10"),as.POSIXct("2018-11-01")))
# Aug 22 - Aug 25

if(timing == 1){
  air.temp.int <- air.temp.1
  water.temp.int <- water.temp.1
  setwd("~/Box/Sarah and Molly's Box/FHL data/plots/Interval 1")
}

if(timing == 2){
air.temp.int <- air.temp.2
water.temp.int <- water.temp.2
setwd("~/Box/Sarah and Molly's Box/FHL data/plots/Interval 2")
}
daily_air_temp <- air.temp.int %>%
  group_by(date = as.Date(ymd_hms(datetime))) %>%
  dplyr::summarise(Low = mean(Low,  na.rm = TRUE),
                   Mid = mean(Mid,  na.rm = TRUE),
                   Upper = mean(Upper,  na.rm = TRUE))
daily_air_temp
daily_water_temp <- water.temp.int %>%
  group_by(date = as.Date(ymd_hms(datetime))) %>%
  dplyr::summarise(Low = mean(Low,  na.rm = TRUE),
                   Mid = mean(Mid,  na.rm = TRUE),
                   Upper = mean(Upper,  na.rm = TRUE))

# Upper 75% quantile ####
#A quartile is the value of the quantile at the probabilities 0.25, 0.5 and 0.75.
#https://stackoverflow.com/questions/26532566/quantile-vs-quartile-in-lay-terms
daily_air_temp_quant <- air.temp.int %>%
  group_by(date = as.Date(ymd_hms(datetime))) %>%
  dplyr::summarise(Low = quantile(Low, probs = 0.75, na.rm = TRUE),
                   Mid = quantile(Mid, probs = 0.75, na.rm = TRUE),
                   Upper = quantile(Upper, probs = 0.75, na.rm = TRUE))

daily_air_temp_quant_low <- air.temp.int %>%
  group_by(date = as.Date(ymd_hms(datetime))) %>%
  dplyr::summarise(Low = quantile(Low, probs = 0.25, na.rm = TRUE),
                   Mid = quantile(Mid, probs = 0.25, na.rm = TRUE),
                   Upper = quantile(Upper, probs = 0.25, na.rm = TRUE))

daily_air_temp <- daily_air_temp[2:cutoff,]
daily_water_temp <- daily_water_temp[2:cutoff,]
daily_air_temp_quant <- daily_air_temp_quant[2:cutoff,]
daily_air_temp_quant_low <- daily_air_temp_quant_low[2:cutoff,]

# Plot temp, 6.5X1.5 ####
# Note dimensions must be called within pdf() - also saving as a .pdf through th GUI is broken. 

pdf("temp_incl_3Q.pdf", width=6.5, height=1.5)
par(mfrow = c(1,3))
par(mar = c(1,1,1,1)+.1)
par(oma = c(1,4,1,1))
min(daily_air_temp$Mid, na.rm = TRUE)
plot(daily_air_temp$date, daily_air_temp$Low, type = "l", ylim = c(0,32),yaxs = "i",
     col = "darkorchid4", xlab = "", ylab = expression(paste("Temp (",degree,"C)")), bty = "n", lty = 1)
points(daily_water_temp$date, daily_water_temp$Low, type = "l", col = "blue", lty = 1)
points(daily_air_temp_quant$date, daily_air_temp_quant$Low, type = "l", yaxs = "i",
       col = "darkred", xlab = "", ylab = "Physiological rate (J / day)", bty = "n", lty = 3)

plot(daily_air_temp$date, daily_air_temp$Mid, type = "l", ylim = c(0,32),yaxs = "i",
     col = "darkorchid4", xlab = "", ylab = "", bty = "n")
points(daily_water_temp$date, daily_water_temp$Mid, type = "l", col = "blue")
points(daily_air_temp_quant$date, daily_air_temp_quant$Mid, type = "l", yaxs = "i",
       col = "darkred", xlab = "", ylab = "Physiological rate (J / day)", bty = "n", lty = 3)

plot(daily_air_temp$date, daily_air_temp$Upper, type = "l", ylim = c(0,32), yaxs = "i",
     col = "darkorchid4", xlab = "", ylab = "", bty = "n")
points(daily_water_temp$date, daily_water_temp$Upper, type = "l", col = "blue")
points(daily_air_temp_quant$date, daily_air_temp_quant$Upper, type = "l", yaxs = "i",
       col = "darkred", xlab = "", ylab = "Physiological rate (J / day)", bty = "n", lty = 3)
dev.off()



pdf("temp_legend_incl_3Q.pdf", width=6.5, height=1.5)
par(mfrow = c(1,3))
par(mfrow = c(1,3))
par(mar = c(1,1,1,1)+.1)
par(oma = c(1,4,1,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend(x = "topleft",
       legend = c("75%-ile low tide temp", "Low tide temp",  "Water temp"),
       col =c("darkred","darkorchid4", "blue"), lty=c(3,1,1), box.lty=0, bg="transparent", bty = "n")
dev.off()


# Calc the time submerged per day ####
# dplyr was intially not working so here is a workaround:
if(timing == 1){
  air.temp.interval <- air.temp.1
  water.temp.interval <- water.temp.1
  food.int <- water.temp.1$food
  datetime.int <- water.temp.1$datetime
  setwd("~/Box/Sarah and Molly's Box/FHL data/plots/Interval 1")
}

if(timing == 2){
  air.temp.interval <- air.temp.2
  water.temp.interval <- water.temp.2
  food.int <- water.temp.2$food
  datetime.int <- water.temp.2$datetime
  setwd("~/Box/Sarah and Molly's Box/FHL data/plots/Interval 2")
}

air.temp.day <- cbind(air.temp.interval,day = yday(air.temp.interval$datetime))
air.temp_count_U <- tapply(!is.na(air.temp.day[,"Upper"]),air.temp.day$day, sum, na.rm = TRUE)
air.temp_count_M <- tapply(!is.na(air.temp.day[,"Mid"]),air.temp.day$day, sum, na.rm = TRUE)
air.temp_count_L <- tapply(!is.na(air.temp.day[,"Low"]),air.temp.day$day, sum, na.rm = TRUE)
air.temp.count <- data.frame(
  Upper = air.temp_count_U,
  Mid = air.temp_count_M,
  Low = air.temp_count_L
)

water.temp.day <- cbind(water.temp.interval,day = yday(water.temp.interval$datetime))
water.temp_count_U <- tapply(!is.na(water.temp.day[,"Upper"]),water.temp.day$day, sum, na.rm = TRUE)
water.temp_count_M <- tapply(!is.na(water.temp.day[,"Mid"]),water.temp.day$day, sum, na.rm = TRUE)
water.temp_count_L <- tapply(!is.na(water.temp.day[,"Low"]),water.temp.day$day, sum, na.rm = TRUE)
water.temp.count <- data.frame(
  Upper = water.temp_count_U,
  Mid = water.temp_count_M,
  Low = water.temp_count_L
)
str(water.temp.count)
water.temp.count
water.temp.count$Upper[is.nan(water.temp.count$Upper)] <- NA
water.temp.count$Mid[is.nan(water.temp.count$Mid)] <- NA
water.temp.count$Low[is.nan(water.temp.count$Low)] <- NA
air.temp.count$Low[is.nan(air.temp.count$Low)] <- NA
air.temp.count$Mid[is.nan(air.temp.count$Mid)] <- NA
air.temp.count$Upper[is.nan(air.temp.count$Upper)] <- NA

time.submerged <- water.temp.count / (water.temp.count + air.temp.count)

# time.submerged[water.temp.count$Upper<=10 |air.temp.count$Upper<=18,"Upper"]<- NA #Wow - actually there is one day with <10 intervals submerged - that is 2.5 hours.
# time.submerged[water.temp.count$Mid<=10 |air.temp.count$Mid<=18,"Mid"]<- NA
# time.submerged[water.temp.count$Low<=10 |air.temp.count$Low<=18,"Low"]<- NA

time.submerged[water.temp.count$Upper+air.temp.count$Upper<=95,"Upper"]<- NA
time.submerged[water.temp.count$Mid+air.temp.count$Mid<=95,"Mid"]<- NA
time.submerged[water.temp.count$Low+air.temp.count$Low<=95,"Low"]<- NA
plot(time.submerged$Low, ylim = c(0,30), type = 'b')
plot(air.temp.count$Upper+water.temp.count$Upper, ylim = c(0,96), type = 'l')
plot(air.temp.count$Upper+water.temp.count$Upper, ylim = c(0,96), type = 'l')
plot(air.temp.count$Upper+water.temp.count$Upper, xlim = c(180,200), ylim = c(0,96), type = 'b')
plot(air.temp.count$Upper+water.temp.count$Upper, xlim = c(195,200), ylim = c(0,96), type = 'b')


# Plot time submerged, pretty version LMU, 6.5X1.5 dim ####
pdf("submergence_time.pdf", width=6.5, height=1.5)
par(mfrow = c(1,3))
par(mar = c(1,1,1,1)+.1)
par(oma = c(1,4,1,1))
par(mfrow = c(1,3))

plot(new_intake$date, time.submerged$Low[2:cutoff],type = "l", ylim = c(0,1), yaxs = "i",
     col = "black", xlab = "", ylab = "Physiological rate (J / day)", bty = "n")
lines(as.Date(water.temp.int$datetime), food.int/(food.int+par[3]), cex = .5,
     xlab = "", ylim = c(0,1),  yaxs = "i", type = "l", lty = 2, bty = "n", ylab = "Food availability")

legend(x = "bottomleft",
       legend = c("Food availability","Time submerged"),
       col =c("black","black"), lty=c(2,1), box.lty=0, bg="transparent", bty = "n")

plot(new_intake$date, time.submerged$Mid[2:cutoff],type = "l", ylim = c(0,1), yaxs = "i",
     col = "black", xlab = "", ylab = "Physiological rate (J / day)", bty = "n")
lines(as.Date(water.temp.int$datetime), food.int/(food.int+par[3]), cex = .5,
      xlab = "", ylim = c(0,1),  yaxs = "i", type = "l", lty = 2, bty = "n", ylab = "Food availability")

plot(new_intake$date, time.submerged$Upper[2:cutoff],type = "l", ylim = c(0,1), yaxs = "i",
     col = "black", xlab = "", ylab = "Physiological rate (J / day)", bty = "n")
lines(as.Date(water.temp.int$datetime), food.int/(food.int+par[3]), cex = .5,
      xlab = "", ylim = c(0,1),  yaxs = "i", type = "l", lty = 2, bty = "n", ylab = "Food availability")

dev.off()

# Plot food over time ####
setwd("~/Box/Sarah and Molly's Box/FHL data/plots/Interval 1 and 2")
pdf("Food availability over time_int1and2.pdf", width=6.5*1.5, height=1.5*1.5)
par(mfrow = c(1,2))
par(mar = c(1,4,1,5)+.1)
par(oma = c(1,2,1,2))

# par(mar = c(5, 4, 4, 5) + 0.3)              # Additional space for second y-axis
# plot(x, y1, pch = 16, col = 2)              # Create first plot
# par(new = TRUE)                             # Add new plot
# plot(x, y2, pch = 17, col = 3,              # Create second plot without axes
#      axes = FALSE, xlab = "", ylab = "")
# axis(side = 4, at = pretty(range(y2)))      # Add second axis
# mtext("y2", side = 4, line = 3)  


par(new = FALSE)
par(mfrow = c(1,2))
plot(water.temp.1$datetime, food.list[[1]]/(food.list[[1]]+par[3]), cex = .5,
     ylim = c(0,1), yaxs = "i",type = "l",bty = "n", ylab = "Food availability", xlab = "")
par(new = TRUE)
plot(water.temp.1$datetime, food.list[[1]], ylim = c(0,20), axes = FALSE,  cex = .5,
     ylab = "", yaxs = "i", type = "l",lty = 2, bty = "n", xlab = "")                           # Add new plot
axis(side = 4, at = pretty(range(food.list[[1]])))      # Add second axis
mtext("ug / L Chl", side = 4, line = 3)  

par(new = FALSE)

plot(water.temp.2$datetime, food.list[[2]]/(food.list[[2]]+par[3]), cex = .5,
     xlab = "", ylim = c(0,1),  yaxs = "i", type = "l", lty = 1, bty = "n", ylab = "Food availability")
par(new = TRUE)
plot(water.temp.2$datetime, food.list[[2]], ylim = c(0,20),  axes = FALSE,cex = .5,
     ylab = "", yaxs = "i", type = "l", lty = 2, bty = "n", xlab = "")
axis(side = 4, at = pretty(range(food.list[[1]])))      # Add second axis
mtext("ug / L Chl", side = 4, line = 3)  
par(new = FALSE)
dev.off()

if(timing == 1){
  setwd("~/Box/Sarah and Molly's Box/FHL data/plots/Interval 1")
  #save.image(file = "plots_time1_workspace.20200412.RData")
  
}
if(timing == 2){
  setwd("~/Box/Sarah and Molly's Box/FHL data/plots/Interval 2")
  #save.image(file = "plots_time2_workspace.20200410.RData")
}

# Climate change
# run model again but adding to temperatures
# Make heatmap, so is iteration
# I'm looking at % increase or loss in growth (mg)
# I'm also looking at % increase or loss in growth (J)
# Where is SFG <0? Would it be less than 0 in certain seasons? 
# Where on shore is this species not viable? 
# Estimate SFG one unit above and 1 unit below. 
# 1.2, 1.55, 1.9
# Interpolate temperatures? Assume temps stay at same temp? 
1.9-1.55
1.55-1.2
1.9+.35


