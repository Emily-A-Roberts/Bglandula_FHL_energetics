# This is a little old, but has more graphs for paper that aren't all organized in the new file. 
# Graphs to accompany "Iter_Model_20210113_iter_small_barnacles_food_3scalars_notbabymodel_optimparallel"


library(reshape2)
library(dplyr)
library(lubridate)
library(tsibble) #Can use function yearweek()
library(ggplot2)
#https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
library(gridExtra)
library(dplyr)
setwd("~/Box/Sarah and Molly's Box/FHL data/plots")

par(mar = c(1,1,1,1)+.1)
par(oma = c(1,4,1,1))



#Check that correct parameter vector is being used.
par1
par2
par

# setwd("~/Box/Sarah and Molly's Box/FHL data/code/Desktop")
# mle <- read.csv(mle_fcn_time.csv)
# par<- c(mle[,2])
# str(par)
# 
# par <- c(1.7, 0.01, 17, .5) #From Feb 14 mle
# 
#Then run code

par(mfrow = c(1,3))

# Select 3 representative samples for many of graphs, near mean size
# Mean initial mean opercular length is 2.17, so picked barnacles with initial sizes ranging 2.0-2.2  
timing <- 2
if(timing == 1){
  elev <- c(4,110,147)

}
if(timing ==2){
  elev <- c(5,95,137)}


head(air.temp.1)
head(air.temp.2)
head(pred_expose_30_dat2)

 table(Elevation)
 par(mfrow = c(1,1))
 plot(Iter.air[,elev[2]], pch = ".") #Mid
 points(Iter.air[,elev[1]], col = "blue", pch = ".") #Upper
 points(Iter.air[,elev[3]], col = "purple", pch = ".") #Low

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

# Plot intake, and aquatic and aerial exposure
 end
#Within one day (i_seq = 1)
 i_seq <- 2
#And then again last day (repeat)
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
 
 str(pred_expose_30)

 par(mfrow = c(1,3))
 plot(cost_15min_dat[,elev[1]], ylim = c(0,.15),
        ylab = "Physiological Rate (J per 15min)")
 points(intake_15min_scaled_dat[,elev[1]], col = "green")
 points(pred_expose_30_dat[, elev[1]], col = "blue")

 plot(cost_15min_dat[,elev[2]], ylim = c(0,.15), ylab = "Physiological Rate (J per 15min)")
 points(intake_15min_scaled_dat[,elev[2]], col = "green")
 points(pred_expose_30_dat[,elev[2]], col = "blue")

 plot(cost_15min_dat[,elev[3]], ylim = c(0,.15), ylab = "Physiological Rate (J per 15min)")
 points(intake_15min_scaled_dat[,elev[3]], col = "green")
 points(pred_expose_30_dat[,elev[3]], col = "blue")
# 
# rowMeans(as.data.frame(Iter.len[1,]), na.rm = TRUE)
# as.data.frame(Iter.len[1,])


 
#
par(mfrow = c(1,3))
#


plot(temp_i.a[,elev[3]],ylim = c(2,30))
points(temp_i.w[,elev[3]], col = "blue")
plot(temp_i.a[,elev[2]],ylim = c(2,30))
points(temp_i.w[,elev[2]], col = "blue")
plot(temp_i.a[,elev[1]], ylim = c(2,30))
points(temp_i.w[,elev[1]], col = "blue")


plot(Iter.len.day[,1])


head(Iter.len.day)
par(mfrow = c(1,1))
plot(matrix(seq(1, length(Iter.len.day), by = 1), nrow = length(Iter.len.day), ncol = Iter.len.day[,names(temp_i.a)=="Upper"]),Iter.len.day[,names(temp_i.a)=="Upper"], ylim = c(0,5), pch = ".", ylab = "Length (mm)", xlab = "Day") #Upper
plot(Iter.len.day[,elev[1]], col = "black", type = "l", ylim = c(1,4), ylab = "Length (mm)", xlab = "Day") #Upper
lines(Iter.len.day[,elev[2]], col = "purple") #Mid
lines(Iter.len.day[,elev[3]], col = "orange") #Low

points(c(1,1,1), Iter.len[1,elev], col = c("black","purple","orange"))
#points(c(len.day,len.day,len.day), LenF[elev], col = c("black","purple","orange")) #Lower mid upper
x.lenday1 <- jitter(c(len.day,len.day,len.day))

means_LenF <- tapply(LenF.list[[1]], as.factor(Elevation.list[[1]]), mean)
var_LenF <- tapply(LenF.list[[1]], as.factor(Elevation.list[[1]]), sd)

points(x.lenday1, means_LenF, col = c("orange","purple","black")) #Lower mid upper
arrows(x0=x.lenday1, y0=means_LenF-var_LenF, x1=x.lenday1, 
       y1=means_LenF+var_LenF, code=0)


color = rep(NA, length=length(Iter.len.day))
color[which(Elevation=="Upper")] = "black"
color[which(Elevation=="Mid")] = "purple"
color[which(Elevation=="Low")] = "orange"


plot(Iter.len.day[,1], ylim = c(1,4), pch = ".", ylab = "Length (mm)", xlab = "Day", type = "l", col =  color[1]) #Upper

for(col in 2:ncol(Iter.len.day)){
  lines(Iter.len.day[,col], pch = ".", col=color[col])
}


points(x.lenday1, means_LenF, col = c("orange","purple","black")) #Lower mid upper
arrows(x0=x.lenday1, y0=means_LenF-var_LenF, x1=x.lenday1, 
       y1=means_LenF+var_LenF, code=0)

means_Obslen <- tapply(obs.growth.len, as.factor(Elevation), mean)
means_Predlen <- tapply(pred.len, as.factor(Elevation), mean)
sd_Obslen <- tapply(obs.growth.len, as.factor(Elevation), sd)
sd_Predlen <- tapply(pred.len, as.factor(Elevation), sd)

par(mfrow = c(1,2))
plot(pred.len ~ obs.growth.len, col = as.factor(Elevation), xlim = c(-1,2.5), ylim = c(-1,2.5))
lines(x = c(-1,2.5),y = c(-1,2.5))
plot(means_Predlen ~ means_Obslen, xlim = c(0,1.5), ylim = c(0,1.5), 
     col = c("orange","purple","black"),
     ylab = "Predicted growth (mm)",
     xlab = "Observed growth (mm)")
lines(x = c(-1,2.5),y = c(-1,2.5))
arrows(x0=means_Obslen, y0=means_Predlen-sd_Predlen, x1=means_Obslen, 
       y1=means_Predlen+sd_Predlen, code=0)
arrows(x0=means_Obslen-sd_Obslen, y0=means_Predlen, x1=means_Obslen+sd_Obslen, 
       y1=means_Predlen, code=0)

# Cool stuff: ####
# This is using the fit from the means
par(mfrow = c(1,3))
plot(c(1.2, 1.55, 1.9), par2[1:3], ylim = c(0,6), xlab = "Elevation", ylab = "Food Scalar")
points(c(1.2, 1.55, 1.9), par1[1:3], pch = 16)
points(c(1.2, 1.55, 1.9), par[1:3], pch = 16, col = "green") # Full dataset, par 1 right now
#Note that the parameters calculated from the full dataset and the mean are nearly identical (green)

legend(x = "topleft", legend = c("Interval 1","Interval 2"), pch = c(16,1), bty = "n")

summary(air.temp.1)
mean_air_temp1 <- colMeans(air.temp.1[,2:4], na.rm = TRUE)
mean_air_time1 <- colSums(!is.na(air.temp.1)) / nrow(air.temp.1)

mean_air_temp2 <- colMeans(air.temp.2[,2:4], na.rm = TRUE)
mean_air_time2 <- colSums(!is.na(air.temp.2)) / nrow(air.temp.2)

weekly_air_temp <- air.temp.1 %>%
  mutate(week = week(ymd_hms(datetime))) %>%
  filter(!is.na(air.temp.1)) %>%
  group_by(week) %>%
  dplyr::summarise(Upper = mean(Upper,  na.rm = TRUE),
                   Mid = mean(Mid,  na.rm = TRUE),
                   Low = mean(Low,  na.rm = TRUE))
weekly_air_temp
weekly_water_temp <- water.temp.1 %>%
  mutate(week = week(ymd_hms(datetime))) %>%
  filter(!is.na(air.temp.1)) %>%
  group_by(week) %>%
  dplyr::summarise(Upper = mean(Upper,  na.rm = TRUE),
                   Mid = mean(Mid,  na.rm = TRUE),
                   Low = mean(Low,  na.rm = TRUE))

# Calc daily air and water temp ####
daily_air_temp <- air.temp.1 %>%
  group_by(date = as.Date(ymd_hms(datetime))) %>%
  dplyr::summarise(Low = mean(Low,  na.rm = TRUE),
                   Mid = mean(Mid,  na.rm = TRUE),
                   Upper = mean(Upper,  na.rm = TRUE))
daily_air_temp
daily_water_temp <- water.temp.1 %>%
  group_by(date = as.Date(ymd_hms(datetime))) %>%
  dplyr::summarise(Low = mean(Low,  na.rm = TRUE),
                   Mid = mean(Mid,  na.rm = TRUE),
                   Upper = mean(Upper,  na.rm = TRUE))

# What about upper quantile ####
#A quartile is the value of the quantile at the probabilities 0.25, 0.5 and 0.75.
#https://stackoverflow.com/questions/26532566/quantile-vs-quartile-in-lay-terms
daily_air_temp_quant <- air.temp.1 %>%
  group_by(date = as.Date(ymd_hms(datetime))) %>%
  dplyr::summarise(Low = quantile(Low, probs = 0.75, na.rm = TRUE),
                   Mid = quantile(Mid, probs = 0.75, na.rm = TRUE),
                   Upper = quantile(Upper, probs = 0.75, na.rm = TRUE))

daily_air_temp_quant_low <- air.temp.1 %>%
  group_by(date = as.Date(ymd_hms(datetime))) %>%
  dplyr::summarise(Low = quantile(Low, probs = 0.25, na.rm = TRUE),
                   Mid = quantile(Mid, probs = 0.25, na.rm = TRUE),
                   Upper = quantile(Upper, probs = 0.25, na.rm = TRUE))

daily_air_temp <- daily_air_temp[2:189,]
daily_water_temp <- daily_water_temp[2:189,]
daily_air_temp_quant <- daily_air_temp_quant[2:189,]
daily_air_temp_quant_low <- daily_air_temp_quant_low[2:189,]


# Plot temp, pretty, 6.5X1.5 ####
# Note dimensions must be called within pdf() - also saving as a .pdf through th GUI is broken. 
pdf("temp_incl_3Q.pdf", width=6.5, height=1.5)
par(mfrow = c(1,3))
par(mar = c(1,1,1,1)+.1)
par(oma = c(1,4,1,1))
plot(daily_air_temp$date, daily_air_temp$Low, type = "l", ylim = c(2,32),yaxs = "i",
     col = "darkorchid4", xlab = "", ylab = expression(paste("Temp (",degree,"C)")), bty = "n", lty = 1)
points(daily_water_temp$date, daily_water_temp$Low, type = "l", col = "blue", lty = 1)
points(daily_air_temp_quant$date, daily_air_temp_quant$Low, type = "l", yaxs = "i",
       col = "darkred", xlab = "", ylab = "Physiological rate (J / day)", bty = "n", lty = 3)

plot(daily_air_temp$date, daily_air_temp$Mid, type = "l", ylim = c(2,32),yaxs = "i",
     col = "darkorchid4", xlab = "", ylab = "", bty = "n")
points(daily_water_temp$date, daily_water_temp$Mid, type = "l", col = "blue")
points(daily_air_temp_quant$date, daily_air_temp_quant$Mid, type = "l", yaxs = "i",
       col = "darkred", xlab = "", ylab = "Physiological rate (J / day)", bty = "n", lty = 3)

plot(daily_air_temp$date, daily_air_temp$Upper, type = "l", ylim = c(2,32), yaxs = "i",
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
       legend = c("Ave low tide temp", "3Q of low tide temp", "High tide temp"),
       col =c("darkorchid4", "darkred", "blue"), lty=c(1,3,1), box.lty=0, bg="transparent")
dev.off()

plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend(x = "topleft",
       legend = c("Low tide temp", "High tide temp"),
       col =c("darkorchid4", "blue"), lty=1:1, box.lty=0, bg="transparent")

# Plot temp with upper quantile, pretty, 6.5X1.5 ####

par(mfrow = c(1,3))
plot(daily_air_temp_quant$date, daily_air_temp_quant$Low, type = "l", ylim = c(2,30),yaxs = "i",
     col = "darkorchid4", xlab = "", ylab = "Physiological rate (J / day)", bty = "n", lty = 3)
# lines(daily_air_temp$date, daily_air_temp$Low, type = "l", ylim = c(2,25),yaxs = "i",
#     col = "darkorchid4", xlab = "", ylab = "Physiological rate (J / day)", bty = "n", lty = 3)
lines(daily_air_temp_quant_low$date, daily_air_temp_quant_low$Low, type = "l", ylim = c(2,30),yaxs = "i",
     col = "darkorchid4", xlab = "", ylab = "Physiological rate (J / day)", bty = "n", lty = 3)
points(daily_water_temp$date, daily_water_temp$Low, type = "l", col = "blue", lty = 1)

plot(daily_air_temp_quant$date, daily_air_temp_quant$Mid, type = "l", ylim = c(2,30),yaxs = "i",
     col = "darkorchid4", xlab = "", ylab = "Physiological rate (J / day)", bty = "n", lty = 3)
# lines(daily_air_temp$date, daily_air_temp$Mid, type = "l", ylim = c(2,25),yaxs = "i",
#     col = "darkorchid4", xlab = "", ylab = "Physiological rate (J / day)", bty = "n", lty = 3)
lines(daily_air_temp_quant_low$date, daily_air_temp_quant_low$Mid, type = "l", ylim = c(2,30),yaxs = "i",
      col = "darkorchid4", xlab = "", ylab = "Physiological rate (J / day)", bty = "n", lty = 3)
points(daily_water_temp$date, daily_water_temp$Mid, type = "l", col = "blue", lty = 1)

plot(daily_air_temp_quant$date, daily_air_temp_quant$Upper, type = "l", ylim = c(2,30),yaxs = "i",
     col = "darkorchid4", xlab = "", ylab = "Physiological rate (J / day)", bty = "n", lty = 3)
# lines(daily_air_temp$date, daily_air_temp$Upper, type = "l", ylim = c(2,25),yaxs = "i",
#     col = "darkorchid4", xlab = "", ylab = "Physiological rate (J / day)", bty = "n", lty = 3)
lines(daily_air_temp_quant_low$date, daily_air_temp_quant_low$Upper, type = "l", ylim = c(2,30),yaxs = "i",
      col = "darkorchid4", xlab = "", ylab = "Physiological rate (J / day)", bty = "n", lty = 3)
points(daily_water_temp$date, daily_water_temp$Low, type = "l", col = "blue", lty = 1)

plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend(x = "topleft",
       legend = c("Low tide temp", "High tide temp"),
       col =c("darkorchid4", "blue"), lty=1:1, box.lty=0, bg="transparent")



weekly_water_temp
#new column for time exposed
head(water.temp.1)
str(water.temp.1)

# From the other file:
# time_exposed_per_day_6mo <- colSums(!is.na(air.temp.list[[2]]))
# time_submerged_per_day_6mo <- colSums(!is.na(water.temp.list[[2]])) #These usually add to 100% except on one or two days when loggers got exchanged
# perc_time_exposed_per_day_6mo <- time_exposed_per_day_6mo / (time_submerged_per_day_6mo + time_exposed_per_day_6mo)

# Plot the time submerged per week ####
# dplyr not working so here is a workaround:
air.temp.interval <- air.temp.1
air.temp.week <- cbind(air.temp.interval,week = week(air.temp.interval$datetime))
air.temp_count_U <- tapply(!is.na(air.temp.week[,"Upper"]),air.temp.week$week, sum, na.rm = TRUE)
air.temp_count_M <- tapply(!is.na(air.temp.week[,"Mid"]),air.temp.week$week, sum, na.rm = TRUE)
air.temp_count_L <- tapply(!is.na(air.temp.week[,"Low"]),air.temp.week$week, sum, na.rm = TRUE)
air.temp.count <- data.frame(
  Upper = air.temp_count_U,
  Mid = air.temp_count_M,
  Low = air.temp_count_L
)

water.temp.interval <- water.temp.1
water.temp.week <- cbind(water.temp.interval,week = week(water.temp.interval$datetime))
water.temp_count_U <- tapply(!is.na(water.temp.week[,"Upper"]),water.temp.week$week, sum, na.rm = TRUE)
water.temp_count_M <- tapply(!is.na(water.temp.week[,"Mid"]),water.temp.week$week, sum, na.rm = TRUE)
water.temp_count_L <- tapply(!is.na(water.temp.week[,"Low"]),water.temp.week$week, sum, na.rm = TRUE)
water.temp.count <- data.frame(
  Upper = water.temp_count_U,
  Mid = water.temp_count_M,
  Low = water.temp_count_L
)
water.temp.count
time.submerged <- water.temp.count / (water.temp.count + air.temp.count)
par(mfrow=c(1,3))
plot(time.submerged$Upper, type = 'b', ylim = c(.1,.8), ylab = "Percent time submerged")
plot(time.submerged$Mid, type = 'b', ylim = c(.1,.8))
plot(time.submerged$Low, type = 'b', ylim = c(.1,.8))



# Plot the time submerged per day ####
# dplyr not working so here is a workaround:
air.temp.interval <- air.temp.1
air.temp.day <- cbind(air.temp.interval,day = yday(air.temp.interval$datetime))
air.temp_count_U <- tapply(!is.na(air.temp.day[,"Upper"]),air.temp.day$day, sum, na.rm = TRUE)
air.temp_count_M <- tapply(!is.na(air.temp.day[,"Mid"]),air.temp.day$day, sum, na.rm = TRUE)
air.temp_count_L <- tapply(!is.na(air.temp.day[,"Low"]),air.temp.day$day, sum, na.rm = TRUE)
air.temp.count <- data.frame(
  Upper = air.temp_count_U,
  Mid = air.temp_count_M,
  Low = air.temp_count_L
)

water.temp.interval <- water.temp.1
water.temp.day <- cbind(water.temp.interval,day = yday(water.temp.interval$datetime))
water.temp_count_U <- tapply(!is.na(water.temp.day[,"Upper"]),water.temp.day$day, sum, na.rm = TRUE)
water.temp_count_M <- tapply(!is.na(water.temp.day[,"Mid"]),water.temp.day$day, sum, na.rm = TRUE)
water.temp_count_L <- tapply(!is.na(water.temp.day[,"Low"]),water.temp.day$day, sum, na.rm = TRUE)
water.temp.count <- data.frame(
  Upper = water.temp_count_U,
  Mid = water.temp_count_M,
  Low = water.temp_count_L
)
water.temp.count
time.submerged <- water.temp.count / (water.temp.count + air.temp.count)
par(mfrow=c(1,3))
nrow(time.submerged)
plot(time.submerged$Upper[2:190], type = "b", cex = .5, ylim = c(0,1), ylab = "Proportion time submerged (unitless)", xlab = "Day")
plot(time.submerged$Mid[2:190], type = 'b', cex = .5, ylim = c(0,1), ylab = "", xlab = "Day")
plot(time.submerged$Low[2:190], type = 'b', cex = .5, ylim = c(0,1), ylab = "", xlab = "Day")

par(mfrow = c(1,3))

# Plot time submerged, pretty version LMU, 6.5X1.5 dim ####
plot(new_intake$date, time.submerged$Low[2:189],type = "l", ylim = c(0,1), yaxs = "i",
     col = "black", xlab = "", ylab = "Physiological rate (J / day)", bty = "n")

legend(x = "bottomleft",
       legend = c("Time submerged"),
       col =c("black"), lty=1:1, box.lty=0, bg="transparent")

plot(new_intake$date, time.submerged$Mid[2:189],type = "l", ylim = c(0,1), yaxs = "i",
     col = "black", xlab = "", ylab = "Physiological rate (J / day)", bty = "n")
plot(new_intake$date, time.submerged$Upper[2:189],type = "l", ylim = c(0,1), yaxs = "i",
     col = "black", xlab = "", ylab = "Physiological rate (J / day)", bty = "n")

library(tidyr)
library(ggplot2)
data_long <- gather(time.submerged[2:190,], condition, proportion, Upper:Low, factor_key=TRUE)

time.submerged[time.submerged$Upper==0,] #Note on the 41st day, there is 0 time submerged in the upper elevation

data_long$condition = factor(data_long$condition, levels = c("Low","Mid","Upper"))

pdf(file = "Time emerged.pdf", width=6, height=5)
ggplot(data = data_long, aes(x=condition, y=100-proportion*100, fill = condition)) + 
  geom_violin(trim = TRUE, width = 1,  position = position_dodge(0.8))+
  scale_fill_manual(values=c("grey", "grey","grey"))  + 
  theme_classic()+
  stat_summary(fun = "mean",
                              geom = "point",
                              color = "black")+
  ylab(label = "Time per day exposed to low tide conditions (%)")+
  ylim(0,100)
  #   stat_summary(fun = means,
  #mult=1, 
  # geom="pointrange", 
  #   position_dodge(0.8))
dev.off()

head(data_long)



# dplyr not working for counts, bu is working for averages 
# there is some issue with the dataframe and tibble
# weekly_water_temp_time <- water.temp_new %>%
#   dplyr::filter(!is.na(water.temp_new)) %>%
#   mutate(week = week(ymd_hms(datetime))) %>%
#   group_by(week) %>%
#   dplyr::summarise(Upper = count(Upper, na.rm = TRUE),
#                    Mid = n(Mid,  na.rm = TRUE),
#                    Low = n(Low,  na.rm = TRUE))
# weekly_water_temp_time

par(mfrow = c(1,3))
plot(weekly_air_temp$Upper, type = "both", col = "purple", ylim = c(5,19), ylab = "Average daily temp (C)", xlab = "week")
points(weekly_water_temp$Upper, type = "both", col = "blue")

plot(weekly_air_temp$Mid, type = "both", col = "purple", ylim = c(5,19), ylab = "",  xlab = "week")
points(weekly_water_temp$Mid, type = "both", col = "blue")

plot(weekly_air_temp$Low, type = "both", col = "purple", ylim = c(5,19), ylab = "", xlab = "week")
points(weekly_water_temp$Low, type = "both", col = "blue")

plot(mean_air_temp2[c(3,2,1)], par2[1:3], ylim = c(0,6), xlim = c(9,14), xlab = "Mean Air Temp", ylab = "Food Scalar")
points(mean_air_temp1[c(3,2,1)], par1[1:3], pch = 16)
#legend(x = "topright", legend = c("Interval 1","Interval 2"), pch = c(16,1), bty = "n")

plot(mean_air_time2[c(4,3,2)], par2[1:3], ylim = c(0,6), xlim = c(0,1), xlab = "Mean Time Exposed", ylab = "Food Scalar")
points(mean_air_time1[c(4,3,2)], par1[1:3], pch = 16)

datam <- melt(data, id.vars = c('day', 'month', 'year'),
              variable.name = "name")


str(elev)
pe <- as.data.frame(pred_expose_30)[,elev]
dim(pred_expose_30)
nrow(pe)

head(datetimes)
# datetimes variable comes out of nowhere but must be from timepoint 2

pred_expose_30_dat2 <- cbind(as.data.frame(pred_expose_30)[,elev], datetime = datetimes[1:nrow(pred_expose_30)])
intake_dat2 <- cbind(as.data.frame(intake_15min_scaled)[elev], datetime = datetimes[1:nrow(intake_15min_scaled)])
cost_15min_dat2 <- cbind(as.data.frame(cost_15min)[elev], datetime = datetimes[1:nrow(cost_15min)])

sum_cols <- function(x, col1, col2, col3 = 3){
  x[[col1]] + x[[col2]]
}

head(pred_expose_30_dat2)

# in terms of day
new_expose <- pred_expose_30_dat2 %>%
  group_by(year = year(ymd_hms(datetime)),week = week(ymd_hms(datetime)), day = day(ymd_hms(datetime))) %>%
  dplyr::summarise(V1 = sum(V4),
                   V2 = sum(V110),
                   V3 = sum(V147))

new_intake <- intake_dat2 %>%
  group_by(year = year(ymd_hms(datetime)),week = week(ymd_hms(datetime)), day = day(ymd_hms(datetime))) %>%
    dplyr::summarise(V1 = sum(V4),
                   V2 = sum(V110),
                   V3 = sum(V147))

new_aq_cost <- cost_15min_dat2 %>%
  group_by(year = year(ymd_hms(datetime)),week = week(ymd_hms(datetime)), day = day(ymd_hms(datetime))) %>%
  dplyr::summarise(V1 = sum(V4),
                   V2 = sum(V110),
                   V3 = sum(V147))

names(new_intake) <- c("Year","Week","Day","Upper", "Mid", "Low")
names(new_expose) <- c("Year","Week","Day","Upper", "Mid", "Low")
names(new_aq_cost) <-  c("Year","Week","Day","Upper", "Mid", "Low")

# new_intake in terms of date ####
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

daily_SFG <- new_intake[,2:4] - new_expose[,2:4] - new_aq_cost[,2:4]

names(new_intake) <- c("date","Upper", "Mid", "Low")
names(new_expose) <- c("date","Upper", "Mid", "Low")
names(new_aq_cost) <- c("date","Upper", "Mid", "Low")
names(daily_SFG) <- c("Upper", "Mid", "Low")

#Don't plot initial and final timepoint, because not a complete timepoint
new_intake <- new_intake[2:189,]
new_expose <- new_expose[2:189,]
new_aq_cost <- new_aq_cost[2:189,]
daily_SFG <- daily_SFG[2:189,]


nrow(new_intake)
# Plot all rates, pretty version LMU, 6.5X1.5 dim ####
par(mfrow = c(1,3))
par(mar = c(1,1,1,1)+.1)
par(oma = c(1,4,1,1))

plot(new_intake$date, new_intake$Low, type = "l", ylim = c(0,4.5),yaxs = "i",
     col = "darkslategray", xlab = "", ylab = "Physiological rate (J / day)", bty = "n", lty = 1)
points(new_intake$date, new_aq_cost$Low, type = "l", col = "blue", lty = 1)
points(new_intake$date, new_expose$Low, type = "l", col = "darkorchid4", lty = 1)

legend(x = "topleft",
       legend = c("Intake", "Aquatic cost", "Aerial cost"),
       col =c("darkslategray", "blue", "darkorchid4"), lty=1:1, box.lty=0, bg="transparent")

plot(new_intake$date, new_intake$Mid, type = "l", ylim = c(0,4.5),yaxs = "i",
     col = "darkslategray", xlab = "", ylab = "Physiological rate\n (J / day)", bty = "n")
points(new_intake$date, new_aq_cost$Mid, type = "l", col = "blue")
points(new_intake$date, new_expose$Mid, type = "l", col = "darkorchid4")

plot(new_intake$date, new_intake$Upper, type = "l", ylim = c(0,4.5), yaxs = "i",
     col = "darkslategray", xlab = "", ylab = "Physiological rate (J / day)", bty = "n")
points(new_intake$date, new_aq_cost$Upper, type = "l", col = "blue")
points(new_intake$date, new_expose$Upper, type = "l", col = "darkorchid4")

par(mfrow = c(1,3))
# Plot SFG, pretty version LMU, 6.5X1.5 dim ####
dim(daily_SFG)
plot(new_intake$date, daily_SFG$Low,type = "l", ylim = c(-.9,4.5), yaxs = "i",
     col = "orange", xlab = "", ylab = "Physiological rate (J / day)", bty = "n")

legend(x = "topleft",
       legend = c("Scope for Growth"),
       col =c("orange"), lty=1:1, box.lty=0, bg="transparent")

plot(new_intake$date, daily_SFG$Mid,type = "l", ylim = c(-.9,4.5), yaxs = "i",
     col = "orange", xlab = "", ylab = "Physiological rate (J / day)", bty = "n")
plot(new_intake$date, daily_SFG$Upper,type = "l", ylim = c(-.9,4.5), yaxs = "i",
     col = "orange", xlab = "", ylab = "Physiological rate (J / day)", bty = "n")

#---
plot(new_intake$Mid, ylim = c(0,5), type = "b", col = "darkgreen", 
     xlab = "day", ylab = "Physiological rate (J / week)")
points(new_aq_cost$Mid, type = "b", col = "blue")
#points(new_expose$Mid, type = "b", col = "purple")

#---
plot(new_intake$Low, ylim = c(0,5), type = "b", col = "darkgreen", 
     xlab = "day", ylab = "Physiological rate (J / week)")
points(new_aq_cost$Low, type = "b", col = "blue")
#points(new_expose$Low, type = "b", col = "purple")





# in terms of week
new_expose <- pred_expose_30_dat2 %>%
  group_by(year = year(ymd_hms(datetime)),week = week(ymd_hms(datetime))) %>%
  dplyr::summarise(V1 = sum(V4),
                   V2 = sum(V110),
                   V3 = sum(V147))

new_intake <- intake_dat2 %>%
  group_by(year = year(ymd_hms(datetime)),week = week(ymd_hms(datetime))) %>%
  dplyr::summarise(V1 = sum(V4),
                   V2 = sum(V110),
                   V3 = sum(V147))

new_aq_cost <- cost_15min_dat2 %>%
  group_by(year = year(ymd_hms(datetime)),week = week(ymd_hms(datetime))) %>%
  dplyr::summarise(V1 = sum(V4),
                   V2 = sum(V110),
                   V3 = sum(V147))

# Manipulating datetimes in dplyr
#https://www.neonscience.org/resources/learning-hub/tutorials/dc-time-series-subset-dplyr-r
intake_count <- intake_dat2 %>%
  group_by(year = year(ymd_hms(datetime)),week = week(ymd_hms(datetime))) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

plot(intake_count[,3])

str(new_aq_cost)
dim(cost_15min_dat2)
18240/29
628*21
628*23
dim(air.temp.1)
plot(air.temp.1$datetime[13188:14444],air.temp.1$Upper[13188:14444], type = "l")
plot(water.temp.1$datetime[13188:14444],water.temp.1$Upper[13188:14444], type = "l")
plot(air.temp.1$datetime[13188:14444],air.temp.1$Upper[13188:14444], type = "l")
plot(water.temp.1$datetime[13188:14444],water.temp.1$Upper[13188:14444], type = "l")
plot(cost_15min_dat2[13188:14444,1]~ as.Date(cost_15min_dat2[13188:14444,4]))


plot(intake_dat2$datetime, intake_dat2$V110)
plot(new_aq_cost$V1~new_aq_cost$week)
plot(new_aq_cost$V1)



#weekly means were here but this seems less interesting
# 
# par(mfrow = c(1,3))
# nrow(new_intake)
# boxplot(new_intake[1,2:4]*1000, col = "green", ylim = c(0,140), ylab = "Physiological rate (mJ per 15 min)")
# boxplot(new_aq_cost[1,2:4]*1000, col = "grey", ylim = c(0,14))
# boxplot(new_expose[1,2:4]*1000, col = "blue", ylim = c(0,14))
# boxplot(new_intake[22:30,2:4]*1000, col = "green", ylim = c(0,14), ylab = "Physiological rate (mJ per 15 min)")
# boxplot(new_aq_cost[22:30,2:4]*1000, col = "grey", ylim = c(0,14))
# boxplot(new_expose[22:30,2:4]*1000, col = "blue", ylim = c(0,14))
# boxplot(new_intake[,2:4]*1000, col = "green", ylim = c(0,15), ylab = "Physiological rate (mJ per 15 min)")
# boxplot(new_aq_cost[,2:4]*1000, col = "grey", ylim = c(0,15))
# boxplot(new_expose[,2:4]*1000, col = "blue", ylim = c(0,15))


shead(new_intake)

names(new_intake) <- c("Year","Week","Upper", "Mid", "Low")
names(new_expose) <- c("Year","Week","Upper", "Mid", "Low")
names(new_aq_cost) <- c("Year","Week","Upper", "Mid", "Low")



# Plot all rates
par(mfrow = c(1,3))
plot(new_intake$Upper, type = "b", ylim = c(0,20),
     col = "darkgreen", xlab = "week", ylab = "Physiological rate (J / week)")
points(new_aq_cost$Upper, type = "b", col = "blue")
points(new_expose$Upper, type = "b", col = "purple")


#---
plot(new_intake$Mid, ylim = c(0,20), type = "b", col = "darkgreen", xlab = "week", ylab = "Physiological rate (J / week)")
points(new_aq_cost$Mid, type = "b", col = "blue")
points(new_expose$Mid, type = "b", col = "purple")

#---
plot(new_intake$Low, ylim = c(0,20), type = "b", col = "darkgreen", xlab = "week", ylab = "Physiological rate (J / week)")
points(new_aq_cost$Low, type = "b", col = "blue")
points(new_expose$Low, type = "b", col = "purple")

points(new_intake$Low[1:28], pch = 20, ylim = c(0,20), type = "b", col = "darkgreen", xlab = "week", ylab = "Physiological rate (J / week)")
points(new_aq_cost$Low[1:28], pch = 20, type = "b", col = "blue")
points(new_expose$Low[1:28], pch = 20, type = "b", col = "purple")


# Plot all rates except for the last ones
par(mfrow = c(1,3))
plot(new_intake$Upper[1:28], type = "b", ylim = c(0,20),
     col = "darkgreen", xlab = "week", ylab = "Physiological rate (J / week)")
points(new_aq_cost$Upper[1:28], type = "b", col = "blue")
points(new_expose$Upper[1:28], type = "b", col = "purple")

points(new_intake$Upper[1:28], pch = 20, ylim = c(0,20), col = "darkgreen", xlab = "week", ylab = "Physiological rate (J / week)")
points(new_aq_cost$Upper[1:28], pch = 20, type = "b", col = "blue")
points(new_expose$Upper[1:28], pch = 20, type = "b", col = "purple")

#---
plot(new_intake$Mid[1:28], ylim = c(0,20), type = "b", col = "darkgreen", xlab = "week", ylab = "Physiological rate (J / week)")
points(new_aq_cost$Mid[1:28], type = "b", col = "blue")
points(new_expose$Mid[1:28], type = "b", col = "purple")

points(new_intake$Mid[1:28], pch = 20, ylim = c(0,20), type = "b", col = "darkgreen", xlab = "week", ylab = "Physiological rate (J / week)")
points(new_aq_cost$Mid[1:28], pch = 20, type = "b", col = "blue")
points(new_expose$Mid[1:28], pch = 20, type = "b", col = "purple")

#---
plot(new_intake$Low[1:28], ylim = c(0,20), type = "b", col = "darkgreen", xlab = "week", ylab = "Physiological rate (J / week)")
points(new_aq_cost$Low[1:28], type = "b", col = "blue")
points(new_expose$Low[1:28], type = "b", col = "purple")

points(new_intake$Low[1:28], pch = 20, ylim = c(0,20), type = "b", col = "darkgreen", xlab = "week", ylab = "Physiological rate (J / week)")
points(new_aq_cost$Low[1:28], pch = 20, type = "b", col = "blue")
points(new_expose$Low[1:28], pch = 20, type = "b", col = "purple")


new_expose$Upper[20:22]


# weekly sums, first 8 weeks
par(mfrow = c(3,3))
nrow(new_intake)
names(new_intake) <- c("week","Upper", "Mid", "Low")
boxplot(new_intake[1:8,2:4], col = "green", ylim = c(0,35), 
        ylab = "Physiological rate (J per week)")
title("intake")
names(new_aq_cost) <- c("week","Upper", "Mid", "Low")

boxplot(new_aq_cost[1:8,2:4], col = "grey", ylim = c(0,35))
title("aquatic cost")
names(new_expose) <- c("week","Upper", "Mid", "Low")
boxplot(new_expose[1:8,2:4], col = "blue", ylim = c(0,35))
title("aerial cost")

# weekly sums, second 8 weeks
#par(mfrow = c(1,3))
nrow(new_intake)
names(new_intake) <- c("week","Upper", "Mid", "Low")
boxplot(new_intake[9:16,2:4], col = "green", ylim = c(0,35), 
        ylab = "Physiological rate (J per week)")
title("intake")
names(new_aq_cost) <- c("week","Upper", "Mid", "Low")

boxplot(new_aq_cost[1:16,2:4], col = "grey", ylim = c(0,35))
title("aquatic cost")
names(new_expose) <- c("week","Upper", "Mid", "Low")
boxplot(new_expose[1:16,2:4], col = "blue", ylim = c(0,35))
title("aerial cost")

nrow(new_intake)/3 # Last row is just zeros
# weekly sums, third 8 weeks
#par(mfrow = c(1,3))
nrow(new_intake)
names(new_intake) <- c("week","Upper", "Mid", "Low")
boxplot(new_intake[19:27,2:4], col = "green", ylim = c(0,35), 
        ylab = "Physiological rate (J per week)")
title("intake")
names(new_aq_cost) <- c("week","Upper", "Mid", "Low")

boxplot(new_aq_cost[19:27,2:4], col = "grey", ylim = c(0,35))
title("aquatic cost")
names(new_expose) <- c("week","Upper", "Mid", "Low")
boxplot(new_expose[19:27,2:4], col = "blue", ylim = c(0,35))
title("aerial cost")


# Now if I put into long format, and include the distributions of say total intake, total costs, for each individual:

data.frame(c(1,1),c(1,1))
str(pred_expose_30_dat3)

intake_dat3 <- as.data.frame(intake_15min_scaled)
cost_15min_dat3 <- cbind(as.data.frame(cost_15min), datetime = datetimes[1:nrow(cost_15min)])
str(pred_expose_30_dat3)

plot(cost_15min_dat3[,1]~cost_15min_dat3$datetime)

# This is missing the first column
pred_expose_30_dat3 <- as_tibble(pred_expose_30)
#pivot_longer(pred_expose_30, cols = c(V1,V153), names_to = "sample", values_to = "value")

# In data table format
head(pred_expose_30)
DT.m1 = melt(pred_expose_30, 
             measure.vars = names(pred_expose_30))

# In tibble format
head(DT.m1)
head(intake_dat3)
names(intake_dat3)
data_long <- gather(data = pred_expose_30_dat3, sample, measurement, V1:V153, factor_key=TRUE)
cbind(data_long, Elevation = rep(Elevation, nrow(pred_expose_30_dat3)))

#Oh but first we sum the full column... oops. 
new_expose2 <- data.frame(
  value = colSums(as.data.frame(pred_expose_30)),
  Elevation = as.factor(Elevation))
new_intake2 <- data.frame(
  value = colSums(as.data.frame(intake_15min_scaled)),
  Elevation = as.factor(Elevation))
new_aq_cost2 <- data.frame(
  value = colSums(as.data.frame(cost_15min)),
  Elevation = as.factor(Elevation))
time <- seq(1,nrow(new_aq_cost2), by =1)  
plot(new_aq_cost2[,1]~time)

# Back-calculate withat intake would be without the coefficient
backcalc_intake <- as.numeric(new_intake2[,1]) / as.numeric(y1)
backcalc_intake2 <- data.frame(value =backcalc_intake, Elevation = as.factor(Elevation))

 str(new_expose2)           
      # sums across half year ####
      # par(mfrow = c(1,3))
      # boxplot(data = new_intake2, value~Elevation, col = "green",  ylab = "Physiological rate (J per week)")
      # title("intake")
      # boxplot(data = new_aq_cost2, value~Elevation, col = "grey")
      # title("aquatic cost")
      # boxplot(data = new_expose2, value~Elevation, col = "blue")
      # title("aerial cost")
      # 
      # plot(data = new_intake2, value~as.numeric(Elevation), col = "green",  ylab = "Physiological rate (J per week)")
      # title("intake")
      # boxplot(data = new_aq_cost2, value~Elevation, col = "grey")
      # title("aquatic cost")
      # boxplot(data = new_expose2, value~Elevation, col = "blue")

comb <- cbind(new_intake2[,1],new_aq_cost2[,1],new_expose2)
names(comb) <- c("intake","aquatic_cost", "aerial_cost")
comb <- comb[1:(length(new_intake2[,1])-1),] #Remove last incomplete week
head(comb) 
tail(comb)
data_long <- gather(comb, condition, measurement, intake:aerial_cost, factor_key=TRUE)
names(data_long) <- c("Elevation", "condition", "measurement")
head(data_long)
str(data_long)

df_means <- data_long %>% group_by(Elevation,condition) %>% summarise(measurement=mean(measurement))

# I think this plots a summation and aerial cost is a sum of oxygen debt and exposure cost ####   
ggplot(data = data_long, aes(x=Elevation, y=measurement, fill = condition)) + 
  geom_violin(trim = FALSE, width = 3,  position = position_dodge(0.8))+
  scale_fill_manual(values=c("light green", "lightblue", "purple"))+ 
  geom_point(data = df_means, width = 0.1, position = position_dodge(0.8)) +
#   stat_summary(fun = means,
                #mult=1, 
                 # geom="pointrange", 
             #   position_dodge(0.8))+ 
  theme_classic()
head(data_long)

#Plot intake 2 and then backcalc intake ####
par(mfrow = c(1,2))
head(backcalc_intake2)
p1 <- ggplot(data = backcalc_intake2, aes(x=Elevation, y=value, fill = "lightgreen")) + 
  geom_violin(trim = FALSE, width = 1,  position = position_dodge(0.8))+
  scale_fill_manual(values=c("light green"))+ 
  geom_boxplot(width = 0.1, position = position_dodge(0.8)) +
  #   stat_summary(fun = means,
  #mult=1, 
  # geom="pointrange", 
  #   position_dodge(0.8))+ 
  theme_classic()+
  theme(legend.position = "none") + #https://www.datanovia.com/en/blog/how-to-remove-legend-from-a-ggplot/
  scale_y_continuous(name="Total intake (J)", limits=c(0, 600))
  
p2 <- ggplot(data = new_intake2, aes(x=Elevation, y=value, fill = "lightgreen")) + 
  geom_violin(trim = FALSE, width = 1,  position = position_dodge(0.8))+
  scale_fill_manual(values=c("light green"))+ 
  geom_boxplot(width = 0.1, position = position_dodge(0.8)) +
  #   stat_summary(fun = means,
  #mult=1, 
  # geom="pointrange", 
  #   position_dodge(0.8))+ 
  theme_classic()+
  theme(legend.position = "none") + 
  scale_y_continuous(name="Total intake (J)", limits=c(0, 600))


grid.arrange(p1, p2, nrow = 1)

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)}

backcalc_summary <- data_summary(backcalc_intake2, varname = "value", 
                                 groupname =c("Elevation"))
# Backcalc intake but with bar plots instead 3 X 6 landscape####
p1 <- ggplot(data = backcalc_summary, aes(x=Elevation, y=value)) + 
  geom_bar(stat = "identity",  width = .75, position = position_dodge(),color="black", fill="white") + 
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2,
                position=position_dodge(.9))+  
  theme_classic() +
  scale_fill_manual(values="green") +
  scale_y_continuous(name="Total intake (J)", limits=c(0, 500), expand = c(0,0))

p1
mod <- aov(backcalc_intake2$value~backcalc_intake2$Elevation)
summary(mod)
TukeyHSD(mod)

new_intake2_summary <- data_summary(new_intake2, varname = "value", 
                                    groupname =c("Elevation"))

p2 <- ggplot(data = new_intake2_summary, aes(x=Elevation, y=value)) + 
  geom_bar(stat = "identity", width = .75,  position = position_dodge(),color="black", fill="white") + 
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2,
                position=position_dodge(.9))+  
  theme_classic() +
  scale_fill_manual(values="green") +
  scale_y_continuous(name="Total intake (J)", limits=c(0, 500), expand = c(0,0))

p2
mod <- aov(new_intake2$value~new_intake2$Elevation)
summary(mod)
TukeyHSD(mod)

grid.arrange(p1, p2, nrow = 1)



  geom_boxplot(width = 0.1, position = position_dodge(0.8)) +
  #   stat_summary(fun = means,
  #mult=1, 
  # geom="pointrange", 
  #   position_dodge(0.8))+ 
  theme_classic()+
  theme(legend.position = "none") + #https://www.datanovia.com/en/blog/how-to-remove-legend-from-a-ggplot/
  scale_y_continuous(name="Total intake (J)", limits=c(0, 600))

p2 <- ggplot(data = new_intake2, aes(x=Elevation, y=value, fill = "lightgreen")) + 
  geom_violin(trim = FALSE, width = 1,  position = position_dodge(0.8))+
  scale_fill_manual(values=c("light green"))+ 
  geom_boxplot(width = 0.1, position = position_dodge(0.8)) +
  #   stat_summary(fun = means,
  #mult=1, 
  # geom="pointrange", 
  #   position_dodge(0.8))+ 
  theme_classic()+
  theme(legend.position = "none") + 
  scale_y_continuous(name="Total intake (J)", limits=c(0, 600))

#https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
# library(gridExtra)
grid.arrange(p1, p2, nrow = 1)


new_intake2

plot(new_aq_cost2[,1]~time, col = as.factor(Elevation)) #plots each sample - time is mislabeled - it should be index. 
cbind(new_aq_cost2,time)
plot(new_intake2[,1]~time, col = as.factor(Elevation)) #plots each sample - time is mislabeled - it should be index. 
cbind(new_aq_cost2,time)
plot(new_expose2[,1]~time, col = as.factor(Elevation)) #plots each sample - time is mislabeled - it should be index. 
cbind(new_aq_cost2,time)

ggplot(data = new_aq_cost2, aes(x=Elevation, y=measurement, fill = condition)) + 
  geom_point(trim = FALSE, width = 3,  position = position_dodge(0.8))+
  scale_fill_manual(values=c("light green", "lightblue", "purple"))+ 
  geom_boxplot(width = 0.1, position = position_dodge(0.8)) +
  #   stat_summary(fun = means,
  #mult=1, 
  # geom="pointrange", 
  #   position_dodge(0.8))+ 
  theme_classic()

#save.image(file = "plots_time2_workspace.v2.RData")


      