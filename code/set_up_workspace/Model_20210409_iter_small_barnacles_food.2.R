# This is now set up to work with the desktop computer (different working directory) -1/20/21
# This sets up all of the data and functions for the model predictions



# This model is calibrated with Lisa's data, 
# and includes padilla bay chlorophyll seasonal data.

# This model does not use tissue mass samples. Go back to previous models, e.g. Model_202006.._iter... for tissue mass reading in. 

# ============================#
# ============================#
# Set up
# ============================#
rm(list = ls())
# load libraries
library(stats)
library(bbmle)
library(OneR)
library(lme4)
library(ggplot2)
library(minpack.lm)
library(effects)
library(nlme)
library(viridis)


themeforggplot <- theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

theme_set(themeforggplot)


#Time period 1 or 2
timing <- 2

#Which subset of barnacles
small_only<-"Y"

#Turn plotting on and off (for the sake of speeding up the iterations)
figs <- "Y" #(Y or N)

#Food dynamics hypothesis
PadillaBay = "Y"

#Test run?
baby_model <- "Y"
baby_model <- "N"

# Plotting values for functional responses
pred.dat <- data.frame(
  Temp.n = rep(seq(from = 5, to = 40, length.out = 20), times = 3),
  OperculumLength = rep(c(4.5, 5.5, 6.5),times = 3,each = 20),
  sizeclass = as.factor(rep(c("small","medium", "large"),times = 3,each = 20))
)

pred.dat.aqua <- data.frame(
  Temp.n = rep(seq(from = 5, to = 20, length.out = 20), times = 3),
  OperculumLength = rep(c(4.5, 5.5, 6.5),times = 3,each = 20),
  sizeclass = as.factor(rep(c("small","medium", "large"),times = 3,each = 20))
)

pred.dat.air <- data.frame(
  Temp.n = rep(seq(from = 5, to = 40, length.out = 20), times = 3),
  OperculumLength = rep(c(4.5, 5.5, 6.5),times = 3,each = 20),
  sizeclass = as.factor(rep(c("small","medium", "large"),times = 3,each = 20))
)

# Climate change variable
air_temp_increment <- c(0,.5,1,1.5,2,2.5,3,3.5,4,4.5,5)
water_temp_increment <- c(0,.5,1,1.5,2) # Global ocean has warmed 1 degree
air_i <- 1
water_i <- 1
inc_air <- air_temp_increment[air_i]
inc_water <- water_temp_increment[water_i]
#https://nca2014.globalchange.gov/report/regions/northwest
#https://science.sciencemag.org/content/363/6423/128 How fast are oceans warming?

# Set up dataframe
air.water.U1 <- matrix( data = NA, 
                        nrow = length(water_temp_increment), 
                        ncol = length(air_temp_increment))

colnames(air.water.U1) <- as.numeric(air_temp_increment)
rownames(air.water.U1) <- water_temp_increment

# Operculum growth data
# ============================#
# Read in observed operculum growth data ####
# ============================#
setwd("C:/Users/eroberts/Box/PhotoAnalysis_Sp2020/datasheets_org/20201007_data")
setwd("~/Box/PhotoAnalysis_Sp2020/datasheets_org/20201007_data")
setwd("~/Box/PhotoAnalysis_Sp2020/FHL/datasheets_org/20201007_data")
#    setwd("~/Box/Sarah and Molly's Box/FHL data/growth_individual/ForSam_20201007")
# growth <- read.csv("growth")
growth.1 <- read.csv("Scaled_data_Feb_Aug.csv")
growth.2 <- read.csv("Scaled_data_Aug_Mar.csv")
growth.2 <- growth.2[growth.2$Elevation!="",] #one line of NA's
growth.1 <- growth.1[growth.1$growth.Aug.Feb>=-.1,] #one line of an outlier with a growth of >-0.1


growth.1$Elevation <- as.factor(growth.1$Elevation)
growth.2$Elevation <- as.factor(growth.2$Elevation)

small_only <- "Y"
if(small_only=="Y"){
  small.1 <- growth.1[growth.1$Feb.Length >= 0.15 & growth.1$Feb.Length <= 0.30,]
  small.2 <- growth.2[growth.2$Aug.Length >= 0.15 & growth.2$Aug.Length <= 0.30,]
  
  growth.1 <- small.1
  growth.2 <- small.2
}

if(baby_model=="Y"){
  growth.1.ave <- growth.1 %>%
    group_by(Elevation)%>%
    dplyr::summarise(
      n = n(),
      growth.Aug.Feb = mean(growth.Aug.Feb, na.rm = TRUE),
      Feb.Length = mean(Feb.Length, na.rm = TRUE),
      Aug.Length = mean(Aug.Length, na.rm = TRUE)
    ) 
  growth.2.ave <- growth.2 %>%
    group_by(Elevation)%>%
    summarise(
      n = n(),
      growth.Mar.Aug = mean(growth.Mar.Aug, na.rm = TRUE),
      Aug.Length = mean(Aug.Length, na.rm = TRUE),
      Mar.Length = mean(Mar.Length, na.rm = TRUE)
    ) 
  growth.1 <- as.data.frame(growth.1.ave)
  growth.2 <- as.data.frame(growth.2.ave)
}
if(figs == "Y"){
  par(mfrow = c(1,2))
  plot(growth.1$Feb.Length, growth.1$growth.Aug.Feb, 
       col = as.factor(growth.1$Elevation),  
       ylim = c(-.1,.5), xlim = c(.15,.3),
       ylab = "Growth Feb to Aug (cm)",
       xlab = "Initial length (cm)"
  )
  plot(growth.2$Aug.Length, growth.2$growth.Mar.Aug, 
       col = as.factor(growth.2$Elevation), 
       ylim = c(-.1,.25), xlim = c(.15,.3), 
       ylab = "Growth Aug to Mar (cm)",
       xlab = "Initial length (cm)"
  )
}

hist(growth.2$growth.Mar.Aug[growth.2$Elevation=="Upper"])
hist(growth.2$growth.Mar.Aug[growth.2$Elevation=="Mid"])
hist(growth.2$growth.Mar.Aug[growth.2$Elevation=="Low"])

library("stats")
par(mfrow = c(1,2))

plot(as.factor(growth.2$Elevation), growth.2$growth.Mar.Aug, ylim = c(-.08, 0.25), ylab = "Growth (mm)", xlab = "Elevation")
plot(as.factor(growth.1$Elevation), growth.1$growth.Aug.Feb, ylim = c(-.08, 0.25), ylab = "", xlab = "Elevation")

anova(lm(growth.2$growth.Mar.Aug~as.factor(growth.2$Elevation)*as.numeric(growth.2$Aug.Length)))

summary(aov(growth.1$growth.Mar.Aug~as.factor(growth.1$Elevation)))
anova(lm(growth.1$growth.Aug.Feb~as.factor(growth.1$Elevation)*as.numeric(growth.1$Feb.Length)))
anova(lm(growth.1$growth.Aug.Feb~as.factor(growth.1$Elevation)+as.numeric(growth.1$Feb.Length)))

min(growth.1$Feb.Length)
max(growth.1$Feb.Length)
summary(lm(growth.1$growth.Mar.Aug~as.factor(growth.1$Elevation)*as.numeric(growth.1$Feb.Length)))

str(growth.1$Feb.Length)
# No interactions:
par(mfrow = c(1,2))
summary(lm(growth.1$growth.Aug.Feb~as.factor(growth.1$Elevation)+as.numeric(growth.1$Feb.Length)))
mod1 <- lm(growth.Aug.Feb~Elevation+Feb.Length, data = growth.1)
dat1.emm <- emmeans(mod1, "Elevation")
# plot(dat1.emm)
#emmeans::emmip(mod1, growth.Mar.Aug ~ Elevation , CIs = TRUE)
dat1.emm.df <- as.data.frame(dat1.emm)
mid <- barplot(data = dat1.emm.df, emmean~Elevation, 
               ylim = c(0,.15), ylab = "Size-corrected growth (cm per 6-months)")
arrows(x0=mid, y0=dat1.emm.df$lower.CL, x1=mid, 
       y1=dat1.emm.df$upper.CL, code = 3, angle=90, 
       length=0.1)
(0.08126638 - 0.05801888) / 0.05801888

mod1 <- lm(growth.2$growth.Mar.Aug~as.factor(growth.2$Elevation)+as.numeric(growth.2$Aug.Length))
mod1 <- lm(data = growth.2, growth.Mar.Aug~Elevation*Aug.Length)
dat1.emm <- emmeans(mod1, "Elevation")
# plot(dat1.emm)
#emmeans::emmip(mod1, growth.Mar.Aug ~ Elevation , CIs = TRUE)
dat1.emm.df <- as.data.frame(dat1.emm)
mid <- barplot(data = dat1.emm.df, emmean~Elevation, 
               ylim = c(0,.15), ylab = "")
arrows(x0=mid, y0=dat1.emm.df$lower.CL, x1=mid, 
       y1=dat1.emm.df$upper.CL, code = 3, angle=90, 
       length=0.1)
(0.08086259 - 0.05984264) / 0.05984264

Timepoint_1 <- as.POSIXct( "2018-02-04", format="%Y-%m-%d", tz = "GMT")
# Timepoint_2 <- as.POSIXct( "2018-08-13", format="%Y-%m-%d", tz = "GMT") #Changed 20210405 to avoid 1 day gap in data as loggers were switched out and photos were taken
Timepoint_1_end <- as.POSIXct( "2018-08-12", format="%Y-%m-%d", tz = "GMT")
Timepoint_2_start <- as.POSIXct( "2018-08-15", format="%Y-%m-%d", tz = "GMT")
Timepoint_3 <- as.POSIXct( "2019-03-01", format="%Y-%m-%d", tz = "GMT")

# ============================#
# Read in relationship between operculum and tissue mass ####
# ============================#
setwd("C:/Users/eroberts/Box/Sarah and Molly's Box/FHL data/length mass relationship/FHL Collected Barnacles")
setwd("~/Box/Sarah and Molly's Box/FHL data/length mass relationship/FHL Collected Barnacles")
len_mass_Feb18 <- read.csv(file = "FHL 201803_Mar.csv", stringsAsFactors = FALSE)
len_mass_Aug18 <- read.csv(file = "FHL 201808_Aug.csv", stringsAsFactors = FALSE)
len_mass_Mar19 <- read.csv(file = "FHL 201903_Mar.csv", stringsAsFactors = FALSE)

# These are not used here. They are used for larger barnacles with the static model. 
# The sampling was mostly for larger barnacles. 


# ===========================#
# Read in temp data, determine aquatic and aerial temperatures ####
# ===========================#
# Stuck: Using subset of data (not Mid summer 2018 onward) b/c timestamp mismatch issues  
# Visually, this looks like:
#   High – 1.97 m = 6.46 ft
# Mid – 1.55 m = 5.08 ft
# Low – 1.20 m = 3.93 ft
# Bars denote plus or minus 0.05 m (= 0.16 ft)
# 
# In email January 8, 2020, Gordon got: 
#   High = 1.98m
#   Mid = 1.58m
#   Low = 1.18m
# 
# From diagrams I know that the whole setup is about 3.5 ft from bottom to top, with 1 ft in between each layer of tiles. More like 2.5 bc not at edge

setwd("~/Box/Sarah and Molly's Box/FHL data/HOBO data/Tides")
setwd("C:/Users/eroberts/Box/Sarah and Molly's Box/FHL data/HOBO data/Tides")
# READING IN NEW TEMP DATASET ####
tempdat <- read.csv("TempAndWaterLevel_QC.20210401.csv", stringsAsFactors = FALSE) #This is saved in r code HoboCombineFHL...v4.R
str(tempdat)
tempdat$datetime <- as.POSIXct(tempdat$datetime, tz = "GMT")
df.new <- tempdat
tempdat <- tempdat[!is.na(tempdat$Water.Level),]

subsetcheck1 <- as.Date("2018-08-20")
subsetcheck2 <- as.Date("2018-08-26")
tempdat_subset <- tempdat[tempdat$datetime>subsetcheck1&tempdat$datetime<subsetcheck2,]

tempdat$Upper_air <- tempdat$Upper
tempdat$Upper_water <- tempdat$Upper
tempdat$Upper_air[tempdat$Water.Level>=1.97] <- NA
tempdat$Upper_water[tempdat$Water.Level<1.97] <- NA
# plot(tempdat$Upper_air ~ tempdat$datetime, col = "red")
# points(tempdat$Upper_water ~ tempdat$datetime, col = "blue")
plot(tempdat_subset$Upper_air ~ tempdat_subset$datetime, col = "red")
points(tempdat_subset$Upper_water ~ tempdat_subset$datetime, col = "blue")

tempdat$Mid_air <- tempdat$Mid
tempdat$Mid_water <- tempdat$Mid
tempdat$Mid_air[tempdat$Water.Level>=1.55] <- NA
tempdat$Mid_water[tempdat$Water.Level<1.55] <- NA
plot(tempdat$Mid_air ~ tempdat$datetime, col = "red")
points(tempdat$Mid_water ~ tempdat$datetime, col = "blue")
head(tempdat)

tempdat$Low_air <- tempdat$Low
tempdat$Low_water <- tempdat$Low
tempdat$Low_air[tempdat$Water.Level>=1.20] <- NA
tempdat$Low_water[tempdat$Water.Level<1.20] <- NA
plot(tempdat$Low_air ~ tempdat$datetime, col = "red")
points(tempdat$Low_water ~ tempdat$datetime, col = "blue")
head(tempdat)

# Estimating air and water temp crudely of above the logger - 
# If curious, why not measure this? 
# Using the previous 2 hour data for times that there is no low tide data -
# Assumption is that we are extending the temps at low tide. 
# This is imperfect because not finding each transition and identifying the drop and then filling in the data
# based on an average. Instead it's just a repeat of the last two hours of data. 
# Can use the SFG model to calculate SFG at this elevation. 
par(mfrow = c(1,1))
Above_height <- 1.9+.35/2 #2.1
tempdat$Above_air <- tempdat$Upper
tempdat$Above_water <- tempdat$Upper
tempdat$Above_air[tempdat$Water.Level>=Above_height] <- NA
len_upper <- length(tempdat$Upper)-8
tempdat$Upper_2hourslater <-c(tempdat$Upper[1:len_upper],c(0,0,0,0,0,0,0,0))
tempdat$Above_air[tempdat$Water.Level<Above_height&tempdat$Water.Level>1.9] <- tempdat$Upper_2hourslater[tempdat$Water.Level<2.25&tempdat$Water.Level<1.9]
tempdat$Above_water[tempdat$Water.Level<Above_height] <- NA
# plot(tempdat$Above_air ~ tempdat$datetime, col = "red")
# points(tempdat$Above_water ~ tempdat$datetime, col = "blue")
head(tempdat)
plot(tempdat$Above_air[1:200] ~ tempdat$datetime[1:200], col = "red")
points(tempdat$Above_water[1:200] ~ tempdat$datetime[1:200], col = "blue")
points(tempdat$Water.Level[1:200]*10 ~ tempdat$datetime[1:200], col = "blue", type = "l")


par(mfrow = c(1,1))

plot(tempdat$Upper, pch = ".")
points(tempdat$Mid, col = "blue", pch=".")
points(tempdat$Low, col = "orange", pch = ".")

plot(tempdat_subset$Upper, pch = ".")
points(tempdat_subset$Mid, col = "blue", pch=".")
points(tempdat_subset$Low, col = "orange", pch = ".")


plot(air.temp.1$Upper, pch = ".")
points(air.temp.1$Mid, col = "blue", pch=".")
points(air.temp.1$Low, col = "orange", pch = ".")

plot(air.temp.2$Upper, pch = ".")
points(air.temp.2$Mid, col = "blue", pch=".")
points(air.temp.2$Low, col = "orange", pch = ".")


water.temp <- data.frame(
  datetime = tempdat$datetime,
  Upper = tempdat$Upper_water,
  Mid = tempdat$Mid_water,
  Low = tempdat$Low_water)

air.temp <- data.frame(
  datetime = tempdat$datetime,
  Upper = tempdat$Upper_air,
  Mid = tempdat$Mid_air,
  Low = tempdat$Low_air)

#if(timing == 1){
  starttime <- Timepoint_1
  endtime <- Timepoint_1_end
  air.temp.1 <- air.temp[air.temp$datetime>=starttime & air.temp$datetime<= endtime & !is.na(air.temp$datetime),]
  water.temp.1 <- water.temp[water.temp$datetime>=starttime & water.temp$datetime<= endtime & !is.na(water.temp$datetime),]
#  }

#if(timing == 2){
  starttime <- Timepoint_2_start
  endtime <- Timepoint_3
  air.temp.2 <- air.temp[air.temp$datetime>=starttime & air.temp$datetime<= endtime & !is.na(air.temp$datetime),]
  water.temp.2 <- water.temp[water.temp$datetime>=starttime & water.temp$datetime<= endtime & !is.na(water.temp$datetime),]
#}
  
# Not filling in ANY missing points (not even just points on the 12th) bc simpler to just cut 
# entire 2 days when loggers were swapped out
 #  plot(tail(air.temp.1$Mid,200))
 #  points(tail(air.temp.1$Low,200),pch = 20)
 #  points(tail(water.temp.1$Low,200))
 #  points(tail(water.temp.1$Mid,200), col = "blue")
 #  
 #  tail(air.temp.1)
 #  
 #  head(air.temp.2,10)
 #  head(water.temp.2)
 #  
 #  #fill in missing points
 # mean(tail(air.temp.1$Low,30)/tail(air.temp.1$Mid,30), na.rm = TRUE)
 # #.9028
 # tail(air.temp.1$Mid, 30)*.9028
 # plot(tail(air.temp.1$Low,30), tail(air.temp.1$Mid, 30)*.9028)
 # tail(air.temp.1$Low,30) #8 NAs
 # newlowvec <- tail(air.temp.1$Mid,8)*0.9028
 # nrow(air.temp.1)
 # 18241-7
 # air.temp.1$Low[18234:18241] <- newlowvec
 # 
 
  
#====#
# Food data read in ####
#====#

setwd("~/Box/Sarah and Molly's Box/FHL data/SONDE data/Padilla Bay/909184_all_data_2003topresent")
  setwd("C:/Users/eroberts/Box/Sarah and Molly's Box/FHL data/SONDE data/Padilla Bay/909184_all_data_2003topresent")
PB <- read.csv("PDBGSWQ_dataonly.csv", stringsAsFactors = FALSE)
head(PB)

library(lubridate)
library(dplyr)
library(tidyr)

#Technically this should be in pdt, but this is being used to calculate weekly averages and lubridate is not working correctly
PB$datetime <-as.POSIXct(PB$m.d.y.hh.mm, format = "%m/%d/%y %H:%M", tz = "UTC")
str(as.factor(year(PB$datetime)))

#subset to only include the past 3 years...

plot(PB$datetime, PB$ug.l, pch = ".")
time_zone <- "UTC"
PB_subset <- PB[PB$datetime > as.POSIXct("2017-01-01 00:00:00", tz = "UTC") & PB$datetime< as.POSIXct("2020-01-01 00:00:00", tz = time_zone),]
head(PB_subset)
PB <- PB_subset

plot(PB$datetime, PB$ug.l, pch = ".")
# plot(PB$datetime, PB$psu, pch = ".")

vars <-"m.d.y.hh.mm"
dat <- PB %>% 
  drop_na(ug.l, any_of(vars))%>%
  dplyr::group_by(time = week(datetime)) %>% 
  dplyr::summarise(value = mean(ug.l, na.rm = TRUE), .groups = 'keep')
dat
plot(dat, type = 'b', ylab = "fluorescence (ug/L)", xlab = "week")

library(lubridate)
#dat$datetime.2017 <- lubridate::ymd_hms( "2017-01-01 00:00:01" ) + lubridate::weeks( dat$time - 1 )
dat$datetime.2018 <- lubridate::ymd_hms( "2018-01-01 00:00:01" ) + lubridate::weeks( dat$time - 1 )
dat$datetime.2019 <- lubridate::ymd_hms( "2019-01-01 00:00:01" ) + lubridate::weeks( dat$time - 1 )

food <- data.frame(
  datetime = as.POSIXct(c(dat$datetime.2018, dat$datetime.2019)),
  food = c(dat$value,dat$value)
)

plot(food)

library(zoo)
# Create zoo objects
zf <- zoo(food$food, food$datetime)   # low freq
zt <- zoo(water.temp.1$Upper, water.temp.1$datetime)  # high freq
z <- merge(zf,zt,all = TRUE)
z$zf <- na.approx(z$zf, rule=2)
head(z)
food.interp <- fortify.zoo(z$zf)
names(food.interp) <- c("datetime", "food")
water.temp.1 <- left_join(water.temp.1, food.interp, by = "datetime")

zf <- zoo(food$food, food$datetime)   # low freq
zt <- zoo(water.temp.2$Upper, water.temp.2$datetime)  # high freq
z <- merge(zf,zt,all = TRUE)
head(z)
z$zf <- na.approx(z$zf, rule=2)
head(z)
food.interp <- fortify.zoo(z$zf)
names(food.interp) <- c("datetime", "food")
water.temp.2 <- left_join(water.temp.2, food.interp, by = "datetime")
head(water.temp.2)

if(figs == "Y"){
head(water.temp.2$food)
par(mfrow = c(1,2))
plot(water.temp.1$food~ water.temp.1$datetime,
     ylim = c(0,20),
     ylab = "Chlorophyll (ug/L)",
     xlab = "Date 2018",
     pch = ".")
plot(water.temp.2$food~ water.temp.2$datetime,
     ylim = c(0,20),
     ylab = "",
     xlab = "Date 2018-2019",
     pch = ".")
}


head(growth.1)
Iter.len.1 <- as.data.frame(matrix(data = 0, ncol = nrow(growth.1), nrow = length(water.temp.1$datetime)))
Iter.len.1[1,] <- growth.1$Feb.Length
head(Iter.len.1)

head(growth.2)
Iter.len.2 <- as.data.frame(matrix(data = 0, ncol = nrow(growth.2), nrow = length(water.temp.2$datetime)))
Iter.len.2[1,] <- growth.2$Aug.Length

# ===========================#
# Read in feeding, respiration relationships ####
# Feeding ~ Temp, Aquatic Respiration ~ Temp, Aerial Respiration ~ Temp
# ===========================#
# Feeding shrimp per min ~ Temp C ####
# Upslope to 20 degrees
# ===========================#
# Use a subset of the data, up to 20 degrees to interpolate feeding rate upslope, 
# Wu and Levings equation with random component, assuming allometric scaling factor
setwd("~/Box/Sarah and Molly's Box/FHL data/aqua feeding and resp")
setwd("C:/Users/eroberts/Box/Sarah and Molly's Box/FHL data/aqua feeding and resp")

feed<- read.table("aqfeedFHL.csv", header = T, sep = ",")
feed<- read.table("aqfeedFHL_log.csv", header = T, sep = ",")
feed$Temp <- as.factor(feed$Temp)
feed$Temp.n <- as.numeric(as.character(feed$Temp))
feed_subset <- feed[feed$Temp.n <= 17,]
feed <- feed_subset
feed$feedrate.new <- feed$m_filter_300 - feed$a_ave
feed$feedrate <- feed$feedrate.new
feed <- feed[!is.na(feed$feedrate.new),]
feed$ID <- as.factor(feed$ID)
feed$bin <- bin(feed$OperculumLength, nbins = 3) #(4.02,4.97] (4.97,5.91] (5.91,6.86]
feed$sizeclass <- bin(feed$OperculumLength, nbins = 3, labels = c("small", "medium", "large"))

# FR_T <- nlme(feedrate ~ exp(a * Temp.n) * OperculumLength^(1.86) * c,
#              data = feed,
#              fixed = a + c~ 1,
#              random = c ~ 1,
#              groups = ~ ID,
#              start = c(a = -.007, c = 100))
# summary(FR_T)
# FR_T_a <- FR_T$coefficients$fixed[[1]]
# FR_T_c <- FR_T$coefficients$fixed[[2]]

# FR_T_fun <- function(temp, size, a = FR_T_a, c = FR_T_c) {
#   out <- exp(a * temp) * size^(1.86) * c
#   return(out)
# }
head(feed)
FR_T <- nls(feedrate ~ exp(a*Temp.n) * OperculumLength^(1.88) * c,
            data = feed, start = c(a = .1, c = .0004))
summary(FR_T)
FR_T_a <- coef(FR_T)[1]
FR_T_c <- coef(FR_T)[2]

FR_T_fun <- function(temp, size, a = FR_T_a, c = FR_T_c) {
  out <- exp(a * temp) * size^(1.88) * c
  return(out)
}
head(feed)

pred.dat.aqua$feedrate <- FR_T_fun(pred.dat.aqua$Temp.n, pred.dat.aqua$OperculumLength)
pred.dat$feedrate <- FR_T_fun(temp = pred.dat$Temp.n, size = pred.dat$OperculumLength)


# ggplot(feed,aes(x=Temp.n,y=feedrate,color=factor(sizeclass)))+
#   geom_point()+
#   geom_line(data = pred.dat.aqua, 
#             aes(x = as.numeric(Temp.n), 
#                 y=as.numeric(feedrate), 
#                 color = factor(sizeclass)))+
#   xlim(5,18)+
#   ylim(0,.1)+
#   xlab(expression(paste("Temp (",degree,"C)")))+
#   ylab("Cysts per minute (min -1)")


 gg1 <- ggplot(data = pred.dat.aqua, 
       aes(x = as.numeric(Temp.n), 
           y=as.numeric(feedrate), 
           color = OperculumLength, group = as.factor(sizeclass)))+
  geom_line()+
  scale_color_viridis(option = "D", limits = c(4, 7))+
  geom_point(data = feed,aes(x=Temp.n,y=feedrate,color=OperculumLength))+
  xlim(5,20)+
  ylim(0,.1)+
  xlab(expression(paste("Seawater temp (",degree,"C)")))+
  ylab("Prey per minute (min -1)")
gg1


# ===========================#
# Aquatic respiration (umol oxygen per min) ~ Temp (deg C)####
# Upslope to 23 degrees
# ===========================#
# Use a subset of the data, up to 23 degrees to interpolate respiration rate upslope, 
# Wu and Levings equation with random component, assuming allometric scaling factor

setwd("~/Box/Sarah and Molly's Box/FHL data/aqua feeding and resp")
setwd("C:/Users/eroberts/Box/Sarah and Molly's Box/FHL data/aqua feeding and resp")
resp<- read.table("AQtotalresp_FHL.csv", header = T, sep = ",")
resp$Temp<- as.factor(resp$Temp)
resp$Operc
resp$ID <- as.factor(resp$Barnacle)
resp$Temp.n <- as.numeric(as.character(resp$Temp))
resp$sizeclass <- bin(resp$Operc, nbins = 3, labels = c("small", "medium", "large"))
resp$bin <- bin(resp$Operc, nbins = 3) #(4.02,4.97] (4.97,5.91] (5.91,6.86]

resp_below_23 <- resp[resp$Temp.n < 23,] #Changed 2020 Aug 12
resp <- resp_below_23

AQR_T <- nlme(Respiration.Rate ~ c * exp(a * Temp.n) * Operc^1.88,
              data = resp,
              fixed = a + c ~ 1,
              random = c ~ 1,
              groups = ~ ID,
              start = c(a = 0.0547657, c = 0.0003792))
summary(AQR_T) 
AQR_T_a <- AQR_T$coefficients$fixed[[1]]
AQR_T_c <- AQR_T$coefficients$fixed[[2]]

AQR_T_fun <- function(temp, size, c = AQR_T_c, a = AQR_T_a) {
  out <- c * exp(a * temp) * size^1.88
  return(out)
}


pred.dat.aqua$aq_resp <- AQR_T_fun(temp = pred.dat.aqua$Temp.n, size = pred.dat.aqua$OperculumLength)
pred.dat$aq_resp <- AQR_T_fun(temp = pred.dat$Temp.n, size = pred.dat$OperculumLength)

head(resp)
if(figs == "Y"){

    # ggplot(resp,aes(x=Temp.n,y=Respiration.Rate,color=factor(sizeclass)))+
    #   geom_point()+
    #   geom_line(data = pred.dat.aqua, 
    #             aes(x = as.numeric(Temp.n), 
    #                 y=as.numeric(aq_resp), 
    #                 color = factor(sizeclass)))+
    #   xlim(5,18)+
    #   ylim(0,.01)

resp_aq <- resp
gg2 <- ggplot(resp_aq,aes(x=Temp.n,y=Respiration.Rate,color=Operc))+
    geom_point()+
    scale_color_viridis(option = "D", limits = c(4,7))+
    geom_line(data = pred.dat.aqua, 
              aes(x = as.numeric(Temp.n), 
                  y=as.numeric(aq_resp), 
                  color = OperculumLength, group = as.factor(sizeclass)))+
    # xlim(5,23)+
    # ylim(0,.01)+
    xlab(expression(paste("Seawater temp (",degree,"C)")))+
    ylab("Aquatic repiration (umol min -1)")
gg2  

    }

# ===========================#
# Aerial exposure respiration (umol oxygen per 15 min) ~ Temp (deg C) ####
# Upslope to 25 deg, 30 deg, and quadratic
# ===========================#
# Two hypotheses: Upslope vs. unimodal hypothesis
setwd("~/Box/Sarah and Molly's Box/FHL data/aerial resp")
setwd("C:/Users/eroberts/Box/Sarah and Molly's Box/FHL data/aerial resp")

resp<- read.csv("FHLmean15mins_recovery_forMolly_40deg.csv", stringsAsFactors = FALSE, header = T, sep = ",")
resp$Temp<- as.factor(resp$Temp)
resp$Operc <- resp$operculum_length
resp$Temp.n <- as.numeric(as.character(resp$Temp))
resp$Respiration.Rate <- resp$air_15min
resp$Barnacle <- as.factor(resp$Barnacle)
resp$sizeclass <- bin(resp$Operc, nbins = 3, labels = c("small", "medium", "large"))
resp$bin <- bin(resp$Operc, nbins = 3) #(4.02,4.97] (4.97,5.91] (5.91,6.86]

resp_below_30 <- resp[resp$Temp.n <= 30,]
resp_below_25 <- resp[resp$Temp.n <= 25,]


AER_EXPOSE_T_25 <- nlme(Respiration.Rate ~ c * exp(a * Temp.n) * Operc^2.32,
                        data = resp_below_25,
                        fixed = a + c ~ 1,
                        random = c ~ 1,
                        groups = ~ Barnacle,
                        start = c(a = 0.0547657, c = 0.0003792))
summary(AER_EXPOSE_T_25) 
AER_EXPOSE_T_25_a <- AER_EXPOSE_T_25$coefficients$fixed[[1]]
AER_EXPOSE_T_25_c <- AER_EXPOSE_T_25$coefficients$fixed[[2]]

AER_EXPOSE_T_30 <- nlme(Respiration.Rate ~ c * exp(a * Temp.n) * Operc^2.32,
                        data = resp_below_30,
                        fixed = a + c ~ 1,
                        random = c ~ 1,
                        groups = ~ Barnacle,
                        start = c(a = 0.0547657, c = 0.0003792))
summary(AER_EXPOSE_T_30) 
AER_EXPOSE_T_30_a <- AER_EXPOSE_T_30$coefficients$fixed[[1]]
AER_EXPOSE_T_30_c <- AER_EXPOSE_T_30$coefficients$fixed[[2]]

AER_EXPOSE_T_all <- nlme(Respiration.Rate ~ c * exp(a1*Temp.n) * exp(a2*Temp.n^2) * Operc^2.32,
                         data = resp,
                         fixed = a1 + a2 + c ~ 1,
                         random = c ~ 1,
                         groups = ~ Barnacle,
                         start = c(a1 = 2.201e-01, a2 = -4.623e-03, c = 1.433e-05))
summary(AER_EXPOSE_T_all) 
AER_EXPOSE_T_all_a1 <- AER_EXPOSE_T_all$coefficients$fixed[[1]]
AER_EXPOSE_T_all_a2 <- AER_EXPOSE_T_all$coefficients$fixed[[2]]
AER_EXPOSE_T_all_c <- AER_EXPOSE_T_all$coefficients$fixed[[3]]

# Aerial exposure respiration rate
AER_EXPOSE_T_25_fcn <- function(temp, size, c = AER_EXPOSE_T_25_c, a = AER_EXPOSE_T_25_a) {
  out <- c * exp(a * temp) * size^2.32
  return(out)
}
AER_EXPOSE_T_30_fcn <- function(temp, size, c = AER_EXPOSE_T_30_c, a = AER_EXPOSE_T_30_a) {
  out <- c * exp(a * temp) * size^2.32
  return(out)
}
AER_EXPOSE_T_all_fcn <- function(temp, size, c = AER_EXPOSE_T_all_c, a1 = AER_EXPOSE_T_all_a1, a2 = AER_EXPOSE_T_all_a2) {
  out <- c * exp(a1 * temp) * exp(a2 * temp^2) * size^2.32
  return(out)
}

pred.dat$pred_25 <- AER_EXPOSE_T_25_fcn(pred.dat$Temp.n, pred.dat$Operc)
pred.dat$pred_30 <- AER_EXPOSE_T_30_fcn(pred.dat$Temp.n, pred.dat$Operc)
pred.dat$pred_all <- AER_EXPOSE_T_all_fcn(pred.dat$Temp.n, pred.dat$Operc)
pred.dat.air$pred_25 <- AER_EXPOSE_T_25_fcn(pred.dat.air$Temp.n, pred.dat.air$Operc)
pred.dat.air$pred_30 <- AER_EXPOSE_T_30_fcn(pred.dat.air$Temp.n, pred.dat.air$Operc)
pred.dat.air$pred_all <- AER_EXPOSE_T_all_fcn(pred.dat.air$Temp.n, pred.dat.air$Operc)
pred.dat.air$air_resp_exposure_25 <- AER_EXPOSE_T_25_fcn(temp = pred.dat.air$Temp.n, size = pred.dat.air$OperculumLength)
pred.dat.air$air_resp_exposure_30 <- AER_EXPOSE_T_30_fcn(temp = pred.dat.air$Temp.n, size = pred.dat.air$OperculumLength)
pred.dat.air$air_resp_exposure_all <- AER_EXPOSE_T_all_fcn(temp = pred.dat.air$Temp.n, size = pred.dat.air$OperculumLength)





if(figs == "Y"){
  # ggplot(resp,aes(x=Temp.n,y=Respiration.Rate,color=factor(sizeclass)))+
  #   geom_point()+
  #   ylab("Aerial Respiration (umol per 15 min)") +
  #   xlab("Temp (deg C)")+
  #   geom_line(data = pred.dat, aes(x = as.numeric(Temp.n),
  #                                  y=as.numeric(pred_30), color = factor(sizeclass)), linetype = "dashed")+
  #   geom_line(data = pred.dat, aes(x = as.numeric(Temp.n),
  #                                  y=as.numeric(pred_all), color = factor(sizeclass)), linetype = "dotted")+
  #   geom_line(data = pred.dat, aes(x = as.numeric(Temp.n),
  #                                  y=as.numeric(pred_25), color = factor(sizeclass)), linetype = "solid")
  resp_exp <- resp
  gg3 <- ggplot(resp_exp,aes(x=Temp.n,y=Respiration.Rate/15,color=Operc))+
    geom_point()+
    scale_color_viridis(option = "D", limits = c(4,7))+
    geom_line(data = pred.dat.air, 
              aes(x = as.numeric(Temp.n), 
                  y=as.numeric(pred_25)/15, 
                  color = OperculumLength, group = as.factor(sizeclass)), linetype = "dashed")+
    geom_line(data = pred.dat.air, 
              aes(x = as.numeric(Temp.n), 
                  y=as.numeric(pred_30)/15, 
                  color = OperculumLength, group = as.factor(sizeclass)))+
    xlim(5,34)+
    ylim(0,.03)+
    xlab(expression(paste("Low tide temp (",degree,"C)")))+
    ylab("Aerial repiration during exposure \n (umol per min)")
  gg3  
  
  
  
  }


# ===========================#
# Aerial recovery respiration ~ Temp ####
# We start with umol oxygen total, but then calculate umol oxygen per 15 min exposure
# Upslope to 30 deg C
# ===========================#
# Two hypotheses: Upslope vs. unimodal hypothesis
setwd("~/Box/Sarah and Molly's Box/FHL data/aerial resp")
setwd("C:/Users/eroberts/Box/Sarah and Molly's Box/FHL data/aerial resp")

resp<- read.csv("FHLmean15mins_recovery_forMolly_40deg.csv", stringsAsFactors = FALSE, header = T, sep = ",")
resp$Temp<- as.factor(resp$Temp)
resp$Operc <- resp$operculum_length
resp$Temp.n <- as.numeric(as.character(resp$Temp))
resp$Barnacle <- as.factor(resp$Barnacle)
resp$Respiration.Rate <- resp$total_aquat
resp$sizeclass <- bin(resp$Operc, nbins = 3, labels = c("small", "medium", "large"))
resp$bin <- bin(resp$Operc, nbins = 3) #(4.02,4.97] (4.97,5.91] (5.91,6.86]

# ramp.time <- (resp$Temp.n-10)/10 * 1
# time.at.temp <- 5 - ramp.time
# plot(resp$Temp.n, time.at.temp*60)

resp_below_30 <- resp[resp$Temp.n <= 30,]
AER_RECOVER_T_30 <- nlme(Respiration.Rate ~ c * exp(a * Temp.n) * Operc^2.32,
                         data = resp_below_30,
                         fixed = a + c ~ 1,
                         random = c ~ 1,
                         groups = ~ Barnacle,
                         start = c(a = 0.0547657, c = 0.0003792))
summary(AER_RECOVER_T_30)
AER_RECOVER_T_30_a <- AER_RECOVER_T_30$coefficients$fixed[[1]]
AER_RECOVER_T_30_c <- AER_RECOVER_T_30$coefficients$fixed[[2]]

resp_below_25 <- resp[resp$Temp.n <= 25,]
AER_RECOVER_T_25 <- nlme(Respiration.Rate ~ c * exp(a * Temp.n) * Operc^2.32,
                         data = resp_below_25,
                         fixed = a + c ~ 1,
                         random = c ~ 1,
                         groups = ~ Barnacle,
                         start = c(a = 0.0547657, c = 0.0003792))
summary(AER_RECOVER_T_25)
AER_RECOVER_T_25_a <- AER_RECOVER_T_25$coefficients$fixed[[1]]
AER_RECOVER_T_25_c <- AER_RECOVER_T_25$coefficients$fixed[[2]]


AER_RECOVER_T_all <- nlme(Respiration.Rate ~ c * exp(a1*Temp.n) * exp(a2*Temp.n^2) * Operc^2.32,
                         data = resp,
                         fixed = a1 + a2 + c ~ 1,
                         random = c ~ 1,
                         groups = ~ Barnacle,
                         start = c(a1 = 2.201e-01, a2 = -4.623e-03, c = 1.433e-05))
summary(AER_RECOVER_T_all)
AER_RECOVER_T_all_a1 <- AER_RECOVER_T_all$coefficients$fixed[[1]]
AER_RECOVER_T_all_a2 <- AER_RECOVER_T_all$coefficients$fixed[[2]]
AER_RECOVER_T_all_c <- AER_RECOVER_T_all$coefficients$fixed[[3]]

AER_RECOVER_T_25_fcn <- function(temp, size, c = AER_RECOVER_T_25_c, a = AER_RECOVER_T_25_a) {
  out <- c * exp(a * temp) * size^2.32
  return(out)
}
AER_RECOVER_T_30_fcn <- function(temp, size, c = AER_RECOVER_T_30_c, a = AER_RECOVER_T_30_a) {
  out <- c * exp(a * temp) * size^2.32
  return(out)
}
AER_RECOVER_T_all_fcn <- function(temp, size, c = AER_RECOVER_T_all_c, a1 = AER_RECOVER_T_all_a1, a2 = AER_RECOVER_T_all_a2) {
  out <- c * exp(a1 * temp) * exp(a2 * temp^2) * size^2.32
  return(out)
}

# Here I need to add the weirdness bit to account for time... but after I calc this is OK... 

pred.dat$pred_25 <- AER_RECOVER_T_25_fcn(pred.dat$Temp.n, pred.dat$Operc)
pred.dat$pred_30 <- AER_RECOVER_T_30_fcn(pred.dat$Temp.n, pred.dat$Operc)
pred.dat$pred_all <- AER_RECOVER_T_all_fcn(pred.dat$Temp.n, pred.dat$Operc)

pred.dat.air$pred_25 <- AER_RECOVER_T_25_fcn(pred.dat.air$Temp.n, pred.dat.air$Operc)
pred.dat.air$pred_30 <- AER_RECOVER_T_30_fcn(pred.dat.air$Temp.n, pred.dat.air$Operc)
pred.dat.air$pred_all <- AER_RECOVER_T_all_fcn(pred.dat.air$Temp.n, pred.dat.air$Operc)


if(figs == "Y"){
  # ggplot(resp,aes(x=Temp.n,y=Respiration.Rate,color=factor(sizeclass)))+
  #   geom_point()+
  #   ylab("Aerial recover respiration (total umol)") +
  #   xlab("Temp (deg C)")+
  #   # geom_line(data = pred.dat, aes(x = as.numeric(Temp.n),
  #   #                                y=as.numeric(pred_all), color = factor(sizeclass)), linetype = "dashed")+
  #   geom_line(data = pred.dat, aes(x = as.numeric(Temp.n),
  #                                  y=as.numeric(pred_30), color = factor(sizeclass)), linetype = "dotted")+
  #   geom_line(data = pred.dat, aes(x = as.numeric(Temp.n),
  #                                  y=as.numeric(pred_25), color = factor(sizeclass)), linetype = "solid")

  resp_recov <- resp
  gg4 <- ggplot(resp_recov,aes(x=Temp.n,y=Respiration.Rate,color=Operc))+
    geom_point()+
    scale_color_viridis(option = "D", limits = c(4,7))+
    geom_line(data = pred.dat.air, 
              aes(x = as.numeric(Temp.n), 
                  y=as.numeric(pred_25), 
                  color = OperculumLength, group = as.factor(sizeclass)), linetype = "dashed")+
    geom_line(data = pred.dat.air, 
              aes(x = as.numeric(Temp.n), 
                  y=as.numeric(pred_30), 
                  color = OperculumLength, group = as.factor(sizeclass)))+
    xlim(5,34)+
    #ylim(0,.01)+
    xlab(expression(paste("Low tide temp (",degree,"C)")))+
    ylab("Aerial recovery total respiration \n (total umol)")
  gg4   
  
}

library(gridExtra)
#grid.arrange(gg1,gg2,gg3,gg4)
library(ggpubr)
ggarrange(gg1,gg3,gg2,gg4, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")


# ===========================#
# Aerial exposure and recovery together ~ Temp ####
# ===========================#

# aerial_cost_exposureANDrecovery <- function(temp, size, type = "upslope30") {
#   ramp.time <- (temp-10)/10 * 1 #10 degrees per hour
#   time.at.temp <- (5 - ramp.time)*60 # low tide minutes = 5 hours of exposure minus ramp time * 60 
#   if (type == "upslope30") { total_oxygen_debt <-AER_RECOVER_T_30_fcn(temp, size) - AER_RECOVER_T_30_fcn(10, size) #respiration at temp minus respiration at 10 degrees
#   oxygen_debt_per15minExposure <- (15/time.at.temp) * total_oxygen_debt
#   total_cost_per15minExposure <- oxygen_debt_per15minExposure + AER_EXPOSE_T_30_fcn(temp, size)} 
#   else if (type == "unimodal") {total_oxygen_debt <-AER_RECOVER_T_all_fcn(temp, size) - AER_RECOVER_T_all_fcn(10, size)
#     oxygen_debt_per15minExposure <- (15/time.at.temp) * total_oxygen_debt
#     total_cost_per15minExposure <- oxygen_debt_per15minExposure + AER_EXPOSE_T_all_fcn(temp, size)}
#   return(total_cost_per15minExposure)
# }

str(AER_RECOVER_T_30)
str(AER_EXPOSE_T_30_fcn)

# aerial_cost_exposureANDrecovery <- function(temp, size, type = "upslope30") {
#   ramp.time <- (temp-10)/10 * 60 #10 degrees per hour
#   time.at.temp <- (300 - ramp.time) # low tide minutes = 5 hours of exposure minus ramp time * 60 
#   if (type == "upslope30") { total_oxygen_debt <-AER_RECOVER_T_30_fcn(temp, size) - AER_RECOVER_T_30_fcn(10, size) #respiration at temp minus respiration at 10 degrees
#   oxygen_debt_per15minExposure <- (15/time.at.temp) * total_oxygen_debt
#   total_cost_per15minExposure <- oxygen_debt_per15minExposure + AER_EXPOSE_T_30_fcn(temp, size)} 
#   else if (type == "unimodal") {total_oxygen_debt <-AER_RECOVER_T_all_fcn(temp, size) - AER_RECOVER_T_all_fcn(10, size)
#   oxygen_debt_per15minExposure <- (15/time.at.temp) * total_oxygen_debt
#   total_cost_per15minExposure <- oxygen_debt_per15minExposure + AER_EXPOSE_T_all_fcn(temp, size)}
#   return(total_cost_per15minExposure)
# }

# If using upslope to 30 - can also use unimodal function. 
aerial_cost_exposureANDrecovery <- function(temp, size, AER_RECOVER_T_30_a, AER_RECOVER_T_30_c, AER_RECOVER_T_30_fcn, 
                                            AER_EXPOSE_T_30_a, AER_EXPOSE_T_30_c, AER_EXPOSE_T_30_fcn) {
  ramp.time <- (temp-10)/10 * 60 #10 degrees per hour
  time.at.temp <- (300 - ramp.time) # low tide minutes = 5 hours of exposure minus ramp time * 60 
  total_oxygen_debt <-AER_RECOVER_T_30_fcn(temp, size) - AER_RECOVER_T_30_fcn(10, size) #respiration at temp minus respiration at 10 degrees
  oxygen_debt_per15minExposure <- (15/time.at.temp) * total_oxygen_debt
  total_cost_per15minExposure <- oxygen_debt_per15minExposure + AER_EXPOSE_T_30_fcn(temp, size)
  return(total_cost_per15minExposure)
}

aerial_cost_recovery <- function(temp, size, type = "upslope30") {
  ramp.time <- (temp-10)/10 * 60 #10 degrees per hour
  time.at.temp <- (300 - ramp.time) # low tide minutes = 5 hours of exposure minus ramp time * 60 
  if (type == "upslope30") { total_oxygen_debt <-AER_RECOVER_T_30_fcn(temp, size) - AER_RECOVER_T_30_fcn(10, size) #respiration at temp minus respiration at 10 degrees
  oxygen_debt_per15minExposure <- (15/time.at.temp) * total_oxygen_debt
  total_cost_per15minExposure <- oxygen_debt_per15minExposure + AER_EXPOSE_T_30_fcn(temp, size)} 
  else if (type == "unimodal") {total_oxygen_debt <-AER_RECOVER_T_all_fcn(temp, size) - AER_RECOVER_T_all_fcn(10, size)
  oxygen_debt_per15minExposure <- (15/time.at.temp) * total_oxygen_debt
  # total_cost_per15minExposure <- oxygen_debt_per15minExposure + AER_EXPOSE_T_all_fcn(temp, size)
  }
  return(oxygen_debt_per15minExposure)
}

pred.dat$pred_30 <- aerial_cost_exposureANDrecovery(pred.dat$Temp.n, pred.dat$OperculumLength, "upslope30")
#pred.dat$pred_all <- aerial_cost_exposureANDrecovery(pred.dat$Temp.n, pred.dat$OperculumLength, "unimodal")

pred.dat$pred_expose_30 <- AER_EXPOSE_T_30_fcn(pred.dat$Temp.n, pred.dat$Operc)
# pred.dat$pred_expose_all <- AER_EXPOSE_T_all_fcn(pred.dat$Temp.n, pred.dat$Operc)

pred.dat.air$pred_expose_25 <- AER_EXPOSE_T_25_fcn(pred.dat.air$Temp.n, pred.dat.air$Operc)
pred.dat.air$pred_expose_30 <- AER_EXPOSE_T_30_fcn(pred.dat.air$Temp.n, pred.dat.air$Operc)
pred.dat.air$pred_expose_all <- AER_EXPOSE_T_all_fcn(pred.dat.air$Temp.n, pred.dat.air$Operc)
pred.dat.air$pred_30_recov <- aerial_cost_recovery(pred.dat.air$Temp.n, pred.dat.air$OperculumLength, "upslope30")


resp$Respiration.Rate <- resp$air_15min

if(figs == "Y"){
  ggplot(resp,aes(x=Temp.n,y=Respiration.Rate,color=factor(sizeclass)))+
    geom_point()+
    ylab("Aerial Exposure and Recovery (umol per 15 min)") +
    xlab("Temp (deg C)")+
    geom_line(data = pred.dat, aes(x = as.numeric(Temp.n),
                                   y=as.numeric(pred_30), color = factor(sizeclass)), linetype = "dashed")+
    geom_line(data = pred.dat, aes(x = as.numeric(Temp.n), 
                                   y=as.numeric(pred_expose_30), color = factor(sizeclass)), linetype = "solid")
  # +
  # geom_line(data = pred.dat, aes(x = as.numeric(Temp.n),
  #                             y=as.numeric(pred_30_recov), color = factor(sizeclass)), linetype = "dotted")
  
  
  # Plot both the exposure and recovery at 30 degrees
  ggplot(resp,aes(x=Temp.n,y=Respiration.Rate,color=factor(sizeclass)))+
    geom_point()+
    ylab("Aerial Exposure and Recovery (umol per 15 min)") +
    xlab("Temp (deg C)")+
    # geom_line(data = pred.dat, aes(x = as.numeric(Temp.n), 
    #                                y=as.numeric(pred_30_recov)), linetype = "dashed")+
    geom_line(data = pred.dat, aes(x = as.numeric(Temp.n), 
                                   y=as.numeric(pred_expose_30)), linetype = "solid")
}

pred.dat.air$pred_recov <- aerial_cost_recovery(pred.dat.air$Temp.n, pred.dat.air$OperculumLength, "upslope30")

if(figs == "Y"){
  ggplot(pred.dat.air, aes(x=datetime,y=pred_recov))+
    geom_point()
  # +
  #   geom_point(data = pred.dat.air, aes(x = datetime, 
  #                                       y=as.numeric(air_resp_exposure_all), color = factor(sizeclass)), col = "blue")+
  #   geom_point(data = pred.dat.air, aes(x = datetime, 
  #                                       y=as.numeric(air_resp_exposure_30), color = factor(sizeclass)), col = "green")
  
  str(pred.dat.air)
  ggplot(pred.dat.air, aes(x=datetime,y=pred_recov+air_resp_exposure_30))+
    geom_point()+
    ylab("Aerial Respiration (umol per 15 min)") +
    xlab("Time")+
    geom_point(data = pred.dat.air, aes(x = datetime, 
                                        y=as.numeric(pred_recov + air_resp_exposure_25)), col = "blue")+
    geom_point(data = pred.dat.air, aes(x = datetime, 
                                        y=as.numeric(pred_recov + air_resp_exposure_all)), col = "green")
}
# p1v2 <- ggplot() + geom_line(data=p1df, aes(x=Tot.Visit,y=yvar,color=(tvar.num),group=tvar)) + facet_grid(. ~ Treatment.f)
# p1v2 +  geom_point(data=cdata17, aes(x=Tot.Visit,y=Finalpct, color=Ex1pct) ) + facet_grid(. ~ Treatment.f) + scale_color_viridis( option = "D")



dim(food.interp)
dim(water.temp.2)
length(water.temp.2$food)

head(pred.dat.aqua)
head(pred.dat.air)





max(feed$OperculumLength)
min(feed$OperculumLength)
max(resp$operculum_length)
min(resp$operculum_length)
max(resp_aq$Operc)
min(resp_aq$Operc)

