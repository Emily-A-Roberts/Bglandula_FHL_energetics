# Determine feeding parameters using Lisa's data

# Dec 10, 2020 - 
# Changed resp scaling exponent to be 1.88, consistent with document on resp exponent. Was previously 1.85
# Changed feeding scaling exponent to be 1.88, consistent with document on feeding exponent. Was previously 1.86

# This is using all 4 quadrats. 

# Now using 3 year average of Padilla Bay (or FHL data) chlorophyll data - 20201105

# Possible - compare two different time points within the growth experiment

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

# Plotting values for functional responses
pred.dat <- data.frame(
  Temp.n = rep(seq(from = 5, to = 40, length.out = 20), times = 3),
  OperculumLength = rep(c(4.5, 5.5, 6.5),times = 3,each = 20),
  sizeclass = as.factor(rep(c("small","medium", "large"),times = 3,each = 20))
)


# ============================#
# Read in observed operculum growth data ####
# ============================#
setwd("~/Box/Sarah and Molly's Box/FHL data/Lisa data")
growth <- read.csv("Lisa_growth.csv", stringsAsFactors = FALSE)

plot(growth$Init_operc, growth$Final_operc-growth$Init_operc)

growth$Final_date
Timepoint_1 <- as.POSIXct( "7/19/11", format="%m/%d/%y", tz = "GMT")
Timepoint_2 <- as.POSIXct( "8/25/11", format="%m/%d/%y", tz = "GMT")

# ============================#
# Read in relationship between operculum and tissue mass ####
# ============================#

growth$AFDWg <- (growth$Final_gon_mass+growth$Final_som_mass)
plot(growth$Final_operc, growth$AFDWg)
growth$AFDWmg <- 1000*growth$AFDWg
# mm to g, same as in Gilman et al., 2013
# But here we expect the conversion coefficient
nlm1 <- nls(AFDWmg ~ c*Final_operc^(2.8322), data = growth, start = c(c = .2)) #Note that this exponent is in relation to g not mg. 

(s1 <- summary(nlm1))
s1$coefficients[[1]]

# ============================#
# Calculate tissue mass ####
# ============================#
growth$len_1 <- growth$Init_operc
growth$len_2 <- growth$Final_operc

growth$DW_1 <- s1$coefficients[[1]]*growth$len_1^(2.8322) #mm to mg
growth$DW_2 <- growth$AFDWg*1000 #mg

growth$growthDW_1 <- growth$DW_2 - growth$DW_1


plot(growth$growthDW_1 ~ growth$len_1)
abline(lm(growth$growthDW_1 ~ growth$len_1), col = "black")

plot(growth$growthDW_1 ~ growth$Temp)
abline(lm(growth$growthDW_1 ~ growth$Temp), col = "black")

# ===========================#
# Read in food data ####
# No chlorophyll in 2011 dataset ####
# ===========================#
# setwd("~/Box/Sarah and Molly's Box/FHL data/Lisa data")
# POM <- read.csv("Lisa_POM.csv", stringsAsFactors = FALSE)
# 
# setwd("~/Box/Sarah and Molly's Box/FHL data/Lisa data/2011_PadillaBay")
# Padilla <- read.csv("PDBGSWQ_forR.csv", stringsAsFactors = FALSE)
# 
# str(Padilla)
# 
# plot(POM$POM_g_p_L)
# plot(Padilla.day$Turb)
# 
# Padilla$datetime <- as.POSIXct(Padilla$DateTimeStamp, format="%m/%d/%y %H:%M", tz = "America/Los_Angeles")
# POM$datetime <- as.POSIXct( POM$Date, format="%m/%d/%y", tz = "GMT")
# 
# 
# # old way
# 
# Padilla$agg <- as.Date(Padilla$datetime)
# Padilla.day <- aggregate(Turb ~ datetime,data = Padilla, mean, na.rm = TRUE)
# Padilla.day <- aggregate(Turb ~ datetime,data = Padilla, mean, na.rm = TRUE)
# 
# plot(Padilla.day$Turb ~ Padilla.day$datetime)
# str(Padilla.day)
# str(POM)
# 
# Padilla.day$datetime <- as.Date(Padilla.day$datetime)
# POM$datetime <- as.Date(POM$datetime)
# 
# library(plyr)
# food_comb <- join(POM, Padilla.day, by = "datetime")
# 
# plot(food_comb$POM_g_p_L, food_comb$Turb)


FH <- 1
IJ <- 1

# ===========================#
# alternative way of looking at POM and turbidity - on a weekly basis
# ===========================#
library(lubridate)
# POM$wk <- week(POM$datetime)
# Padilla$wk <- week(Padilla$datetime)
# 
# 
# POM.wk <- aggregate(POM_g_p_L ~ wk,data = POM, mean, na.rm = TRUE)
# Padilla$l.Turb <- log(Padilla$Turb+1) #Turbidity is logged
# 
# Padilla.wk1 <- aggregate(l.Turb ~ wk,data = Padilla, mean, na.rm = TRUE)
# Padilla.wk1 <- aggregate(l.Turb ~ wk,data = Padilla, mean, na.rm = TRUE)
# Padilla.wk2 <- aggregate(Temp ~ wk,data = Padilla, mean, na.rm = TRUE)
# 
# Padilla.wk <- join(Padilla.wk1, Padilla.wk2, by = "wk")


# ===========================#
# Import relationship between Padilla Bay POM and FHL POM
# ===========================#
# setwd("~/Box/Sarah and Molly's Box/FHL data/SONDE data")
# Turb.Pad.FHL <- read.csv("Turb.model.input.csv", stringsAsFactors = FALSE)
# str(Turb.Pad.FHL)
# log(PB$Turb+1)
# full.model <- lm(Turb.FHL ~ Temp.PB + Turb.PB + Sal.PB , data = Turb.Pad.FHL)
# # Padilla bay chlorophyll doesn't seem to be available in the earlier timeperiod. 
# # Stepwise regression model
# step.model <- stepAIC(full.model, direction = "both", 
#                       trace = FALSE)
# summary(step.model)
# data.frame(Padilla.wk$Turb)
# Padilla.wk$Temp.PB <- Padilla.wk$Temp
# Padilla.wk$Turb.PB <- Padilla.wk$l.Turb
# Padilla.wk$prediction <- predict(step.model, new = Padilla.wk)
# 
# 
# #Padilla.wk <- aggregate(l.Turb  ~ wk,data = Padilla, mean, na.rm = TRUE) #Average the log
# food_comb <- join(POM.wk, Padilla.wk, by = "wk")
# 
# str(food_comb)
# 
# #plot(food_comb$POM_g_p_L, food_comb$Turb)
# plot(food_comb$POM_g_p_L, food_comb$prediction) #Logged turbidity before averaging. Not much of a difference. 
# m2 <- lm(food_comb$POM_g_p_L ~ food_comb$prediction)
# abline(lm(food_comb$POM_g_p_L ~ food_comb$prediction))
# 
# food_comb_trunc <- food_comb[food_comb$wk>=29 & food_comb$wk<=34,]
# plot(log(food_comb_trunc$POM_g_p_L+1), food_comb_trunc$prediction) #Logged turbidity before averaging. Not much of a difference. 
# abline(food_comb_trunc$prediction~log(food_comb_trunc$POM_g_p_L))
# mod1 <- lm(food_comb_trunc$prediction~log(food_comb_trunc$POM_g_p_L))
# summary(mod1)

# ==========#
# New - 3 year average of this time of year. 
# ==========#
# setwd("~/Box/Sarah and Molly's Box/FHL data/SONDE data")
# Turb.Pad.FHL <- read.csv("Turb.model.input.csv", stringsAsFactors = FALSE)

setwd("~/Box/Sarah and Molly's Box/FHL data/SONDE data/Padilla Bay/909184_all_data_2003topresent")
PB <- read.csv("PDBGSWQ_dataonly.csv", stringsAsFactors = FALSE)
head(PB)

library(dplyr)
library(tidyr)

PB$datetime <-as.POSIXct(PB$m.d.y.hh.mm, format = "%m/%d/%y %H:%M")
str(as.factor(year(PB$datetime)))

#subset to only include the past 3 years...

# plot(PB$datetime, PB$ug.l, pch = ".")
PB_subset <- PB[PB$datetime > as.Date("2017-01-01") & PB$datetime < as.Date("2020-01-01"),]
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

# Which week was this? Wk 30 - 35, ~5ug/L (would be about saturating for mussels)
# Use a 2 week lag between food availability and growth
# plot(dat[28:35,])
# abline(lm(dat[28:35,]))
# Just averaging of this full timeperiod, including a 2 week lag. 

# Food value ####
FoodLevel_ug_L <- mean(dat$value[28:33]) 

#Which week was this? Wk 30 - 35, ~5ug/L (would be about saturating for mussels)
# 
# cx <- c(0,cumsum(dat$value))
# rsum <- (cx[(n+1):length(cx)] - cx[1:(length(cx) - n)]) / n
# plot(cx, type = 'b', ylab = "fluorescence (ug/L)", xlab = "week")
# library(data.table)
# new <-frollmean(dat[, "value"], 3)
# 
# ma <- function(arr, n=2){
#   res = arr
#   for(i in n:length(arr)){
#     res[i] = mean(arr[(i-n):i])
#   }
#   res
# }
# 
# plot(ma(dat$value), type = "b")
# 
# vars <-"m.d.y.hh.mm"
# dat <- PB %>% 
#   drop_na(ug.l, any_of(vars))%>%
#   dplyr::group_by(time = week(datetime)) %>% 
#   dplyr::summarise(value = ave(ug.l), .groups = 'keep')
# dat
# plot(dat, type = 'b', ylab = "fluorescence (ug/L)", xlab = "week")
# 
# vars <-"m.d.y.hh.mm"
# dat <- PB %>% 
#   drop_na(ug.l, any_of(vars))%>%
#   dplyr::group_by(time = year(datetime),month(datetime)) %>% 
#   dplyr::summarise(value = ave(ug.l), .groups = 'keep')
# dat
# plot(paste(dat$time,dat$`month(datetime)`),dat$value, type = 'b', ylab = "fluorescence (ug/L)", xlab = "week")
# 
# plot(PB$datetime, PB$ug.l, pch = ".")
# plot(PB$datetime, PB$psu, pch = ".")
# 
# vars <-"m.d.y.hh.mm"
# dat <- PB %>% 
#   drop_na(psu, any_of(vars))%>%
#   dplyr::group_by(time = week(datetime)) %>% 
#   dplyr::summarise(value = ave(psu), .groups = 'keep')
# dat
# plot(dat, type = 'b', ylab = "psu", xlab = "week")
# 


# ===========================#
# Read in temp data, determine aquatic and aerial temperatures ####
# ===========================#

# Temp is included in the growth file and was constant 

# ===========================#
# First run model over one particular week 
# where low tide is visibly different from high tide. 
# Subset data into this week. 
# ===========================#

# ===========================#
# Dataframe for predictions - start with a static model
# ===========================#

temp.num <- length(air.H.subset$Temp)
# pred.dat.air <- data.frame(
#   datetime = air.H.subset$datetime,
#   Temp.n = air.H.subset$Temp,
#   OperculumLength = rep(5.5,each = temp.num),
#   sizeclass = as.factor(rep(c("all"),each = temp.num))
# )

temp.num <- length(growth$Temp)
pred.dat.aqua <- data.frame(
  Temp.n = growth$Temp,
  Food.n = FoodLevel_ug_L,
  OperculumLength = growth$len_1,
  sizeclass = as.factor(rep(c("all"),each = temp.num))
)



# ======#
# Some graphs including water level ####
# ======#

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

FR_T_a <- coef(FR_T)[1]
FR_T_c <- coef(FR_T)[2]

FR_T_fun <- function(temp, size, a = FR_T_a, c = FR_T_c) {
  out <- exp(a * temp) * size^(1.88) * c
  return(out)
}


pred.dat$pred <- FR_T_fun(pred.dat$Temp.n, pred.dat$OperculumLength)
ggplot(feed,aes(x=Temp.n,y=feedrate,color=factor(sizeclass)))+
  geom_point()+
  geom_line(data = pred.dat, 
            aes(x = as.numeric(Temp.n), 
                                 y=as.numeric(pred), 
                                 color = factor(sizeclass)))+
  xlim(5,18)+
  ylim(0,.1)

pred.dat.aqua$feedrate <- FR_T_fun(temp = pred.dat.aqua$Temp.n, size = pred.dat.aqua$OperculumLength)
# plot(pred.dat.aqua$Temp.n,pred.dat.aqua$feedrate)


# ===========================#
# Aquatic respiration (umol oxygen per min) ~ Temp (deg C)####
# Upslope to 23 degrees
# ===========================#
# Use a subset of the data, up to 23 degrees to interpolate respiration rate upslope, 
# Wu and Levings equation with random component, assuming allometric scaling factor

setwd("~/Box/Sarah and Molly's Box/FHL data/aqua feeding and resp")
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
pred.dat$pred <- AQR_T_fun(pred.dat$Temp.n, pred.dat$OperculumLength)
ggplot(resp,aes(x=Temp.n,y=Respiration.Rate,color=factor(sizeclass)))+
  geom_point()+
  geom_line(data = pred.dat, aes(x = as.numeric(Temp.n), y=as.numeric(pred), color = factor(sizeclass)))

pred.dat.aqua$aq_resp <- AQR_T_fun(temp = pred.dat.aqua$Temp.n, size = pred.dat.aqua$OperculumLength)
plot(pred.dat.aqua$aq_resp ~ pred.dat.aqua$Temp.n)

# For very small barnacles (Operculum 1.3mm), I'm getting respiration values of 0.00015 - 0.00035 umol per minute

# ===========================#
# Aerial exposure respiration (umol oxygen per 15 min) ~ Temp (deg C) ####
# Upslope to 25 deg, 30 deg, and quadratic
# ===========================#

# No aerial respiration

# ===========================#
# Aerial recovery respiration ~ Temp ####
# We start with umol oxygen total, but then calculate umol oxygen per 15 min exposure
# Upslope to 30 deg C
# ===========================#

# ===========================#
# Aerial exposure and recovery together ~ Temp ####
# ===========================#

# ===========================#
# Determine cumulative food intake, aquatic, and aerial respiration over each 6 month ####
# ===========================#
#SFG = intake + cost aerial + cost aquatic
#SFG = B1 * f1(T,mass)  + f2(T,mass) + f3(T,mass)
# Start with sum, but then later can progress through masses if I write a function that makes new predictions each timestep.   
# aquatic respiration and feed rate are converted to a 15 min time period here

pred.dat.aqua <- pred.dat.aqua[!is.na(pred.dat.aqua$Temp.n),]


 pred.dat.aqua.1 <- pred.dat.aqua[pred.dat.aqua$datetime<=Timepoint_2&pred.dat.aqua$datetime>=Timepoint_1,]


Time_aqua_1 <- length(!is.na(pred.dat.aqua.1$Temp.n)) * 15 # Number of minutes
Time_1_days <- as.numeric((Timepoint_2 - Timepoint_1))
Time_1_num_15min_intervals <- as.numeric((Timepoint_2 - Timepoint_1) * (4 * 24)) # Number of days TIMES number of 15 min periods in a day
Time_1_min <- as.numeric((Timepoint_2 - Timepoint_1) * (24 *60)) # Number of days TIMES number of 15 min periods in a day


# 4.8 cal/ml 02 (Crisp, 1971)
# Not cO2 (µmol L-1) = 44.6596 · cO2 (mLSTP L-1) https://repository.oceanbestpractices.org/bitstream/handle/11329/417/56281.pdf?sequence=1&isAllowed=y
# 1J = 4.184 cal
# CF_J_per_umolO2 <- (1/4.184)*(4.8)*(44.6596) # What is the conversion between J and oxygen consumption?
CF_J_per_umolO2 <- .457 #Ober 2019, Hill et al. 2008, Fly et al. 2012 - can't find original reference
ED <- 1.31 #23.01 # Energy density of growth tissue J / mg AFDW, Wu and Levings
ED_J_p_mg <- 23.01

cost_total <- pred.dat.aqua$aq_resp*Time_1_min*CF_J_per_umolO2 # total cost over all 15-min periods
intake_total <- pred.dat.aqua$feedrate*Time_1_min # total intake over all 15-min periods
intake_total_ugL <- intake_total * FoodLevel_ug_L

cost_per_day <- cost_total / Time_1_days # cost per day
intake_per_day <- intake_total / Time_1_days # intake per day in terms of shrimp total
intake_per_day_ugL <- intake_per_day * FoodLevel_ug_L

# growth.Aug.Feb is growth in terms of operculum length. 
growth_total_in_J <- growth$growthDW_1*ED # Calculate J of tissue growth from mg, using energy density value
growth_per_day_in_J <- growth_total_in_J / Time_1_days# Calculate J of tissue growth per day
# Note that these barnacles grew much more than in the Wu and Levings experiment
# This is likely because the change in gonad is included along with the change in body tissue. 
# Here our respiration values are per operculum length and not so much per somatic vs. gonadic tissue mass. 

# There is an issue here which is that respiration is a tiny fraction of the energy going to growth.
# This seems wrong. 

mod <- lm(growth_total_in_J ~ intake_total_ugL + offset (-cost_total)+ 0) # This doesn't work because it is a single value. This will be three values... 
summary(mod)

lm(growth_per_day_in_J ~ intake_per_day + offset (-cost_per_day)+ 0) # This doesn't work because it is a single value. This will be three values... 
# The coefficient is 0.00235 that relates the per day cost, intake and growth per day in J. 

# If using initial length for length
# Call:
#   lm(formula = growth_total_in_J ~ intake_total + offset(-cost_total) + 
#        0)
# 
# Coefficients:
#   intake_total  
# 0.0002069  


par(mfrow= c(1,1))
plot(intake_per_day * .0002069 ~ pred.dat.aqua$Temp.n, ylim = c(0,1),  
     col = "black", ylab = "Power (Joules per day)")
points(cost_per_day ~ pred.dat.aqua$Temp.n, col = "blue")
points(growth_per_day_in_J ~ pred.dat.aqua$Temp.n, col = "green")


par(mfrow= c(1,1))
plot(intake_total * .0002069 ~ pred.dat.aqua$Temp.n, ylim = c(0,25), 
     col = "black", ylab = "Power (Joules over 39 days)")
  points(cost_total ~ pred.dat.aqua$Temp.n, col = "blue")
points(growth_total_in_J ~ pred.dat.aqua$Temp.n, col = "green")

10*30

# Check with Wu and Levings estimate
# Consumption is 699 Calories per year
(699.5/4.184)*(1/365) #0.45 J per day

# aquatic respiration cost was 300 calories per year

# A reasonable coefficient is _ J per food unit

# ===========================#
# Correlation of SFG calculation with observed growth ####
# Compare 2 alternative conceptual models of aerial costs, using AIC
# ===========================#

# ===========================#
# Calibrate SFG given observed growth by determining f (maximum likelihood estimation) ####
# Compare 2 alternative conceptual models of aerial costs, using AIC
# ===========================#

# ===========================#
# Graphs 

temp.num <- length(growth$Temp)
pred.dat.aqua <- data.frame(
  Temp.n = growth$Temp,
  Food.n = FoodLevel_ug_L,
  OperculumLength = growth$len_1,
  sizeclass = as.factor(rep(c("all"),each = temp.num))
)

# # Now iterate through growth measurements
# Iter.temp <- data.frame(
#   datetime = seq(),####
#   aq_temp = growth$Temp,
#   len = rep(NA, length.out = nrow(temp.dat)),
#   mass = rep(NA, length.out = nrow(temp.dat))
# )
# 
# Iter.temp <- Iter.temp[order(Iter.temp$datetime),]
# 
# Iter.len <- matrix(data = NA, nrow = nrow(Test.dat), ncol = nrow(temp.dat))
# 
# i <- 1
# Iter.len[,1] <- Test.dat$Len0
# for(i in 1:(length(Iter.temp$datetime)-1)){
#   pred_expose_30 <- rep(0, length.out = length(Iter.len[,i]))
#   pred_recov <- rep(0, length.out = length(Iter.len[,i]))
#   feedrate <- rep(0, length.out = length(Iter.len[,i]))
#   aq_resp <- rep(0, length.out = length(Iter.len[,i]))
#   if(!is.na(Iter.temp$air_temp[i])){ #aerial
#     pred_expose_30 <- AER_EXPOSE_T_30_fcn(Iter.temp$air_temp[i], Iter.len[,i])
#     pred_recov <- aerial_cost_recovery(Iter.temp$air_temp[i], Iter.len[,i], "upslope30")
#   }else if(!is.na(Iter.temp$aq_temp[i])){ #aquatic
#     feedrate <- FR_T_fun(temp = Iter.temp$aq_temp[i], size = Iter.len[,i])
#     aq_resp <- AQR_T_fun(temp = Iter.temp$aq_temp[i], size = Iter.len[,i])
#   }
#   cost_15min <- aq_resp*15 + pred_expose_30 + pred_recov #total
#   intake_15min <- feedrate*15 #total
#   intake_scaled <- intake_15min*0.000207
#   f_conver <- f_conver # f_conver is a starting point for this parameter that is defined in the previous code
#   del <- intake_scaled*f_conver - cost_15min 
#   # convert the change in mass to a change in length
#   new.mass <- .2 *Iter.len[,i]^2.8322 + del #mm AFDW, approx, assuming a coefficient of 0.2
#   new.len <- (new.mass/.2)^(1/2.8322)
#   Iter.len[,i+1] <- new.len #add new length to the next column
# }  
# 
# Iter.mass <- .2 *Iter.len^2.8322
# 
# #change.wt.15.min <- 
# j <- 2
# plot(Iter.temp$datetime, Iter.len[1,1:length(Iter.temp$datetime)], type = "l",
#      ylab = "Length (mm)", xlab = "Date")
# for(j in 2:nrow(Test.dat)){
#   lines(Iter.temp$datetime, Iter.len[j,1:length(Iter.temp$datetime)], type = "l", col = j)
# }
# 
# #change.wt.15.min <- 
# j <- 2
# plot(Iter.temp$datetime, Iter.mass[1,1:length(Iter.temp$datetime)], type = "l",
#      ylab = "Mass (mg)", xlab = "Date")
# for(j in 2:nrow(Test.dat)){
#   lines(Iter.temp$datetime, Iter.mass[j,1:length(Iter.temp$datetime)], type = "l", col = j)
# }
# 
# plot(Iter.mass[1,length(Iter.temp$datetime)] - Iter.mass[1,1:length(Iter.temp$datetime)])


