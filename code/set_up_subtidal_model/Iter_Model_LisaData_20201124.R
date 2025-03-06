# 1st run Model_LisaData_20201105 to set up functions. 


UML <- "U" #This "Upper" marker is a placeholder
climate_air_change <- 0
climate_water_change <- 0
f_conver <- 1


    growth <- growth
    n <- length(growth$len_1)
    Len0 <- growth$len_1
    Tot.growth.mm <- growth$len_2-growth$len_1
    plot(growth$len_1, Tot.growth.mm)
 

Test.dat <- data.frame(
  Elevation = as.factor(rep(UML,times = n)),
  Time = as.factor(rep(1, length.out = n)),
  ID = seq(from = 1, to = n, by =1),
  Len0, 
  Tot.growth.mm
)

Test.dat$code <- paste(Test.dat$Elevation, Test.dat$Time, Test.dat$ID)
str(Test.dat)

# Experimental relationships between length and mass are here:

# AIC(m1 = upper_len_mass[s1$df], m2 = upper_len_mass[s2$df], m3 = upper_len_mass[s3$df])
# AIC(m1 = upper_len_mass[s1$df], m2 = upper_len_mass[s2$df], m3 = upper_len_mass[s3$df])

# But instead we will use one relationship between length and mass
# Gilman 2013 2.8322 with SE 0.2631
# Palmer 1980 2.8

# starting values ####
start <- Test.dat
# head(pred.dat.air)
# if(pred.dat.air[1] = NA ){ #This criteria may change
# temp.dat <- full_join(pred.dat.air, pred.dat.aqua, by = "datetime")
# }else{
#   temp.dat <- cbind(pred.dat.aqua, datetime = as.Date(Timepoint_1))
# }
# head(temp.dat)
saved
#<-Timepoint_2 
Timepoint_1
test.Timepoint_2 <- "2011-07-20"
Timepoint_2<- test.Timepoint_2

library(lubridate)

# 2011-07-19 GMT
# "2011-08-25 GMT"

#datetime_seq <- seq(from = as.POSIXct(Timepoint_1), to = as.POSIXct(Timepoint_2), by = minute(15))
datetime_seq <- seq(ymd_hm('2011-07-19 00:00'),ymd_hm('2011-08-25 00:00'), by = '15 mins')
#datetime_seq <- seq(ymd_hm('2011-07-19 00:00'),ymd_hm('2011-07-20 00:00'), by = '15 mins')



length(datetime_seq)
head(datetime_seq, 200)
temp_seq <- pred.dat.aqua$Temp.n

# Iter.temp <- data.frame(
#   datetime = rep(datetime_seq, each = 4),
#   aq_temp = rep(temp_seq, times = length(datetime_seq)) + climate_water_change,
#   len = NA,
#   mass = NA
# )

Iter.date <- data.frame(
  datetime = datetime_seq,
  len = NA,
  mass = NA
)

Iter.len <- matrix(data = 0, nrow = nrow(Test.dat), ncol = length(datetime_seq))
str(Iter.len)
Iter.temp <- Iter.len
for (i in 1:(length(datetime_seq))){
  Iter.temp[1:4,i] = temp_seq
}
Iter.temp <- data.frame(t(Iter.temp))
Iter.len <- data.frame(t(Iter.len))

head(Iter.temp)
head(Iter.len)
str(Iter.len)
str(Iter.temp)

i <- 1


Iter.len[1,] <- Test.dat$Len0

head(Iter.temp)

Iter.comb <- cbind(Iter.len, Iter.temp)
obs.len <- growth$len_2-growth$len_1

MLE_mod <- function(data, par) {
  
  Iter.len <- data[,1:4]
  Iter.temp <- data[,5:8]
  ED_J_p_mg <- par[1]
  f_conver <- par[2]
  sigma <- par[3]
  
  pred_expose_30 <- rep(0, length.out = length(Iter.len[i,]))
  pred_recov <- rep(0, length.out = length(Iter.len[i,]))
  feedrate <- rep(0, length.out = length(Iter.len[i,]))
  aq_resp <- rep(0, length.out = length(Iter.len[i,]))
  for(i in 1:(length(datetime_seq)-1)){
    feedrate <- FR_T_fun(temp = Iter.temp[i,], size = Iter.len[i,])
    aq_resp <- AQR_T_fun(temp = Iter.temp[i,], size = Iter.len[i,])
    cost_15min <- aq_resp*15 #+ pred_expose_30 + pred_recov #total
    intake_15min <- feedrate*15 #total
    intake_scaled <- intake_15min/1000 
    del_J <- intake_scaled*f_conver*FoodLevel_ug_L - cost_15min # Change in terms of Joules ####
    #cost_of_growth <- ED_J_p_mg *1.4 #X1.4 for overhead costs
    del_mg <-  del_J / ED_J_p_mg # convert to mg AFDW
    new.mass <- .2 *Iter.len[i,]^2.8322 + del_mg #mm AFDW, approx, assuming a coefficient of 0.2
    new.len <- (new.mass/.2)^(1/2.8322)
    Iter.len[i+1,] <- new.len #add new length to the next column
  }  

  library(foreach)
  library(doSNOW)
  ?cl
  system.time(MLE_mod(Iter.comb, par))
  # Calculate simulated change in length / mass
  pred.len <- Iter.len[length(datetime_seq),] - Iter.len[1,]
  #pred.len <- c(t(pred.len))
  # Calculate observed change in length
  obs.len <- obs.len
  NLL <- -sum(dnorm(x = obs.len, mean = pred.len, sd = sigma, log = TRUE))
  return(NLL)
}

library(data.table)
Iter.comb <- data.table(Iter.comb)
str(Iter.comb)
data <- Iter.comb

sigma = .1
par_vec = c(ED_J_p_mg = 23.01,f_conver = 0.05, sigma = 0.3)
mle = optim(data = Iter.comb, par = par_vec, fn = MLE_mod, method = "L-BFGS-B", 
            lower=c(10,.01, .1),
            upper=c(30,2, .3))

# length(Iter.len[,1])
# length(datetime_seq)
# plot(datetime_seq, Iter.len[,1], pch = ".", 
#      ylab = "length (mm)",
#      ylim = c(0,5))
# points(datetime_seq, Iter.len[,2], pch = ".")
# points(datetime_seq, Iter.len[,3], pch = ".", col = "blue")
# points(datetime_seq, Iter.len[,4], pch = ".", col = "blue")
# points(rep(as.POSIXct(datetime_seq[1]), times = 4),Len0)
# n <- length(datetime_seq)
# points(rep(as.POSIXct(datetime_seq[n]), times = 4),growth$Final_operc)


# mle = optim(par = c(ED_J_p_mg = 23.01,f_conver = 2.75, sigma = 1), 
#             fn = MLE_mod, 
#             method = "L-BFGS-B", lower=c(2,0),upper=c(4,100),
#             data = Iter.comb)

head(Iter.comb) #This will be a good way to set up all the tidal elevations

#method = "L-BFGS-B", lower=c(2,0),upper=c(4,100)
       # ,
       # method = "L-BFGS-B", lower=c(2,0),upper=c(4,100))
      
      

#change.wt.15.min <- 
j <- 2
plot(Iter.temp$datetime, Iter.len[1,1:length(Iter.temp$datetime)], type = "l",
     ylab = "Length (mm)", xlab = "Date")
for(j in 2:nrow(Test.dat)){
  lines(Iter.temp$datetime, Iter.len[j,1:length(Iter.temp$datetime)], type = "l", col = j)
}

#change.wt.15.min <- 
j <- 2
plot(Iter.temp$datetime, Iter.mass[1,1:length(Iter.temp$datetime)], type = "l",
     ylab = "Mass (mg)", xlab = "Date")
for(j in 2:nrow(Test.dat)){
  lines(Iter.temp$datetime, Iter.mass[j,1:length(Iter.temp$datetime)], type = "l", col = j)
}

plot(Iter.mass[1,length(Iter.temp$datetime)] - Iter.mass[1,1:length(Iter.temp$datetime)])


plot(pred.6mo.len, obs.6mo.len)
abline(lm(obs.6mo.len ~pred.6mo.len))
summary(lm(obs.6mo.len ~pred.6mo.len)) #slope is significant, but not tight R fit, like ecology, energetics is messy. 
summary(lm(obs.6mo.len ~pred.6mo.len+Test.dat$Len0)) #slope is not significant, can be predicted by Length 0. 

plot(Test.dat$Len0, pred.6mo.len)


plot(pred.6mo.mass, obs.6mo.mass)
abline(lm(obs.6mo.mass ~pred.6mo.mass))
summary(lm(obs.6mo.mass ~pred.6mo.mass)) #slope is NS
summary(lm(obs.6mo.mass ~pred.6mo.mass+Test.dat$Len0)) #slope is NS



pred.6mo.len - obs.6mo.len
pred.6mo.mass - obs.6mo.mass