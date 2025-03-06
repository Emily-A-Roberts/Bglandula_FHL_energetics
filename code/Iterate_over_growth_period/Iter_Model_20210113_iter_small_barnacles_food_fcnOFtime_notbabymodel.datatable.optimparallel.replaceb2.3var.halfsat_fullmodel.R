# 1st run Model_20201208_iter_small_barnacles_food to set up functions. 

# This halfsat model is the one of choice!!! -3/11/2021

# 2/5/2021 - removed i:j in rows section of the 2ns c_factor mult

# See time food plots for initial par estimate  with fewer params

# Full version using all *small barnacle samples* from all tidal elevations
# 1. assign temp column for each length row based on elevation 

# Inputting food as separate vector to avoid all the splicing and dicing of dataframes 1/9/21

# 1/21/21 - put all graphs into different 

# Note functional response equations are in mm not cm. 

# Work in terms of data.tables for faster iterations

# 2/24/21 - remove collinear term, b2

# 2/24/21 - replace collinear term in new file with terms specific to site

# 3/5/21 - this version has 3 variables - not too many (4) where the 
# exponent is estimated twice and not too few (2) both the coefficient and exponent are measured twice

# 3/5/21 - Pushing forward:
# - We are estimating growth daily and then monthly when just checking the optimization is working
# - Switching to time submerged rather than time exposed because this value is just a multiplier of 1 for Lisa's data. This also means there is a neg sign in front of the exponent
# - We are finding food and putting it back in!

# If using the old format of finding an 'f' for each situation:
# par_1_from_desktop <- c(.614,.772,1.634,.503)
# par_2_from_desktop <- c(1.492,2.062,5.026, 3.7022)
# par <- par_1_from_desktop

# Now with just the three parameters, plus sigma
# par <- c(b1 = 2.95, b2 = 2.95,  a3 = 10, a4 = 10, sigma = .5) 
# par <- c(b1 = 2.95, b2 = 2.95,  a3 = 200, a4 = 200, sigma = 4) 
# par <- c(b1 = 1.5,  a3 = .18, a4 = .18, sigma = 4) 
#par <- c(b1 = 1.1609430, a3 = 2.0412612, halfsat = 0.1206389, sigma = 0.4664785) #3/11/2021
#par <- c(b1 = 1.16428, a3 = 2.044664, halfsat = 0.1261, sigma = 0.46627) #3/11/2021
#par <- c(b1 = 1.1642804 , a3 = 2.0446641 , halfsat = 0.1261037, sigma = 0.4662876) #4/8/2021, Full model, still missing AUg 22-25
par <- c(b1 = 1.1663914 , a3 = 2.0440979  , halfsat = 0.1334580, sigma = 0.4663794) #4/8/2021, Full model


mle
# Assuming that food is about .5 - 2
# Assuming that time is about .35 - .75
#par <- par_new



#
library(gdata)
library(data.table)
options(digits.secs = 2)

climate_air_change <- 0
climate_water_change <- 0
UML <- "U"

par

head(Iter.len.1)
head(Iter.len.2)
head(water.temp.1)
head(water.temp.2)
head(air.temp.1)
head(air.temp.2)

timing <- 3


if(timing == 3){
  growth <- growth.1
  n <- nrow(growth.1)
  Len0 <- growth.1$Feb.Length*10 #now in mm
  LenF <- growth.1$Aug.Length*10
  Tot.growth.mm <- growth.1$Aug.Length*10 - growth.1$Feb.Length*10 #growth.1$growth.Aug.Feb #now in mm
  Elevation <- as.character(growth.1$Elevation)
  plot(Len0, Tot.growth.mm)
  
  food<-water.temp.1$food
  Iter.len = Iter.len.1*10 #Now in mm
  # water.temp = cbind(water.temp.1$Low,water.temp.1$Mid,water.temp.1$Upper, water.temp.1$food)
  # air.temp = cbind(air.temp.1$Low, air.temp.1$Mid, air.temp.1$Upper)
  datetimes = water.temp.1$datetime
  #water temp expanded
  water.temp.exp <- as.data.frame(matrix(0, ncol = ncol(Iter.len), nrow = nrow(Iter.len)))
  for(col in 1:ncol(Iter.len)){
    if(Elevation[col]=="Upper"){
      water.temp.exp[1:nrow(Iter.len),col] <- c(water.temp.1$Upper)
    } else if(Elevation[col]=="Mid"){
      water.temp.exp[1:nrow(Iter.len),col] <- c(water.temp.1$Mid)
    } else if(Elevation[col]=="Low"){
      water.temp.exp[1:nrow(Iter.len),col] <- c(water.temp.1$Low)}
  }
  names(water.temp.exp) <- growth.1$Elevation
  #water.temp.exp$food <- water.temp.1$food
  water.temp.exp.1 <-water.temp.exp 
  
  air.temp.exp <- as.data.frame(matrix(0, ncol = ncol(Iter.len), nrow = nrow(Iter.len)))
  for(col in 1:ncol(Iter.len)){
    if(Elevation[col]=="Upper"){
      air.temp.exp[1:nrow(Iter.len),col] <- c(air.temp.1$Upper)
    } else if(Elevation[col]=="Mid"){
      air.temp.exp[1:nrow(Iter.len),col] <- c(air.temp.1$Mid)
    } else if(Elevation[col]=="Low"){
      air.temp.exp[1:nrow(Iter.len),col] <- c(air.temp.1$Low)}
  }
  names(air.temp.exp) <- growth.1$Elevation
  air.temp.exp.1 <-air.temp.exp 
  
}
if(timing == 3){
  growth <- growth.2
  n <- nrow(growth.2)
  Len0 <- growth.2$Aug.Length*10#now in mm
  LenF <- growth.2$Mar.Length*10
  Tot.growth.mm <- growth.2$Mar.Length*10 - growth.2$Aug.Length*10#now in mm
  Elevation <- as.character(growth.2$Elevation)
  plot(Len0, Tot.growth.mm)
  
  food<-water.temp.2$food
  Iter.len = Iter.len.2*10
  # water.temp = cbind(water.temp.2$Low,water.temp.2$Mid,water.temp.2$Upper, water.temp.2$food)
  # air.temp = cbind(air.temp.2$Low, air.temp.2$Mid, air.temp.2$Upper)
  datetimes = water.temp.2$datetime
  
  
  #water temp expanded
  water.temp.exp <- as.data.frame(matrix(NA, ncol = ncol(Iter.len), nrow = nrow(Iter.len)))
  dim(water.temp.exp)
  col <- 130
  for(col in 1:ncol(Iter.len)){
    if(Elevation[col]=="Upper"){
      water.temp.exp[1:nrow(Iter.len),col] <- c(water.temp.2$Upper)
    } else if(Elevation[col]=="Mid"){
      water.temp.exp[1:nrow(Iter.len),col] <- c(water.temp.2$Mid)
    } else if(Elevation[col]=="Low"){
      water.temp.exp[1:nrow(Iter.len),col] <- c(water.temp.2$Low)}
  }
  names(water.temp.exp) <- growth.2$Elevation
  #water.temp.exp$food <- water.temp.2$food #Keep food separate for now for simplicity
  water.temp.exp.2 <-water.temp.exp 
  # head(water.temp)
  # water.temp <- water.temp
  
  
  air.temp.exp <- as.data.frame(matrix(NA, ncol = ncol(Iter.len), nrow = nrow(Iter.len)))
  for(col in 1:ncol(Iter.len)){
    if(Elevation[col]=="Upper"){
      air.temp.exp[1:nrow(Iter.len),col] <- c(air.temp.2$Upper)
    } else if(Elevation[col]=="Mid"){
      air.temp.exp[1:nrow(Iter.len),col] <- c(air.temp.2$Mid)
    } else if(Elevation[col]=="Low"){
      air.temp.exp[1:nrow(Iter.len),col] <- c(air.temp.2$Low)
    }
  }
  names(air.temp.exp) <- growth.2$Elevation
  air.temp.exp.2 <-air.temp.exp 
  # head(air.temp)
  # air.temp.2 <- air.temp
}
if(timing == 3){ # 3 means together
  growth.list <- list(growth.1,growth.2)
  n.list <- list(nrow(growth.1),nrow(growth.2))
  Len0.list <- list(growth.1$Feb.Length*10,growth.2$Aug.Length*10)
  LenF.list <- list(growth.1$Aug.Length*10,growth.2$Mar.Length*10)
  Tot.growth.mm.list <- list(growth.1$Aug.Length*10 - growth.1$Feb.Length*10, growth.2$Mar.Length*10 - growth.2$Aug.Length*10)
  Elevation.list <- list(as.character(growth.1$Elevation),as.character(growth.2$Elevation))
  
  food.list <- list(water.temp.1$food, water.temp.2$food)
  water.temp.list <- list(water.temp.exp.1,water.temp.exp.2)
  air.temp.list <- list(air.temp.exp.1,air.temp.exp.2)
  Iter.len.list <- list(Iter.len.1*10, Iter.len.2*10)
  
  datetimes <- list(water.temp.1$datetime,water.temp.2$datetime)
  
}

# Water temp and air temp must be made to match the number of samples and what elevations they were at.
# I'm sure there is a better way to do this, but it would involve making the df's long and then matching. 
# Right now it is easier to do by hand. 




#par_fixed <- NA

par_fixed = c(n_dat1 = ncol(Iter.len.list[[1]]),
              n_dat2 = ncol(Iter.len.list[[2]]),
              f_conver = 84.131, # For Lisa's data, this was the food scalar at 6.9ug / L 
              FR_T_a = FR_T_a, 
              FR_T_c = FR_T_c, 
              AQR_T_a = AQR_T_a,
              AQR_T_c = AQR_T_c,
              AER_EXPOSE_T_30_a = AER_EXPOSE_T_30_a,
              AER_EXPOSE_T_30_c = AER_EXPOSE_T_30_c,
              AER_RECOVER_T_30_a = AER_RECOVER_T_30_a,
              AER_RECOVER_T_30_c = AER_RECOVER_T_30_c)





MLE_mod_3coeff <- function(par, Iter.len.list, Tot.growth.mm.list, water.temp.list, food.list, air.temp.list, par_fixed, Elevation.list, FR_T_fun, AQR_T_fun, 
                           AER_EXPOSE_T_30_fcn, AER_RECOVER_T_30_fcn 
                           #aerial_cost_exposureANDrecovery, 
                           ) {
  #Read in timeperiod 1 and do optimization
  halfsat <- par[3] #new parameter, replacing second a, "a4"
  
  ncol_len <- as.numeric(par_fixed[1]) #Par fixed 2 is the number of columns in the second time point
  f_conver <- par_fixed[3]
  FR_T_a <- par_fixed[4] 
  FR_T_c <- par_fixed[5] 
  AQR_T_a <- par_fixed[6]
  AQR_T_c <- par_fixed[7]
  AER_EXPOSE_T_30_a <- par_fixed[8]
  AER_EXPOSE_T_30_c <- par_fixed[9]
  AER_RECOVER_T_30_a <- par_fixed[10]
  AER_RECOVER_T_30_c <- par_fixed[11]
  Elevation <- Elevation.list[[1]]
  Iter.len <- Iter.len.list[[1]] ###Update me list
  len_datetime_seq <- nrow(Iter.len.list[[1]])###Update me list
  Iter.water <- water.temp.list[[1]]
  nrow(Iter.water)
  Iter.food <- food.list[[1]]
  length(Iter.food)
  Iter.air <- air.temp.list[[1]]
  obs.growth.len <- Tot.growth.mm.list[[1]]
  #Iter.len[2,] <- rep(0, length.out = ncol_len)
  ED_J_p_mg <- 22.8 #####Changed this in sensitivity analysis
  par_new <- par[c(1,2)] # This is using the new par vector for b1, and a3
  sigma <- par[4]#[3]
  FR_T_fun <- FR_T_fun
  AQR_T_fun <- AQR_T_fun
  AER_EXPOSE_T_30_fcn <- AER_EXPOSE_T_30_fcn
  AER_RECOVER_T_30_fcn <- AER_RECOVER_T_30_fcn
  #aerial_cost_exposureANDrecovery <- aerial_cost_exposureANDrecovery
  
  time_exposed_per_day_6mo <- colSums(!is.na(air.temp.list[[1]]))
  time_submerged_per_day_6mo <- colSums(!is.na(water.temp.list[[1]])) #These usually add to 100% except on one or two days when loggers got exchanged
  perc_time_exposed_per_day_6mo <- time_exposed_per_day_6mo / (time_submerged_per_day_6mo + time_exposed_per_day_6mo)
  
  pred_expose_30 <- as.data.table(matrix(data = 0, nrow = len_datetime_seq-1, ncol = as.numeric(ncol_len)))
  pred_recov <- as.data.table(matrix(data = 0, nrow = len_datetime_seq-1, ncol = ncol_len))
  feedrate <- as.data.table(matrix(data = 0, nrow = len_datetime_seq-1, ncol = ncol_len))
  aq_resp <- as.data.table(matrix(data = 0, nrow = len_datetime_seq-1, ncol = ncol_len))
  
  cost_15min <- as.data.table(matrix(data = 0, nrow = len_datetime_seq-1, ncol = ncol_len))
  intake_15min_scaled <- as.data.table(matrix(data = 0, nrow = len_datetime_seq-1, ncol = ncol_len))
  del_J <- as.data.table(matrix(data = 0, nrow = len_datetime_seq-1, ncol = ncol_len))
  del_mg <- as.data.table(matrix(data = 0, nrow = len_datetime_seq-1, ncol = ncol_len))
  del_mg_sum <- as.data.table(matrix(data = 0, nrow = len_datetime_seq-1, ncol = ncol_len))
  new.mass <- as.data.table(matrix(data = 0, nrow = len_datetime_seq-1, ncol = ncol_len))
  new.len <- as.data.table(matrix(data = 0, nrow = len_datetime_seq-1, ncol = ncol_len))
  
  ncol.I <- ncol(Iter.len)
  
  
  intake.mult.samecoeff <- as.data.frame(matrix(NA, ncol = ncol(Iter.len), nrow = nrow(Iter.len)))
  for(col in 1:ncol(Iter.len)){
    if(Elevation[col]=="Upper"){
      intake.mult.samecoeff[1:nrow(Iter.len),col] <- Iter.food
    } else if(Elevation[col]=="Mid"){
      intake.mult.samecoeff[1:nrow(Iter.len),col] <- Iter.food
    } else if(Elevation[col]=="Low"){
      intake.mult.samecoeff[1:nrow(Iter.len),col] <- Iter.food
    }
  }
  intake.mult.samecoeff <- as.data.table(intake.mult.samecoeff)
  intake.mult.samecoeff.halfsat <- intake.mult.samecoeff/(intake.mult.samecoeff+halfsat) #incorporate half saturation here
  intake.mult.samecoeff <- intake.mult.samecoeff.halfsat
  length(Iter.food)
  

  # There are 96 15 minute intervals in a day. 
  # 3/5/21 *30 to do a dry run of the model (monthly)
  ts<-96
  end <- as.integer(floor(len_datetime_seq/ts)-1)
  
  #head(temp_i.a)
  #https://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table

  f_unknown_to_zero = function(DT) {
    for (colnum in seq_len(ncol(DT)))
      set(DT,which(is.na(DT[[colnum]])),colnum,0)
  }
  
  i_seq <- 1
#  start_time <-Sys.time()
  for(i_seq in 1:end){
    i <- ts*(i_seq-1)+1
    j <- i+ts-1
    f <- j+1
    
    
    
#    time0 <-Sys.time()
    #len_i.df <- as.data.frame(apply(Iter.len[i,], 2, function(c) rep(c,ts)))
    len_i.dt <- as.data.table(apply(Iter.len[i,], 2, function(c) rep(c,ts)))
    #temp_i.w.df <- as.data.frame(Iter.water[i:j,])
    temp_i.w.dt <- as.data.table(Iter.water[i:j,])
    #temp_i.a <- as.data.table(Iter.air[i:j,])
    #temp_i.a.df <- as.data.frame(Iter.air[i:j,])
    temp_i.a.dt <- as.data.table(Iter.air[i:j,])
    
    food_i <- Iter.food[i:j]
#   time1 <-Sys.time()
    
    
    
    #New as of 1/27/21: Time exposed per day
    # time_exposed_per_day <- colSums(!is.na(Iter.air[i:j,]))
    # time_submerged_per_day <- colSums(!is.na(Iter.water[i:j,])) #These usually add to 100% except on one or two days when loggers got exchanged
    # perc_time_exposed_per_day <- time_exposed_per_day / (time_submerged_per_day + time_exposed_per_day)
    
    #Modified as of 3/5/21: Time exposed per day
    perc_time_exposed_per_day <- perc_time_exposed_per_day_6mo
    y1 <- (1-perc_time_exposed_per_day) ^ -par_new[1]
    
    #New as of 1/27/21: Food value - this was collinear
    #y2 <- colMeans(intake.mult.samecoeff[i:j]) ^ -par_new[2]
    
    c_factor <- par_new[2] * y1 #* y2
    head(c_factor)
    # oh no - some data points are less than 0. 

    # Feeding activity graph 3 by 3####
    graphing <- 0
    if(graphing == 1){
    pdf("Feeding activity.pdf", width=4, height=4)
    par(mfrow = c(1,1))
    #plot(c_factor[elev])
      sub_time <- 1-perc_time_exposed_per_day[elev]
    #plot(y1[elev]~(sub_time))
    #plot(c_factor[elev]~(sub_time))
    plot(y1[elev]~(sub_time), xlim = c(0,1),
         #pch = c(0,1,2),
         pch = c(15,16,17),
         bty = "n", 
         #col = c("darkgrey","purple","orange"),
         ylim = c(1,6), ylab = "Feeding activity (unitless)", xlab = "Proportion of time submerged \n(unitless)")
    x <- seq(.1,1,.001) # time emerged
    y_graph <- par_new[2]*(x) ^ -par_new[1]
    y_graph <- (x) ^ -par_new[1]
    lines(y_graph~(x), ylab = "Feeding activity \n (unitless)", xlab = "Time submerged")
    y_graph_upper <- (x) ^ -(par_new[1]+.04)
    y_graph_lower <- (x) ^ -(par_new[1]-.04)
    lines(y_graph_upper~(x), lty = 1, col = "darkgrey", ylab = "Feeding activity \n (unitless)", xlab = "Time submerged")
    lines(y_graph_lower~(x), lty = 1, col = "darkgrey", ylab = "Feeding activity \n (unitless)", xlab = "Time submerged")
    points(y1[elev]~(sub_time), xlim = c(0,1),
         #pch = c(0,1,2),
         pch = c(15,16,17),
         bty = "n", 
         #col = c("darkgrey","purple","orange"),
         ylim = c(1,6), ylab = "Feeding activity (unitless)", xlab = "Proportion of time submerged \n(unitless)")
    dev.off()
    }

    feedrate[i:j,] <- FR_T_fun(temp = temp_i.w.dt, size = len_i.dt, FR_T_a, FR_T_c)
    head(feedrate[i:j,130],20)
    test <- feedrate[i:j,130]*c_factor[130]
    head(test)
    # feedrate.dt <- FR_T_fun(temp = temp_i.w.dt, size = len_i.dt, FR_T_a, FR_T_c)
    # feedrate.df <- FR_T_fun(temp = temp_i.w.df, size = len_i.df, FR_T_a, FR_T_c)
    # par(mfrow = c(1,1))
    # plot(
    #   feedrate.df[,100] -
    #      as.data.frame(feedrate.dt)[,(100)])
    
    #feedrate[i:j,] %>% summarise_all(mean, na.rm=TRUE)
    
    c_factor_mult <- feedrate[i:j,Map("*",.SD,c_factor)] #Multiplies feedrate by c_factor using map function for a data.table structure
    # https://stackoverflow.com/questions/53319000/multiply-columns-of-a-data-table-by-a-vector
    head(c_factor_mult[,130]) #Is the same as above, for barnacle 130
    
    # newvar <- quote(1)
    # for(newvar in 1:ncol(feedrate)){
    #   c_factor_mult_2[,newvar] <- feedrate[, eval(newvar)]*c_factor[newvar]
    # }
    # head(c_factor_mult[,1])
    
    # added back intake.mult.samecoeff here - it had been missing ####
    
    intake_15min_scaled[i:j, ] <- c_factor_mult[,] *(15/1000)*intake.mult.samecoeff[i:j,]*f_conver #f_conver is from Lisa's data not to be confused with c_factor
    # str(intake.mult.samecoeff)
    # str(c_factor_mult)
    # (test <- c_factor_mult[,130] *(15/1000)*f_conver*intake.mult.samecoeff[i:j,130])
    
    
    

    
    
    
    
    
    aq_resp[i:j,] <- AQR_T_fun(temp = temp_i.w.dt, size = len_i.dt, AQR_T_c, AQR_T_a)
    # aq_resp.df <- AQR_T_fun(temp = temp_i.w.df, size = len_i.df, AQR_T_c, AQR_T_a)
    # aq_resp.dt <- AQR_T_fun(temp = temp_i.w.dt, size = len_i.dt, AQR_T_c, AQR_T_a)
    
      # aq_resp.df[,1] -
      #    as.data.frame(aq_resp.dt)[,(1)] #Same
    
    
    # Calling a function within a function here, so extracting the functions out and just calling it line by line
    # Oh, the issue is that the global variables (a and c) are not called explicitly by AER Recover and AER Expose. That's OK. Maybe best to keep it written out here.
    # pred_expose_30[i:j,] <- as.data.table(aerial_cost_exposureANDrecovery(temp = temp_i.a.dt, size = len_i.dt,
    #                                                         AER_RECOVER_T_30_a, AER_RECOVER_T_30_c,
    #                                                         AER_RECOVER_T_30_fcn, AER_EXPOSE_T_30_a,
    #                                                         AER_EXPOSE_T_30_c, AER_EXPOSE_T_30_fcn)) # This is already scaled to a 15 min value.
        temp <- temp_i.a.dt
        size <- len_i.dt
        ramp.time <- (temp-10)/10 * 60 #10 degrees per hour
        time.at.temp <- (300 - ramp.time) # low tide minutes = 5 hours of exposure minus ramp time * 60 
        total_oxygen_debt <-AER_RECOVER_T_30_fcn(temp, size, c = AER_RECOVER_T_30_c, a = AER_RECOVER_T_30_a) - AER_RECOVER_T_30_fcn(10, size, c = AER_RECOVER_T_30_c, a = AER_RECOVER_T_30_a) #respiration at temp minus respiration at 10 degrees
        oxygen_debt_per15minExposure <- (15/time.at.temp) * total_oxygen_debt
        total_cost_per15minExposure <- oxygen_debt_per15minExposure + AER_EXPOSE_T_30_fcn(temp, size, c = AER_EXPOSE_T_30_c, a = AER_EXPOSE_T_30_a)
        pred_expose_30[i:j,] <- total_cost_per15minExposure
    
      #  pred_expose_30.dt <- as.data.table(aerial_cost_exposureANDrecovery(temp = temp_i.a.df, size = len_i.df, 
  #                                                                         AER_RECOVER_T_30_a, AER_RECOVER_T_30_c, 
  #                                                                         AER_RECOVER_T_30_fcn, AER_EXPOSE_T_30_a, 
  #                                                                         AER_EXPOSE_T_30_c, AER_EXPOSE_T_30_fcn)) # This is already scaled to a 15 min value.
  #   pred_expose_30.df <- as.data.table(aerial_cost_exposureANDrecovery(temp = temp_i.a.df, size = len_i.df, 
  #                                                                         AER_RECOVER_T_30_a, AER_RECOVER_T_30_c, 
  #                                                                         AER_RECOVER_T_30_fcn, AER_EXPOSE_T_30_a, 
  #                                                                         AER_EXPOSE_T_30_c, AER_EXPOSE_T_30_fcn)) # This is already scaled to a 15 min value.

    #pred_expose_30.df[,100] - as.data.frame(pred_expose_30.dt)[,(100)] #Same
    
    cost_15min[i:j,] <- as.data.table(aq_resp[i:j,]*15) #+ pred_expose_30 + pred_recov #total
    #intake.mult is specified before the forloop
    
   #    time2 <-Sys.time()
    
    f_unknown_to_zero(cost_15min)
    f_unknown_to_zero(intake_15min_scaled)
    f_unknown_to_zero(pred_expose_30)
    
 #   time3 <-Sys.time()
    del_J[i:j,] <- intake_15min_scaled[i:j,] - cost_15min[i:j,] - pred_expose_30[i:j,] # Change in terms of Joules ####
    del_mg[i:j,] <-  del_J[i:j,] / (ED_J_p_mg *1.4) # convert to mg AFDW, energy density (Wu and Levings) times 1.4 (DEB) for overhead costs

    #del_mg_sum[i,1:ncol.I] <- colSums(del_mg[i:j,])
    del_mg_i <- del_mg[i:j,]
    del_mg_sum[i,1:ncol.I]<-del_mg_i[ ,lapply(.SD, sum),]
    
    new.mass[i,] <- 10^(0.4864*(len_i.dt[1,])-0.7912) + del_mg_sum[i,] #mm AFDW, approx, assuming a coefficient of 0.2
    new.len[i,] <- 2.0559*log10(new.mass[i,])+1.6267
    Iter.len[f,] <- new.len[i,] #add new length to the next column
#    time4 <-Sys.time()
  }    

  Iter.len.day <- Iter.len[Iter.len$V1!=0,]
  (len.day <- nrow(Iter.len.day))
  
  # Calculate simulated change in length / mass
  pred.len <- Iter.len.day[len.day,] - Iter.len.day[1,]
  pred.len <- c(t(pred.len))
  
  pred.len <- pred.len #b/c just one day's predictions
  
 # plot(pred.len~obs.growth.len, col = as.factor(Elevation))

  
  # Calculate observed change in length
  # This hadn't been optimizing with the full dataset because I had only included low here. fixed -1/19/21
  #obs.len <- growth$len_2-growth$len_1
  NLL_1L <- -sum(dnorm(x = obs.growth.len[Elevation=="Low"], mean = pred.len[Elevation=="Low"], sd = sigma, log = TRUE))
  NLL_1M <- -sum(dnorm(x = obs.growth.len[Elevation=="Mid"], mean = pred.len[Elevation=="Mid"], sd = sigma, log = TRUE))
  NLL_1U <- -sum(dnorm(x = obs.growth.len[Elevation=="Upper"], mean = pred.len[Elevation=="Upper"], sd = sigma, log = TRUE))
  NLL_1 <- NLL_1L+NLL_1M+NLL_1U
  
# 
# setwd("~/Box/Sarah and Molly's Box/FHL data/code/Desktop")
# save.image(file = "my_work_space_time1_20210409.RData") # Run to save time 1 or 2 workspace
# version on 20210408 -excludes the 12th through the 14th of august and no missing data in mid august
# accidentally saved different version 20210410
 
# Now calculate for timepoint 2 ####
  
  #Read in timeperiod 1 and do optimization
  ncol_len <- as.numeric(par_fixed[2]) #Par fixed 2 is the number of columns in the second time point
  f_conver <- par_fixed[3]
  FR_T_a <- par_fixed[4] 
  FR_T_c <- par_fixed[5] 
  AQR_T_a <- par_fixed[6]
  AQR_T_c <- par_fixed[7]
  AER_EXPOSE_T_30_a <- par_fixed[8]
  AER_EXPOSE_T_30_c <- par_fixed[9]
  AER_RECOVER_T_30_a <- par_fixed[10]
  AER_RECOVER_T_30_c <- par_fixed[11]
  Elevation <- Elevation.list[[2]]
  Iter.len <- Iter.len.list[[2]] ###Update me list
  len_datetime_seq <- nrow(Iter.len.list[[2]])###Update me list
  Iter.water <- water.temp.list[[2]]
  Iter.food <- food.list[[2]]
  Iter.air <- air.temp.list[[2]]
  obs.growth.len <- Tot.growth.mm.list[[2]]
  #Iter.len[2,] <- rep(0, length.out = ncol_len)
  ED_J_p_mg <- 22.8 #
  par_new <- par[c(1,2)] # This is using the new par vector for b1, and a3
  sigma <- par[4]#[3]
  FR_T_fun <- FR_T_fun
  AQR_T_fun <- AQR_T_fun
  AER_EXPOSE_T_30_fcn <- AER_EXPOSE_T_30_fcn
  AER_RECOVER_T_30_fcn <- AER_RECOVER_T_30_fcn
  #aerial_cost_exposureANDrecovery <- aerial_cost_exposureANDrecovery
  
  time_exposed_per_day_6mo <- colSums(!is.na(air.temp.list[[2]]))
  time_submerged_per_day_6mo <- colSums(!is.na(water.temp.list[[2]])) #These usually add to 100% except on one or two days when loggers got exchanged
  perc_time_exposed_per_day_6mo <- time_exposed_per_day_6mo / (time_submerged_per_day_6mo + time_exposed_per_day_6mo)
  
  
  pred_expose_30 <- as.data.table(matrix(data = 0, nrow = len_datetime_seq-1, ncol = as.numeric(ncol_len)))
  pred_recov <- as.data.table(matrix(data = 0, nrow = len_datetime_seq-1, ncol = ncol_len))
  feedrate <- as.data.table(matrix(data = 0, nrow = len_datetime_seq-1, ncol = ncol_len))
  aq_resp <- as.data.table(matrix(data = 0, nrow = len_datetime_seq-1, ncol = ncol_len))
  
  cost_15min <- as.data.table(matrix(data = 0, nrow = len_datetime_seq-1, ncol = ncol_len))
  intake_15min_scaled <- as.data.table(matrix(data = 0, nrow = len_datetime_seq-1, ncol = ncol_len))
  del_J <- as.data.table(matrix(data = 0, nrow = len_datetime_seq-1, ncol = ncol_len))
  del_mg <- as.data.table(matrix(data = 0, nrow = len_datetime_seq-1, ncol = ncol_len))
  del_mg_sum <- as.data.table(matrix(data = 0, nrow = len_datetime_seq-1, ncol = ncol_len))
  new.mass <- as.data.table(matrix(data = 0, nrow = len_datetime_seq-1, ncol = ncol_len))
  new.len <- as.data.table(matrix(data = 0, nrow = len_datetime_seq-1, ncol = ncol_len))
  
  ncol.I <- ncol(Iter.len)
  
 
  intake.mult.samecoeff <- as.data.frame(matrix(NA, ncol = ncol(Iter.len), nrow = nrow(Iter.len)))
  for(col in 1:ncol(Iter.len)){
    if(Elevation[col]=="Upper"){
      intake.mult.samecoeff[1:nrow(Iter.len),col] <- Iter.food
    } else if(Elevation[col]=="Mid"){
      intake.mult.samecoeff[1:nrow(Iter.len),col] <- Iter.food
    } else if(Elevation[col]=="Low"){
      intake.mult.samecoeff[1:nrow(Iter.len),col] <- Iter.food
    }
  }
  intake.mult.samecoeff <- as.data.table(intake.mult.samecoeff)
  intake.mult.samecoeff.halfsat <- intake.mult.samecoeff/(intake.mult.samecoeff+halfsat) #incorporate half saturation here
  intake.mult.samecoeff <- intake.mult.samecoeff.halfsat
  length(Iter.food)
  
  # par(mfrow = c(1,1))
  # plot(food*f_U, type = "l")
  # lines(food*f_M)
  # lines(food*f_L) 
  
  
  # Removed, because should be the same number of intervals as above. 
  # There are 96 15 minute intervals in a day. 
  # ts<-96
  # end <- as.integer(floor(len_datetime_seq/ts)-1) 
  
  #head(temp_i.a)
  #https://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table
  # This is repeated so commented out
  # f_unknown_to_zero = function(DT) {
  #   for (colnum in seq_len(ncol(DT)))
  #     set(DT,which(is.na(DT[[colnum]])),colnum,0)
  # }
  
  i_seq <- 1
  #end <- 10
  #  start_time <-Sys.time()
  for(i_seq in 1:end){
    i <- ts*(i_seq-1)+1
    j <- i+ts-1
    f <- j+1
    
    #    time0 <-Sys.time()
    #len_i.df <- as.data.frame(apply(Iter.len[i,], 2, function(c) rep(c,ts)))
    len_i.dt <- as.data.table(apply(Iter.len[i,], 2, function(c) rep(c,ts)))
    #temp_i.w.df <- as.data.frame(Iter.water[i:j,])
    temp_i.w.dt <- as.data.table(Iter.water[i:j,])
    #temp_i.a <- as.data.table(Iter.air[i:j,])
    #temp_i.a.df <- as.data.frame(Iter.air[i:j,])
    temp_i.a.dt <- as.data.table(Iter.air[i:j,])
    
    food_i <- Iter.food[i:j]
    #   time1 <-Sys.time()
    
    
    
    #New as of 1/27/21: Time exposed
    # time_exposed_per_day <- colSums(!is.na(Iter.air[i:j,]))
    # time_submerged_per_day <- colSums(!is.na(Iter.water[i:j,])) #These usually add to 100% except on one or two days when loggers got exchanged
    # perc_time_exposed_per_day <- time_exposed_per_day / (time_submerged_per_day + time_exposed_per_day)
    # y1 <- perc_time_exposed_per_day ^ par_new[1]
    
    #Modified as of 3/5/21: Time exposed per day
    perc_time_exposed_per_day <- perc_time_exposed_per_day_6mo
    y1 <- (1 - perc_time_exposed_per_day) ^ -par_new[1] #To make this fit with Lisa's data use submergence time, then this is just 1
    
    #New as of 1/27/21: Food value #This was collinear
    #y2 <- colMeans(intake.mult.samecoeff[i:j]) ^ -par_new[2]
    
    c_factor <- par_new[2] * y1 #* y2
    
    
    
    feedrate[i:j,] <- FR_T_fun(temp = temp_i.w.dt, size = len_i.dt, FR_T_a, FR_T_c)
    head(feedrate[i:j,130],20)
    test <- feedrate[i:j,130]*c_factor[130]
    head(test)
    # feedrate.dt <- FR_T_fun(temp = temp_i.w.dt, size = len_i.dt, FR_T_a, FR_T_c)
    # feedrate.df <- FR_T_fun(temp = temp_i.w.df, size = len_i.df, FR_T_a, FR_T_c)
    # par(mfrow = c(1,1))
    # plot(
    #   feedrate.df[,100] -
    #      as.data.frame(feedrate.dt)[,(100)])
    
    #feedrate[i:j,] %>% summarise_all(mean, na.rm=TRUE)
    
    c_factor_mult <- feedrate[i:j,Map("*",.SD,c_factor)] #Multiplies feedrate by c_factor using map function for a data.table structure
    # https://stackoverflow.com/questions/53319000/multiply-columns-of-a-data-table-by-a-vector
    head(c_factor_mult[,130]) #Is the same as above, for barnacle 130
    
    # newvar <- quote(1)
    # for(newvar in 1:ncol(feedrate)){
    #   c_factor_mult_2[,newvar] <- feedrate[, eval(newvar)]*c_factor[newvar]
    # }
    # head(c_factor_mult[,1])
    
    # added back intake.mult.samecoeff here - it had been missing ####
    
    #intake.mult is specified before the forloop
    intake_15min_scaled[i:j, ] <- c_factor_mult[,] *(15/1000)*intake.mult.samecoeff[i:j,]*f_conver #f_conver is from Lisa's data not to be confused with c_factor
    # str(intake.mult.samecoeff)
    # str(c_factor_mult)
    # (test <- c_factor_mult[,130] *(15/1000)*f_conver*intake.mult.samecoeff[i:j,130])
    
    
    
    #feedrate[i:j,] %>% summarise_all(mean, na.rm=TRUE)
    aq_resp[i:j,] <- AQR_T_fun(temp = temp_i.w.dt, size = len_i.dt, AQR_T_c, AQR_T_a)
    # aq_resp.df <- AQR_T_fun(temp = temp_i.w.df, size = len_i.df, AQR_T_c, AQR_T_a)
    # aq_resp.dt <- AQR_T_fun(temp = temp_i.w.dt, size = len_i.dt, AQR_T_c, AQR_T_a)
    
    # aq_resp.df[,1] -
    #    as.data.frame(aq_resp.dt)[,(1)] #Same
    
    
    # Calling a function within a function here, so extracting the functions out and just calling it line by line
    # Oh, the issue is that the global variables (a and c) are not called explicitly by AER Recover and AER Expose. That's OK. Maybe best to keep it written out here.
    # pred_expose_30[i:j,] <- as.data.table(aerial_cost_exposureANDrecovery(temp = temp_i.a.dt, size = len_i.dt,
    #                                                         AER_RECOVER_T_30_a, AER_RECOVER_T_30_c,
    #                                                         AER_RECOVER_T_30_fcn, AER_EXPOSE_T_30_a,
    #                                                         AER_EXPOSE_T_30_c, AER_EXPOSE_T_30_fcn)) # This is already scaled to a 15 min value.
    temp <- temp_i.a.dt
    size <- len_i.dt
    ramp.time <- (temp-10)/10 * 60 #10 degrees per hour
    time.at.temp <- (300 - ramp.time) # low tide minutes = 5 hours of exposure minus ramp time * 60 
    total_oxygen_debt <-AER_RECOVER_T_30_fcn(temp, size, c = AER_RECOVER_T_30_c, a = AER_RECOVER_T_30_a) - AER_RECOVER_T_30_fcn(10, size, c = AER_RECOVER_T_30_c, a = AER_RECOVER_T_30_a) #respiration at temp minus respiration at 10 degrees
    oxygen_debt_per15minExposure <- (15/time.at.temp) * total_oxygen_debt
    total_cost_per15minExposure <- oxygen_debt_per15minExposure + AER_EXPOSE_T_30_fcn(temp, size, c = AER_EXPOSE_T_30_c, a = AER_EXPOSE_T_30_a)
    pred_expose_30[i:j,] <- total_cost_per15minExposure
    
    #  pred_expose_30.dt <- as.data.table(aerial_cost_exposureANDrecovery(temp = temp_i.a.df, size = len_i.df, 
    #                                                                         AER_RECOVER_T_30_a, AER_RECOVER_T_30_c, 
    #                                                                         AER_RECOVER_T_30_fcn, AER_EXPOSE_T_30_a, 
    #                                                                         AER_EXPOSE_T_30_c, AER_EXPOSE_T_30_fcn)) # This is already scaled to a 15 min value.
    #   pred_expose_30.df <- as.data.table(aerial_cost_exposureANDrecovery(temp = temp_i.a.df, size = len_i.df, 
    #                                                                         AER_RECOVER_T_30_a, AER_RECOVER_T_30_c, 
    #                                                                         AER_RECOVER_T_30_fcn, AER_EXPOSE_T_30_a, 
    #                                                                         AER_EXPOSE_T_30_c, AER_EXPOSE_T_30_fcn)) # This is already scaled to a 15 min value.
    
    #pred_expose_30.df[,100] - as.data.frame(pred_expose_30.dt)[,(100)] #Same
    
    cost_15min[i:j,] <- as.data.table(aq_resp[i:j,]*15) #+ pred_expose_30 + pred_recov #total
    

    #    time2 <-Sys.time()
    
    f_unknown_to_zero(cost_15min)
    f_unknown_to_zero(intake_15min_scaled)
    f_unknown_to_zero(pred_expose_30)
    
    #   time3 <-Sys.time()
    del_J[i:j,] <- intake_15min_scaled[i:j,] - cost_15min[i:j,] - pred_expose_30[i:j,] # Change in terms of Joules ####
    del_mg[i:j,] <-  del_J[i:j,] / (ED_J_p_mg *1.4) # convert to mg AFDW, energy density (Wu and Levings) times 1.4 (DEB) for overhead costs
    
    #del_mg_sum[i,1:ncol.I] <- colSums(del_mg[i:j,])
    del_mg_i <- del_mg[i:j,]
    del_mg_sum[i,1:ncol.I]<-del_mg_i[ ,lapply(.SD, sum),]
    
    new.mass[i,] <- 10^(0.4864*(len_i.dt[1,])-0.7912) + del_mg_sum[i,] #mm AFDW, approx, assuming a coefficient of 0.2
    new.len[i,] <- 2.0559*log10(new.mass[i,])+1.6267
    Iter.len[f,] <- new.len[i,] #add new length to the next column
    #    time4 <-Sys.time()
  }    
  
  Iter.len.day <- Iter.len[Iter.len$V1!=0,]
  (len.day <- nrow(Iter.len.day))
  
  # Calculate simulated change in length / mass
  pred.len <- Iter.len.day[len.day,] - Iter.len.day[1,]
  pred.len <- c(t(pred.len))
  
  pred.len <- pred.len #b/c just one day's predictions
  
  # Calculate observed change in length
  # This hadn't been optimizing with the full dataset because I had only included low here. fixed -1/19/21
  #obs.len <- growth$len_2-growth$len_1
  NLL_2L <- -sum(dnorm(x = obs.growth.len[Elevation=="Low"], mean = pred.len[Elevation=="Low"], sd = sigma, log = TRUE))
  NLL_2M <- -sum(dnorm(x = obs.growth.len[Elevation=="Mid"], mean = pred.len[Elevation=="Mid"], sd = sigma, log = TRUE))
  NLL_2U <- -sum(dnorm(x = obs.growth.len[Elevation=="Upper"], mean = pred.len[Elevation=="Upper"], sd = sigma, log = TRUE))
  NLL_2 <- NLL_2L+NLL_2M+NLL_2U
  
# Calculate full NLL ####
  NLL<- NLL_1 + NLL_2
  
    return(NLL)
}

  # setwd("~/Box/Sarah and Molly's Box/FHL data/code/Desktop")
  # save.image(file = "my_work_space_time2_20210409.RData") # Run to save time 1 or 2 workspace

#save.image(file = "Environment_4_desktop20210409_new_AUG13_temp_split.RData")



library(optimParallel)
cl <- makeCluster(2)
library(data.table)
setDefaultCluster(cl=cl)
parallel=list(loginfo=TRUE)
clusterEvalQ(cl,search())
clusterEvalQ(cl, library("data.table"))
clusterEvalQ(cl, library("dplyr"))
clusterEvalQ(cl, library("gdata"))
clusterEvalQ(cl, library("nlme"))
clusterEvalQ(cl, library("lubridate"))
clusterEvalQ(cl, library("effects"))
clusterEvalQ(cl, library("Matrix"))
clusterEvalQ(cl, library("reshape2"))
clusterEvalQ(cl, library("tidyr"))
clusterEvalQ(cl, library("minpack.lm"))
clusterEvalQ(cl, library("lme4"))
clusterEvalQ(cl, library("OneR"))
clusterEvalQ(cl, library("stats4"))
clusterEvalQ(cl, library("stats"))


# I extracted 
start_time = Sys.time()
mle <- optimParallel(c(b1 = 1.5,  a3 = .18, a4 = 5, sigma = .5), 
                     Iter.len.list = Iter.len.list, 
                     Tot.growth.mm.list =  Tot.growth.mm.list,
                     water.temp.list = water.temp.list, 
                     air.temp.list = air.temp.list,
                     food.list = food.list,
                     par_fixed = par_fixed,  
                     Elevation.list = Elevation.list,
                     FR_T_fun = FR_T_fun,
                     AQR_T_fun = AQR_T_fun, 
                     AER_EXPOSE_T_30_fcn = AER_EXPOSE_T_30_fcn,
                     AER_RECOVER_T_30_fcn = AER_RECOVER_T_30_fcn, 
                     #aerial_cost_exposureANDrecovery = aerial_cost_exposureANDrecovery,
                     fn = MLE_mod_3coeff, 
                     method = "L-BFGS-B", 
                     lower=c(.1,.01, .01, .01),
                     upper=c(5,5, 20, 5))
end_time = Sys.time()

setwd()
save(par_new, 
     Iter.len.list, 
     Tot.growth.mm.list,
     water.temp.list,
     air.temp.list,
     food.list,
     par_fixed,
     Elevation.list,
     FR_T_fun,
     AQR_T_fun,
     AER_EXPOSE_T_30_fcn,
     AER_RECOVER_T_30_fcn, 
     MLE_mod_3coeff, 
     file = "optim_workspace_MLE_mod_3coeff.RData")




# Regular optim, before using optimParallel
# start_time <- Sys.time()
# mle_separate <- optim(par = par, 
#                       Iter.len = Iter.len, 
#                       Tot.growth.mm =  Tot.growth.mm,
#                       water.temp = water.temp, 
#                       air.temp = air.temp,
#                       food = food,
#                       par_fixed = par_fixed,                     
#                       FR_T_fun = FR_T_fun,
#                       AQR_T_fun = AQR_T_fun, 
#                       AER_EXPOSE_T_30_fcn = AER_EXPOSE_T_30_fcn,
#                       AER_RECOVER_T_30_fcn = AER_RECOVER_T_30_fcn, 
#                       aerial_cost_exposureANDrecovery = aerial_cost_exposureANDrecovery,
#                       fn = MLE_mod_3coeff, 
#                       method = "L-BFGS-B", 
#                       lower=c(.001,.001,.001, .00001),
#                       upper=c(50,50,50, 100))
# end_time <- Sys.time()
# system("say I have finished running your script!")


if(timing == 1){
  mle_1_separate <- mle_separate
}
if(timing == 2){
  mle_2_separate <- mle_separate
}



# Results
# Single 
#Time 1
# par <- c(f_H = 3, sigma = 0.288) #Initial run
# par <- c(f_H = .001, sigma = 0.06005534) #Output for f_H is just at the lowest value. Output 12/13/20
# par <- c(f_H = 2.7513634, sigma = 0.1847629) #f_H is now a scalar rather than a saturation coefficient. Output 12/14/20
# par <- c(f_H = 4.795836, sigma = 1.284960) #f_H is now a scalar rather than a saturation coefficient. Corrected cm to mm length input. Output 12/14/20
# par <- c(f_H = 5.0396173, sigma = 0.9241603) #f_H is now a scalar rather than a saturation coefficient. 
#    # Corrected cm to mm length input. Corrected water and air temp to match. Output 12/14/20
par <- c(f_H = 0.9010861, sigma = 0.8581707) #f_H is now a scalar rather than a saturation coefficient. # This now includes food 


#Time 2
#par <- c(f_H = 4.2559695, sigma = 0.7673048) #f_H is now a scalar rather than a saturation coefficient. 
par <- c(f_H = 2.3686521, sigma = 0.9888733) #f_H is now a scalar rather than a saturation coefficient. # This now includes food

#Interval 1, separate feeding coeff
par <- c(f_L = 2.3686521, f_M = 2.3686521, f_U = 2.3686521, sigma = 0.9888733) #f_H is now a scalar rather than a saturation coefficient. # This now includes food




par <- mle_2_separate$par
par1 <- c(f_L = 0.6186989208, f_M = 0.7827267459, f_U = 1.6478691026, sigma = 0.0009889493) #Interval 1
par2 <- c(f_L = 1.5493149704, f_M = 2.1414766345, f_U = 5.2183304893, sigma = 0.0009782029) #Interval 2
par <- par1

# With the three separate f's. Sigma is very small because there is only one sample for each. 
# mle1
# $par
# [1] 0.6186989208 0.7827267459 1.6478691026 0.0009889493
# 
# $value
# [1] -17.98425
# 
# $counts
# function gradient 
# 135      135 
# 
# $convergence
# [1] 52
# 
# $message
# [1] "ERROR: ABNORMAL_TERMINATION_IN_LNSRCH"

# > mle_2_separate
# $par
# f_L          f_M          f_U        sigma 
# 1.5493149704 2.1414766345 5.2183304893 0.0009782029 
# 
# $value
# [1] -17.83272
# 
# $counts
# function gradient 
# 116      116 
# 
# $convergence
# [1] 52
# 
# $message
# [1] "ERROR: ABNORMAL_TERMINATION_IN_LNSRCH"




# Can evaluate whethr time or temperature affects this? 
colSum(!is.na[Iter.air])
# average air temperature

# Duration of feeding




mle <- optimParallel(par = par_new, 
                     Iter.len = Iter.len, 
                     Tot.growth.mm =  Tot.growth.mm,
                     water.temp = water.temp, 
                     air.temp = air.temp,
                     par_fixed = par_fixed,                     
                     FR_T_fun = FR_T_fun,
                     AQR_T_fun = AQR_T_fun, 
                     AER_EXPOSE_T_30_fcn = AER_EXPOSE_T_30_fcn,
                     AER_RECOVER_T_30_fcn = AER_RECOVER_T_30_fcn, 
                     aerial_cost_exposureANDrecovery = aerial_cost_exposureANDrecovery,
                     fn = MLE_mod, 
                     method = "L-BFGS-B", 
                     lower=c(.001, .00001),
                     upper=c(50, 100))
system("say I have finished running your script!")



end <- as.integer(floor(len_datetime_seq/96)-1)




plot(summarize(pred_expose_30))
plot(mean(intake_15min_scaled))




timing




Test.dat <- data.frame(
  Elevation = Elevation,
  Time = as.factor(rep(1, length.out = n)),
  ID = seq(from = 1, to = n, by =1),
  Len0, 
  Tot.growth.mm
)

Test.dat$code <- paste(Test.dat$Elevation, Test.dat$Time, Test.dat$ID)
str(Test.dat)

# Experimental relationships between length and mass are here:
upper_len_mass
mid_len_mass
# AIC(m1 = upper_len_mass[s1$df], m2 = upper_len_mass[s2$df], m3 = upper_len_mass[s3$df])
# AIC(m1 = upper_len_mass[s1$df], m2 = upper_len_mass[s2$df], m3 = upper_len_mass[s3$df])

# But instead we will use one relationship between length and mass
# Gilman 2013 2.8322 with SE 0.2631
# Palmer 1980 2.8

# starting values ####
start <- Test.dat
head(pred.dat.air)
temp.dat <- full_join(pred.dat.air, pred.dat.aqua, by = "datetime")
head(temp.dat)


Iter.temp <- data.frame(
  datetime = temp.dat$datetime,
  air_temp = temp.dat$Temp.n.x,
  aq_temp = temp.dat$Temp.n.y,
  len = rep(NA, length.out = nrow(temp.dat)),
  mass = rep(NA, length.out = nrow(temp.dat))
)

str(Iter.temp)

Iter.temp <- Iter.temp[order(Iter.temp$datetime),]

Iter.len <- matrix(data = NA, nrow = nrow(Test.dat), ncol = nrow(temp.dat))

i <- 1
Iter.len[,1] <- Test.dat$Len0
for(i in 1:length(Iter.temp$datetime)){
  pred_expose_30 <- rep(0, length.out = length(Iter.len[,i]))
  pred_recov <- rep(0, length.out = length(Iter.len[,i]))
  feedrate <- rep(0, length.out = length(Iter.len[,i]))
  aq_resp <- rep(0, length.out = length(Iter.len[,i]))
  if(!is.na(Iter.temp$air_temp[i])){ #aerial
    pred_expose_30 <- AER_EXPOSE_T_30_fcn(Iter.temp$air_temp[i], Iter.len[,i])
    pred_recov <- aerial_cost_recovery(Iter.temp$air_temp[i], Iter.len[,i], "upslope30")
  }else if(!is.na(Iter.temp$aq_temp[i])){ #aquatic
    feedrate <- FR_T_fun(temp = Iter.temp$aq_temp[i], size = Iter.len[,i])
    aq_resp <- AQR_T_fun(temp = Iter.temp$aq_temp[i], size = Iter.len[,i])
  }
  cost_15min <- aq_resp*15 + pred_expose_30 + pred_recov #total
  intake_15min <- feedrate*15 #total
  intake_scaled <- intake_15min*0.000207
  f_conver <- f_conver # f_conver is a starting point for this parameter that is defined in the previous code
  del <- intake_scaled*f_conver - cost_15min 
  # convert the change in mass to a change in length
  new.mass <- .2 *Iter.len[,i]^2.8322 + del #mm AFDW, approx, assuming a coefficient of 0.2
  new.len <- (new.mass/.2)^(1/2.8322)
  Iter.len[,i+1] <- new.len
}  

Iter.mass <- .2 *Iter.len^2.8322

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

# Calculate simulated change in length / mass
pred.6mo.len <- Iter.len[,length(Iter.temp$datetime)] - Iter.len[,1]
pred.6mo.mass <- ((Iter.len[,length(Iter.temp$datetime)] - Iter.len[,1])/.2)^(1/2.8322)

# Calculate observed change in length
obs.6mo.len <- Test.dat$Tot.growth.mm
obs.6mo.mass <- .2 * Test.dat$Tot.growth.mm^2.8322 #Right - there are some that had negative growth

plot(pred.6mo.len, obs.6mo.len)
plot(pred.6mo.mass, obs.6mo.mass)

pred.6mo.len - obs.6mo.len
pred.6mo.mass - obs.6mo.mass



