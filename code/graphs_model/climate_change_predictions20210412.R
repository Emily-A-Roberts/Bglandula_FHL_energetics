# This is to make the climate change graphs for the small barnacles model. 

# 1st run Model_20201208_iter_small_barnacles_food to set up functions. 
# Then run Iter_Model.... halfsat_fullmodel.R to find par
# I also have Graphs_for_iter_model... 
# for the graphs (2 different files right now - one is more organized and writes pdfs - the other is where I first wrote the code for these graphs and has additional graphs)
# This is for climate change predictions. 

# The issue is that I need to run my big model multiple times and get specific outputs. 
# You know, might be easiest to calc separately for time 1 and time 2 using mean barnacle length
Initial.length.mean <- mean(c(as.numeric(Iter.len.1[1,]),as.numeric(Iter.len.2[1,])),, na.rm = TRUE)*10 # 0.224 is the mean barnacle size. 

# it might be easier to just modify to use the workspace I already have

MLE_mod_3coeff_climate_change <- function(par, Iter.len.list, Tot.growth.mm.list, water.temp.list, food.list, air.temp.list, par_fixed, Elevation.list, FR_T_fun, AQR_T_fun, 
                           AER_EXPOSE_T_30_fcn, AER_RECOVER_T_30_fcn 
                           #aerial_cost_exposureANDrecovery, 
) {
  #Read in timeperiod 1 and do optimization
  timing <- 1
  water_temp_increment <- seq(0,2,by = 1)
  air_temp_increment <- seq(0,2,by = 1)
  
  if(timing == 1){
    air.water.U1 <- matrix(data = NA, nrow = length(water_temp_increment), ncol = length(air_temp_increment))
    air.water.M1 <- matrix(data = NA, nrow = length(water_temp_increment), ncol = length(air_temp_increment))
    air.water.L1 <- matrix(data = NA, nrow = length(water_temp_increment), ncol = length(air_temp_increment))
  }
  if(timing == 2){
    air.water.U2 <- matrix(data = NA, nrow = length(water_temp_increment), ncol = length(air_temp_increment))
    air.water.M2 <- matrix(data = NA, nrow = length(water_temp_increment), ncol = length(air_temp_increment))
    air.water.L2 <- matrix(data = NA, nrow = length(water_temp_increment), ncol = length(air_temp_increment))
  }
  
  air_i <- 1
  water_i <- 1
  for(air_i in 1:length(air_temp_increment)){
    for(water_i in 1:length(water_temp_increment)){
      inc_air <- air_temp_increment[air_i]
      inc_water <- water_temp_increment[water_i]
      
  

  
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
  
  Elevation <- c("Upper","Mid","Low")
  
  if(timing == 1){
    elev <- Elev.abbrev.list[[1]]
    #Elevation <- Elevation.list[[1]]
    Iter.len <- Iter.len.list[[1]][,elev] ###Update me list
    Iter.len[1,] <- rep(Initial.length.mean,times = 3)
    
    len_datetime_seq <- nrow(Iter.len.list[[1]])###Update me list
    Iter.water <- water.temp.list[[1]][,elev] + inc_water
    nrow(Iter.water)
    Iter.food <- food.list[[1]]
    length(Iter.food)
    Iter.air <- air.temp.list[[1]][,elev] + inc_air
    obs.growth.len <- Tot.growth.mm.list[[1]]
    
  }
  if(timing == 2){
    elev <- Elev.abbrev.list[[2]]
    #Elevation <- Elevation.list[[2]]
    Iter.len <- Iter.len.list[[2]][,elev] ###Update me list
    Iter.len[1,] <- rep(Initial.length.mean,times = 3)
    len_datetime_seq <- nrow(Iter.len.list[[2]])###Update me list
    Iter.water <- water.temp.list[[2]][,elev] + inc_water
    nrow(Iter.water)
    Iter.food <- food.list[[2]]
    length(Iter.food)
    Iter.air <- air.temp.list[[2]][,elev] + inc_air
    obs.growth.len <- Tot.growth.mm.list[[2]]
  }
  
  ED_J_p_mg <- 22.8 #
  par_new <- par[c(1,2)] # This is using the new par vector for b1, and a3
  sigma <- par[4]#[3]
  FR_T_fun <- FR_T_fun
  AQR_T_fun <- AQR_T_fun
  AER_EXPOSE_T_30_fcn <- AER_EXPOSE_T_30_fcn
  AER_RECOVER_T_30_fcn <- AER_RECOVER_T_30_fcn
  #aerial_cost_exposureANDrecovery <- aerial_cost_exposureANDrecovery
  
  if(timing == 1){
    time_exposed_per_day_6mo <- colSums(!is.na(air.temp.list[[1]]))
    time_submerged_per_day_6mo <- colSums(!is.na(water.temp.list[[1]])) #These usually add to 100% except on one or two days when loggers got exchanged
    perc_time_exposed_per_day_6mo <- time_exposed_per_day_6mo / (time_submerged_per_day_6mo + time_exposed_per_day_6mo)
    perc_time_exposed_per_day_6mo <- perc_time_exposed_per_day_6mo[elev]
  }
  if(timing == 2){
    time_exposed_per_day_6mo <- colSums(!is.na(air.temp.list[[2]]))
    time_submerged_per_day_6mo <- colSums(!is.na(water.temp.list[[2]])) #These usually add to 100% except on one or two days when loggers got exchanged
    perc_time_exposed_per_day_6mo <- time_exposed_per_day_6mo / (time_submerged_per_day_6mo + time_exposed_per_day_6mo)
    perc_time_exposed_per_day_6mo <- perc_time_exposed_per_day_6mo[elev]
  }

  #Need to update timing below here
  
  ncol_len <- 3 # Just want 3 outputs
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
  
  ncol.I <- 3 #ncol(Iter.len)
  
  
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
  
  Joules_from_cm <- function(length_input) { #Just calc's energy content at any length (no overhead cost)
    mass <- 10^(0.4864*(length_input)-0.7912)
    joule_mass <- mass * ED_J_p_mg
  }
  
  i_seq <- 1
  for(i_seq in 1:end){
    i <- ts*(i_seq-1)+1
    j <- i+ts-1
    f <- j+1

    len_i.dt <- as.data.table(apply(Iter.len[i,], 2, function(c) rep(c,ts)))
    temp_i.w.dt <- as.data.table(Iter.water[i:j,])
    temp_i.a.dt <- as.data.table(Iter.air[i:j,])
    food_i <- Iter.food[i:j]
    
    # plot(temp_i.a.dt$Upper)
    # points(temp_i.w.dt$Upper)

    #Modified as of 3/5/21: Time exposed per day
    perc_time_exposed_per_day <- perc_time_exposed_per_day_6mo
    y1 <- (1-perc_time_exposed_per_day) ^ -par_new[1]
    c_factor <- par_new[2] * y1 
    head(c_factor)

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
    # head(feedrate[i:j,130],20)
    # test <- feedrate[i:j,130]*c_factor[130]
    c_factor_mult <- feedrate[i:j,Map("*",.SD,c_factor)] #Multiplies feedrate by c_factor using map function for a data.table structure
    # https://stackoverflow.com/questions/53319000/multiply-columns-of-a-data-table-by-a-vector
    # added back intake.mult.samecoeff here - it had been missing ####
    intake_15min_scaled[i:j, ] <- c_factor_mult[,] *(15/1000)*intake.mult.samecoeff[i:j,]*f_conver #f_conver is from Lisa's data not to be confused with c_factor
    aq_resp[i:j,] <- AQR_T_fun(temp = temp_i.w.dt, size = len_i.dt, AQR_T_c, AQR_T_a)

    
    temp <- temp_i.a.dt
    size <- len_i.dt
    ramp.time <- (temp-10)/10 * 60 #10 degrees per hour
    time.at.temp <- (300 - ramp.time) # low tide minutes = 5 hours of exposure minus ramp time * 60 
    total_oxygen_debt <-AER_RECOVER_T_30_fcn(temp, size, c = AER_RECOVER_T_30_c, a = AER_RECOVER_T_30_a) - AER_RECOVER_T_30_fcn(10, size, c = AER_RECOVER_T_30_c, a = AER_RECOVER_T_30_a) #respiration at temp minus respiration at 10 degrees
    oxygen_debt_per15minExposure <- (15/time.at.temp) * total_oxygen_debt
    total_cost_per15minExposure <- oxygen_debt_per15minExposure + AER_EXPOSE_T_30_fcn(temp, size, c = AER_EXPOSE_T_30_c, a = AER_EXPOSE_T_30_a)
    pred_expose_30[i:j,] <- total_cost_per15minExposure
    cost_15min[i:j,] <- as.data.table(aq_resp[i:j,]*15) 
    
    f_unknown_to_zero(cost_15min)
    f_unknown_to_zero(intake_15min_scaled)
    f_unknown_to_zero(pred_expose_30)
    
    del_J[i:j,] <- intake_15min_scaled[i:j,] - cost_15min[i:j,] - pred_expose_30[i:j,] # Change in terms of Joules ####
    del_mg[i:j,] <-  del_J[i:j,] / (ED_J_p_mg *1.4) # convert to mg AFDW, energy density (Wu and Levings) times 1.4 (DEB) for overhead costs
    
    del_mg_i <- del_mg[i:j,]
    del_mg_sum[i,1:ncol.I]<-del_mg_i[ ,lapply(.SD, sum),]
    
    
    new.mass[i,] <- 10^(0.4864*(len_i.dt[1,])-0.7912) + del_mg_sum[i,] #mm AFDW, approx, assuming a coefficient of 0.2
    new.len[i,] <- 2.0559*log10(new.mass[i,])+1.6267
    Iter.len[f,] <- new.len[i,] #add new length to the next column
  }    
  if(timing == 1){
    Iter.len.day <- Iter.len[Iter.len$V4!=0,] # gets rid of all the 0's in between the timesteps, column names happen to differ between the two timepoints
  }
  if(timing == 2){
    Iter.len.day <- Iter.len[Iter.len$V36!=0,] # gets rid of all the 0's in between the timesteps, column names happen to differ between the two timepoints
  }
  (len.day <- nrow(Iter.len.day))
  
  SFG_sum <- del_J[ ,lapply(.SD, sum),]
  
  # Calculate simulated change in length / mass
  pred.len <- Iter.len.day[len.day,] - Iter.len.day[1,]
  pred.len <- c(t(pred.len))
  #pred.energycontent <- Joules_from_cm(Iter.len.day[len.day,]) - Joules_from_cm(Iter.len.day[1,])
  #pred.energycontent.1 <- pred.energycontent

  pred.len.1 <- pred.len #b/c just one day's predictions
  
  # par(mfrow = c(1,2))
  # plot(Iter.len.day[,3], type = "l", col = "orange")
  # lines(Iter.len.day[,2], lty = 2)
  # lines(Iter.len.day[,1], lty = 3)
  # plot(pred.len)
  
  
  # Calculate observed change in length
  # This hadn't been optimizing with the full dataset because I had only included low here. fixed -1/19/21
  #obs.len <- growth$len_2-growth$len_1
  # NLL_1L <- -sum(dnorm(x = obs.growth.len[Elevation=="Low"], mean = pred.len[Elevation=="Low"], sd = sigma, log = TRUE))
  # NLL_1M <- -sum(dnorm(x = obs.growth.len[Elevation=="Mid"], mean = pred.len[Elevation=="Mid"], sd = sigma, log = TRUE))
  # NLL_1U <- -sum(dnorm(x = obs.growth.len[Elevation=="Upper"], mean = pred.len[Elevation=="Upper"], sd = sigma, log = TRUE))
  # NLL_1 <- NLL_1L+NLL_1M+NLL_1U
  
Elevation
Elevation.list # In timepoint 1
elev[2]
# I used this for the sensitivity analysis:
SFG_sum <- del_J[ ,lapply(.SD, sum),]
  pred <- SFG_sum 
  as.numeric(pred[[elev[2]]]) #If just interested in mid SFG_sum
#I used the SD for the sensitivity analysis  
  mids <- Elevation=="Mid"
  mid_pred_value <- as.numeric(pred)[mids]
  (mids_mean <- mean(mid_pred_value))
  mids_sd <- sd(mid_pred_value)
  mids_n <-length(mid_pred_value)
  mids_sd / sqrt(mids_n -1)

  if(timing == 1){
    air.water.U1[water_i, air_i] <- as.numeric(pred[[1]])#wait... this is not calculating 1 per elevation. 
    air.water.M1[water_i, air_i] <- as.numeric(pred[[2]])
    air.water.L1[water_i, air_i] <- as.numeric(pred[[3]])
  }
  if(timing == 2){
    air.water.U2[water_i, air_i] <- as.numeric(pred[[1]])
    air.water.M2[water_i, air_i] <- as.numeric(pred[[2]])
    air.water.L2[water_i, air_i] <- as.numeric(pred[[3]])
  }
  
    }}
  
  # Joules_from_cm <- function(length_input) { #Differs from energy content with overhead cost
  #   mass <- 10^(0.4864*(length_input)-0.7912)
  #   joule_mass <- mass * ED_J_p_mg *1.4
  # }
  
  # air.water.resp.U1 <- (air.water.U1 - air.water.U1[1,1]) / air.water.U1[1,1]
  # air.water.resp.M1 <- (air.water.M1 - air.water.M1[1,1]) / air.water.M1[1,1]
  # air.water.resp.L1 <- (air.water.L1 - air.water.L1[1,1]) / air.water.L1[1,1]
  
  air.water.resp.U1 <- air.water.U1
  air.water.resp.M1 <- air.water.M1
  air.water.resp.L1 <- air.water.L1
  
  
  colnames(air.water.resp.U1) <- air_temp_increment
  rownames(air.water.resp.U1) <- water_temp_increment
  colnames(air.water.resp.M1) <- air_temp_increment
  rownames(air.water.resp.M1) <- water_temp_increment
  colnames(air.water.resp.L1) <- air_temp_increment
  rownames(air.water.resp.L1) <- water_temp_increment
  
  air.water.resp.U2 <- air.water.U2
  air.water.resp.M2 <- air.water.M2
  air.water.resp.L2 <- air.water.L2
  
  
  colnames(air.water.resp.U2) <- air_temp_increment
  rownames(air.water.resp.U2) <- water_temp_increment
  colnames(air.water.resp.M2) <- air_temp_increment
  rownames(air.water.resp.M2) <- water_temp_increment
  colnames(air.water.resp.L2) <- air_temp_increment
  rownames(air.water.resp.L2) <- water_temp_increment
  
  library("RColorBrewer")
  library("lattice")
  my.padding <- list(layout.heights = list(
    top.padding = 0,
    main.key.padding = 0,
    key.axis.padding = 0,
    axis.xlab.padding = 0,
    xlab.key.padding = 0,
    key.sub.padding = 0),
    layout.widths = list(
      left.padding = 0,
      key.ylab.padding = 0,
      ylab.axis.padding = 0,
      axis.key.padding = 0,
      right.padding = 0)
  )
  
  lattice.options(
    layout.heights=list(bottom.padding=list(x=0), top.padding=list(x=0)),
    layout.widths=list(left.padding=list(x=0), right.padding=list(x=0))
  )
  green_pink <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
  U1 <- levelplot(air.water.resp.U1, 
            xlab="", 
            ylab=expression(paste("Low tide temp (",Delta, degree,"C)")),
            at = c(-Inf, seq(0, 100, by = 10), Inf),
            col.regions = green_pink, colorkey=FALSE)
  M1 <- levelplot(air.water.resp.M1, 
                  xlab="", 
                  ylab=expression(paste("Low tide temp (",Delta, degree,"C)")),
                  at = c(-Inf, seq(0, 100, by = 10), Inf),
                  col.regions = green_pink, colorkey=FALSE)
  L1 <- levelplot(air.water.resp.L1, 
                  xlab=expression(paste("Seawater temp (",Delta, degree,"C)")),
                  ylab=expression(paste("Low tide temp (",Delta, degree,"C)")),
                  at = c(-Inf, seq(0, 100, by = 10), Inf),
                  col.regions = green_pink, colorkey=FALSE)
  
  #grid.arrange(U1,M1,L1)
  
  
  U2 <- levelplot(air.water.resp.U2, 
                  xlab="", 
                  ylab="",
                  at = c(-Inf, seq(0, 100, by = 10), Inf),
                  col.regions = green_pink, colorkey=FALSE)
  M2 <- levelplot(air.water.resp.M2, 
                  xlab="", 
                  ylab="",
                  at = c(-Inf, seq(0, 100, by = 10), Inf),
                  col.regions = green_pink, colorkey=FALSE)
  L2 <- levelplot(air.water.resp.L2, 
                  xlab=expression(paste("Seawater temp (",Delta, degree,"C)")),
                  ylab="",
                  at = c(-Inf, seq(0, 100, by = 10), Inf),
                  col.regions = green_pink, colorkey=FALSE)
  
  #grid.arrange(U2,M2,L2)  
  
  #par(oma = c(0,0,0,0), mar = c(0,0,0,0))
  grid.arrange(U1,U2,M1,M2,L1,L2, ncol = 2, nrow = 3)  
  
  grid.arrange(U1 + opts(legend.position="none"),
               U1 + opts(legend.position="none"),
               legend=legend,
               main ="this is a title",
               left = "This is my global Y-axis title")
  print(U1, split=c(1,1,2,3), more=TRUE)
  print(U2, split=c(2,1,2,3))
  
  
  

  # setwd("~/Box/Sarah and Molly's Box/FHL data/code/Desktop")
  # save.image(file = "my_work_space_time1_20210409.RData") # Run to save time 1 or 2 workspace

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
  ED_J_p_mg <- 22.8 #
  par_new <- par[c(1,2)] # This is using the new par vector for b1, and a3
  sigma <- par[4]#[3]
  FR_T_fun <- FR_T_fun
  AQR_T_fun <- AQR_T_fun
  AER_EXPOSE_T_30_fcn <- AER_EXPOSE_T_30_fcn
  AER_RECOVER_T_30_fcn <- AER_RECOVER_T_30_fcn

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
  
  
  i_seq <- 1
  for(i_seq in 1:end){
    i <- ts*(i_seq-1)+1
    j <- i+ts-1
    f <- j+1
    
    len_i.dt <- as.data.table(apply(Iter.len[i,], 2, function(c) rep(c,ts)))
    temp_i.w.dt <- as.data.table(Iter.water[i:j,])
    temp_i.a.dt <- as.data.table(Iter.air[i:j,])
    
    food_i <- Iter.food[i:j]

          #Modified as of 3/5/21: Time exposed per day
    perc_time_exposed_per_day <- perc_time_exposed_per_day_6mo
    y1 <- (1 - perc_time_exposed_per_day) ^ -par_new[1] #To make this fit with Lisa's data use submergence time, then this is just 1
    c_factor <- par_new[2] * y1 #* y2

    feedrate[i:j,] <- FR_T_fun(temp = temp_i.w.dt, size = len_i.dt, FR_T_a, FR_T_c)
    head(feedrate[i:j,130],20)
    test <- feedrate[i:j,130]*c_factor[130]
    head(test)
    
    c_factor_mult <- feedrate[i:j,Map("*",.SD,c_factor)] #Multiplies feedrate by c_factor using map function for a data.table structure
    # https://stackoverflow.com/questions/53319000/multiply-columns-of-a-data-table-by-a-vector
    # added back intake.mult.samecoeff here - it had been missing ####
    #intake.mult is specified before the forloop
    intake_15min_scaled[i:j, ] <- c_factor_mult[,] *(15/1000)*intake.mult.samecoeff[i:j,]*f_conver #f_conver is from Lisa's data not to be confused with c_factor

    aq_resp[i:j,] <- AQR_T_fun(temp = temp_i.w.dt, size = len_i.dt, AQR_T_c, AQR_T_a)
 
    temp <- temp_i.a.dt
    size <- len_i.dt
    ramp.time <- (temp-10)/10 * 60 #10 degrees per hour
    time.at.temp <- (300 - ramp.time) # low tide minutes = 5 hours of exposure minus ramp time * 60 
    total_oxygen_debt <-AER_RECOVER_T_30_fcn(temp, size, c = AER_RECOVER_T_30_c, a = AER_RECOVER_T_30_a) - AER_RECOVER_T_30_fcn(10, size, c = AER_RECOVER_T_30_c, a = AER_RECOVER_T_30_a) #respiration at temp minus respiration at 10 degrees
    oxygen_debt_per15minExposure <- (15/time.at.temp) * total_oxygen_debt
    total_cost_per15minExposure <- oxygen_debt_per15minExposure + AER_EXPOSE_T_30_fcn(temp, size, c = AER_EXPOSE_T_30_c, a = AER_EXPOSE_T_30_a)
    pred_expose_30[i:j,] <- total_cost_per15minExposure
    cost_15min[i:j,] <- as.data.table(aq_resp[i:j,]*15) #+ pred_expose_30 + pred_recov #total

    
    f_unknown_to_zero(cost_15min)
    f_unknown_to_zero(intake_15min_scaled)
    f_unknown_to_zero(pred_expose_30)

    del_J[i:j,] <- intake_15min_scaled[i:j,] - cost_15min[i:j,] - pred_expose_30[i:j,] # Change in terms of Joules ####
    del_mg[i:j,] <-  del_J[i:j,] / (ED_J_p_mg *1.4) # convert to mg AFDW, energy density (Wu and Levings) times 1.4 (DEB) for overhead costs
    
    del_mg_i <- del_mg[i:j,]
    del_mg_sum[i,1:ncol.I]<-del_mg_i[ ,lapply(.SD, sum),]
    
    new.mass[i,] <- 10^(0.4864*(len_i.dt[1,])-0.7912) + del_mg_sum[i,] #mm AFDW, approx, assuming a coefficient of 0.2
    new.len[i,] <- 2.0559*log10(new.mass[i,])+1.6267
    Iter.len[f,] <- new.len[i,] #add new length to the next column

  }    
  
  Iter.len.day <- Iter.len[Iter.len$V1!=0,]
  (len.day <- nrow(Iter.len.day))
  
  # Calculate simulated change in length / mass
  pred.len <- Iter.len.day[len.day,] - Iter.len.day[1,]
  pred.len <- c(t(pred.len))
  pred.len.2 <- pred.len #b/c just one day's predictions
  
  # Calculate observed change in length
  # This hadn't been optimizing with the full dataset because I had only included low here. fixed -1/19/21
  #obs.len <- growth$len_2-growth$len_1
  NLL_2L <- -sum(dnorm(x = obs.growth.len[Elevation=="Low"], mean = pred.len[Elevation=="Low"], sd = sigma, log = TRUE))
  NLL_2M <- -sum(dnorm(x = obs.growth.len[Elevation=="Mid"], mean = pred.len[Elevation=="Mid"], sd = sigma, log = TRUE))
  NLL_2U <- -sum(dnorm(x = obs.growth.len[Elevation=="Upper"], mean = pred.len[Elevation=="Upper"], sd = sigma, log = TRUE))
  NLL_2 <- NLL_2L+NLL_2M+NLL_2U
  
  # Calculate full NLL ####
  NLL<- NLL_1 + NLL_2
  
  return(list(pred.len.1, pred.len.2))
}

pred.len.list <- MLE_mod_3coeff_climate_change(par = par, 
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
                     AER_RECOVER_T_30_fcn = AER_RECOVER_T_30_fcn) 
                     #aerial_cost_exposureANDrecovery = aerial_cost_exposureANDrecovery)

baseline_length <- pred.len.list

#oh no - this only needs to be for a mean size barnacle, not for all barnacles. 
# 

air_i <- 1
water_i <- 1
for(air_i in 1:length(air_temp_increment)){
  for(water_i in 1:length(water_temp_increment)){
    for(air_i in 1:1)){
      for(water_i in 1:1)){   
        inc_air <- air_temp_increment[air_i]
        inc_water <- water_temp_increment[water_i]
        
        
        pred.len.list <- MLE_mod_3coeff_climate_change(par = par, 
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
                                                       AER_RECOVER_T_30_fcn = AER_RECOVER_T_30_fcn) 
        

}