# Feeding and respiration graph ####

setwd("~/Box/Sarah and Molly's Box/FHL data/aqua feeding and resp")
setwd("C:/Users/eroberts/Box/Sarah and Molly's Box/FHL data/aqua feeding and resp")

pred.dat.aqua_onesize <- data.frame(
  Temp.n = rep(seq(from = 7, to = 16, length.out = 16)),
  OperculumLength = rep(c(5.5),each = 16),
  sizeclass = as.factor(rep(c("medium"),each = 16))
)

pred.dat.aer_onesize <- data.frame(
  Temp.n = rep(seq(from = 10, to = 38, length.out = 16)),
  Operc = rep(c(5.5),each = 16),
  sizeclass = as.factor(rep(c("medium"),each = 16))
)

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

pred.dat.aqua_onesize$feedrate <- FR_T_fun(temp = pred.dat.aqua_onesize$Temp.n, size = pred.dat.aqua_onesize$OperculumLength)

pred.dat.aqua_onesize$prey_per_size <- pred.dat.aqua_onesize$feedrate / (pred.dat.aqua_onesize$OperculumLength^1.88)
feed$prey_per_size <- feed$feedrate.new / feed$OperculumLength^1.88


ggplot(data = pred.dat.aqua_onesize, 
       aes(x = as.numeric(Temp.n), 
           y=as.numeric(prey_per_size*60), 
           color = OperculumLength, group = as.factor(sizeclass)))+
  geom_line()+
  scale_color_viridis(option = "D", limits = c(4, 7))+
  geom_point(data = feed,aes(x=Temp.n,y=prey_per_size*60,color=OperculumLength))+
  xlim(5,40)+
  ylim(0,.2)+
  xlab(expression(paste("Seawater temp (",degree,"C)")))+
  ylab("Size-corrected feeding rate\n (Prey per hr X size^-1.88)")


# aerial_cost_exposureANDrecovery <- function(temp, size, AER_RECOVER_T_30_a = AER_RECOVER_T_30_a, AER_RECOVER_T_30_c = AER_RECOVER_T_30_c, AER_RECOVER_T_30_fcn = AER_RECOVER_T_30_fcn, 
#                                             AER_EXPOSE_T_30_a = AER_EXPOSE_T_30_a, AER_EXPOSE_T_30_c = AER_EXPOSE_T_30_c, AER_EXPOSE_T_30_fcn = AER_EXPOSE_T_30_fcn) {
#   ramp.time <- (temp-10)/10 * 60 #10 degrees per hour
#   time.at.temp <- (300 - ramp.time) # low tide minutes = 5 hours of exposure minus ramp time * 60 
#   total_oxygen_debt <-AER_RECOVER_T_30_fcn(temp, size) - AER_RECOVER_T_30_fcn(10, size) #respiration at temp minus respiration at 10 degrees
#   oxygen_debt_per15minExposure <- (15/time.at.temp) * total_oxygen_debt
#   total_cost_per15minExposure <- oxygen_debt_per15minExposure + AER_EXPOSE_T_30_fcn(temp, size)
#   return(total_cost_per15minExposure)
# }

# pred.dat.aer_onesize$pred_30 <- aerial_cost_exposureANDrecovery(temp = pred.dat.aer_onesize$Temp.n, 
#                                                                 size = pred.dat.aer_onesize$Operc)
temp <- pred.dat.aer_onesize$Temp.n
size <- pred.dat.aer_onesize$Operc
ramp.time <- (temp-10)/10 * 60 #10 degrees per hour
time.at.temp <- (300 - ramp.time) # low tide minutes = 5 hours of exposure minus ramp time * 60 
total_oxygen_debt <-AER_RECOVER_T_30_fcn(temp, size) - AER_RECOVER_T_30_fcn(10, size) #respiration at temp minus respiration at 10 degrees
oxygen_debt_per15minExposure <- (15/time.at.temp) * total_oxygen_debt
pred.dat.aer_onesize$total_cost_per15minExposureRecov <- oxygen_debt_per15minExposure + AER_EXPOSE_T_30_fcn(temp, size)
pred.dat.aer_onesize$total_cost_per15minExposureOnly <- AER_EXPOSE_T_30_fcn(temp, size)
str(pred.dat.aer_onesize$Operc)
ggplot(resp_exp,aes(x=Temp.n,y=Respiration.Rate*4/(Operc^2.32),color = Operc, group=factor(sizeclass)))+
  geom_point()+
  ylab("Size-corrected aerial respiration \n(Xsize^2.32 X umol per hour)") +
  xlab("Temp (deg C)")+
  scale_color_viridis(option = "D", limits = c(4, 7))+
  geom_line(data = pred.dat.aer_onesize, aes(x = as.numeric(Temp.n), 
                                             y=as.numeric(total_cost_per15minExposureRecov*4)/(Operc^2.32), color = as.numeric(Operc)), linetype = "solid")+
  geom_line(data = pred.dat.aer_onesize, aes(x = as.numeric(Temp.n), 
                                 y=as.numeric(total_cost_per15minExposureOnly*4)/(Operc^2.32), color = as.numeric(Operc)), linetype = "dashed")

#Combine
ggplot(resp_exp,aes(x=Temp.n,y=Respiration.Rate*4/(Operc^2.32)*5,color = Operc, group=factor(sizeclass)))+
  geom_point()+
  ylab("Size-corrected aerial respiration \n(Xsize^2.32 X umol per hour)") +
  xlab("Temp (deg C)")+
  scale_color_viridis(option = "D", limits = c(4, 7))+
  geom_line(data = pred.dat.aer_onesize, aes(x = as.numeric(Temp.n), 
                                             y=as.numeric(total_cost_per15minExposureRecov*4)/(Operc^2.32)*5, color = as.numeric(Operc)), linetype = "solid")+
  geom_line(data = pred.dat.aer_onesize, aes(x = as.numeric(Temp.n), 
                                             y=as.numeric(total_cost_per15minExposureOnly*4)/(Operc^2.32)*5, color = as.numeric(Operc)), linetype = "dashed")+
  geom_line(data = pred.dat.aqua_onesize, 
            aes(x = as.numeric(Temp.n), 
                y=as.numeric(prey_per_size*60), 
                color = OperculumLength, group = as.factor(sizeclass)))+
  geom_point(data = feed,aes(x=Temp.n,y=prey_per_size*60,color=OperculumLength))
head(resp_exp_above30)
str(resp_exp_above30)
#Combine
resp_exp_above30 <- resp_exp[resp_exp$Temp.n>30,]
ggplot(resp_exp,aes(x=Temp.n,y=Respiration.Rate*4/(Operc^2.32)*5))+
  geom_point()+
  geom_point(data = resp_exp_above30,aes(x=Temp.n,y=Respiration.Rate*4/(Operc^2.32)*5), color = "white", size = .9)+
  #ylab("Size-corrected aerial respiration \n(Xsize^2.32 X umol per hour)") +
  xlab("Temperature (°C)")+
  scale_color_viridis(option = "D", limits = c(4, 7))+
  geom_line(data = pred.dat.aer_onesize, aes(x = as.numeric(Temp.n), 
                                             y=as.numeric(total_cost_per15minExposureRecov*4)/(Operc^2.32)*5), linetype = "solid")+
  geom_line(data = pred.dat.aer_onesize, aes(x = as.numeric(Temp.n), 
                                             y=as.numeric(total_cost_per15minExposureOnly*4)/(Operc^2.32)*5),linetype = "dashed")+
  geom_line(data = pred.dat.aqua_onesize, 
            aes(x = as.numeric(Temp.n), 
                y=as.numeric(prey_per_size*60), 
                group = as.factor(sizeclass)), color = "green")+
  geom_point(data = feed,aes(x=Temp.n,y=prey_per_size*60),color="lightgreen")+
  scale_y_continuous(
    
    # Features of the first axis
    name = "Size-corrected aerial respiration \n(Xsize^-2.32 X umol per hour)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*1/5, name="Size-corrected aerial respiration \n(Xsize^2.32 X umol per hour)")
  ) 

# setwd("~/Box/Sarah and Molly's Box/FHL data/plots/Context")            
# save.image("expt.data.plot.workspace_20210510.RData")
