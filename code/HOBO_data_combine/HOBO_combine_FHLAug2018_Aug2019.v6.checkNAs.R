# Hobo temperature data
# To do: Use offset function to get everything on same 15 minute intervals and joint together (see fcn)
# Combine data together
# Combine *694 SN (mid) data in, now that we have found it... 

# 3/5/20 - removed logger (low, bad data) that fell off because likely fell off before 2018 Feb expt period started

# 8/6/20 - adding in NOAA underwater data. Check time zone. 

# 9/15/20 - fixed issue with mid level data getting cut off

#-------------
# can plot earlier and later data - need to combine them together... 

rm(list = ls())

library(ggplot2)
library(xts)
library(reshape)
library(lubridate)
library(reshape2)
library(ggplot2)
library(Hmisc)
require("PerformanceAnalytics")



round15min <- function(timect_start, n){
  cd <- ceiling_date(timect_start, "15 min")
  test_start <- ymd_hms(cd)
  dt_vec <- rep(timect_start, times = n)
  dt_vec[1] <- test_start
  i <- 1
  for (i in 1:n){
    dt_vec[i+1]   <- test_start + i*15*60
  }
  return(dt_vec)
}




theme_set(theme_minimal())
theme_update(panel.grid.minor.y = element_blank(),
             panel.grid.major.y = element_line(colour = "grey91"),
             panel.grid.minor.x = element_line(colour = "grey81"),
             panel.grid.major.x = element_line(colour = "grey61"),
             axis.line = element_blank())

# ====================== #
# Load first set of data - Feb 2017 to August 2018 ####
# ====================== #
setwd("~/Box/Sarah and Molly's Box/FHL data/HOBO data/Friday Harbor/2017-2018 HOBO files")
dir.1 <- getwd()
files <- list.files(full.names = TRUE, include.dirs = TRUE)
file_list <- 0
start <- 0
end <- 0
n <- 0

for(j in 1:length(files)){
  data <- read.csv(files[j], header = TRUE, skip = 1, stringsAsFactors = FALSE)
  file_list[j] <- files[j]
  start[j] <- data[1,2]
  n[j] <- length(data[,2])
  end[j] <- data[n[j],2]
}

df.files.1 <- data.frame(
  file_list,
  start,
  end,
  n
)
df.files.1

# ====================== #
# Load second set of data - Feb 2018 to August 2018 ####
# ====================== #
setwd("~/Box/Sarah and Molly's Box/FHL data/HOBO data/Friday Harbor/Mar2019FHL")
dir.2 <- getwd()
files <- list.files(full.names = TRUE, include.dirs = TRUE)
file_list <- 0
start <- 0
end <- 0
n <- 0

for(j in 1:length(files)){
  data <- read.csv(files[j], header = TRUE, skip = 1, stringsAsFactors = FALSE)
  # Here run the function which lets us generate the possible deltas
  # Save in a new folder with (1) the individual code that includes both the force trajectory and (2) a delta file for each 
  
  file_list[j] <- files[j]
  start[j] <- data[1,2]
  n[j] <- length(data[,2])
  end[j] <- data[n[j],2]
}

df.files.2 <- data.frame(
  file_list,
  start,
  end,
  n
)
df.files.2


# ================= #
# # Read in Upper Intertidal #### 
# - August 2017 to August 2018 (only need starting in Feb)
# ================= #
# 1 = U, 2 = M, 3 = L
# Go by Hobo number - not 1, 2, 3. I'm renumbering here... 
# ================ #
# First replicate
setwd(dir.1)
d1m <- read.csv(file = "./10367970_UpperFHL1.csv",
                header = TRUE, skip = 1, stringsAsFactors = FALSE)
(n1 <- length(d1m[,2]))
head(d1m[,1:3])
tail(d1m[,1:3])
dim(d1m)

par(mfrow = c(2,1))
n <- length(d1m[,3])
n_minus <- n-500
plot(d1m[1:500,3], pch = ".")
plot(d1m[n_minus:n,3], pch = ".")

start <- 100
end <- n-150
d1m <- d1m[start:end,]


setwd(dir.2)
d2m <- read.csv(file = "./10367970_FHL1_Mar2019_high3.csv", 
                header = TRUE, skip = 1, stringsAsFactors = FALSE)
n2 <- length(d2m[,2])
head(d2m)
dim(d2m)

par(mfrow = c(2,1))
n <- length(d2m[,3])
n_minus <- n-400
plot(d2m[1:400,3], pch = ".")
plot(d2m[n_minus:n,3], pch = ".")

start <- 50
end <- n-75

d2m <- d2m[start:end,]

df_depth <- rbind(d1m[,1:3],d2m)
head(df_depth)
names(df_depth)<- c("ID","datetime","Temp_F")
df_depth$datetime <- as.POSIXct(df_depth$datetime, format = "%m/%d/%y %I:%M:%S %p", tz = "GMT")
plot(df_depth$datetime, df_depth$Temp_F, pch = ".")




df_depth_U1 <- df_depth[,2:3]

# ============== #
# 2nd Replicate ####
df.files.1
df.files.2
setwd(dir.1)
d1m <- read.csv(file = "./10367972_UpperFHL2.csv",
                header = TRUE, skip = 1, stringsAsFactors = FALSE)
(n1 <- length(d1m[,2]))
head(d1m[,1:3])
dim(d1m)

par(mfrow = c(2,1))
n <- length(d1m[,3])
n_minus <- n-500
plot(d1m[1:500,3], pch = ".")
plot(d1m[n_minus:n,3], pch = ".")

start <- 100
end <- n-50
d1m <- d1m[start:end,]

setwd(dir.2)
d2m <- read.csv(file = "./10367972-FHL2_Mar2019_high1.csv", 
                header = TRUE, skip = 1, stringsAsFactors = FALSE)
n2 <- length(d2m[,2])
head(d2m)
dim(d2m)

par(mfrow = c(2,1))
n <- length(d2m[,3])
n_minus <- n-400
plot(d2m[1:400,3], pch = ".")
plot(d2m[n_minus:n,3], pch = ".")

start <- 50
end <- n-75
d2m <- d2m[start:end,]

df_depth <- rbind(d1m[,1:3],d2m)
head(df_depth)
names(df_depth)<- c("ID","datetime","Temp_F")
df_depth$datetime <- as.POSIXct(df_depth$datetime, format = "%m/%d/%y %I:%M:%S %p", tz = "GMT")
plot(df_depth$datetime, df_depth$Temp_F, pch = ".")

df_depth_U2 <- df_depth[,2:3]

# ============== #
# 3rd Replicate ####
df.files.1
df.files.2
setwd(dir.1)
d1m <- read.csv(file = "./10367896_UpperFHL3.csv",
                header = TRUE, skip = 1, stringsAsFactors = FALSE)
(n1 <- length(d1m[,2]))
head(d1m[,1:3])
dim(d1m)

par(mfrow = c(2,1))
n <- length(d1m[,3])
n_minus <- n-500
plot(d1m[1:500,3], pch = ".")
plot(d1m[n_minus:n,3], pch = ".")

start <- 100
end <- n-50
d1m <- d1m[start:end,]

setwd(dir.2)
d2m <- read.csv(file = "./10367896-FHL3_Mar2019_high2.csv", 
                header = TRUE, skip = 1, stringsAsFactors = FALSE)
n2 <- length(d2m[,2])
head(d2m)
dim(d2m)

par(mfrow = c(2,1))
n <- length(d2m[,3])
n_minus <- n-400
plot(d2m[1:400,3], pch = ".")
plot(d2m[n_minus:n,3], pch = ".")

start <- 50
end <- n-75
d2m <- d2m[start:end,]

df_depth <- rbind(d1m[,1:3],d2m)
head(df_depth)
names(df_depth)<- c("ID","datetime","Temp_F")
df_depth$datetime <- as.POSIXct(df_depth$datetime, format = "%m/%d/%y %I:%M:%S %p", tz = "GMT")
plot(df_depth$datetime, df_depth$Temp_F, pch = ".")
head(df_depth)

df_depth_U3 <- df_depth[,2:3]

# =========== #
# Combine 3 replicates into 1 dataframe ####

JF <- 0
df <- 0

require(dplyr)
J1 <- left_join(df_depth_U1,df_depth_U2, by = "datetime", suffix = c("2","3"))
df <- left_join(J1,df_depth_U3, by = "datetime")
head(df)

#=========#
# Data QC ####
#=========#
#1. Go back and make sure that starts and ends 
# of each dataset are clipped for transport to site. (above)

#2. Check that temps match up among the replicates
# par (mfrow = c(2,2))
# plot(df[,2],df[,3], pch = ".")
# plot(df[,2],df[,4], pch = ".")
# plot(df[,3],df[,4], pch = ".")
library(Hmisc)
library("PerformanceAnalytics")
my_data <- df[,2:4]
#chart.Correlation(my_data, histogram=FALSE, pch=19)

m_df <- melt(df, "datetime")
head(m_df)

ggplot(m_df, aes(x=datetime, y=value, fill = variable)) +
  geom_point(aes(colour = variable), size = .1, alpha = .2)
# Hm... One of them has much higher temps than the 

summarise(group_by(m_df, variable),
          mean=mean(!is.na(value)), sd=sd(!is.na(value)))
# This shows up a little bit here... ?

df_U <- df
m_df_U <- m_df

# ================= #
# # Read in Middle Intertidal ####
# - August 2017 to August 2018 (only need starting in Feb)
# ================= #
# 1 = U, 2 = M, 3 = L
# Go by Hobo number - not 1, 2, 3. I'm renumbering here... 
#================#
# First replicate ####
d1m <- 0
d2m <- 0
setwd(dir.1)
df.files.1
d1m <- read.csv(file = "./10367964_MiddleFHL1.csv",
                header = TRUE, skip = 1, stringsAsFactors = FALSE)
(n1 <- length(d1m[,2]))
head(d1m[,1:3])
dim(d1m)

par(mfrow = c(2,1))
n <- length(d1m[,3])
n_minus <- n-500
plot(d1m[1:500,3], pch = ".")
plot(d1m[n_minus:n,3], pch = ".")

start <- 100
end <- n-50
d1m <- d1m[start:end,]

df_depth <- d1m

head(df_depth)
names(df_depth)<- c("ID","datetime","Temp_F")
df_depth$datetime <- as.POSIXct(df_depth$datetime, format = "%m/%d/%y %I:%M:%S %p", tz = "GMT")
plot(df_depth$datetime, df_depth$Temp_F, pch = ".")


df_depth_M1 <- df_depth[,2:3]

#==============#
# 2nd Replicate ####
df.files.1
df.files.2
setwd(dir.1)
d1m <- read.csv(file = "./10385383_MiddleFHL2.csv",
                header = TRUE, skip = 1, stringsAsFactors = FALSE)
(n1 <- length(d1m[,2]))
head(d1m[,1:3])
dim(d1m)

par(mfrow = c(2,1))
n <- length(d1m[,3])
n_minus <- n-500
plot(d1m[1:500,3], pch = ".")
plot(d1m[n_minus:n,3], pch = ".")

start <- 100
end <- n-50
d1m <- d1m[start:end,]
head(d1m)

setwd(dir.2)
d2m <- read.csv(file = "./10385383-FHL5_Mar2019_mid1.csv", 
                header = TRUE, skip = 1, stringsAsFactors = FALSE)
n2 <- length(d2m[,2])
head(d2m)
dim(d2m)

par(mfrow = c(2,1))
n <- length(d2m[,3])
n_minus <- n-400
plot(d2m[1:400,3], pch = ".")
plot(d2m[n_minus:n,3], pch = ".")

start <- 50
end <- n-75
d2m <- d2m[start:end,]

df_depth <- rbind(d1m[,1:3],d2m)
head(df_depth)
names(df_depth)<- c("ID","datetime","Temp_F")
df_depth$datetime <- as.POSIXct(df_depth$datetime, format = "%m/%d/%y %I:%M:%S %p", tz = "GMT")
plot(df_depth$datetime, df_depth$Temp_F, pch = ".")

df_depth_M2 <- df_depth[,2:3]

#============== #
# 3rd Replicate ####
df.files.1
df.files.2
setwd(dir.1)
d1m <- read.csv(file = "./10367898_MiddleFHL3.csv",
                header = TRUE, skip = 1, stringsAsFactors = FALSE)
(n1 <- length(d1m[,2]))
head(d1m[,1:3]) #From Aug 17
tail(d1m[,1:3]) #To November 18
dim(d1m)

par(mfrow = c(2,1))
n <- length(d1m[,3])
n_minus <- n-8890
plot(d1m[1:500,3], pch = ".")
plot(d1m[n_minus:n,3], pch = ".")

plot(d1m[,3], pch = ".")

start <- 100
end <- n-8890
d1m <- d1m[start:end,]
tail(d1m) #To 7/31/18

setwd(dir.2)
df.files.2
d2m <- read.csv(file = "./pt2_fhl_1l_Mar2019_mid3.csv", 
                header = TRUE, skip = 1, stringsAsFactors = FALSE)
n2 <- length(d2m[,2])
head(d2m)
dim(d2m)

par(mfrow = c(2,1))
n <- length(d2m[,3])
n_minus <- n-400
plot(d2m[1:400,3], pch = ".")
plot(d2m[n_minus:n,3], pch = ".")

start <- 50
end <- n-75

d2m <- d2m[start:end,]
d2m.part1 <- d2m 

setwd(dir.2)
df.files.2
d2m <- read.csv(file = "./20343872_FHL_Mar2019_mid2.csv", 
                header = TRUE, skip = 1, stringsAsFactors = FALSE)
n2 <- length(d2m[,2])
head(d2m)
dim(d2m)

par(mfrow = c(2,1))
n <- length(d2m[,3])
n_minus <- n-400
plot(d2m[1:400,3], pch = ".")
plot(d2m[n_minus:n,3], pch = ".")

start <- 50
end <- n-75

# startTime <- d2m$Date.Time..GMT.07.00[start]
# endTime <- d2m$Date.Time..GMT.07.00[end]
# 

# (start_hr <- strptime(startTime, format="%m/%d/%y %I:%M:%S %p", tz = "GMT"))
# (end_hr <- strptime(endTime, format="%m/%d/%y %I:%M:%S %p", tz = "GMT"))

# start_hr
# end_hr
# seq.POSIXt(from = as.POSIXct("2018-10-03 20:08:15 GMT"), 
#            to = as.POSIXct("2019-03-01 15:38:19 GMT"),  
#            by = "GMT min")

# require(lubridate)
# input$result <- approx(first$time,
#                        xout = input$time,
#                        rule = 2)$y
# 
# d2m <- d2m[start:end,]



# Offset - interpolate to get reading at 15 minute interval. ####
# J1 <- left_join(df,tides, by = "datetime")
# head(J1)
# 
# J1.s <- J1[,c("datetime","Water.Level")]
# 
# z <- zoo(J1.s)
# 
# z <- read.zoo(J1.s , tz = "GMT")
# 
# J2 <- na.approx(z, na.rm = FALSE)
# J2
# 
# plot(J1.s, pch= ".", yaxt="n", xaxt="n", bty="n")
# plot(J2, pch = ".",yaxt="n", xaxt="n", bty="n")
# 
# df.J2 <- as.vector(J2)
# df.new <- cbind(df, df.J2)
# 
# names(df.new)[5] <- "Water.Level"





d2m.part2 <- d2m
head(d2m.part2)
names(d2m.part2)<- c("ID","datetime","Temp_F")
plot(d2m.part2$datetime,d2m.part2$Temp_F)
d2m.part2$datetime <- as.POSIXct(d2m.part2[,2], format = "%m/%d/%y %I:%M:%S %p", tz = "GMT")
start <- min(d2m.part2$datetime)
n.dt <- length(d2m.part2$datetime)

start 
start + 1 *15*60
start + 14411 *15*60

xout.15 <- round15min(start, n.dt)
out.int <- approx(d2m.part2$datetime,d2m.part2$Temp_F,xout.15)$y
head(d2m.part2$datetime,1)
tail(d2m.part2$datetime,1)
head(xout.15,1)
tail(xout.15,1)
plot(xout.15, out.int)
plot(xout.15, out.int)

  
names(d2m.part2)<- c("ID","datetime","Temp_F")

names(d2m.part1)<- c("ID","datetime","Temp_F")

d1m <- d1m[,1:3]
names(d1m) <- c("ID","datetime","Temp_F")


head(d1m)
head(d2m.part1)
head(d2m.part2)
RB1 <- rbind(d1m,d2m.part1)
names(RB1)<- c("ID","datetime","Temp_F")
df_depth <- rbind(RB1,d2m.part2)

head(df_depth)
names(df_depth)<- c("ID","datetime","Temp_F")
df_depth$datetime <- as.POSIXct(df_depth$datetime, format = "%m/%d/%y %I:%M:%S %p", tz = "GMT")
plot(df_depth$datetime, df_depth$Temp_F, pch = ".")
head(df_depth)

df_depth_M3 <- df_depth[,2:3]

# =========== #
# Combine 3 replicates into 1 dataframe ####

JF <- 0
df <- 0

df_depth_M3$datetime <- round(df_depth_M3$datetime, units = "mins")

require(dplyr)
J1 <- right_join(df_depth_M1,df_depth_M2, by = "datetime", suffix = c("2","3")) #M1 is cut off so use right join
df <- left_join(J1,df_depth_M3, by = "datetime")
head(df)
head(df_depth_M3)

# require(mergeTime)
# nearestTimeandID(df_depth_M1, df_depth_M2, "datetime", "datetime", suffix = c("2","3"))


# This doesn't work either because it is too much interpolation
# 
# Temp_F1 <- approx(x = df_depth_M1$datetime,
#                   y = df_depth_M1$Temp_F,
#                   xout = df_depth_M1$datetime,
#                   rule = 2)$y
# 
# Temp_F2 <- approx(x = df_depth_M2$datetime,
#                        y = df_depth_M2$Temp_F,
#                        xout = df_depth_M1$datetime,
#                        rule = 2)$y
# 
# Temp_F3 <- approx(x = df_depth_M3$datetime,
#                              y = df_depth_M3$Temp_F,
#                              xout = df_depth_M1$datetime,
#                              rule = 2)$y
# 
# df <- cbind(df_depth_M1, Temp_F2 , Temp_F3)
# head(df)
#=========#
# Data QC ####
#========#
#1. Go back and make sure that starts and ends 
# of each dataset are clipped for transport to site. (above)

#2. Check that temps match up among the replicates
# par (mfrow = c(2,2))
# plot(df[,2],df[,3], pch = ".")
# plot(df[,2],df[,4], pch = ".")
# plot(df[,3],df[,4], pch = ".")
library(Hmisc)
library("PerformanceAnalytics")
my_data <- df[,2:4]
#chart.Correlation(my_data, histogram=FALSE, pch=19)

m_df <- melt(df, "datetime")
head(m_df)

ggplot(m_df, aes(x=datetime, y=value, fill = variable)) +
  geom_point(aes(colour = variable), size = .2, alpha = .1)
# Hm... One of them has much higher temps than the 

dev.off()

summarise(group_by(m_df, variable),
          mean=mean(!is.na(value)), sd=sd(!is.na(value)))
# This shows up a little bit here... ?

df_M <- df
m_df_M <- m_df

# ================= #
# # Read in Lower Intertidal #### 
# - August 2017 to August 2018 (only need starting in Feb)
# ================= #
# 1 = U, 2 = M, 3 = L
# Go by Hobo number - not 1, 2, 3. I'm renumbering here... 
# ================ #
# First replicate
d1m <- 0
d2m <- 0
df.files.1
setwd(dir.1)
d1m <- read.csv(file = "./10367894_LowerFHL1.csv",
                header = TRUE, skip = 1, stringsAsFactors = FALSE)
(n1 <- length(d1m[,2]))
head(d1m[,1:3])
dim(d1m)

par(mfrow = c(2,1))
n <- length(d1m[,3])
n_minus <- n-400
plot(d1m[1:400,3], pch = ".")
plot(d1m[n_minus:n,3], pch = ".")

start <- 100
end <- n-100
d1m <- d1m[start:end,]

df.files.2
setwd(dir.2)
d2m <- read.csv(file = "./10367894-FHL7_Mar2019_low3.csv", 
                header = TRUE, skip = 1, stringsAsFactors = FALSE)
n2 <- length(d2m[,2])
head(d2m)
dim(d2m)

par(mfrow = c(2,1))
n <- length(d2m[,3])
n_minus <- n-600
plot(d2m[1:600,3], pch = ".")
plot(d2m[n_minus:n,3], pch = ".")

start <- 50
end <- n-75

d2m <- d2m[start:end,]

df_depth <- rbind(d1m[,1:3],d2m)
head(df_depth)
names(df_depth)<- c("ID","datetime","Temp_F")
df_depth$datetime <- as.POSIXct(df_depth$datetime, format = "%m/%d/%y %I:%M:%S %p", tz = "GMT")
plot(df_depth$datetime, df_depth$Temp_F, pch = ".")




df_depth_L1 <- df_depth[,2:3]

# ============== #
# 2nd Replicate, with gap in dataset ####
d1m <- 0
d2m <- 0
df.files.1
df.files.2
setwd(dir.1)
d1m <- read.csv(file = "./10367895_LowerFHL2.csv",
                header = TRUE, skip = 1, stringsAsFactors = FALSE)
(n1 <- length(d1m[,2]))
head(d1m[,1:3])
dim(d1m)

par(mfrow = c(2,1))
n <- length(d1m[,3])
n_minus <- n-500
plot(d1m[1:500,3], pch = ".")
plot(d1m[n_minus:n,3], pch = ".")

start <- 100
end <- n-100
d1m <- d1m[start:end,]

# Gap in between the two periods of sampling

setwd(dir.2)
d2m <- read.csv(file = "./20343877_FHL_Mar2019_low1.csv", 
                header = TRUE, skip = 1, stringsAsFactors = FALSE)
n2 <- length(d2m[,2])
head(d2m)
dim(d2m)

par(mfrow = c(2,1))
n <- length(d2m[,3])
n_minus <- n-400
plot(d2m[1:400,3], pch = ".")
plot(d2m[n_minus:n,3], pch = ".")

start <- 50
end <- n-75
d2m <- d2m[start:end,]

d1m <- d1m[,1:3]
names(d1m)<- c("ID","datetime","Temp_F")
names(d2m)<- c("ID","datetime","Temp_F")
head(d1m)
df_depth <- rbind(d1m,d2m)
head(df_depth)
names(df_depth)<- c("ID","datetime","Temp_F")
df_depth$datetime <- as.POSIXct(df_depth$datetime, format = "%m/%d/%y %I:%M:%S %p", tz = "GMT")
plot(df_depth$datetime, df_depth$Temp_F, pch = ".")
head(df_depth)

df_depth_L2 <- df_depth[,2:3]

# ============== #
# 3rd Replicate ####
# (Remove bad data) ####
d1m <- 0
d2m <- 0
df.files.1
df.files.2
setwd(dir.1)
d1m <- read.csv(file = "./10154179_LowerFHL3.csv",
                header = TRUE, skip = 1, stringsAsFactors = FALSE)
(n1 <- length(d1m[,2]))
head(d1m[,1:3])
dim(d1m)

par(mfrow = c(2,1))
n <- length(d1m[,3])
n_minus <- n-10000
plot(d1m[1:10000,3], pch = ".")
plot(d1m[n_minus:n,3], pch = ".")
plot(d1m[,3], pch = ".")

start <- 100
end <- n-6000
d1m <- d1m[start:end,]

setwd(dir.2)
d2m <- read.csv(file = "./10154179-FHL9_Mar2019_low2.csv", 
                header = TRUE, skip = 1, stringsAsFactors = FALSE)
n2 <- length(d2m[,2])
head(d2m)
dim(d2m)

par(mfrow = c(2,1))
n <- length(d2m[,3])
n_minus <- n-400
plot(d2m[1:400,3], pch = ".")
plot(d2m[n_minus:n,3], pch = ".")

start <- 50
end <- n-75
d2m <- d2m[start:end,]

#df_depth <- rbind(d1m[,1:3],d2m)
df_depth <- d2m #Not including sensor that fell. 
head(df_depth)
names(df_depth)<- c("ID","datetime","Temp_F")
df_depth$datetime <- as.POSIXct(df_depth$datetime, format = "%m/%d/%y %I:%M:%S %p", tz = "GMT")
plot(df_depth$datetime, df_depth$Temp_F, pch = ".")
head(df_depth)

df_depth_L3 <- df_depth[,2:3]



# =========== #
# Combine 3 replicates into 1 dataframe ####

JF <- 0
df <- 0

require(dplyr)
J1 <- left_join(df_depth_L1,df_depth_L2, by = "datetime", suffix = c("2","3"))
df <- left_join(J1,df_depth_L3, by = "datetime")
head(df)


#=========#
# Data QC ####
#=========#
#1. Go back and make sure that starts and ends 
# of each dataset are clipped for transport to site. (above)

#2. Check that temps match up among the replicates
# par (mfrow = c(2,2))
# plot(df[,2],df[,3], pch = ".")
# plot(df[,2],df[,4], pch = ".")
# plot(df[,3],df[,4], pch = ".")

my_data <- df[,2:4]
#chart.Correlation(my_data, histogram=FALSE, pch=19)

m_df <- melt(df, "datetime")
head(m_df)

ggplot(m_df, aes(x=datetime, y=value, fill = variable)) +
  geom_point(aes(colour = variable), size = .1, alpha = .2)
# Hm... One of them has much higher temps than the 

summarise(group_by(m_df, variable),
          mean=mean(!is.na(value)), sd=sd(!is.na(value)))
# This shows up a little bit here... ?

df_L <- df
m_df_L <- m_df

# ====== #
# Combine datasets ####
# Now take the average of each, upper, middle, low, and graph together 
# Also output this final dataset
# AND put the tidal data with it and separate by aerial and aquatic temperatures

Upper <- rowMeans(df_U[2:4], na.rm = TRUE)
Mid <- rowMeans(df_M[2:4], na.rm = TRUE)
Lower <- rowMeans(df_L[2:4], na.rm = TRUE)

df_U_ave <- data.frame (
  datetime = df_U[,1],
  Temp = Upper)
head(df_U_ave)

plot(df_U_ave$datetime)
plot(df_U_ave$Temp)


df_M_ave <- data.frame (
  datetime = df_M[,1],
  Temp = Mid)
head(df_M_ave)

plot(df_M_ave$datetime)
plot(df_M_ave$Temp)

plot(df_M_ave$datetime)

df_L_ave <- data.frame (
  datetime = df_L[,1],
  Temp = Lower)
head(df_L_ave)

plot(df_L_ave$datetime)
plot(df_L_ave$Temp)

par (mfrow = c(1,1))
plot(df_U_ave$datetime, df_U_ave$Temp, pch = ".", col = "red")
points(df_M_ave$datetime, df_M_ave$Temp, pch = ".", col = "blue" )
points(df_L_ave$datetime, df_L_ave$Temp, pch = ".", col = "green" )

J1 <- full_join(df_U_ave,df_M_ave, by = "datetime", suffix = c("_U","_M"))
df <- full_join(J1, df_L_ave, by = "datetime")
head(J1)
head(df)
names(df) <- c("datetime", "Upper", "Mid", "Low")

# Account for HOBO data being in timezone "GMT -7" 
# rather than "GMT," with an offset of 7 hours

require(lubridate)
require(magrittr)

df$datetime %<>% magrittr::subtract(lubridate::hm("-07:00"))
df$datetime %<>% lubridate::with_tz("GMT")

par(mfrow = c(2,1))
diff <- df$Upper -df$Mid
plot(df$datetime, diff, pch = ".")
diff <- df$Mid -df$Low
plot(df$datetime, diff, pch = ".")


m_df <- melt(df, "datetime")
head(m_df)

ggplot(m_df, aes(x=datetime, y=value, fill = variable)) +
  geom_point(aes(colour = variable), size = .4, alpha = .2) + ylab("Temperature deg C") + xlim(c(as.POSIXct("2018-08-10"),as.POSIXct("2018-08-28")))

ggplot(m_df, aes(x=datetime, y=value, fill = variable)) +
  geom_point(aes(colour = variable), size = .4, alpha = .2) + ylab("Temperature deg C")

ggplot(m_df, aes(x=datetime, y=value, fill = variable)) +
  geom_line(aes(colour = variable), size = .4) + ylab("Temperature deg C")

# Combine HOBO data with water level data ####

setwd("~/Box/Sarah and Molly's Box/FHL data/HOBO data/Tides")
tides <- read.csv("FHtides201708201903.csv", stringsAsFactors = FALSE)
head(tides)
tides$datetime <- as.POSIXct(tides$gmt.dt, tz = "GMT")
plot(tides$datetime, tides$Water.Level, pch = ".",xlim = c(as.POSIXct("2018-08-01"),as.POSIXct("2018-08-25")))
plot(df$datetime, df$Upper, pch = ".",xlim = c(as.POSIXct("2018-08-01"),as.POSIXct("2018-08-25"))) #Missing data from 8/13-8/14, but not later on
plot(df$datetime, df$Mid, pch = ".",xlim = c(as.POSIXct("2018-08-01"),as.POSIXct("2018-08-25")))
plot(df$datetime, df$Low, pch = ".",xlim = c(as.POSIXct("2018-08-01"),as.POSIXct("2018-08-25")))

#No missing temp or tide data so far

J1 <- left_join(df,tides, by = "datetime")
head(J1)

J1.s <- J1[,c("datetime","Water.Level")]

z <- zoo(J1.s)

z <- read.zoo(J1.s , tz = "GMT")

J2 <- na.approx(z, na.rm = FALSE)
J2

par(omi = c(0,0,0,0), mar = c(0,0,0,0))

plot(J1.s, pch= ".", yaxt="n", xaxt="n", bty="n",xlim = c(as.POSIXct("2018-08-12"),as.POSIXct("2018-08-25")))#Data missing 8/13-8/14
plot(J1.s, pch= ".", yaxt="n", xaxt="n", bty="n",xlim = c(as.POSIXct("2018-08-12"),as.POSIXct("2018-08-25")))#No missing data 8/13-8/14

plot(J2, pch = ".",yaxt="n", xaxt="n", bty="n",xlim = c(as.POSIXct("2018-08-12"),as.POSIXct("2018-08-25")))#Missing data 8/13-8/14 only

df.J2 <- as.vector(J2)
df.new <- cbind(df, df.J2)
names(df.new)[5] <- "Water.Level"


plot(df.new$datetime, df.new$Upper , pch = ".",yaxt="n", xaxt="n", bty="n",xlim=c(as.POSIXct("2018-08-12"),as.POSIXct("2018-08-25")))#Missing data 8/13-8/14 only



# Write .csv####
# write.csv(file = "TempAndWaterLevel.csv", df.new)
# setwd("~/Box/Sarah and Molly's Box/FHL data/HOBO data/Tides")
# df.new <- read.csv(file = "TempAndWaterLevel.csv", stringsAsFactors = FALSE) 
#If I want to skip the above part...



# Combine HOBO data with seawater temp data from NOAA, FHL site ####
# https://www.ndbc.noaa.gov/station_history.php?station=frdw1
# https://www.ndbc.noaa.gov/measdes.shtml
#2017
setwd("~/Box/Sarah and Molly's Box/FHL data/HOBO data/Tides/FridayHarborUnderwaterNOAA")
names.dat <- read.table("frdw1h2017.txt", stringsAsFactors = FALSE, skip = 0, header = FALSE, sep = "", dec =".")
names.dat <- as.data.frame(names.dat)
names.dat <- as.character(names.dat[1,1:18])
dat <- read.table("frdw1h2017.txt", stringsAsFactors = FALSE, skip = 2, header = FALSE, sep = "", dec =".")
names(dat) <- as.character(names.dat)
dat.17 <- dat

#2018
names.dat <- read.table("frdw1h2018.txt", stringsAsFactors = FALSE, skip = 0, header = FALSE, sep = "", dec =".")
names.dat <- as.data.frame(names.dat)
names.dat <- as.character(names.dat[1,1:18])
dat <- read.table("frdw1h2018.txt", stringsAsFactors = FALSE, skip = 2, header = FALSE, sep = "", dec =".")
names(dat) <- as.character(names.dat)
dat.18 <- dat

#2019
names.dat <- read.table("frdw1h2019.txt", stringsAsFactors = FALSE, skip = 0, header = FALSE, sep = "", dec =".")
names.dat <- as.data.frame(names.dat)
names.dat <- as.character(names.dat[1,1:18])
dat <- read.table("frdw1h2019.txt", stringsAsFactors = FALSE, skip = 2, header = FALSE, sep = "", dec =".")
names(dat) <- as.character(names.dat)
dat.19 <- dat

dat <- rbind(dat.17,dat.18,dat.19)


dat$WTMP[dat$WTMP>=200] <- NA
plot(dat$WTMP)




# 2005-10-21 18:47:22
dat$datetime <- paste(dat$YY,"-",dat$MM,"-",dat$DD," ", dat$hh, ":", dat$mm,":0", sep = "")
(dat$datetime.f <- as.POSIXct(strptime(dat$datetime, format = "%Y-%m-%d %H:%M:%S"), tz = "GMT"))
str(dat)
NOAA <- dat[,c("datetime.f","WTMP")]
names(NOAA) <- c("datetime","WaterTemp")
str(df.new)
str(NOAA)
df.new <- df.new[,2:6]
df.new$datetime <- as.POSIXct(df.new$datetime, tz = "GMT")


plot(dat$datetime.f, dat$WTMP)


str(df.new)
library(zoo)
# zl <- zoo(NOAA) #Low frequency water temp
# zh <- zoo(df.new) #High frequency HOBO

head(zh)

library(dplyr)
joined <- left_join(df.new,NOAA, by = "datetime")

str(joined)
# Can work in number of seconds since Jan 19something
z1 <- na.spline(joined[,"WaterTemp"],  na.rm = FALSE) #datetime


df.new <- cbind(df.new, Water.Temp = as.vector(z1))

#QC check
str(df.new)
df.new[df.new$Upper-df.new$Water.Temp>=5&df.new$Water.Level>=2,]
df.new[df.new$datetime>="2018-08-22 00:00:00"&df.new$datetime<="2018-08-25 06:30:00",] 
plot(df.new$datetime, df.new$Upper,xlim = c(as.POSIXct("2018-08-15"),as.POSIXct("2018-08-25")))
points(df.new$datetime, df.new$Water.Temp,xlim = c(as.POSIXct("2018-08-15"),as.POSIXct("2018-08-25")), col = "blue")
#plot(df.new$datetime, df.new$Water.Level,xlim = c(as.POSIXct("2018-08-15"),as.POSIXct("2018-08-25")))
plot(df.new$datetime, df.new$Upper,xlim = c(as.POSIXct("2018-08-10"),as.POSIXct("2018-08-16")))

#plot(df.new$datetime, df.new$Upper,xlim = c(as.POSIXct("2018-08-10"),as.POSIXct("2018-08-11")))
replace.missing_data <- df.new[df.new$datetime>="2018-08-12 00:00:00"&df.new$datetime<="2018-08-13 06:00:00",] 
replace.missing_data2 <- df.new[df.new$datetime>="2018-08-14 00:00:00"&df.new$datetime<="2018-08-15 06:00:00",] 

#plot(replace.missing_data$Low)
# replace_Aug_13 <- data.frame(
#   day_before = replace.missing_data$datetime+days(1),
#   day_after = replace.missing_data2$datetime-days(1),
#   Upper_before = replace.missing_data$Upper,
#   Upper_after = replace.missing_data2$Upper
# )
replace.missing_data$new_day <- replace.missing_data$datetime+days(1)
replace.missing_data2$new_day <- replace.missing_data2$datetime-days(1)
april_13 <- merge(replace.missing_data, replace.missing_data2, by = "new_day")
mean(april_13$Upper.x,april_13$Upper.y, trim = 1)
april_13$Upper_new <- rowMeans(cbind(april_13$Upper.x,april_13$Upper.y), na.rm=TRUE)
april_13$Mid_new <- rowMeans(cbind(april_13$Mid.x,april_13$Mid.y), na.rm=TRUE)
april_13$Low_new <- rowMeans(cbind(april_13$Low.x,april_13$Low.y), na.rm=TRUE)
head(april_13)
head(df.new)
april_13_comb <- merge(df.new, april_13[,c(1,16,17,18)], by.x = c("datetime"), by.y = c("new_day"), all.y=TRUE)
head(april_13_comb)
plot(april_13_comb$Upper)
points(as.POSIXct(april_13_comb$datetime),april_13_comb$Upper_new)

# points(replace.missing_data$datetime+days(1), replace.missing_data$Upper)
# points(replace.missing_data2$datetime-days(1), replace.missing_data2$Upper)
plot(df.new$datetime, df.new$Upper,xlim = c(as.POSIXct("2018-08-10"),as.POSIXct("2018-08-16")))
points(as.POSIXct(april_13_comb$datetime),april_13_comb$Upper_new)
df.new[df.new$datetime<=max(april_13_comb$datetime)&df.new$datetime>=min(april_13_comb$datetime),] <- april_13_comb$Upper_new[is.na(april_13_comb$Upper)]
plot(as.POSIXct(april_13_comb$datetime),april_13_comb$Upper)
df.new[is.na(df.new$Low),]
head(df.new)
head(april_13_comb)
april_13_rbind <- data.frame(
  datetime = april_13_comb$datetime,
  Upper = april_13_comb$Upper_new,
  Mid = april_13_comb$Mid_new,
  Low = april_13_comb$Low_new,
  Water.Level = NA,
  Water.Temp = NA
)
head(april_13_rbind)
df.new.april13 <- rbind(df.new,april_13_rbind)
tail(df.new.april13)

plot(df.new.april13$datetime, df.new.april13$Upper,xlim = c(as.POSIXct("2018-08-10"),as.POSIXct("2018-08-16")))
plot(df.new.april13$datetime, df.new.april13$Mid,xlim = c(as.POSIXct("2018-08-10"),as.POSIXct("2018-08-16")))
plot(df.new.april13$datetime, df.new.april13$Low,xlim = c(as.POSIXct("2018-08-10"),as.POSIXct("2018-08-16")))
#df.new.unique <- df.new.april13[unique(as.POSIXct(df.new.april13$datetime)),]
df.new.order <- df.new.april13[order(as.POSIXct(df.new.april13$datetime)),]
# points(df.new$datetime, df.new$Water.Temp,xlim = c(as.POSIXct("2018-08-15"),as.POSIXct("2018-08-25")), col = "blue")
plot(df.new.order$datetime, df.new.order$Upper,xlim = c(as.POSIXct("2018-08-10"),as.POSIXct("2018-08-16")), type = "l")
plot(df.new.order$datetime, df.new.order$Mid,xlim = c(as.POSIXct("2018-08-10"),as.POSIXct("2018-08-16")), type = "l")
plot(df.new.order$datetime, df.new.order$Low,xlim = c(as.POSIXct("2018-08-10"),as.POSIXct("2018-08-16")),type = "l")


## Correction - loggers were removed and replaced on 8/13 not 8/23. -MAR 4/1/2021
## Previously wrote: 
#I think that even loggers that were not removed and replaced were removed and read out on 8/23. I'm throwing out the data from 8/23, because I don't trust it.
# df.new$Upper[df.new$datetime>="2018-08-22 09:30:00"&df.new$datetime<="2018-08-25 06:30:00"] <- NA
# df.new$Mid[df.new$datetime>="2018-08-22 09:30:00"&df.new$datetime<="2018-08-25 06:30:00"] <- NA
# df.new$Low[df.new$datetime>="2018-08-22 09:30:00"&df.new$datetime<="2018-08-25 06:30:00"] <- NA

# The issue is the water temp logger - it drops down on the 22 and 23. Everything else looks fine -MAR 4/1/2021
df.new$Water.Temp[df.new$datetime>="2018-08-22 16:00:00"&df.new$datetime<="2018-08-23 12:45:00"] <- NA #Must subtract 7 hours to be in GMT

# df.new$Low[df.new$datetime>="2018-08-22 09:30:00"&df.new$datetime<="2018-08-25 06:30:00"] <- NA



str(df.new)

aqua.H <- df.new[df.new$Water.Level>=1.97,
                 c("datetime","Upper","Water.Level","Water.Temp")]
air.H <- df.new[df.new$Water.Level<1.97,
                c("datetime","Upper","Water.Level","Water.Temp")]
aqua.M <- df.new[df.new$Water.Level>=1.55,
                 c("datetime","Mid","Water.Level","Water.Temp")]
air.M <- df.new[df.new$Water.Level<1.55,
                c("datetime","Mid","Water.Level","Water.Temp")]
aqua.L <- df.new[df.new$Water.Level>=1.20,
                 c("datetime","Low","Water.Level","Water.Temp")]
air.L <- df.new[df.new$Water.Level<1.20,
                c("datetime","Low","Water.Level","Water.Temp")]

plot(aqua.M$datetime, aqua.M$Mid)

m_df <- melt(df.new, "datetime")
head(m_df)
str(m_df)

min(m_df$datetime)
ggplot(m_df, aes(x=datetime, y=value, fill = variable)) +
  geom_line(aes(colour = variable), size = 1, alpha = 1)+
  coord_cartesian(xlim = c(as.POSIXct("2017-08-06 17:45:00", tz = "GMT"), as.POSIXct("2017-08-10 17:45:00",tz = "GMT")))

m_df <- melt(air.H, "datetime")
m_df_aqua <- melt(aqua.H, "datetime")
head(m_df)
min(m_df$datetime)
ggplot(m_df, aes(x=datetime, y=value, fill = variable)) +
  geom_point(aes(colour = variable), size = 1, alpha = 1)+
  coord_cartesian(xlim = c(as.POSIXct("2017-08-06 17:45:00", tz = "GMT"), 
                           as.POSIXct("2017-08-10 17:45:00",tz = "GMT")))+
  geom_point(data = m_df_aqua, aes(colour = variable), size = 1, alpha = 1, col = "black")
  #geom_point(data = m_df_aqua, aes(colour = variable), size = 1, alpha = 1, col = "black")
  # overlay points of seawater data
  
m_df <- melt(air.M, "datetime")
m_df_aqua <- melt(aqua.M, "datetime")
head(m_df)
min(m_df$datetime)
ggplot(m_df, aes(x=datetime, y=value, fill = variable)) +
  geom_point(aes(colour = variable), size = 1, alpha = 1)+
  coord_cartesian(xlim = c(as.POSIXct("2017-08-06 17:45:00", tz = "GMT"), 
                           as.POSIXct("2017-08-10 17:45:00",tz = "GMT")))+
  geom_point(data = m_df_aqua, aes(colour = variable), size = 1, alpha = 1, col = "black")
  

m_df <- melt(air.L, "datetime")
m_df_aqua <- melt(aqua.L, "datetime")
head(m_df)
min(m_df$datetime)
ggplot(m_df, aes(x=datetime, y=value, fill = variable)) +
  geom_point(aes(colour = variable), size = 1, alpha = 1)+
  coord_cartesian(xlim = c(as.POSIXct("2017-08-06 17:45:00", tz = "GMT"), 
                           as.POSIXct("2017-08-10 17:45:00",tz = "GMT")))+
  geom_point(data = m_df_aqua, aes(colour = variable), size = 1, alpha = 1, col = "black")


  
# 
# ggplot(m_df, aes(x=datetime, y=value, fill = variable)) +
#   geom_line(aes(colour = variable), size = 1, alpha = 1)+
#   coord_cartesian(xlim = c(as.POSIXct("2018-12-06 17:45:00", tz = "GMT"), as.POSIXct("2019-01-02 17:45:00",tz = "GMT")))
# 
# ggplot(m_df, aes(x=datetime, y=value, fill = variable)) +
#   geom_line(aes(colour = variable), size = 1, alpha = 1)+
#   coord_cartesian(xlim = c(as.POSIXct("2018-11-06 17:45:00", tz = "GMT"), as.POSIXct("2019-01-02 17:45:00",tz = "GMT")))
# 
# ggplot(m_df, aes(x=datetime, y=value, fill = variable)) +
#   geom_line(aes(colour = variable), size = 1, alpha = 1)+
#   coord_cartesian(xlim = c(as.POSIXct("2019-01-06 17:45:00", tz = "GMT"), as.POSIXct("2019-02-02 17:45:00",tz = "GMT")))

ggplot(m_df, aes(x=datetime, y=value, fill = variable)) +
  geom_point(aes(colour = variable), size = .5, alpha = 1)+
  coord_cartesian(xlim = c(as.POSIXct("2019-01-29 17:45:00", tz = "GMT"), as.POSIXct("2019-02-01 17:45:00",tz = "GMT")))



ggplot(m_df, aes(x=datetime, y=value, fill = variable)) +
  geom_point(aes(colour = variable), size = 1, alpha = 1)
dev.off()

par(mfrow = c(3,1), mar = c(2, 3, 0, 0) + 0.1, oma = c(0,0,0,0))
head(df.new)
plot(df.new$Water.Level, df.new$Upper, col = "lightblue", pch = ".", ylim = c(0,40), xlim = c(.5,3.1))
lines(x = c(1.92, 1.92), y = c(0,40))
lines(x = c(2.02, 2.02), y = c(0,40))
plot(df.new$Water.Level, df.new$Mid, pch = ".", col = "blue", ylim = c(0,40), xlim = c(.5,3.1))
lines(x = c(1.5, 1.5), y = c(0,40))
lines(x = c(1.6, 1.6), y = c(0,40))
plot(df.new$Water.Level, df.new$Low, pch = ".", col = "black", ylim = c(0,40), xlim = c(.5,3.1))
lines(x = c(1.15, 1.15), y = c(0,40))
lines(x = c(1.25, 1.25), y = c(0,40))

df.upper <- df.new[!is.na(df.new$Upper),]
str(df.upper)



par(mfrow = c(3,1), mar = c(2, 3, 0, 0) + 0.1, oma = c(0,0,0,0))
#head(df.new)
head(df.upper)
plot(df.new$Water.Level, df.new$Upper-df.new$Water.Temp, col = "lightblue", pch = ".", 
     ylim = c(0,20), xlim = c(.5,3.1)
     )
#lines(x = c(1.92, 1.92), y = c(0,40))
#lines(x = c(2.02, 2.02), y = c(0,40))
lines(x = c(1.97, 1.97), y = c(0,40))
plot(df.new$Water.Level, df.new$Mid-df.new$Water.Temp, pch = ".", col = "blue", 
     ylim = c(0,20), xlim = c(.5,3.1)
     )
#lines(x = c(1.5, 1.5), y = c(0,40))
#lines(x = c(1.6, 1.6), y = c(0,40))
lines(x = c(1.55, 1.55), y = c(0,40))
plot(df.new$Water.Level, df.new$Low-df.new$Water.Temp, pch = ".", col = "black", 
     ylim = c(0,20), xlim = c(.5,3.1)
     )
#lines(x = c(1.15, 1.15), y = c(0,40))
#lines(x = c(1.25, 1.25), y = c(0,40))
lines(x = c(1.20, 1.20), y = c(0,40))

 setwd("~/Box/Sarah and Molly's Box/FHL data/HOBO data/Tides")
 write.csv(file = "TempAndWaterLevel_QC.20210401.csv", df.new)


