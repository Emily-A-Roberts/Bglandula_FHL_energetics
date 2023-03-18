#Example of confidence intervals on mixed model
#https://stats.stackexchange.com/questions/231074/confidence-intervals-on-predictions-for-a-non-linear-mixed-model-nlme

library(effects)
library(nlme)
library(MASS)
library(rootSolve)

logistic <- function (x, times) {
  
  with (as.list(x),
        {
          N <- K / (1+(K-N0)/N0*exp(-r*times))
          return(c(N = N))
        })
}

# parameters for the US population from 1900
x <- c(N0 = 76.1, r = 0.02, K = 500)

# Sensitivity function: SF: dfi/dxj at
# output intervals from 1900 to 1950
SF <- gradient(f = logistic, x, times = 0:50)

# AQR_T ####
coef(AQR_T)

set.seed(6)
df=data.frame(y=c(1:5+runif(5,0,1),21:25+runif(5,1,5)),
              x=rep(1:5,2),entry=rep(letters[1:2],each=5))

#Group data by entry
df=groupedData(y~x|entry,data=df)

#Build model
mod=nlme(y~a+b*x,fixed=a+b~1,random=~b~1,start=c(a=1,b=1),data=df)

#Create data frame for predictions
pred=do.call(rbind,lapply(rownames(coef(mod)),function(i) {
  data.frame(x=seq(0.1,max(df[df$entry==i,"x"]),0.1),entry=i) } ) )
i <- 1
pred=do.call(rbind,lapply(rownames(coef(AQR_T)),function(i) {
  data.frame(Temp.n=seq(5,max(resp_aq[resp_aq$ID==i,"Temp.n"]),0.1),entry=i) } ) )


#Grab coefficients from model for a and b
a=coef(mod)[match(pred$entry,rownames(coef(mod))),1]
b=coef(mod)[match(pred$entry,rownames(coef(mod))),2]

b=coef(AQR_T)[match(pred$entry,rownames(coef(AQR_T))),1]
a=coef(AQR_T)[match(pred$entry,rownames(coef(AQR_T))),2]


#Grab fixed coefficients for overall trend and fitted values for individual entries using a and b above
# pred$fixed=fixef(mod)[1]+fixef(mod)[2]*pred$x 
# pred$fitted=a+pred$x*b

5.25^1.88
mean(resp_aq$Operc)

head(pred)
#note fixef(AQR_T)[2] is c but also the multiplier or scalar
pred$fixed=fixef(AQR_T)[2]*exp(pred$Temp.n*fixef(AQR_T)[1])*22.6
pred$fitted=a*exp(pred$Temp.n*b)*22.6

library(AICcmodavg)
predictSE(AQR_T)
#Get SEs on predictions using code from Ben Bolker's website
predvar=diag(as.matrix(data.frame(b,a)) %*% vcov(AQR_T)%*% t(as.matrix(data.frame(b,a))))
pred$fixed.SE=sqrt(predvar)
pred$fixed.SE2=sqrt(predvar+AQR_T$sigma^2)

library(ggplot2)
head(pred)
ggplot()+geom_line(data=pred,aes(x=Temp.n,y=fixed),col="grey50",alpha=0.5)+
geom_line(data=pred,aes(x=Temp.n,y=fixed+fixed.SE2),col="black",alpha=0.5)+
geom_line(data=pred,aes(x=Temp.n,y=fixed-fixed.SE2),col="black",alpha=0.5) +
  geom_point(data = resp_aq, aes(Temp.n,Respiration.Rate))+
  geom_line(data=pred,aes(x=Temp.n,y=fixed),col="blue",alpha=0.5)+
  geom_line(data=pred,aes(x=Temp.n,y=fixed),col="blue",alpha=0.5) 
head(resp_aq)
  
geom_line(data=subset(pred,max(df$Temp.n)>Temp.n & Temp.n>1),aes(Temp.n=Temp.n,y=fitted,group=entry),col="grey50")+
  geom_ribbon(data=subset(pred,max(df$x)>x & x>1),aes(x=x,ymin=fixed-2*fixed.SE,ymax=fixed+2*fixed.SE),alpha=0.5,fill="grey10")+
  geom_ribbon(data=subset(pred,max(df$x)>x & x>1),aes(x=x,ymin=fixed-2*fixed.SE2,ymax=fixed+2*fixed.SE2),alpha=0.3,fill="grey50")+
  geom_line(data=subset(pred,max(df$x)>x & x>1),aes(x=x,y=fixed),col="black",lwd=1.1)


coef(AQR_T)

set.seed(6)
df=data.frame(y=c(1:5+runif(5,0,1),21:25+runif(5,1,5)),
              x=rep(1:5,2),entry=rep(letters[1:2],each=5))

#Group data by entry
df=groupedData(y~x|entry,data=df)

#Build model
mod=nlme(y~a+b*x,fixed=a+b~1,random=~b~1,start=c(a=1,b=1),data=df)

#Create data frame for predictions
pred=do.call(rbind,lapply(rownames(coef(mod)),function(i) {
  data.frame(x=seq(0.1,max(df[df$entry==i,"x"]),0.1),entry=i) } ) )
i <- 1
pred=do.call(rbind,lapply(rownames(coef(AQR_T)),function(i) {
  data.frame(Temp.n=seq(5,max(resp_aq[resp_aq$ID==i,"Temp.n"]),0.1),entry=i) } ) )


#Grab coefficients from model for a and b
a=coef(mod)[match(pred$entry,rownames(coef(mod))),1]
b=coef(mod)[match(pred$entry,rownames(coef(mod))),2]

b=coef(AQR_T)[match(pred$entry,rownames(coef(AQR_T))),1]
a=coef(AQR_T)[match(pred$entry,rownames(coef(AQR_T))),2]


#Grab fixed coefficients for overall trend and fitted values for individual entries using a and b above
# pred$fixed=fixef(mod)[1]+fixef(mod)[2]*pred$x 
# pred$fitted=a+pred$x*b

5.25^1.88
mean(resp_aq$Operc)

head(pred)
#note fixef(AQR_T)[2] is c but also the multiplier or scalar
pred$fixed=fixef(AQR_T)[2]*exp(pred$Temp.n*fixef(AQR_T)[1])*22.6
pred$fitted=a*exp(pred$Temp.n*b)*22.6

library(AICcmodavg)
predictSE(AQR_T)
#Get SEs on predictions using code from Ben Bolker's website
predvar=diag(as.matrix(data.frame(b,a)) %*% vcov(AQR_T)%*% t(as.matrix(data.frame(b,a))))
pred$fixed.SE=sqrt(predvar)
pred$fixed.SE2=sqrt(predvar+AQR_T$sigma^2)

library(ggplot2)
head(pred)
ggplot()+geom_line(data=pred,aes(x=Temp.n,y=fixed),col="grey50",alpha=0.5)+
  geom_line(data=pred,aes(x=Temp.n,y=fixed+fixed.SE2),col="black",alpha=0.5)+
  geom_line(data=pred,aes(x=Temp.n,y=fixed-fixed.SE2),col="black",alpha=0.5) +
  geom_point(data = resp_aq, aes(Temp.n,Respiration.Rate))+
  geom_line(data=pred,aes(x=Temp.n,y=fixed),col="blue",alpha=0.5)+
  geom_line(data=pred,aes(x=Temp.n,y=fixed),col="blue",alpha=0.5) 
head(resp_aq)

geom_line(data=subset(pred,max(df$Temp.n)>Temp.n & Temp.n>1),aes(Temp.n=Temp.n,y=fitted,group=entry),col="grey50")+
  geom_ribbon(data=subset(pred,max(df$x)>x & x>1),aes(x=x,ymin=fixed-2*fixed.SE,ymax=fixed+2*fixed.SE),alpha=0.5,fill="grey10")+
  geom_ribbon(data=subset(pred,max(df$x)>x & x>1),aes(x=x,ymin=fixed-2*fixed.SE2,ymax=fixed+2*fixed.SE2),alpha=0.3,fill="grey50")+
  geom_line(data=subset(pred,max(df$x)>x & x>1),aes(x=x,y=fixed),col="black",lwd=1.1)











# Now for aerial resp

max(resp_exp$Temp.n)
x <- c(a = as.numeric(coef(AER_EXPOSE_T_30)[1,2]), c = as.numeric(coef(AQR_T)[1,1]))
# (SF <- gradient(f = AER_EXPOSE_T_30_fun, temp = 5:40, size = 5, x))

#Create data frame for predictions
head(df)
pred=do.call(rbind,lapply(rownames(coef(AER_EXPOSE_T_30)),function(i) {
  data.frame(Temp.n=seq(5,max(df[df$entry==i,"x"]),0.1),entry=i) } ) )
#Grab coefficients from model for a and b
a=coef(mod)[match(pred$entry,rownames(coef(mod))),1]
b=coef(mod)[match(pred$entry,rownames(coef(mod))),2]
#Grab fixed coefficients for overall trend and fitted values for individual entries using a and b above
pred$fixed=fixef(mod)[1]+fixef(mod)[2]*pred$x 
pred$fitted=a+pred$x*b

SSexp(x, b, y0)

AQR_T <- nlme(Respiration.Rate ~ c * exp(a * Temp.n) * Operc^1.88,
              data = resp,
              fixed = a + c ~ 1,
              random = c ~ 1,
              groups = ~ ID,
              start = c(a = 0.0547657, c = 0.0003792))

install.packages("drLumi")
remotes::install_version("drLumi",version="0.1.2")
AQR_T <- nlme(Respiration.Rate ~ SSexp(Temp.n, b, y0) * Operc^1.88,
              data = resp_aq,
              fixed = b + y0 ~ 1,
              random = y0 ~ 1,
              groups = ~ ID,
              start = c(b = 0.0547657, y0 = 0.0003792))
summary(AQR_T) 
AQR_T_a <- AQR_T$coefficients$fixed[[1]]
AQR_T_c <- AQR_T$coefficients$fixed[[2]]

AQR_T_fun <- function(temp, size, c = AQR_T_c, a = AQR_T_a) {
  out <- c * exp(a * temp) * size^1.88
  return(out)
}




fm1 <- nlme(height ~ SSasymp(age, Asym, R0, lrc),
            data = Loblolly,
            fixed = Asym + R0 + lrc ~ 1,
            random = Asym ~ 1,
            start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
head(Loblolly)

xvals=seq(min(Loblolly$age),max(Loblolly$age),length.out=100)
nresamp=1000
pars.picked = mvrnorm(nresamp, mu = fixef(fm1), Sigma = vcov(fm1)) # pick new parameter values by sampling from multivariate normal distribution based on fit
yvals = matrix(0, nrow = nresamp, ncol = length(xvals))

for (i in 1:nresamp) 
{
  yvals[i,] = sapply(xvals,function (x) SSasymp(x,pars.picked[i,1], pars.picked[i,2], pars.picked[i,3]))
} 

quant = function(col) quantile(col, c(0.025,0.975)) # 95% percentiles
conflims = apply(yvals,2,quant) # 95% confidence intervals

meany = sapply(xvals,function (x) SSasymp(x,fixef(fm1)[[1]], fixef(fm1)[[2]], fixef(fm1)[[3]]))

par(cex.axis = 2.0, cex.lab=2.0)
plot(0, type='n', xlim=c(3,25), ylim=c(0,65), axes=F, xlab="age", ylab="height");
axis(1, at=c(3,1:5 * 5), labels=c(3,1:5 * 5)) 
axis(2, at=0:6 * 10, labels=0:6 * 10)   

for(i in 1:14)
{
  data = subset(Loblolly, Loblolly$Seed == unique(Loblolly$Seed)[i])   
  lines(data$age, data$height, col = "red", lty=3)
}

lines(xvals,meany, lwd=3)
lines(xvals,conflims[1,])

#Answer

library(nlme)
library(MASS)

fm1 <- nlme(height ~ SSasymp(age, Asym, R0, lrc),
            data = Loblolly,
            fixed = Asym + R0 + lrc ~ 1,
            random = Asym ~ 1,
            start = c(Asym = 103, R0 = -8.5, lrc = -3.3))

xvals <-  with(Loblolly,seq(min(age),max(age),length.out=100))
nresamp <- 1000
## pick new parameter values by sampling from multivariate normal distribution based on fit
pars.picked <- mvrnorm(nresamp, mu = fixef(fm1), Sigma = vcov(fm1))

## predicted values: useful below
pframe <- with(Loblolly,data.frame(age=xvals))
pframe$height <- predict(fm1,newdata=pframe,level=0)

## utility function
get_CI <- function(y,pref="") {
  r1 <- t(apply(y,1,quantile,c(0.025,0.975)))
  setNames(as.data.frame(r1),paste0(pref,c("lwr","upr")))
}

set.seed(101)
yvals <- apply(pars.picked,1,
               function(x) { SSasymp(xvals,x[1], x[2], x[3]) }
)
c1 <- get_CI(yvals)

## bootstrapping
sampfun <- function(fitted,data,idvar="Seed") {
  pp <- predict(fitted,levels=1)
  rr <- residuals(fitted)
  dd <- data.frame(data,pred=pp,res=rr)
  ## sample groups with replacement
  iv <- levels(data[[idvar]])
  bsamp1 <- sample(iv,size=length(iv),replace=TRUE)
  bsamp2 <- lapply(bsamp1,
                   function(x) {
                     ## within groups, sample *residuals* with replacement
                     ddb <- dd[dd[[idvar]]==x,]
                     ## bootstrapped response = pred + bootstrapped residual
                     ddb$height <- ddb$pred +
                       sample(ddb$res,size=nrow(ddb),replace=TRUE)
                     return(ddb)
                   })
  res <- do.call(rbind,bsamp2)  ## collect results
  if (is(data,"groupedData"))
    res <- groupedData(res,formula=formula(data))
  return(res)
}

pfun <- function(fm) {
  predict(FR_T, newdata = pred.dat.aqua, level = 0)
}
set.seed(101)
yvals2 <- replicate(nresamp,
                    pfun(update(FR_T,data=sampfun(FR_T,feed,"ID"))))
c2 <- get_CI(yvals2,"boot_")


# pfun <- function(fm) {
#   predict(fm,newdata=pframe,level=0)
# }

set.seed(101)
replicate?
yvals2 <- replicate(nresamp,
                    pfun(update(fm1,data=sampfun(fm1,Loblolly,"Seed"))))
c2 <- get_CI(yvals2,"boot_")

## delta method
ss0 <- with(as.list(fixef(fm1)),SSasymp(xvals,Asym,R0,lrc))
str(ss0)
gg <- attr(ss0,"gradient")
V <- vcov(fm1)
delta_sd <- sqrt(diag(gg %*% V %*% t(gg)))
c3 <- with(pframe,data.frame(delta_lwr=height-1.96*delta_sd,
                             delta_upr=height+1.96*delta_sd))

pframe <- data.frame(pframe,c1,c2,c3)

library(ggplot2); theme_set(theme_bw())
ggplot(Loblolly,aes(age,height))+
  geom_line(alpha=0.2,aes(group=Seed))+
  geom_line(data=pframe,col="red")+
  geom_ribbon(data=pframe,aes(ymin=lwr,ymax=upr),colour=NA,alpha=0.3,
              fill="blue")+
  geom_ribbon(data=pframe,aes(ymin=boot_lwr,ymax=boot_upr),
              colour=NA,alpha=0.3,
              fill="red")+
  geom_ribbon(data=pframe,aes(ymin=delta_lwr,ymax=delta_upr),
              colour=NA,alpha=0.3,
              fill="cyan")


ggplot(Loblolly,aes(age))+
  geom_hline(yintercept=0,lty=2)+
  geom_ribbon(data=pframe,aes(ymin=lwr-height,ymax=upr-height),
              colour="blue",
              fill=NA)+
  geom_ribbon(data=pframe,aes(ymin=boot_lwr-height,ymax=boot_upr-height),
              colour="red",
              fill=NA)+
  geom_ribbon(data=pframe,aes(ymin=delta_lwr-height,ymax=delta_upr-height),
              colour="cyan",
              fill=NA)
lines(xvals,conflims[2,])