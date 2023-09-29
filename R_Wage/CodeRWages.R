#Wage Codes for R

##House Keeping--------------------------
rm(list=ls())
#workpath <- getwd()
setwd("C:/Users/teohhongken/Desktop/Summer2014/Research/DieboldResearch/R_Wage")

wage95 <- read.csv("DataWages.csv", header=TRUE)
attach(wage95)


##Packages------------------------------
library(lmtest)
library(sandwich)

##Figure 2.5----------------------------

#pdf(file="Fig 2.5.pdf")

#Draw Histograms for WAGE and LNWAGE
par(mfrow=c(3,2))

hist(WAGE, breaks=28, main="")
hist(LNWAGE, breaks=40, main="")

#Draw Kernel Density Plot for WAGE and LNWAGE 
wage.density <- density(WAGE) #returns the density data for WAGE
wage.normal <- density(rnorm(1000000, mean=mean(WAGE), sd=sd(WAGE))) #returns the density data for normal distribution from the WAGE data
plot(wage.density, col="blue", main="", xlab="Wage", ylab="Density") + points(wage.normal, type="l", col="red")

lnwage.density <- density(LNWAGE) #returns the density data for LNWAGE
lnwage.normal <- density(rnorm(1000000, mean=mean(LNWAGE), sd=sd(LNWAGE))) #returns the density data for normal distribution from the LNWAGE data
plot(lnwage.density, col="blue", main="", xlab="Log Wage", ylab="Density", ylim=c(0,.8)) + points(lnwage.normal, type="l", col="red")

#Draw QQ-Plot for WAGE and LNWAGE
qqnorm(WAGE, pch=20, col="red", main=""); qqline(WAGE)
qqnorm(LNWAGE, pch=20, col="red", main=""); qqline(LNWAGE)

#dev.off()

par(mfrow=c(1,1))

##Figure 4.1---------------------------

#pdf(file="Fig 4.1.pdf",height=6,width=7.5)

par(mfrow=c(3,1))
#First Figure for LNWAGE Histogram
hist(LNWAGE, breaks=40, main="")

#Second Figure for EDUC Histogram
hist(EDUC, breaks=40, main="")

#Third Figure for EXPER Histogram
hist(EXPER, breaks=25, main="")

#dev.off()
par(mfrow=c(1,1))
##Figure 4.2---------------------------
#pdf(file="Fig 4.2.pdf",height=6,width=7.5)
plot(EDUC, LNWAGE, col="blue")
#dev.off()

##Figure 4.3---------------------------
#pdf(file="Fig 4.3.pdf",height=6,width=7.5)
plot(EDUC, LNWAGE, col="blue") + abline(reg=lm(LNWAGE~EDUC), col="red")
#dev.off()

##Figure 4.4----------------------------
reg.lnwage.by.educ.exper <- lm(LNWAGE~EDUC + EXPER)
summary(reg.lnwage.by.educ.exper)


##Figure 4.5---------------------------
#pdf(file="Fig 4.5.pdf")
fit.lnwage.by.educ.exper <- fitted.values(reg.lnwage.by.educ.exper)
plot(fit.lnwage.by.educ.exper, LNWAGE, col="blue", xlab="FIT", xlim=c(0,5), ylim=c(0,5))
#dev.off()

##Figure 4.6---------------------------

lnwage.actual <- LNWAGE
lnwage.pred <- predict(reg.lnwage.by.educ.exper, wage95)
lnwage.resid <- lnwage.actual - lnwage.pred

#pdf(file="Fig 4.6.pdf")
plot(lnwage.actual, type="l", axes=FALSE, xlim=c(0,1300), ylim=c(-2,5), xlab="", ylab="", col="red")+lines(lnwage.pred, col="darkgreen")+lines(lnwage.resid, col="blue")
box()
axis(side=1, at=c(0,250,500,750,1000,1250))
axis(side=2, at=c(-2,-1,0,1,2))
axis(side=4, at=c(0,1,2,3,4,5))
par(xpd=TRUE)
legend(150, -5,inset=c(-0.2,0), legend=c("Residual","Actual","Fitted"),lty=c(1,1),col=c("blue","red","green"), cex=0.7, horiz=TRUE)
#dev.off()

##Figure 5.1---------------------------

#pdf(file="Fig 5.1.pdf")
par(mfrow=c(2,2))
#Histogram for EDUC
hist(EDUC, breaks=40)

#Histogram for EXPER
hist(EXPER, breaks=30)

#Histogram for NONWHITE
hist(NONWHITE)

#Histogram for UNION
hist(UNION)
#dev.off()
par(mfrow=c(1,1))
##Figure 5.2--------------------------
summary(reg.lnwage.by.educ.exper)


##Figure 5.3--------------------------
reg.lnwage.by.educ.exper.female.nonwhite.union <- lm(LNWAGE~.-AGE-WAGE, data=wage95)
summary(reg.lnwage.by.educ.exper.female.nonwhite.union)


##Figure 5.4---------------------------
fit.lnwage.by.educ.exper.female.nonwhite.union <- fitted.values(reg.lnwage.by.educ.exper.female.nonwhite.union)

#pdf(file="Fig 5.4.pdf")
plot(fit.lnwage.by.educ.exper.female.nonwhite.union, LNWAGE, col="blue",xlab="FIT", xlim=c(0,5), ylim=c(0,5))
#dev.off()

##Figure 5.5---------------------------
time.sample <- seq(from=0, to=100, by=0.5)
trend.sample <- 10-0.25*time.sample
BB <- seq(from=-50, to=30, by=0.4)
plot(time.sample, trend.sample, type="l", col="red", xlim=c(0,100), ylim=c(-60,40), xlab="Time", ylab="Trend")# + abline(a=-50, b=.8, col="blue")
lines(time.sample, BB, col="blue")

##Figure 7.1----------------------------
summary(reg.lnwage.by.educ.exper.female.nonwhite.union)


##Figure 7.2----------------------------
EDUC2 <- EDUC^2 #create a vector of EDUC^2
EXPER2 <- EXPER^2 #create a vector of EXPER^2

reg.lnwage.quadratic <- lm(LNWAGE~EDUC+EDUC2+EXPER+EXPER2+EDUC:EXPER+FEMALE+UNION+NONWHITE)
summary(reg.lnwage.quadratic)


##Figure 7.3----------------------------
reg.lnwage.educ.exper.dummy.interact <- lm(LNWAGE~EDUC+EXPER+FEMALE+UNION+NONWHITE+FEMALE:UNION+FEMALE:NONWHITE+UNION:NONWHITE)
summary(reg.lnwage.educ.exper.dummy.interact)


##Figure 7.4----------------------------
reg.lnwage.quadratic.continter.discreteinter <- lm(LNWAGE~EDUC+EDUC2+EXPER+EXPER2+EDUC:EXPER+FEMALE+UNION+NONWHITE+FEMALE:UNION+FEMALE:NONWHITE+UNION:NONWHITE)
summary(reg.lnwage.quadratic.continter.discreteinter)


##Figure 7.5----------------------------
fig76.a <- 10+0.3*time.sample+0.3*time.sample^2
plot(time.sample, fig76.a, type="l", xlab="", ylab="", main="TREND = 10 + .3TIME + .3TIME2")

fig76.b <- 10+30*time.sample-0.3*time.sample^2
plot(time.sample, fig76.b, type="l", xlab="", ylab="", main="TREND = 10 + 30TIME + .3TIME2")

fig76.c <- 10-.4*time.sample-.4*time.sample^2
plot(time.sample, fig76.c, type="l", xlab="", ylab="", main="TREND = 10 - .4TIME - .4TIME2")

fig76.d <- 10-.25*time.sample+.3*time.sample^2
plot(time.sample, fig76.d, type="l", xlab="", ylab="", main="TREND = 10 - .25TIME + .3TIME2")


##Figure 9.x (Codes repeat throughout ch9)-----------------------------
reg.lnwage.quadratic2 <- lm(LNWAGE~EDUC+EXPER+EXPER2+EDUC:EXPER+FEMALE+UNION+NONWHITE)
summary(reg.lnwage.quadratic2)


##Figure 9.x-----------------------------
resid.lnwage.quadratic2 <- resid(reg.lnwage.quadratic2)
resid.lnwage.quadratic2.squared <- resid.lnwage.quadratic2^2
plot(EDUC, resid.lnwage.quadratic2.squared, col="blue", xlab="EDUC", ylab="RESID2")



