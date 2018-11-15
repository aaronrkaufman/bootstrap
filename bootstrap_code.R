setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# This neat snippet sets your working directory to wherever the script is located!
set.seed(02138)

## Libraries:
library(mosaic)
library(readstata13)

## Reading in the data
## https://electionstudies.org/project/2016-time-series-study/
## https://github.com/aaronrkaufman/bootstrap/anes_timeseries_2016.dta

dat = readstata13::read.dta13("anes_timeseries_2016.dta")
dat = dat[dat$V161011 >= 0 & dat$V161267 > 0,]

## Calculating a registration variable
dat$registered = ifelse(dat$V161011 %in% c(1,2), 1, 0)


X = mean(dat$registered)
X

SD = sd(dat$registered)
SD

SE = SD/sqrt(nrow(dat))
SE

X + 1.96*SE # 0.8683307
X - 1.96*SE # 0.8473624
analytic.ses = c(X + 1.96*SE, X - 1.96*SE)

## A bootstrapping approach:

## First, we resample and take a statistic
?mosaic::shuffle


bootstrapped.data = mosaic::shuffle(dat, replace=TRUE)
mean(bootstrapped.data$registered)

## We can wrap that in a replicate() to repeat the process 100 times
?replicate

# We need to tell replicate how many samples to take
bootstrapped.estimates = replicate(1000, {
  bootstrapped.data = mosaic::shuffle(dat, replace=TRUE)
  mean(bootstrapped.data$registered)
})

quants = quantile(bootstrapped.estimates, c(0.025, 0.975))
quants # 0.846, 0.867

png("hist1.jpg")
hist(bootstrapped.estimates, breaks=15, 
     xlab="Bootstrapped Means")
dev.off()

png("hist2.jpg")
hist(bootstrapped.estimates, breaks=15, 
     xlab="Bootstrapped Means")
abline(v=as.numeric(analytic.ses), col="blue", lty="dashed", lwd=2)
dev.off()


bootstrapped.estimates = replicate(50, {
  bootstrapped.data = mosaic::shuffle(dat, replace=TRUE)
  mean(bootstrapped.data$registered)
})
quants = quantile(bootstrapped.estimates, c(0.025, 0.975))
png("hist1.jpg")
hist(bootstrapped.estimates, breaks=15, 
     xlab="Bootstrapped Means", main = "50 Bootstraps", xlim=c(0.84, 0.88))
abline(v=as.numeric(quants), col="red", lty="dashed", lwd=2)
abline(v=as.numeric(analytic.ses), col="blue", lty="dashed", lwd=2)
dev.off()



bootstrapped.estimates = replicate(1000, {
  bootstrapped.data = mosaic::shuffle(dat, replace=TRUE)
  mean(bootstrapped.data$registered)
})
quants = quantile(bootstrapped.estimates, c(0.025, 0.975))
png("hist2.jpg")
hist(bootstrapped.estimates, breaks=15, 
     xlab="Bootstrapped Means", main = "1,000 Bootstraps", xlim=c(0.84, 0.88))
abline(v=as.numeric(quants), col="red", lty="dashed", lwd=2)
abline(v=as.numeric(analytic.ses), col="blue", lty="dashed", lwd=2)
dev.off()



bootstrapped.estimates = replicate(10000, {
  bootstrapped.data = mosaic::shuffle(dat, replace=TRUE)
  mean(bootstrapped.data$registered)
})
quants = quantile(bootstrapped.estimates, c(0.025, 0.975))
png("hist3.jpg")
hist(bootstrapped.estimates, breaks=15, 
     xlab="Bootstrapped Means", main = "10,000 Bootstraps", xlim=c(0.84, 0.88))
abline(v=as.numeric(quants), col="red", lty="dashed", lwd=2)
abline(v=as.numeric(analytic.ses), col="blue", lty="dashed", lwd=2)
dev.off()


bootstrapped.estimates = replicate(10000, {
  bootstrapped.data = mosaic::shuffle(dat, replace=TRUE)
  mean(bootstrapped.data$registered)
})
qs = quantile(bootstrapped.estimates, c(0.005, 0.025, 0.975, 0.995))
png("bootdist0.jpg")
hist(bootstrapped.estimates, breaks=50, 
     xlab="Bootstrapped Means", main = NULL, xlim=c(0.84, 0.88),
     yaxt='n', ylab=NULL)
dev.off()

png("bootdist1.jpg")
hist(bootstrapped.estimates, breaks=50, 
     xlab="Bootstrapped Means", main = NULL, xlim=c(0.84, 0.88),
     yaxt='n', ylab=NULL)
abline(v=as.numeric(qs), col=c("lightblue", "darkblue", "darkblue", "lightblue"),
       lty="dashed", lwd=3)
dev.off()




## Why do we sample with replacement?



## A bootstrap for more complicated stuff:
dat$age = dat$V161267

hist(dat$age, main="Distribution of Age in the ANES", xlab="Age", breaks=40)

cor(dat$age, dat$registered)

# How would we get a confidence interval for that correlation?

bootstrapped.data = mosaic::shuffle(dat, replace=TRUE)
cor(bootstrapped.data$age, bootstrapped.data$registered)


# We only need to change the second part of our bootstrap
bootstrapped.corr = replicate(100, {
  bootstrapped.data = mosaic::shuffle(dat, replace=TRUE)
   cor(bootstrapped.data$age, bootstrapped.data$registered)
})

hist(bootstrapped.corr, breaks=10,
     xlab="Bootstrapped Correlations")

abline(v=as.numeric(corrs), col="red", lty="dashed", lwd=2)

corrs = quantile(bootstrapped.corr, c(0.025, 0.975))
corrs

## EVEN MORE complicated stuff:
## How certain are we about our predictive models?

bootstrapped.corr = replicate(100, {
  bootstrapped.data = mosaic::shuffle(dat[1:4000], replace=TRUE)
  m = lm(registered ~ age + race + gender + partyID + age*race + age*income, bootstrapped.data)
  preds = predict(m, newdata = dat[4001:4142])
  accuracy = mean(preds == dat$registered[4001:4142])
})
