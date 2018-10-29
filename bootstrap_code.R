setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# This neat snippet sets your working directory to wherever the script is located!

## Reading in the data
## https://electionstudies.org/project/2016-time-series-study/
dat = readstata13::read.dta13("anes_timeseries_2016.dta")
dat = dat[dat$V161011 >= 0 & dat$V161267 > 0,]

## Calculating a registration variable
dat$registered = ifelse(dat$V161011 %in% c(1,2), 1, 0)


X = mean(dat$registered)

SD = sd(dat$registered)

SE = SD/sqrt(nrow(dat))

X + 1.96*SE # 0.8683307
X - 1.96*SE # 0.8473624


## A bootstrapping approach:

## First, we resample and take a statistic
bootstrapped.data = mosaic::shuffle(dat, replace=TRUE)
mean(bootstrapped.data$registered)

## We can wrap that in a replicate() to repeat the process 100 times
bootstrapped.estimates = replicate(100, {
  bootstrapped.data = mosaic::shuffle(dat, replace=TRUE)
  mean(bootstrapped.data$registered)
})

hist(bootstrapped.estimates, breaks=10)

quants = quantile(bootstrapped.estimates, c(0.025, 0.975))
quants

abline(v=as.numeric(quants), col="red", lty="dashed", lwd=2)

## Why do we sample with replacement?



## A bootstrap for more complicated stuff:
dat$age = dat$V161267

hist(dat$age, main="Distribution of Age in the ANES", xlab="Age", breaks=40)

cor(dat$age, dat$registered) # 0.199

# How would we get a confidence interval for that correlation?

# We only need to change the second part of our bootstrap
bootstrapped.corr = replicate(100, {
  bootstrapped.data = mosaic::shuffle(dat, replace=TRUE)
   cor(bootstrapped.data$age, bootstrapped.data$registered)
})

hist(bootstrapped.corr, breaks=10)

corrs = quantile(bootstrapped.corr, c(0.025, 0.975))
corrs

abline(v=as.numeric(corrs), col="red", lty="dashed", lwd=2)

## What is our interpretation of these bounds?