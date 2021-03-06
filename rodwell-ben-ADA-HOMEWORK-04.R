library(resample)
library(dplyr)
library(data.table)

## Load Data

library(dplyr)
library(readr)
library(curl)
f <- curl("https://raw.githubusercontent.com/difiore/ADA-2019/master/KamilarAndCooperData.csv")
d <- read.csv(f, header = TRUE, sep = ",", stringsAsFactors = TRUE)
head(d)



d <- read.csv("KamilarAndCooperData.csv", header = T, sep = ",")
n <- length(d[,1])
## Part 1
## Run the Linear Model
model <- lm(data = d, formula = log(HomeRange_km2) ~ log(Body_mass_female_mean))
model
summary(model)
par(mfrow = c(2,2))
plot(model)
par(mfrow = c(1,1))


# Coefficients
model$coefficients




## PART 2
## Bootstrap the data
### code to resample the dataset
n <- length(d[,1]) # Number of observations
dsample <- d[sample(nrow(d), n, replace = T), ]
head(dsample)
str(dsample)

## Loop to repeat it 1000 times while storing the beta coefficients
n <- length(d[,1]) # Number of observations/rows in the dataset
beta0 <- rep(0,1000) 
beta1 <- rep(0,1000)
for (i in 1:1000){
  dsample <- d[sample(nrow(d), n, replace = T), ]
  model <- lm(data = dsample, formula = log(HomeRange_km2) ~ log(Body_mass_female_mean))
  beta0[i] <- model$coefficients[1]
  beta1[i] <- model$coefficients[2]
  }
head(beta1)
head(beta0)

## Histograms of the bootstrapped beta coefficients
par(mfrow = c(1,2))
hist(beta0, breaks = 20)
hist(beta1, breaks = 20)
par(mfrow = c(1,1))
```

confint(model)

# [3]
## Estimate the standard error for each of your β coefficients
## as the standard deviation of the sampling distribution from your bootstrap.

SEbeta0 <- sd(beta0)
SEbeta0
SEbeta1 <- sd(beta1)
SEbeta1


# [4]
## Also determine the 95% CI for each of your β coefficients based
## on the appropriate quantiles from your sampling distribution.


beta0confupper <- mean(beta0) + qnorm(0.975) * SEbeta0
beta0conflower <- mean(beta0) - qnorm(0.975) * SEbeta0
beta0conf <- cbind(beta0conflower, beta0confupper)
beta0conf

beta1confupper <- mean(beta1) + qnorm(0.975) * SEbeta1
beta1conflower <- mean(beta1) - qnorm(0.975) * SEbeta1
beta1conf <- cbind(beta1conflower, beta1confupper)
beta1conf 

# [5]
## How does your answer to part [3] compare to the SE estimated
## from your entire dataset using the formula for standard error implemented in lm()?


# [6]
## How does your answer to part [4] compare to the 95% CI estimated from your entire dataset?



