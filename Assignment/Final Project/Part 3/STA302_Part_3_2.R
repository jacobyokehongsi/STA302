setwd("D:/UofT/Y3 Fall/STA302/Assignment/Final Project/Part 3")
library(dplyr)

data = data.frame(read.csv("owid-covid-data.csv"))
data_MYS = na.omit(data[(data$location=="Malaysia"), 
                        c("new_cases_per_million", 
                          "people_vaccinated_per_hundred", 
                          "stringency_index", "reproduction_rate", "date")])

data_MYS_cases = data_MYS$new_cases_per_million
data_MYS_vaccine = data_MYS$people_vaccinated_per_hundred
data_MYS_strin = data_MYS$stringency_index
data_date = data_MYS$date

data = data.frame(data_MYS_cases, data_MYS_vaccine, data_MYS_strin, data_date)
data$vaccine_effect <- as.numeric(ifelse(data$data_MYS_vaccine >= 60, 1, 0))
data_vaccine_effect = data$vaccine_effect

# train and test data ####################################################################

set.seed(1)
train <- data[sample(1:nrow(data), 139, replace=F), ]
test <- data[which(!(data$data_date %in% train$data_date)),]

#################################TRAIN##########################################

# summary stats
mtr <- apply(train[,-c(4)], 2, mean)
sdtr <- apply(train[,-c(4)], 2, sd)
mtest <- apply(test[,-c(4)], 2, mean)
sdtest <- apply(test[,-c(4)], 2, sd)

# EDA
# par(mfrow = c(1,3))
hist(train$data_MYS_cases, main='Histogram for New confirmed cases of 
     COVID-19 per 1,000,000 people', 
     ylab='New COVID-19 cases per million people')
hist(train$data_MYS_vaccine, main='Histogram for Total number of people who 
     received at least one vaccine dose per 100 
     people in the total population', 
     ylab='% of population that received at least 1 dose')
hist(train$data_MYS_strin, main='Histogram for the Stringency Index', 
     ylab='Stringency Index')


boxplot(train$data_MYS_vaccine, main='Boxplot for Total number of
     people who received at least one vaccine dose per 100 people in the total 
     population', 
        ylab='% of population that received at least 1 dose')
boxplot(train$data_MYS_cases, main='Boxplot for New confirmed cases of 
     COVID-19 per 1,000,000 people', 
        ylab='New COVID-19 cases per million people')
boxplot(train$data_MYS_strin, main='Boxplot for the Stringency Index', 
        ylab='Stringency Index')

# par(mfrow = c(1,2))
plot(train$data_MYS_cases~train$data_MYS_vaccine,
     xlab = 'Total number of people who received at least one vaccine dose per 
     100 people in the total population', 
     ylab = 'New COVID-19 cases per million people',
     main = 'Scatter plots for % of population that received at least 1 dose vs. 
     New COVID-19 cases per million people')

plot(train$data_MYS_cases~train$data_MYS_strin, 
     xlab = 'Stringency Index', 
     ylab = 'New COVID-19 cases per million people',
     main = 'Scatter plots for Stringency Index vs. New COVID-19 cases per 
     million people')

# plot(train$data_MYS_cases~train$vaccine_effect, 
#      xlab = 'Vaccination Effect', 
#      ylab = 'New COVID-19 cases per million people',
#      main = 'Scatter plots for Stringency Index vs. New COVID-19 cases per 
#      million people')


# model
full <- lm(data_MYS_cases ~ data_MYS_vaccine*vaccine_effect + vaccine_effect
           + data_MYS_strin, data=train[,-c(4)])
summary(full)

r <- rstandard(full)

# cond2
pairs(train[-c(1, 4, 5)])

# cond1
plot(train$data_MYS_cases ~ fitted(full), 
     main="Covid Cases vs Fitted Covid Cases", xlab="Fitted Covid Cases", 
     ylab="Covid Cases")
lines(lowess(train$data_MYS_cases ~ fitted(full)), lty=2)
abline(a = 0, b = 1)

# plot(r ~ fitted(full), main="title", xlab="Fitted", ylab="res.")
# plot(r ~ train$data_MYS_vaccine) #===============================================
# plot(r ~ train$data_MYS_strin)
# 
# qqnorm(r)
# qqline(r)

library(car)
p <- powerTransform(cbind(train[,1], train[,2], train[,3])~1, family="bcnPower")
summary(p)


# AFTER BOX COX
train$logdata_MYS_cases = log(train$data_MYS_cases)
train$corrected_data_MYS_vaccine = (train$data_MYS_vaccine)^0.33 #+============
train$logdata_MYS_strin = log(train$data_MYS_strin)

test$logdata_MYS_cases = log(test$data_MYS_cases)
test$corrected_data_MYS_vaccine = test$data_MYS_vaccine^0.33
test$logdata_MYS_strin = log(test$data_MYS_strin)

full2 <- lm(train$logdata_MYS_cases ~ train$corrected_data_MYS_vaccine 
            * train$vaccine_effect + train$vaccine_effect + train$logdata_MYS_strin)
summary(full2)

r <- rstandard(full2)

pairs(train[-c(1, 2, 3, 4, 5, 6)])

plot(train$logdata_MYS_cases ~ fitted(full2), 
     main="Covid Cases vs Fitted Covid Cases", xlab="Fitted Covid Cases", 
     ylab="Covid Cases")
abline(a = 0, b = 1)
lines(lowess(train$logdata_MYS_cases ~ fitted(full2)), lty=2)


plot(r ~ fitted(full2), xlab="Fitted Values", 
     ylab="residuals")
plot(r ~ train$corrected_data_MYS_vaccine, xlab="Transformed Vaccination Rate", 
     ylab="residuals") #========================================================
plot(r ~ train$logdata_MYS_strin, xlab="Transformed Stringency Index", 
     ylab="residuals")
plot(r ~ train$vaccine_effect, xlab="Vaccination Effect", 
     ylab="residuals")
# plot(r ~ data_MYS_reprod)
qqnorm(r)
qqline(r)

###########################VIF AND PROBLEMATIC OBS #############################
vif(full2)

# problematic obs
n <- length(train$logdata_MYS_cases)
p <- length(coef(full2))-1
# calculate the leverage values and compare to cutoff
h <- hatvalues(full2)
hcut <- 2*(p+1)/n


# leverage points?
w1 <- which(h > hcut)
w1

# par(mfrow=c(1,3))
plot(train[,6]~train[,7], xlab = 'Transformed Total number of people who received at least one vaccine dose per 
     100 people in the total population', 
     ylab = 'Transformed New COVID-19 cases per million people',
     main = 'Transformed Scatter plots for % of population that received at least 1 dose vs. 
     New COVID-19 cases per million people displaying Leverage Points')
points(train[w1,6]~train[w1,7], col="red", pch=19)

plot(train[,6]~train[,8], xlab = 'Transformed Stringency Index', 
     ylab = 'TransformedNew COVID-19 cases per million people',
     main = 'Transformed Scatter plots for Stringency Index vs. New COVID-19 cases per 
     million people displaying Leverage Points')
points(train[w1,6]~train[w1,8], col="red", pch=19)

# plot(train[,1]~train[,5], main="", xlab="", ylab="")
# points(train[w1,1]~train[w1,5], col="red", pch=19)


# outliers
r <- rstandard(full2)
w2 <- which(r < -2 | r > 2)
w2

# par(mfrow=c(1,3))
plot(train[,6]~train[,7], xlab = 'Transformed Total number of people who received at least one vaccine dose per 
     100 people in the total population', 
     ylab = 'Transformed New COVID-19 cases per million people',
     main = 'Transformed Scatter plots for % of population that received at least 1 dose vs. 
     New COVID-19 cases per million people displaying Outliers')
points(train[w2,6]~train[w2,7], col="red", pch=19)

plot(train[,6]~train[,8], xlab = 'Transformed Stringency Index', 
     ylab = 'TransformedNew COVID-19 cases per million people',
     main = 'Transformed Scatter plots for Stringency Index vs. New COVID-19 cases per 
     million people displaying Outliers')
points(train[w2,6]~train[w2,8], col="red", pch=19)

# plot(train[,6]~train[,5], main="", xlab="", ylab="")
# points(train[w2,6]~train[w2,5], col="red", pch=19)

# influential points

# find the cooks distance and compare to cutoff
Dcutoff <- qf(0.5, p+1, n-p-1)
D <- cooks.distance(full2)
which(D > Dcutoff)
## named integer(0)
# find the DFFITS and compare to cutoff
DFFITScut <- 2*sqrt((p+1)/n)
dfs <- dffits(full2)
w3 <- which(abs(dfs) > DFFITScut)
w3
# find the DFBETAS and compare to cutoff (notice the dimension of DFBETAS)
DFBETAcut <- 2/sqrt(n)
dfb <- dfbetas(full2)
w4 <- which(abs(dfb[,2]) > DFBETAcut)
w4
w5 <- which(abs(dfb[,4]) > DFBETAcut)
w5
w6 <- which(abs(dfb[,3]) > DFBETAcut)
w6

# stepwise

library(MASS)

# start with forward selection
stepAIC(lm(train$logdata_MYS_cases ~ train$corrected_data_MYS_vaccine 
           + train$logdata_MYS_strin + train$vaccine_effect 
           + train$corrected_data_MYS_vaccine * train$vaccine_effect), 
        scope=list(upper=lm(train$logdata_MYS_cases ~ 
                                    train$corrected_data_MYS_vaccine + 
                                    train$logdata_MYS_strin + 
                                    train$ vaccine_effect + 
                                    train$corrected_data_MYS_vaccine 
                            * train$vaccine_effect)), 
        direction = "forward", k=2)

###################################TEST########################################

full2_test <- lm(test$logdata_MYS_cases ~ test$corrected_data_MYS_vaccine 
                 + test$vaccine_effect + test$logdata_MYS_strin 
                 + test$corrected_data_MYS_vaccine * test$vaccine_effect)
summary(full2)
summary(full2_test)

r <- rstandard(full2_test)

pairs(test[-c(1, 2, 3, 4, 5, 6)])

plot(test$logdata_MYS_cases ~ fitted(full2_test), 
     main="Covid Cases vs Fitted Covid Cases", xlab="Fitted Covid Cases", 
     ylab="Covid Cases")
abline(a = 0, b = 1)
lines(lowess(test$logdata_MYS_cases ~ fitted(full2_test)), lty=2)


plot(r ~ fitted(full2_test), xlab="Fitted Values", ylab="residuals")
plot(r ~ test$corrected_data_MYS_vaccine, xlab="Transformed Vaccination Rate", 
     ylab="residuals")
plot(r ~ test$logdata_MYS_strin, xlab="Transformed Stringency Index", 
     ylab="residuals")
plot(r ~ test$vaccine_effect, xlab="Vaccination Effect", 
     ylab="residuals")
qqnorm(r)
qqline(r)


vif(full2_test)

# TEST DATA

# problematic obs

n <- length(test$data_MYS_cases)
p <- length(coef(full2_test))-1
# calculate the leverage values and compare to cutoff
h <- hatvalues(full2_test)
hcut <- 2*(p+1)/n


# leverage points?
w1 <- which(h > hcut)
w1

# par(mfrow=c(1,3))
plot(test[,6]~test[,7], xlab = 'Transformed Total number of people who received at least one vaccine dose per 
     100 people in the total population', 
     ylab = 'Transformed New COVID-19 cases per million people',
     main = 'Transformed Scatter plots for % of population that received at least 1 dose vs. 
     New COVID-19 cases per million people displaying Leverage Points')
points(test[w1,6]~test[w1,7], col="red", pch=19)

plot(test[,6]~test[,8], xlab = 'Transformed Stringency Index', 
     ylab = 'TransformedNew COVID-19 cases per million people',
     main = 'Transformed Scatter plots for Stringency Index vs. New COVID-19 cases per 
     million people displaying Leverage Points')
points(test[w1,6]~test[w1,8], col="red", pch=19)



# outliers
r <- rstandard(full2_test)
w2 <- which(r < -2 | r > 2)
w2

# par(mfrow=c(1,3))
plot(test[,6]~test[,7], xlab = 'Transformed Total number of people who received at least one vaccine dose per 
     100 people in the total population', 
     ylab = 'Transformed New COVID-19 cases per million people',
     main = 'Transformed Scatter plots for % of population that received at least 1 dose vs. 
     New COVID-19 cases per million people displaying Outliers')
points(test[w2,6]~test[w2,7], col="red", pch=19)

plot(test[,6]~test[,8], xlab = 'Transformed Stringency Index', 
     ylab = 'TransformedNew COVID-19 cases per million people',
     main = 'Transformed Scatter plots for Stringency Index vs. New COVID-19 cases per 
     million people displaying Outliers')
points(test[w2,6]~test[w2,8], col="red", pch=19)

# influential points

# find the cooks distance and compare to cutoff
Dcutoff <- qf(0.5, p+1, n-p-1)
D <- cooks.distance(full2_test)
which(D > Dcutoff)
## named integer(0)
# find the DFFITS and compare to cutoff
DFFITScut <- 2*sqrt((p+1)/n)
dfs <- dffits(full2_test)
w3 <- which(abs(dfs) > DFFITScut)
w3
# find the DFBETAS and compare to cutoff (notice the dimension of DFBETAS)
DFBETAcut <- 2/sqrt(n)
dfb <- dfbetas(full2_test)
w4 <- which(abs(dfb[,1]) > DFBETAcut)
w4
w5 <- which(abs(dfb[,2]) > DFBETAcut)
w5
w6 <- which(abs(dfb[,3]) > DFBETAcut)
w6
w7 <- which(abs(dfb[,5]) > DFBETAcut)
w7

