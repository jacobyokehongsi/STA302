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



summary(data_MYS_vaccine)
summary(data_MYS_cases)
summary(data_MYS_strin)
summary(data_vaccine_effect)

sd(data_MYS_vaccine)
sd(data_MYS_cases)
sd(data_MYS_strin)
sd(data_vaccine_effect)


# par(mfrow = c(2,2))
hist(data_MYS_vaccine, main='Histogram for Total number of
     people who received at least one vaccine dose per 100 people in the total 
     population', 
     ylab='% of population that received at least 1 dose')
hist(data_MYS_cases, main='Histogram for New confirmed cases of 
     COVID-19 per 1,000,000 people corresponding to vaccination', 
     ylab='New COVID-19 cases per million people w.r.t to vaccination')
hist(data_MYS_strin, main='Histogram for the Stringency Index', 
     ylab='Stringency Index')


boxplot(data_MYS_vaccine, main='Boxplot for Total number of
     people who received at least one vaccine dose per 100 people in the total 
     population', 
        ylab='% of population that received at least 1 dose')
boxplot(data_MYS_cases, main='Boxplot for New confirmed cases of 
     COVID-19 per 1,000,000 people', 
        ylab='New COVID-19 cases per million people')
boxplot(data_MYS_strin, main='Boxplot for the Stringency Index', 
        ylab='Stringency Index')

# par(mfrow = c(1,2))
plot(data_MYS_cases~data_MYS_vaccine,
     xlab = ' Total number of people who received at least one vaccine dose per 
     100 people in the total population', 
     ylab = 'New COVID-19 cases per million people',
     main = 'Scatter plots for % of population that received at least 1 dose vs. 
     New COVID-19 cases per million people')

plot(data_MYS_cases~data$vaccine_effect)

plot(data_MYS_cases~data_MYS_strin, 
     xlab = 'Stringency Index', 
     ylab = 'New COVID-19 cases per million people',
     main = 'Scatter plots for Stringency Index vs. New COVID-19 cases per 
     million people')


# first check condition 1 and 2

# condition 2
mod <- lm(data_MYS_cases ~ data_MYS_vaccine*data_vaccine_effect + data_MYS_strin)
summary(mod)

r <- resid(mod)

pairs(data[-c(1, 4, 5)])


# BEFORE BOX COX
# condition 1
plot(data_MYS_cases ~ fitted(mod), main="MYS_cases versus Fitted MYS_cases", 
     xlab="Fitted MYS_cases", ylab="MYS_cases")
abline(a = 0, b = 1)
lines(lowess(data_MYS_cases ~ fitted(mod)), lty=2)

# residual plots cant interpret since condition 1 violated
# plot(r ~ fitted(mod), main="title", xlab="Fitted", ylab="res.")
# plot(r ~ data_MYS_vaccine)
# plot(r ~ data_MYS_strin)
# plot(r ~ data_MYS_reprod)

# qqnorm(r)
# qqline(r)

library(car)
p <- powerTransform(cbind(data[,1], data[,2], data[,3])~1, family="bcnPower")
summary(p)

mod2 <- lm(I(log(data_MYS_cases)) ~ I(data_MYS_vaccine^0.257)*data_vaccine_effect 
           + I(log(data_MYS_strin)))  
#FINAL MODEL
summary(mod2)
summary(mod)

r <- resid(mod2)

data2=data
data2$data_MYS_cases = log(data$data_MYS_cases)
data2$data_MYS_vaccine = data$data_MYS_vaccine^0.257
data2$data_MYS_strin = log(data$data_MYS_strin)
data2$vaccine_effect = data$vaccine_effect


pairs(data2[-c(1, 4, 5)])

# AFTER BOX COX
plot(log(data_MYS_cases) ~ fitted(mod2), main="MYS_cases versus Fitted MYS_cases", 
     xlab="Fitted MYS_cases", ylab="MYS_cases")
abline(a = 0, b = 1)
lines(lowess(log(data_MYS_cases) ~ fitted(mod2)), lty=2)

plot(r ~ fitted(mod2), main="title", xlab="Fitted", ylab="res.")
plot(r ~ I(data_MYS_vaccine^0.257))
plot(r ~ I(log(data_MYS_strin)))
# plot(r ~ data_MYS_reprod)
qqnorm(r)
qqline(r)


mod3 <- lm(data2$data_MYS_cases ~ data2$data_MYS_vaccine*data2$vaccine_effect + 
                   data2$data_MYS_strin)
qqnorm(resid(mod3))
qqline(resid(mod3))

# problematic obs
n <- length(data2$data_MYS_cases)
p <- length(coef(mod2))-1
# calculate the leverage values and compare to cutoff
h <- hatvalues(mod2)
hcut <- 2*(p+1)/n


# leverage points?
w1 <- which(h > hcut)
w1

# par(mfrow=c(1,3))
plot(data2[,1]~data2[,2], main="", xlab="", ylab="")
points(data2[w1,1]~data2[w1,2], col="red", pch=19)

plot(data2[,1]~data2[,3], main="", xlab="", ylab="")
points(data2[w1,1]~data2[w1,3], col="red", pch=19)

plot(data2[,1]~data2[,5], main="", xlab="", ylab="")
points(data2[w1,1]~data2[w1,5], col="red", pch=19)


# outliers
r <- rstandard(mod2)
w2 <- which(r < -2 | r > 2)
w2

# par(mfrow=c(1,3))
plot(data2[,1]~data2[,2], main="", xlab="", ylab="")
points(data2[w2,1]~data2[w2,2], col="red", pch=19)

plot(data2[,1]~data2[,3], main="", xlab="", ylab="")
points(data2[w2,1]~data2[w2,3], col="red", pch=19)

plot(data2[,1]~data2[,5], main="", xlab="", ylab="")
points(data2[w2,1]~data2[w2,5], col="red", pch=19)

# influential points

# find the cooks distance and compare to cutoff
Dcutoff <- qf(0.5, p+1, n-p-1)
D <- cooks.distance(mod2)
which(D > Dcutoff)
## named integer(0)
# find the DFFITS and compare to cutoff
DFFITScut <- 2*sqrt((p+1)/n)
dfs <- dffits(mod2)
w3 <- which(abs(dfs) > DFFITScut)
w3
# find the DFBETAS and compare to cutoff (notice the dimension of DFBETAS)
DFBETAcut <- 2/sqrt(n)
dfb <- dfbetas(mod2)
w4 <- which(abs(dfb[,1]) > DFBETAcut)
w4
w5 <- which(abs(dfb[,2]) > DFBETAcut)
w5
w6 <- which(abs(dfb[,3]) > DFBETAcut)
w6
w7 <- which(abs(dfb[,5]) > DFBETAcut)
w7


# multicollinearity

library(car)
vif(mod2)


# train and test data ####################################################################

set.seed(1)
train <- data[sample(1:nrow(data2), 139, replace=F), ]
test <- data[which(!(data2$data_date %in% train$data_date)),]

# summary stats
mtr <- apply(train[,-c(4)], 2, mean)
sdtr <- apply(train[,-c(4)], 2, sd)
mtest <- apply(test[,-c(4)], 2, mean)
sdtest <- apply(test[,-c(4)], 2, sd)

full <- lm(data_MYS_cases ~ data_MYS_vaccine*vaccine_effect 
           + data_MYS_strin, data=train[,-c(4)])
summary(full)

r <- resid(full)

# cond2
pairs(train[-c(1, 4, 5)])

# cond1
plot(train$data_MYS_cases ~ fitted(full), main="Y vs Fitted", xlab="Fitted", ylab="Cases")
lines(lowess(train$data_MYS_cases ~ fitted(full)), lty=2)
abline(a = 0, b = 1)

plot(r ~ fitted(full), main="title", xlab="Fitted", ylab="res.")
plot(r ~ train$data_MYS_vaccine)
plot(r ~ train$data_MYS_strin)

qqnorm(r)
qqline(r)

p <- powerTransform(cbind(data[,1], data[,2], data[,3])~1, family="bcnPower")
summary(p)

train$logdata_MYS_cases = log(train$data_MYS_cases)
train$corrected_data_MYS_vaccine = train$data_MYS_vaccine^0.257
train$logdata_MYS_strin = log(train$data_MYS_strin)

test$logdata_MYS_cases = log(test$data_MYS_cases)
test$corrected_data_MYS_vaccine = test$data_MYS_vaccine^0.257
test$logdata_MYS_strin = log(test$data_MYS_strin)

full2 <- lm(train$logdata_MYS_cases ~ train$corrected_data_MYS_vaccine 
            * train$vaccine_effect + train$logdata_MYS_strin)
summary(full2)
vif(full2)

# problematic obs
n <- length(train$data_MYS_cases)
p <- length(coef(full2))-1
# calculate the leverage values and compare to cutoff
h <- hatvalues(full2)
hcut <- 2*(p+1)/n


# leverage points?
w1 <- which(h > hcut)
w1

# par(mfrow=c(1,3))
plot(train[,1]~train[,2], main="", xlab="", ylab="")
points(train[w1,1]~train[w1,2], col="red", pch=19)

plot(train[,1]~train[,3], main="", xlab="", ylab="")
points(train[w1,1]~train[w1,3], col="red", pch=19)

plot(train[,1]~train[,5], main="", xlab="", ylab="")
points(train[w1,1]~train[w1,5], col="red", pch=19)


# outliers
r <- rstandard(full2)
w2 <- which(r < -2 | r > 2)
w2

# par(mfrow=c(1,3))
plot(train[,1]~train[,2], main="", xlab="", ylab="")
points(train[w2,1]~train[w2,2], col="red", pch=19)

plot(train[,1]~train[,3], main="", xlab="", ylab="")
points(train[w2,1]~train[w2,3], col="red", pch=19)

plot(train[,1]~train[,5], main="", xlab="", ylab="")
points(train[w2,1]~train[w2,5], col="red", pch=19)

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
w4 <- which(abs(dfb[,1]) > DFBETAcut)
w4
w5 <- which(abs(dfb[,2]) > DFBETAcut)
w5
w6 <- which(abs(dfb[,3]) > DFBETAcut)
w6
w7 <- which(abs(dfb[,5]) > DFBETAcut)
w7

full2_test <- lm(test$logdata_MYS_cases ~ test$corrected_data_MYS_vaccine 
            * test$vaccine_effect + test$logdata_MYS_strin)
summary(full)
summary(full2_test)
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
plot(test[,1]~test[,2], main="", xlab="", ylab="")
points(test[w1,1]~test[w1,2], col="red", pch=19)

plot(test[,1]~test[,3], main="", xlab="", ylab="")
points(test[w1,1]~test[w1,3], col="red", pch=19)

plot(test[,1]~test[,5], main="", xlab="", ylab="")
points(test[w1,1]~test[w1,5], col="red", pch=19)


# outliers
r <- rstandard(full2_test)
w2 <- which(r < -2 | r > 2)
w2

# par(mfrow=c(1,3))
plot(test[,1]~test[,2], main="", xlab="", ylab="")
points(test[w2,1]~test[w2,2], col="red", pch=19)

plot(test[,1]~test[,3], main="", xlab="", ylab="")
points(test[w2,1]~test[w2,3], col="red", pch=19)

plot(test[,1]~test[,5], main="", xlab="", ylab="")
points(test[w2,1]~test[w2,5], col="red", pch=19)

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






