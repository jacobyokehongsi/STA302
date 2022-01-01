setwd("D:/UofT/Y3 Fall/STA302/Assignment/Final Project/Part 1/Submit")

data = data.frame(read.csv("owid-covid-data.csv"))
data_MYS_c_v = na.omit(data[(data$location=="Malaysia"), 
        c("new_cases_per_million", "people_vaccinated_per_hundred")])
data_MYS_c_s = na.omit(data[(data$location=="Malaysia"), 
        c("new_cases_per_million", "stringency_index")])
data_MYS_c_s = na.omit(data[(data$location=="Malaysia"), 
                            c("new_cases_per_million", "stringency_index")])


data_MYS_vaccine = data_MYS_c_v$people_vaccinated_per_hundred
data_MYS_cases_v = data_MYS_c_v$new_cases_per_million
data_MYS_strin = data_MYS_c_s$stringency_index
data_MYS_cases_s = data_MYS_c_s$new_cases_per_million


summary(data_MYS_vaccine)
summary(data_MYS_cases_v)
summary(data_MYS_strin)
summary(data_MYS_cases_s)

sd(data_MYS_vaccine)
sd(data_MYS_cases_v)
sd(data_MYS_strin)
sd(data_MYS_cases_s)



par(mfrow = c(2,2))
hist(data_MYS_vaccine, main='Histogram for Total number of
     people who received at least one vaccine dose per 100 people in the total 
     population', 
     ylab='% of population that received at least 1 dose')
hist(data_MYS_cases_v, main='Histogram for New confirmed cases of 
     COVID-19 per 1,000,000 people corresponding to vaccination', 
     ylab='New COVID-19 cases per million people w.r.t to vaccination')
hist(data_MYS_strin, main='Histogram for the Stringency Index', 
     ylab='Stringency Index')
hist(data_MYS_cases_s, main='Histogram for New confirmed cases of 
     COVID-19 per 1,000,000 people corresponding to stringency index', 
     ylab='New COVID-19 cases per million people w.r.t to stringency index')


boxplot(data_MYS_vaccine, main='Boxplot for Total number of
     people who received at least one vaccine dose per 100 people in the total 
     population', 
     ylab='% of population that received at least 1 dose')
boxplot(data_MYS_cases_v, main='Boxplot for New confirmed cases of 
     COVID-19 per 1,000,000 people corresponding to vaccination', 
     ylab='New COVID-19 cases per million people w.r.t to vaccination')
boxplot(data_MYS_strin, main='Boxplot for the Stringency Index', 
     ylab='Stringency Index')
boxplot(data_MYS_cases_s, main='Boxplot for New confirmed cases of 
     COVID-19 per 1,000,000 people corresponding to stringency index', 
     ylab='New COVID-19 cases per million people w.r.t to stringency index')

par(mfrow = c(1,2))
plot(data_MYS_cases_v~data_MYS_vaccine, 
     xlab = ' Total number of people who received at least one vaccine dose per 
     100 people in the total population', 
     ylab = 'New COVID-19 cases per million people w.r.t to vaccination',
     main = 'Scatter plots for % of population that received at least 1 dose vs. 
     New COVID-19 cases per million people w.r.t to vaccination')

plot(data_MYS_cases_s~data_MYS_strin, 
     xlab = 'Stringency Index', 
     ylab = 'New COVID-19 cases per million people w.r.t to stringency index',
     main = 'Scatter plots for Stringency Index vs. New COVID-19 cases per 
     million people w.r.t to stringency index')

# lm(data_MYS$new_cases_per_million~data_MYS$people_vaccinated_per_hundred+
#      data_MYS$stringency_index)

