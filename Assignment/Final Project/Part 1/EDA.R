setwd("D:/UofT/Y3 Fall/STA302/Assignment/Final Project/Part 1")

data = data.frame(read.csv("owid-covid-data.csv"))
data_MYS = data[(data$location=="Malaysia"),]
# data_MYS = data_MYS[!is.na(data_MYS$people_fully_vaccinated_per_hundred),]
# data_US = data[(data$location=="United States"),]
# data_UK = data[(data$location=="United Kingdom"),]
data_CAD = data[(data$location=="Canada"),]

hist(data_MYS$new_cases_per_million, main='Histogram for New deaths due to 
     COVID-19 per 1,000,000 people', 
     ylab='New deaths due to COVID-19 per 1,000,000 people')
boxplot(data_MYS$new_cases_per_million, main='Boxplot for New deaths due to 
        COVID-19 per 1,000,000 people',
        ylab='New deaths due to COVID-19 per 1,000,000 people')
plot(x=data_MYS$people_fully_vaccinated_per_hundred,y=data_MYS$new_cases_per_million, 
     xlab = 'Total number of COVID-19 vaccination doses administered', 
     ylab = 'New deaths due to COVID-19 per 1,000,000 people',
     main = 'Scatter plots for total vaccination doses administered vs. 
     New deaths due to COVID-19 per 1,000,000 people')

hist(data_CAD$new_cases_per_million, main='Histogram for New deaths due to 
     COVID-19 per 1,000,000 people', 
     ylab='New deaths due to COVID-19 per 1,000,000 people')
boxplot(data_CAD$new_cases_per_million, main='Boxplot for New deaths due to 
        COVID-19 per 1,000,000 people',
        ylab='New deaths due to COVID-19 per 1,000,000 people')


# plot(x=data_US$people_fully_vaccinated_per_hundred,y=data_US$new_cases_per_million, 
#      xlab = 'Total number of COVID-19 vaccination doses administered', 
#      ylab = 'New deaths due to COVID-19 per 1,000,000 people',
#      main = 'Scatter plots for total vaccination doses administered vs. 
#      New deaths due to COVID-19 per 1,000,000 people')
# plot(x=data_UK$people_fully_vaccinated_per_hundred,y=data_UK$new_cases_per_million, 
#      xlab = 'Total number of COVID-19 vaccination doses administered', 
#      ylab = 'New deaths due to COVID-19 per 1,000,000 people',
#      main = 'Scatter plots for total vaccination doses administered vs. 
#      New deaths due to COVID-19 per 1,000,000 people')
plot(x=data_CAD$people_fully_vaccinated_per_hundred,y=data_CAD$new_cases_per_million, 
     xlab = 'Total number of COVID-19 vaccination doses administered', 
     ylab = 'New deaths due to COVID-19 per 1,000,000 people',
     main = 'Scatter plots for total vaccination doses administered vs. 
     New deaths due to COVID-19 per 1,000,000 people')


# plot(x=data_US$people_fully_vaccinated_per_hundred,y=data_US$new_deaths_per_million, 
#      xlab = 'Total number of COVID-19 vaccination doses administered', 
#      ylab = 'New deaths due to COVID-19 per 1,000,000 people',
#      main = 'Scatter plots for total vaccination doses administered vs. 
#      New deaths due to COVID-19 per 1,000,000 people')
# plot(x=data_UK$people_fully_vaccinated_per_hundred,y=data_UK$new_deaths_per_million, 
#      xlab = 'Total number of COVID-19 vaccination doses administered', 
#      ylab = 'New deaths due to COVID-19 per 1,000,000 people',
#      main = 'Scatter plots for total vaccination doses administered vs. 
#      New deaths due to COVID-19 per 1,000,000 people')
plot(x=data_CAD$people_fully_vaccinated_per_hundred,y=data_CAD$new_deaths_per_million, 
     xlab = 'Total number of COVID-19 vaccination doses administered', 
     ylab = 'New deaths due to COVID-19 per 1,000,000 people',
     main = 'Scatter plots for total vaccination doses administered vs. 
     New deaths due to COVID-19 per 1,000,000 people')






############################

mean(cleaning_dataset$Case) # Case Mean
mean(cleaning_dataset$Crews) # Crews Mean
mean(cleaning_dataset$Rooms) # Rooms Mean

sd(cleaning_dataset$Case) # Case SD
sd(cleaning_dataset$Crews) # Crews SD
sd(cleaning_dataset$Rooms) # Rooms SD

par(mfrow=c(1,3))
boxplot(cleaning_dataset$Case)
boxplot(cleaning_dataset$Crews)
boxplot(cleaning_dataset$Rooms)

plot(cleaning_dataset$Rooms~cleaning_dataset$Crews, main="Rooms vs Crews", xlab="Crews", ylab="Rooms")

#### 2

# load you data and find numerical summaries of t he variables
body_fat_complete_data = read.csv("body_fat_complete.csv")
summary(body_fat_complete_data)
head(body_fat_complete_data)

# make your plots here
# try to arrange them in grids that group similar plots together
# par(mfrow=c(2,2))  # would create a 2x2 grid of plots if using base R plot functions
# 

par(mfrow =c(3, 3))# make seperate plot grids
hist(data$Pct.BF, main='Histogram for body fat', ylab='Percentage body fat')
# plot body fat %
hist(data$Waist, main='Histogram for waist size (in)', ylab='Waist size (in)')
# plot waist size
hist(data$Height, main='Histogram for height (in)',ylab='Height (in)')
# plot heightbox
plot(data$Pct.BF, main='Boxplot for body fat',ylab='Percentage body fat', horizontal = TRUE)
# plot body fat %
boxplot(data$Waist, main='Boxplot for waist size (in)',ylab='Waist size (in)', horizontal = TRUE)
# plot waist size
boxplot(data$Height, main='Boxplot for height (in)',ylab='Height (in)',horizontal = TRUE)
# plot height
plot(x=data$Waist,y=data$Pct.BF,xlab ='waist size of the individual (in)',ylab='percent body fat',main ='Scatter Plots for waist v.s. body fat')
# scatter plot 1
plot(x=data$Height,y=data$Pct.BF,xlab ='height of the individual (in)',ylab='percent body fat',main ='Scatter Plots for height v.s. body fat')
# scatter plot 2

# par(mfrow=c(2,2))
# plot(body_fat_complete_data$Pct.BF)
# plot(body_fat_complete_data$Waist)
# plot(body_fat_complete_data$Height)
# plot(body_fat_complete_data$Pct.BF~body_fat_complete_data$Waist, main="Body Fat vs. Waist", xlab="Waist", ylab="Body Fat")
# plot(body_fat_complete_data$Pct.BF~body_fat_complete_data$Height, main="Body Fat vs. Height", xlab="Height", ylab="Body Fat")