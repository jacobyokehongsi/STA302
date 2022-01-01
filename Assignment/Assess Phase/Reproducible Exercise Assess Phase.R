setwd("D:/UofT/Y3 Fall/STA302/Assignment/Assess Phase")

# birth year (to calculate the age), homeworld, sex, gender, species, height and mass

file = data.frame(read.csv("starwars.csv"))

birth_year = file$birth_year
homeworld = file$homeworld
sex = file$sex
gender = file$gender
species = file$species
height = file$height
mass = file$mass

data = cbind(birth_year, homeworld, sex, gender, height, mass, species)
data = na.omit(data)
data = data.frame((data))

data = data[(data$species=="Human"),]
row.names(data) <- NULL

write.csv(data, file="starwars2_cleaned.csv")
