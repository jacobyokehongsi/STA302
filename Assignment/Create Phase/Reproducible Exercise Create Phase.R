setwd("D:/UofT/Y3 Fall/STA302/Assignment")

file = data.frame(read.csv("penguins.csv"))
file = file[(file$year=="2009"),]
file = na.omit(file)

bill_length = file$bill_length_mm
bill_depth = file$bill_depth_mm
ratio_bill_lengthdepth = file$bill_length_mm/file$bill_depth_mm

flipper_length = file$flipper_length_mm
flipper_length_catagorised = ifelse(flipper_length<=200, "small", "large")

body_mass = file$body_mass_g
median_body_mass = median(body_mass)
body_mass_catagorised = ifelse(body_mass < median_body_mass, "below", 
                               ifelse(body_mass > median_body_mass, "above", 
                                      "equal"))

sex = file$sex

species = file$species

location = file$island

data = cbind(ratio_bill_lengthdepth, flipper_length_catagorised, 
             body_mass_catagorised, sex, species, location)

write.csv(data, file="penguins_cleaned.csv")
