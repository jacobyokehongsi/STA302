setwd("D:/UofT/Y3 Fall/STA302/Video Project")

data = data.frame(read.csv("Video_project_dataset.csv"))


# hist(data$UNITID)
# hist(data$INSTNM)


library("ggplot2")
# ggplot(data, aes(x = STABBR)) +
  # geom_bar()
hist(data$NUMBRANCH)
ggplot(data, aes(x = CONTROL)) +
  geom_bar()
ggplot(data, aes(x = REGION)) +
  geom_bar()
ggplot(data, aes(x = HBCU)) +
  geom_bar()
ggplot(data, aes(x = PBI)) +
  geom_bar()
ggplot(data, aes(x = TRIBAL)) +
  geom_bar()
ggplot(data, aes(x = HSI)) +
  geom_bar()
ggplot(data, aes(x = WOMENONLY)) +
  geom_bar()
hist(data$ADM_RATE) # y variable
hist(data$COSTT4_A)
hist(data$AVGFACSAL)
hist(data$PFTFAC)
hist(data$PCTPELL)
hist(data$UG25ABV)
hist(data$INC_PCT_LO)
hist(data$PAR_ED_PCT_1STGEN)
hist(data$FEMALE)
hist(data$MD_FAMINC)
hist(data$PCT_WHITE)
hist(data$PCT_BLACK)
hist(data$PCT_ASIAN)
hist(data$PCT_HISPANIC)
hist(data$PCT_BA)
hist(data$PCT_GRAD_PROF)
hist(data$PCT_BORN_US)
hist(data$POVERTY_RATE)
hist(data$UNEMP_RATE)


# plot(data$ADM_RATE ~ data$UNITID)
# plot(data$ADM_RATE ~ data$INSTNM)
# plot(data$ADM_RATE ~ data$STABBR)
plot(data$ADM_RATE ~ data$NUMBRANCH)
plot(data$ADM_RATE ~ data$CONTROL)
plot(data$ADM_RATE ~ data$REGION)
plot(data$ADM_RATE ~ data$HBCU)
plot(data$ADM_RATE ~ data$PBI)
plot(data$ADM_RATE ~ data$TRIBAL)
plot(data$ADM_RATE ~ data$HSI)
plot(data$ADM_RATE ~ data$WOMENONLY)
# plot(data$ADM_RATE ~ data$ADM_RATE)
plot(data$ADM_RATE ~ data$COSTT4_A)
plot(data$ADM_RATE ~ sqrt(data$AVGFACSAL))
plot(data$ADM_RATE ~ data$PFTFAC)
plot(data$ADM_RATE ~ data$PCTPELL)
plot(data$ADM_RATE ~ data$UG25ABV)
plot(data$ADM_RATE ~ data$INC_PCT_LO)
plot(data$ADM_RATE ~ data$PAR_ED_PCT_1STGEN)
plot(data$ADM_RATE ~ data$FEMALE)
plot(data$ADM_RATE ~ data$MD_FAMINC)
plot(data$ADM_RATE ~ data$PCT_WHITE)
plot(data$ADM_RATE ~ data$PCT_BLACK)
plot(data$ADM_RATE ~ data$PCT_ASIAN) # remove?
plot(data$ADM_RATE ~ data$PCT_HISPANIC)
plot(data$ADM_RATE ~ data$PCT_BA)
plot(data$ADM_RATE ~ data$PCT_GRAD_PROF)
plot(data$ADM_RATE ~ data$PCT_BORN_US)
plot(data$ADM_RATE ~ data$POVERTY_RATE)
plot(data$ADM_RATE ~ data$UNEMP_RATE)


# first check condition 1 and 2
mod <- lm(data$ADM_RATE ~ data$NUMBRANCH + data$CONTROL + data$REGION + data$HBCU + 
            data$PBI + data$TRIBAL + data$HSI + data$WOMENONLY + data$COSTT4_A + data$AVGFACSAL + data$PFTFAC + 
            data$PCTPELL + data$UG25ABV + data$INC_PCT_LO + data$PAR_ED_PCT_1STGEN + data$FEMALE + data$MD_FAMINC + data$PCT_WHITE +
            data$PCT_BLACK + data$PCT_ASIAN + data$PCT_HISPANIC + data$PCT_BA + data$PCT_GRAD_PROF + data$PCT_BORN_US + 
            data$POVERTY_RATE + data$UNEMP_RATE)
summary(mod)

r <- resid(mod)

pairs(data[c(5, 6, 7, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 
              27, 28, 29, 30, 31)], lower.panel = NULL)

#BEFORE BOX COX
plot(data$ADM_RATE ~ fitted(mod), main="Admission Rate versus Fitted Admission Rate", 
     xlab="Fitted Admission Rate", ylab="Admission Rate")
abline(a = 0, b = 1)
lines(lowess(data$ADM_RATE ~ fitted(mod)), lty=2)

plot(r ~ fitted(mod), main="title", xlab="Fitted", ylab="res.")
plot(r ~ data$NUMBRANCH)
plot(r ~ data$CONTROL)
plot(r ~ data$REGION)
plot(r ~ data$HBCU)
plot(r ~ data$PBI)
plot(r ~ data$TRIBAL)
plot(r ~ data$HSI)
plot(r ~ data$WOMENONLY)
plot(r ~ data$COSTT4_A)
plot(r ~ data$AVGFACSAL)
plot(r ~ data$PFTFAC)
plot(r ~ data$UG25ABV)
plot(r ~ data$INC_PCT_LO)
plot(r ~ data$PAR_ED_PCT_1STGEN)
plot(r ~ data$FEMALE)
plot(r ~ data$MD_FAMINC)
plot(r ~ data$PCT_WHITE)
plot(r ~ data$PCT_BLACK)
plot(r ~ data$PCT_ASIAN)
plot(r ~ data$PCT_HISPANIC)
plot(r ~ data$PCT_BA)
plot(r ~ data$PCT_GRAD_PROF)
plot(r ~ data$PCT_BORN_US)
plot(r ~ data$POVERTY_RATE)
plot(r ~ data$UNEMP_RATE)

qqnorm(r)
qqline(r)


library(car)
p <- powerTransform(cbind(data[,13], data[,14], data[,15], data[,16], data[,17], data[,18], data[,19],
                          data[,20], data[,21], data[,22], data[,23], data[,24], data[,25], data[,26],
                          data[,27], data[,28], data[,29], data[,30], data[,31])~1, family="bcnPower")
summary(p)

# after box cox 
# we sqrt the response to correct for normality

mod2 <- lm(I(sqrt(data$ADM_RATE^3)) ~ data$NUMBRANCH + data$CONTROL + data$REGION + data$HBCU + 
             data$PBI + data$TRIBAL + data$HSI + data$WOMENONLY + I(data$COSTT4_A^0.33) + I(log(data$AVGFACSAL)) + I(data$PFTFAC^1) + 
             I(data$PCTPELL^0.33) + I(data$UG25ABV^-0.33) + I(data$INC_PCT_LO^0.19) + I(data$PAR_ED_PCT_1STGEN^0.614) + I(data$FEMALE^1.457) + 
             I(data$MD_FAMINC^0.273) + I(data$PCT_WHITE^3) + I(data$PCT_BLACK^0.456) + I(data$PCT_ASIAN^-0.048) + I(log(data$PCT_HISPANIC)) + 
             I(data$PCT_BA^0.848) + I(log(data$PCT_GRAD_PROF)) + I(data$PCT_BORN_US^3) + I(data$POVERTY_RATE^-0.763) + I(data$UNEMP_RATE^-1.783))
summary(mod2)
summary(mod)

r <- resid(mod2)

# first check condition 1 and 2, condition 2 ignore binary preds.
data2=data
data2$ADM_RATE = sqrt(data$ADM_RATE^3)
data2$COSTT4_A = data2$COSTT4_A^0.33
data2$AVGFACSA = log(data2$AVGFACSA)
data2$PFTFAC = data2$PFTFAC^1
data2$PCTPELL = data2$PCTPELL^0.33
data2$UG25ABV = data2$UG25ABV^-0.33
data2$INC_PCT_LO = data2$INC_PCT_LO^0.19
data2$PAR_ED_PCT_1STGEN = data2$PAR_ED_PCT_1STGEN^0.614
data2$FEMALE = data2$FEMALE^1.457
data2$MD_FAMINC = data2$MD_FAMINC^0.273
data2$PCT_WHITE = data2$PCT_WHITE^3
data2$PCT_BLACK = data2$PCT_BLACK^0.456
data2$PCT_ASIAN = data2$PCT_ASIAN^-0.048
data2$PCT_HISPANIC = log(data2$PCT_HISPANIC)
data2$PCT_BA = data2$PCT_BA^0.848
data2$PCT_GRAD_PROF = log(data2$PCT_GRAD_PROF)
data2$PCT_BORN_US = data2$PCT_BORN_US^3
data2$POVERTY_RATE = data2$POVERTY_RATE^-0.763
data2$UNEMP_RATE = data2$UNEMP_RATE^-1.783

# pairs(data[,13:31], lower.panel = NULL)
pairs(data2[c(5, 6, 7, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 
              27, 28, 29, 30, 31)], lower.panel = NULL)



plot(sqrt(data$ADM_RATE^3) ~ fitted(mod2), main="Admission Rate versus Fitted Admission Rate", 
     xlab="Fitted Admission Rate", ylab="Admission Rate")
abline(a = 0, b = 1)
lines(lowess(sqrt(data$ADM_RATE^3) ~ fitted(mod2)), lty=2)

# make all residual plots
plot(r ~ fitted(mod2), main="title", xlab="Fitted", ylab="res.")
plot(r ~ data$NUMBRANCH)
plot(r ~ data$CONTROL)
plot(r ~ data$REGION)
plot(r ~ data$HBCU)
plot(r ~ data$PBI)
plot(r ~ data$TRIBAL)
plot(r ~ data$HSI)
plot(r ~ data$WOMENONLY)
plot(r ~ I(data$COSTT4_A^0.33))
plot(r ~ I(log(data$AVGFACSAL)))
plot(r ~ I(data$PFTFAC^1))
plot(r ~ I(data$PCTPELL^0.33))
plot(r ~ I(log(data$UG25ABV^-0.33))) #log to correct for constant variance
plot(r ~ I(data$INC_PCT_LO^0.19))
plot(r ~ I(data$PAR_ED_PCT_1STGEN^0.614))
plot(r ~ I(data$FEMALE^1.457))
plot(r ~ I(data$MD_FAMINC^0.273))
plot(r ~ I(data$PCT_WHITE^3))
plot(r ~ I(data$PCT_BLACK^0.456))
plot(r ~ I(data$PCT_ASIAN^-0.048))
plot(r ~ I(log(data$PCT_HISPANIC)))
plot(r ~ I(data$PCT_BA^0.848))
plot(r ~ I(log(data$PCT_GRAD_PROF)))
plot(r ~ I(data$PCT_BORN_US^3))
plot(r ~ I(data$POVERTY_RATE^-0.763))
plot(r ~ I(data$UNEMP_RATE^-1.783))
qqnorm(r)
qqline(r)
# will cause an error with inhibitor function

# if you create a new response and then add to model, QQ plot works
mod3 <- lm(data2$ADM_RATE ~ data$NUMBRANCH + data$CONTROL + data$REGION + data$HBCU + 
             data$PBI + data$TRIBAL + data$HSI + data$WOMENONLY + data2$COSTT4_A + data2$AVGFACSA + data2$PFTFAC + 
             data2$PCTPELL + data2$UG25ABV + data2$INC_PCT_LO + data2$PAR_ED_PCT_1STGEN + data2$FEMALE + 
             data2$MD_FAMINC + data2$PCT_WHITE + data2$PCT_BLACK + data2$PCT_ASIAN + data2$PCT_HISPANIC + 
             data2$PCT_BA + data2$PCT_GRAD_PROF + data2$PCT_BORN_US + data2$POVERTY_RATE + data2$UNEMP_RATE)
qqnorm(resid(mod3))
qqline(resid(mod3))



# Partial F-Tests

mod_full <- lm(I(sqrt(data$ADM_RATE^3)) ~ data$NUMBRANCH + data$CONTROL + data$REGION + data$HBCU + 
             data$PBI + data$TRIBAL + data$HSI + data$WOMENONLY + I(data$COSTT4_A^0.33) + I(log(data$AVGFACSAL)) + I(data$PFTFAC^1) + 
             I(data$PCTPELL^0.33) + I(log(data$UG25ABV^-0.33)) + I(data$INC_PCT_LO^0.19) + I(data$PAR_ED_PCT_1STGEN^0.614) + I(data$FEMALE^1.457) + 
             I(data$MD_FAMINC^0.273) + I(data$PCT_WHITE^3) + I(data$PCT_BLACK^0.456) + I(data$PCT_ASIAN^-0.048) + I(log(data$PCT_HISPANIC)) + 
             I(data$PCT_BA^0.848) + I(log(data$PCT_GRAD_PROF)) + I(data$PCT_BORN_US^3) + I(data$POVERTY_RATE^-0.763) + I(data$UNEMP_RATE^-1.783))
summary(mod_full)
# anova(mod_full)

mod_red <- lm(I(sqrt(data$ADM_RATE^3)) ~ data$NUMBRANCH + data$HSI + data$REGION +
                I(data$COSTT4_A^0.33) + I(log(data$AVGFACSAL)) + I(data$PFTFAC^1) +
                I(log(data$UG25ABV^-0.33)) + I(data$INC_PCT_LO^0.19) + I(data$PAR_ED_PCT_1STGEN^0.614) +
                I(data$FEMALE^1.457) + 
                I(data$PCT_WHITE^3) + I(data$PCT_BLACK^0.456) + 
                I(data$PCT_ASIAN^-0.048) + I(log(data$PCT_HISPANIC)) + 
                I(data$PCT_BA^0.848) + I(log(data$PCT_GRAD_PROF)) + I(data$PCT_BORN_US^3))
summary(mod_red)

# mod_red_2 <- lm(I(data$ADM_RATE^3) ~ data$NUMBRANCH + data$CONTROL + data$REGION + dataHBCU + 
#                 data$PBI + data$TRIBAL + data$HSI + I(data$COSTT4_A^0.33) + I(log(data$AVGFACSAL)) + I(data$UG25ABV^-0.33) + 
#                 I(data$INC_PCT_LO^0.19) + I(data$PAR_ED_PCT_1STGEN^0.614) + I(data$FEMALE^1.457) + I(data$PCT_WHITE^3) + I(data$PCT_BLACK^0.456) + 
#                 I(data$PCT_ASIAN^-0.048) + I(log(data$PCT_HISPANIC)) + I(log(data$PCT_GRAD_PROF)))

anova(mod_red, mod_full)

# anova(mod_red_2, mod_full)





