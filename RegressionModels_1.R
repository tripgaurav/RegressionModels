library(datasets)
library(car)

cars_data <- mtcars

head(cars_data)
corr_var <- data.frame(round(cor(cars_data),2))
cars_data$am <- factor(x=cars_data$am, labels=c("Automatic", "Manual"))
par(mfrow = c(1,1))
plot(cars_data$am, cars_data$mpg, xlab = "Type of Transmission", ylab = "MPG", 
     main="Impact of Transmission Type on MPG")
t.test(cars_data[cars_data$am == "Automatic",]$mpg, cars_data[cars_data$am == "Manual",]$mpg)

# Low R2 in the model below (0.36)
summary(lm(mpg ~ am, data=cars_data))

# AM is insignificant in models below
summary(lm(mpg ~ am + cyl, data = cars_data))
summary(lm(mpg ~ am + disp, data = cars_data))

# The model below looks okay in terms of variable significance and R2
# But it has the problem of Heteroscedasticity, as seen from Residuals plot
summary(lm(mpg ~ am + hp, data = cars_data))
fit <- lm(mpg ~ am + hp, data = cars_data)
plot(fit)

# New variables added are all insignificant
summary(lm(mpg ~ am + hp + drat, data = cars_data))
summary(lm(mpg ~ am + hp + qsec, data = cars_data))
summary(lm(mpg ~ am + hp + vs, data = cars_data))
summary(lm(mpg ~ am + hp + gear, data = cars_data))
summary(lm(mpg ~ am + hp + carb, data = cars_data))

# AM is insignificant in model below
summary(lm(mpg ~ am + hp + wt, data = cars_data))

# Since combinations with HP didn't work, removing that variable
# AM is insignificant in model below
summary(lm(mpg ~ am + wt, data = cars_data))

# Looking at interactions
# Model below has all significant variables, and decent R2. Also, it seems to make sense logically
# i.e. MPG decreasing with increasing weight, assuming transmission type is not changed
summary(lm(mpg ~ am*wt, data = cars_data))

# Same can be validated from the following chart
plot(cars_data$wt, cars_data$mpg, col=cars_data$am, xlab = "Weight of car", ylab = "MPG")
fit_am_wt <- lm(mpg ~ wt*am, data=cars_data)
summary(fit_am_wt)
abline(coef(fit_am_wt)[1], coef(fit_am_wt)[2])
abline(coef(fit_am_wt)[1]+coef(fit_am_wt)[3], coef(fit_am_wt)[2]+coef(fit_am_wt)[4], col="red")

# Hence this model is definitely making sense
# Iterate through models by adding variables to it, to see if R2 improves, without variables getting dropped
summary(lm(mpg ~ am*wt + cyl, data = cars_data))    # R2 = 0.877
summary(lm(mpg ~ am*wt + disp, data = cars_data))   # Wt insignificant
summary(lm(mpg ~ am*wt + hp, data = cars_data))     # R2 = 0.87
summary(lm(mpg ~ am*wt + drat, data = cars_data))   # Drat insignificant
summary(lm(mpg ~ am*wt + qsec, data = cars_data))   # R2 = 0.90
summary(lm(mpg ~ am*wt + vs, data = cars_data))     # R2 = 0.87
summary(lm(mpg ~ am*wt + gear, data = cars_data))   # Gear insignificant
summary(lm(mpg ~ am*wt + carb, data = cars_data))   # Carb insignificant

# Hence, improved model is the one with interaction between am and wt, and qsec variable added
# This model below has high R2, all signifiant variables, and no heteroscedasticity. Hence it is selected
fit_am_wt_qsec <- lm(mpg ~ am*wt + qsec, data = cars_data)
par(mfrow=c(2,2), mar=c(6,5,2,4))
plot(fit_am_wt_qsec)

# To analyse of variance using Anova and VIF
fit_am_wt_qsec_noint <- lm(mpg ~ am + wt + qsec, data = cars_data)
anova(fit_am_wt_qsec, fit_am_wt_qsec_noint)

vif(fit_am_wt_qsec)
vif(fit_am_wt_qsec_noint)