library(AER)
library(systemfit)

setwd("/Users/wanlin/Documents/Career/Work/WB/2017/econ/Instrumental Variables")
data <- read.csv("mxm.csv")
head(data)

# IV regression
eq1 <- Total_Loss_perc ~ Rating +Minute_In_Commercial+interaction_of_X_AD
inst <- ~ X +Minute_In_Commercial+interaction_of_X_AD
lm.test <- systemfit(eq1, method = "2SLS", inst = inst, data = data)

summary(lm.test)

plot(data$X, data$Rating, main = "X v.s. Rating", xlab = "X",ylab ="Rating")
lm.iv <- lm(Rating ~X, data = data)
summary(lm.iv)

lm.loss <- lm(Total_Loss_perc ~X, data = data)
plot(data$X, data$Total_Loss_perc, main = "X v.s. Total_Loss_perc", xlab = "X",ylab ="Total_Loss_perc")

summary(lm.loss)

# Test on IV regression
interaction_of_X_AD <- data$Minute_In_Commercial*data$X
# fisrt stage
lm.iv <- lm(Rating ~X+Minute_In_Commercial+interaction_of_X_AD, data = data)
summary(lm.iv)
# second
iv1 = ivreg(Total_Loss_perc ~ Rating +Minute_In_Commercial+interaction_of_X_AD| X +Minute_In_Commercial+interaction_of_X_AD, data = data)
summary(iv1, diagnostics = TRUE)

## residual v.s. fitted
ols <- lm(Total_Loss_perc ~ Rating +Minute_In_Commercial+interaction_of_X_AD, data = data)
plot(lm.test)
residualPlot(iv1)

par(mfrow = c(1,1))
plot(fitted(iv1), resid(iv1), main = "Residual v.s. Fitted Plot of IV Regression", xlab = "Fitted", ylab= "Residuals")
plot(fitted(ols), resid(ols), main = "Residual v.s. Fitted Plot of OLS", xlab = "Fitted", ylab= "Residuals")
