wdir -> "C:\Users\Parv\Documents\Spring 2020\AMS 315\Project 1"
setwd(wdir)

PartA_IV <- read.csv('P1A_IV69570.csv', header = TRUE)
PartA_DV <- read.csv('P1A_DV69570.csv', header = TRUE)
PartA <- merge(PartA_IV, PartA_DV, by = 'ID')

str(PartA)
View(PartA)

any(is.na(PartA[,2]) == TRUE)
any(is.nan(PartA[,2]) == TRUE)

PartA_incomplete <- PartA
install.packages('mice')
library(mice)
md.pattern(PartA_incomplete)
# There are 456 complete data sets
# IV is missing in 49, DV is missing in 153 and both are missing in 10 cases.

PartA_imp <- PartA[!is.na(PartA$IV)==TRUE|!is.na(PartA$DV)==TRUE,]
imp <- mice(PartA_imp, method = "norm.boot", printFlag = FALSE)
PartA_complete <- complete(imp)

md.pattern(PartA_complete)

M <- lm(DV ~ IV, data=PartA_complete)
summary(M)

install.packages('knitr')
library(knitr)
kable(anova(M), caption='ANOVA Table')

coef(M)

plot(PartA_complete$DV ~ PartA_complete$IV, main='Scatter : DV ~ IV', xlab='IV', ylab='DV', pch=20)
abline(M, col='red', lty=3, lwd=2)
legend('topleft', legend='Estimated Regression Line', lty=2, lwd=4, col='red')

confint(M, level=0.95)

# Confidence Interval
obs <- nrow(PartA_complete)
CI_L <- fitted(M) - qt(0.975, df=obs-2)*summary(M)$sigma*sqrt(1/obs + (PartA_complete$IV-mean(PartA_complete$IV))^2/(var(PartA_complete$IV)*(obs-1)))
CI_U <- fitted(M) + qt(0.975, df=obs-2)*summary(M)$sigma*sqrt(1/obs + (PartA_complete$IV-mean(PartA_complete$IV))^2/(var(PartA_complete$IV)*(obs-1)))
M1 <- lm(CI_L ~ IV, data=PartA_complete)
abline(M1, col='blue', 3, lwd=3)
M2 <- lm(CI_U ~ IV, data=PartA_complete)
abline(M2, col='blue', 3, lwd=3)
legend('topright', legend='Confidence Interval', lty=1, lwd=3, col='blue')

# Prediction Interval
PI_L <- fitted(M) - qt(0.975, df=obs-2)*summary(M)$sigma*sqrt(1+1/obs + (PartA_complete$IV-mean(PartA_complete$IV))^2/(var(PartA_complete$IV)*(obs-1)))
PI_U <- fitted(M) + qt(0.975, df=obs-2)*summary(M)$sigma*sqrt(1+1/obs + (PartA_complete$IV-mean(PartA_complete$IV))^2/(var(PartA_complete$IV)*(obs-1)))
M3 <- lm(PI_L ~ IV, data=PartA_complete)
abline(M3, col='green', 3, lwd=3)
M4 <- lm(PI_U ~ IV, data=PartA_complete)
abline(M4, col='green', 3, lwd=3)
legend('bottomleft', legend='Prediction Interval', lty=1, lwd=3, col='green')

# Working-Hetelling Band
WH_L <- fitted(M) - qf(0.975, 2, obs-2)*summary(M)$sigma*sqrt(1/obs + (PartA_complete$IV-mean(PartA_complete$IV))^2/(var(PartA_complete$IV)*(obs-1)))#
WH_U <- fitted(M) + qf(0.975, 2, obs-2)*summary(M)$sigma*sqrt(1/obs + (PartA_complete$IV-mean(PartA_complete$IV))^2/(var(PartA_complete$IV)*(obs-1)))
M5 <- lm(WH_L ~ IV, data=PartA_complete)
abline(M5, col='orange', 3, lwd=3)
M6 <- lm(WH_U ~ IV, data=PartA_complete)
abline(M6, col='orange', 3, lwd=3)
legend('bottomright', legend='Working-Hetelling Band', lty=1, lwd=3, col='orange')