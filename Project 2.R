wdir -> "C:\Users\Parv\Documents\Spring 2020\AMS 315\Project 2"
setwd(wdir)

Data <- read.csv('P2_69570.csv', header = TRUE)

M_E <- lm(Y ~ E1+E2+E3+E4, data=Data)
summary(M_E)
summary(M_E)$adj.r.squared

#Assuming only up to 2nd order interactions
M_raw <- lm( Y ~ ((E1+E2+E3+E4+G1+G2+G3+G4+G5+G6+G7+G8+G9+G10+G11+G12+G13+G14+G15+G16+G17+G18+G19+G20)^2), data=Data)
plot(resid(M_raw) ~ fitted(M_raw), main='Residual Plot')

#Using Box-Cox Transformation
library(MASS)
bc=boxcox(M_raw)
best.lam = bc$x[which(bc$y==max(bc$y))]
best.lam

M_trans <- lm( (Y^(best.lam)) ~ ((E1+E2+E3+E4+G1+G2+G3+G4+G5+G6+G7+G8+G9+G10+G11+G12+G13+G14+G15+G16+G17+G18+G19+G20)^2), data=Data)
# Here is the ADJ R^2 for Raw
summary(M_raw)$adj.r.square
# Here is the ADJ R^2 for Transformed
summary(M_trans)$adj.r.square
plot(resid(M_trans) ~ fitted(M_trans), main='New Residual Plot')

#Stepwise Regression
install.packages("leaps")
library(leaps)
M <- regsubsets( model.matrix(M_trans)[,-1], Data$Y^(best.lam), nbest = 1, nvmax=5, method = 'forward', intercept = TRUE )
temp <- summary(M)
temp

install.packages("knitr")
library(knitr)
Var <- colnames(model.matrix(M_trans))
M_select <- apply(temp$which, 1, function(x) paste0(Var[x], collapse='+'))
kable(data.frame(cbind( model = M_select, adjR2 = temp$adjr2, BIC = temp$bic)), caption='Model Summary')

M_main <- lm( (Y^(best.lam)) ~ (E1+E2+E3+E4+G1+G2+G3+G4+G5+G6+G7+G8+G9+G10+G11+G12+G13+G14+G15+G16+G17+G18+G19+G20), data=Data)
temp1 <- summary(M_main)
kable(temp1$coefficients[ abs(temp1$coefficients[,4]) <= 0.001, ], caption='Sig Coefficients')

M_2nd <- lm( (Y^(best.lam)) ~ (.)^2, data=Data)
temp2  <- summary(M_2nd)
kable(temp2$coefficients[ abs(temp2$coefficients[,4]) <= 0.01, ], caption='2nd Interaction')
#usage of p-value <= 0.01 is done instead of <= 0.001 since there are no variable or interactions with p-value <= 0.001
temp2
#temp2 gives p-values of all variables and its interations

#I tried to check if there were any 3rd interactions, but I found none
M_3rd <- lm( (Y^(best.lam)) ~ (G19+E2+E4+G2+G4)^3, data=Data)
temp3  <- summary(M_3rd)
kable(temp3$coefficients[ abs(temp3$coefficients[,4]) <= 0.01, ], caption='3rd Interaction')
#usage of p-value <= 0.01 is done instead of <= 0.001 since there are no variable or interactions with p-value <= 0.001
temp3
#temp3 gives p-values of all variables and its interations

M_2stage <- lm( (Y^(best.lam)) ~ (G19+E2+E4+G2+G4)^2, data=Data)
temp4 <- summary(M_2stage)
kable(temp4$coefficients[ abs(temp4$coefficients[,3]) >= 2, ], caption='M_2stage')
#usage of p-value <= 0.01 is done again
temp4
#temp4 gives p-values of all variables and its interations

#Plotting Residual vs Fitted Final Model
plot(resid(M_2stage) ~ fitted(M_2stage), main='Final Residual Plot')

# Anova Table
kable(anova(M_2stage), caption='ANOVA Table')

# Result Check - Using Confidence Intervals
confint(M_2stage, level=1-0.05)

# Model Found: Y^(best.lam)) ~ 56.8871 + 10.2680*E2 + 6.7590*E4 + 0.8517*E2*E4 - 16.6731*G2*G4
# Here is a check of regular regression to confirm the relevent varaibles
M_check <- lm( (Y^(best.lam)) ~ (E2+E4+G19+G2+G4), data=Data)
check <- summary(M_check)
check
