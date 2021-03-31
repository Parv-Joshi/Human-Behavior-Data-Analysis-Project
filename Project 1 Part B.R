wdir -> "C:\Users\Parv\Documents\Spring 2020\AMS 315\Project 1"
setwd(wdir)

Data <- read.csv('P1B69570.csv', header = TRUE)

X=Data$x
Y=Data$y

plot(Y~X)
install.packages('knitr')
library(knitr)
M <- lm(Y ~ X, data=Data)
kable(anova(M), caption='ANOVA Table')
summary(M)


summary(lm(Y~X))$r.squared
summary(lm(Y~X^2))$r.squared
summary(lm(Y~X^3))$r.squared
summary(lm(Y~abs(X)))$r.squared
summary(lm(Y~(1/X)))$r.squared
summary(lm(Y~sqrt(X)))$r.squared
summary(lm(Y~exp(X)))$r.squared
summary(lm(Y~log(X)))$r.squared
summary(lm(Y~log10(X)))$r.squared
summary(lm(Y~log2(X)))$r.squared
summary(lm(Y~cos(X)))$r.squared
summary(lm(Y~sin(X)))$r.squared
summary(lm(Y~tan(X)))$r.squared
summary(lm(Y~poly(X)))$r.squared
summary(lm(Y~X))$r.squared
summary(lm(Y^2~X))$r.squared
summary(lm(Y^3~X))$r.squared
summary(lm(abs(Y)~X))$r.squared
summary(lm((1/Y)~X))$r.squared
summary(lm(sqrt(Y)~X))$r.squared
summary(lm(exp(Y)~X))$r.squared
summary(lm(log(Y)~X))$r.squared
summary(lm(log10(Y)~X))$r.squared
summary(lm(log2(Y)~X))$r.squared
summary(lm(cos(Y)~X))$r.squared
summary(lm(sin(Y)~X))$r.squared
summary(lm(tan(Y)~X))$r.squared
summary(lm(poly(Y)~X))$r.squared
summary(lm(Y^2~X))$r.squared
summary(lm(Y^2~X))$r.squared
summary(lm(Y^2~X^2))$r.squared
summary(lm(Y^2~X^3))$r.squared
summary(lm(Y^2~abs(X)))$r.squared
summary(lm(Y^2~(1/X)))$r.squared
summary(lm(Y^2~sqrt(X)))$r.squared
summary(lm(Y^2~exp(X)))$r.squared
summary(lm(Y^2~log(X)))$r.squared
summary(lm(Y^2~log10(X)))$r.squared
summary(lm(Y^2~log2(X)))$r.squared
summary(lm(Y^2~cos(X)))$r.squared
summary(lm(Y^2~sin(X)))$r.squared
summary(lm(Y^2~tan(X)))$r.squared
summary(lm(Y^2~poly(X)))$r.squared
summary(lm(Y^3~X))$r.squared
summary(lm(Y^3~X))$r.squared
summary(lm(Y^3~X^2))$r.squared
summary(lm(Y^3~X^3))$r.squared
summary(lm(Y^3~abs(X)))$r.squared
summary(lm(Y^3~(1/X)))$r.squared
summary(lm(Y^3~sqrt(X)))$r.squared
summary(lm(Y^3~exp(X)))$r.squared
summary(lm(Y^3~log(X)))$r.squared
summary(lm(Y^3~log10(X)))$r.squared
summary(lm(Y^3~log2(X)))$r.squared
summary(lm(Y^3~cos(X)))$r.squared
summary(lm(Y^3~sin(X)))$r.squared
summary(lm(Y^3~tan(X)))$r.squared
summary(lm(Y^3~poly(X)))$r.squared
summary(lm(abs(Y)~X))$r.squared
summary(lm(abs(Y)~X^2))$r.squared
summary(lm(abs(Y)~X^3))$r.squared
summary(lm(abs(Y)~abs(X)))$r.squared
summary(lm(abs(Y)~(1/X)))$r.squared
summary(lm(abs(Y)~sqrt(X)))$r.squared
summary(lm(abs(Y)~exp(X)))$r.squared
summary(lm(abs(Y)~log(X)))$r.squared
summary(lm(abs(Y)~log10(X)))$r.squared
summary(lm(abs(Y)~log2(X)))$r.squared
summary(lm(abs(Y)~cos(X)))$r.squared
summary(lm(abs(Y)~sin(X)))$r.squared
summary(lm(abs(Y)~tan(X)))$r.squared
summary(lm(abs(Y)~poly(X)))$r.squared
summary(lm((1/Y)~X))$r.squared
summary(lm((1/Y)~X^2))$r.squared
summary(lm((1/Y)~X^3))$r.squared
summary(lm((1/Y)~abs(X)))$r.squared
summary(lm((1/Y)~(1/X)))$r.squared
summary(lm((1/Y)~sqrt(X)))$r.squared
summary(lm((1/Y)~exp(X)))$r.squared
summary(lm((1/Y)~log(X)))$r.squared
summary(lm((1/Y)~log10(X)))$r.squared
summary(lm((1/Y)~log2(X)))$r.squared
summary(lm((1/Y)~cos(X)))$r.squared
summary(lm((1/Y)~sin(X)))$r.squared
summary(lm((1/Y)~tan(X)))$r.squared
summary(lm((1/Y)~poly(X)))$r.squared
summary(lm(sqrt(Y)~X))$r.squared
summary(lm(sqrt(Y)~X^2))$r.squared
summary(lm(sqrt(Y)~X^3))$r.squared
summary(lm(sqrt(Y)~abs(X)))$r.squared
summary(lm(sqrt(Y)~(1/X)))$r.squared
summary(lm(sqrt(Y)~sqrt(X)))$r.squared
summary(lm(sqrt(Y)~exp(X)))$r.squared
summary(lm(sqrt(Y)~log(X)))$r.squared
summary(lm(sqrt(Y)~log10(X)))$r.squared
summary(lm(sqrt(Y)~log2(X)))$r.squared
summary(lm(sqrt(Y)~cos(X)))$r.squared
summary(lm(sqrt(Y)~sin(X)))$r.squared
summary(lm(sqrt(Y)~tan(X)))$r.squared
summary(lm(sqrt(Y)~poly(X)))$r.squared
summary(lm(exp(Y)~X))$r.squared
summary(lm(exp(Y)~X^2))$r.squared
summary(lm(exp(Y)~X^3))$r.squared
summary(lm(exp(Y)~abs(X)))$r.squared
summary(lm(exp(Y)~(1/X)))$r.squared
summary(lm(exp(Y)~sqrt(X)))$r.squared
summary(lm(exp(Y)~exp(X)))$r.squared
summary(lm(exp(Y)~log(X)))$r.squared
summary(lm(exp(Y)~log10(X)))$r.squared
summary(lm(exp(Y)~log2(X)))$r.squared
summary(lm(exp(Y)~cos(X)))$r.squared
summary(lm(exp(Y)~sin(X)))$r.squared
summary(lm(exp(Y)~tan(X)))$r.squared
summary(lm(exp(Y)~poly(X)))$r.squared
summary(lm(log(Y)~X))$r.squared
summary(lm(log(Y)~X^2))$r.squared
summary(lm(log(Y)~X^3))$r.squared
summary(lm(log(Y)~abs(X)))$r.squared
summary(lm(log(Y)~(1/X)))$r.squared
summary(lm(log(Y)~sqrt(X)))$r.squared
summary(lm(log(Y)~exp(X)))$r.squared
summary(lm(log(Y)~log(X)))$r.squared
summary(lm(log(Y)~log10(X)))$r.squared
summary(lm(log(Y)~log2(X)))$r.squared
summary(lm(log(Y)~cos(X)))$r.squared
summary(lm(log(Y)~sin(X)))$r.squared
summary(lm(log(Y)~tan(X)))$r.squared
summary(lm(log(Y)~poly(X)))$r.squared
summary(lm(log10(Y)~X))$r.squared
summary(lm(log10(Y)~X^2))$r.squared
summary(lm(log10(Y)~X^3))$r.squared
summary(lm(log10(Y)~abs(X)))$r.squared
summary(lm(log10(Y)~(1/X)))$r.squared
summary(lm(log10(Y)~sqrt(X)))$r.squared
summary(lm(log10(Y)~exp(X)))$r.squared
summary(lm(log10(Y)~log(X)))$r.squared
summary(lm(log10(Y)~log10(X)))$r.squared
summary(lm(log10(Y)~log2(X)))$r.squared
summary(lm(log10(Y)~cos(X)))$r.squared
summary(lm(log10(Y)~sin(X)))$r.squared
summary(lm(log10(Y)~tan(X)))$r.squared
summary(lm(log10(Y)~poly(X)))$r.squared
summary(lm(log2(Y)~X))$r.squared
summary(lm(log2(Y)~X^2))$r.squared
summary(lm(log2(Y)~X^3))$r.squared
summary(lm(log2(Y)~abs(X)))$r.squared
summary(lm(log2(Y)~(1/X)))$r.squared
summary(lm(log2(Y)~sqrt(X)))$r.squared
summary(lm(log2(Y)~exp(X)))$r.squared
summary(lm(log2(Y)~log(X)))$r.squared
summary(lm(log2(Y)~log10(X)))$r.squared
summary(lm(log2(Y)~log2(X)))$r.squared
summary(lm(log2(Y)~cos(X)))$r.squared
summary(lm(log2(Y)~sin(X)))$r.squared
summary(lm(log2(Y)~tan(X)))$r.squared
summary(lm(log2(Y)~poly(X)))$r.squared
summary(lm(cos(Y)~X))$r.squared
summary(lm(cos(Y)~X^2))$r.squared
summary(lm(cos(Y)~X^3))$r.squared
summary(lm(cos(Y)~abs(X)))$r.squared
summary(lm(cos(Y)~(1/X)))$r.squared
summary(lm(cos(Y)~sqrt(X)))$r.squared
summary(lm(cos(Y)~exp(X)))$r.squared
summary(lm(cos(Y)~log(X)))$r.squared
summary(lm(cos(Y)~log10(X)))$r.squared
summary(lm(cos(Y)~log2(X)))$r.squared
summary(lm(cos(Y)~cos(X)))$r.squared
summary(lm(cos(Y)~sin(X)))$r.squared
summary(lm(cos(Y)~tan(X)))$r.squared
summary(lm(cos(Y)~poly(X)))$r.squared
summary(lm(sin(Y)~X))$r.squared
summary(lm(sin(Y)~X^2))$r.squared
summary(lm(sin(Y)~X^3))$r.squared
summary(lm(sin(Y)~abs(X)))$r.squared
summary(lm(sin(Y)~(1/X)))$r.squared
summary(lm(sin(Y)~sqrt(X)))$r.squared
summary(lm(sin(Y)~exp(X)))$r.squared
summary(lm(sin(Y)~log(X)))$r.squared
summary(lm(sin(Y)~log10(X)))$r.squared
summary(lm(sin(Y)~log2(X)))$r.squared
summary(lm(sin(Y)~cos(X)))$r.squared
summary(lm(sin(Y)~sin(X)))$r.squared
summary(lm(sin(Y)~tan(X)))$r.squared
summary(lm(sin(Y)~poly(X)))$r.squared
summary(lm(tan(Y)~X))$r.squared
summary(lm(tan(Y)~X^2))$r.squared
summary(lm(tan(Y)~X^3))$r.squared
summary(lm(tan(Y)~abs(X)))$r.squared
summary(lm(tan(Y)~(1/X)))$r.squared
summary(lm(tan(Y)~sqrt(X)))$r.squared
summary(lm(tan(Y)~exp(X)))$r.squared
summary(lm(tan(Y)~log(X)))$r.squared
summary(lm(tan(Y)~log10(X)))$r.squared
summary(lm(tan(Y)~log2(X)))$r.squared
summary(lm(tan(Y)~cos(X)))$r.squared
summary(lm(tan(Y)~sin(X)))$r.squared
summary(lm(tan(Y)~tan(X)))$r.squared
summary(lm(tan(Y)~poly(X)))$r.squared
summary(lm(exp(Y)~log(X)))
#Highest R^2 Values:
#> summary(lm(exp(Y)~log(X)))$r.squared
#[1] 0.6110614
#> summary(lm(exp(Y)~log10(X)))$r.squared
#[1] 0.6110614
#> summary(lm(exp(Y)~log2(X)))$r.squared
#[1] 0.6110614
library(MASS)
Model=lm(exp(y)~log(x), data = Data)
bc=boxcox(Model, lambda = seq(-3,3))
best.lam = bc$x[which(bc$y==max(bc$y))]
best.lam
Model.inv = lm(y^-1 ~ x, data=Data)
#plot(Model.inv)
#Boxcox R^2:
summary(lm((Y)^(-0.3030)~X))$r.squared
data_trans <- data.frame(xtrans=log(X), ytrans=exp(Y))
install.packages('alr3')
library(alr3)
fit_a <- lm(exp(Y)~log(X), data = data_trans)
pureErrorAnova(fit_a)
groupsx <- cut(data_trans$xtrans,breaks=c(-Inf,seq(min(data_trans$xtrans)+0.01, max(data_trans$xtrans)-0.01,by=0.01),Inf))
table(groups)
groupsy <- cut(data_trans$ytrans,breaks=c(-Inf,seq(min(data_trans$ytrans)+0.01, max(data_trans$ytrans)-0.01,by=0.01),Inf))
table(groups)
xBest <- ave(data_trans$xtrans, groupsx)
yBest <- ave(data_trans$ytrans, groupsy)
data_bin <- data.frame(x=x, y=data_trans$ytrans)
fit_b <- lm(yBest ~ xBest, data = data_bin)
pureErrorAnova(fit_b)
summary(fit_b)
coef(fit_b)
confint(fit_b, level=0.95)
plot(fit_b)