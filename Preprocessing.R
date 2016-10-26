## Preprocessing script 
file <- list.files(pattern = ".csv")
file <- import_fn(file)


import_fn <- function(file)
{
  library(data.table)
  
  file <- fread(file[1])
  
}

## extract class and name of each colums

filename <- names(file)
fileclass <- sapply(file,class)
class(fileclass)
summarydf <- cbind(filename,fileclass)
class(summarydf)

## NA in the file


lapply(file,function(x) is.na(x))
NAC <- apply(is.na(file), 2, any) ## Nice genius1
as.vector(NAC)
class(NAC)
sumdf$NAC <- NULL
sumdf$NAC <- NAC
sumdf <- as.data.frame(summarydf)
cbind(summarydf,NAC)



install.packages("mice")
library(mice)
install.packages("glmnet")
install.packages("VIM")
install.packages("pbkrtest")
fil <- read.csv("msleep_ggplot2.csv")
md.pattern(fil)
library("VIM")
aggr(fil, prop=FALSE, numbers=TRUE)

## Imputation

library(mice)
data(sleep, package="VIM")
imp <- mice(sleep, seed=1234)
fit <- with(imp, lm(Dream ~ Span + Gest))
pooled <- pool(fit)
summary(pooled)
imp

## Data Exploration - Mutliple paramater


states <- as.data.frame(state.x77[,c("Murder", "Population", "Illiteracy", "Income", "Frost")])

## Relation btw parameter =- > Bivariate relations

cor(states)

install.packages("car")
library(car)


scatterplotMatrix(states, spread=FALSE, lty.smooth=2,
                  main="Scatter Plot Matrix")

# Simpler Linear Regression - one dependent one independent


fit <- lm(weight ~ height, data=women)
summary(fit)

fitted(fit)


residuals(fit)
plot(women$height,women$weight,
     xlab="Height (in inches)",
     ylab="Weight (in pounds)")
abline(fit)

# Polynomial Regression  - degree is higher than one

fit2 <- lm(weight ~ height + I(height^2), data=women)
plot(women$height,women$weight,
     xlab="Height (in inches)",
     ylab="Weight (in lbs)")

lines(women$height,fitted(fit2))

# Multiple Linear Regression

fitm <- lm(Murder ~ Population + Illiteracy + Income + Frost,data=states)
fitm <- lm(Murder ~ .,data=states)
summary(fitm)


## y = 

# Multi Collinearity  - Variance Inflation factor

vif(fitm)

fitm <- lm(Murder ~ Population + Illiteracy + Income + Frost,data=states)

## General rule - Sqrt vif > 2  indicates multi collinearity problem

outlierTest(fitm)
warnings()



## Feature selection - remove unwanted feature improve prediction accuracy

# Adjusted R2 , Cp Mellow, AIC , BIC 




# Sub set selection

library(leaps)
regfit.full <- regsubsets(Murder~. , data=states)
reg.summary <- summary(regfit.full)
names(summary(regfit.full))
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC")
coefficients(regfit.full,4)
coefficients(regfit.full,3)
coefficients(regfit.full,2)

#Forwards selection

# Forward and Backward Stepwise Selection

regfit.fwd=regsubsets(Murder~.,data=states,method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Murder~.,data=states,method="backward")
summary(regfit.bwd)
coef(regfit.full,4)
coef(regfit.fwd,3)
coef(regfit.bwd,3)

states

install.packages("riv")
library("riv")

install.packages("rpart")
library("riv")
install.packages("devtools")
library(devtools)
install_github("riv","tomasgreif")



#Regularization
#Dimension Reduction

# All the above is common for all model 

# Subset selection - Best subset 


# BIC - bayesion Information criterion





## Residuals:
#  Min      1Q  Median      3Q     Max 
# -4.7960 -1.6495 -0.0811  1.4815  7.6210 

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 1.235e+00  3.866e+00   0.319   0.7510    
# Population  2.237e-04  9.052e-05   2.471   0.0173 *  
#  Illiteracy  4.143e+00  8.744e-01   4.738 2.19e-05 ***
#  Income      6.442e-05  6.837e-04   0.094   0.9253    
# Frost       5.813e-04  1.005e-02   0.058   0.9541    
# ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 2.535 on 45 degrees of freedom
# Multiple R-squared:  0.567,	Adjusted R-squared:  0.5285 
# F-statistic: 14.73 on 4 and 45 DF,  p-value: 9.133e-08

# Estimate (parameter coefficent) 4.14 (illiteracy)

# For example, the regression coefficient for Illiteracy is 4.14, suggesting that an 
# increase of 1 percent in illiteracy is associated with a 4.14 percent increase in the murder rate, 
#controlling for population, income, and temperature. (suggesting an important parameter)

#  P Value (imporatance of a parameter)

# The coefficient is significantly different from zero at the p < .0001  level. On the other hand, 
# the coefficient for Frost isn't significantly different from zero (p = 0.954) suggesting that 
# Frost and Murder aren't linearly related when controlling for the other predictor variables

# R-squared:  0.567,  --> 57% variance is accounted for in the model

# The F statistic tests whether the predictor variables taken together, 
# predict the response variable above chance levels 





# Tomoroow - Imputation / Outlier / 
