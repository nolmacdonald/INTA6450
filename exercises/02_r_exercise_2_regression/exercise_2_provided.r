# Instrumental Variables example
library(sem)
#If gets error, add code< install.packages("Hmisc",dependecies=T)> or download a new version R studio
wages<-read.csv('http://inta.gatech.s3.amazonaws.com/wage2.csv')
iv.results<-tsls(lwage ~ educ + age + married + black + exper, ~ feduc + age + married + black + exper, data=wages)
# Run an instrumental variables regression. Note that father's education is
# used for an instrument, and is not in the first set of variables. Age,
# marital, status, and race are assumed to be less related to underlying
# ability and so are controlled for in the "first stage" regression too.

ols.results<-lm(lwage ~ educ + age + married + black + exper, data=wages)
# Run the analogous OLS regression without father's education

print(summary(ols.results))
print(summary(iv.results))
# Note that the point estimate for education has now gone up. This suggests
# that people who have higher education in a way that's correlated with
# their father having higher education, even holding fixed age, marital
# status, and race, receive ~13% higher wages for every year of education.
# Also note that the standard error on education is higher for the iv than
# ols results. The reason for this is that in IV, we're using only some of
# the variation in education: only the part related to father's education,
# so it's like there's less variation overall

# Stepwise regression
library(MASS)
start.model<-lm(wage ~ hours + IQ + KWW + educ + exper + tenure + age + married + black + south + urban + sibs, data=wages)
# Give an initial model, which will be the most coefficients we'd want to ever use
summary(start.model)
stepwise.model<- step(start.model)
# The command "step" adds and subtracts coefficients to maximize a measure of
# goodness of fit
summary(stepwise.model)

stepwise.model.interactions <- step(start.model, scope=wage~.^2)
# The command "step" adds and subtracts coefficients to maximize a measure of
# goodness of fit. 
summary(stepwise.model.interactions)

