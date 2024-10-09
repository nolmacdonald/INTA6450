# Load packages
library(ggplot2)
library(reshape)

# Wages ------------------------------------------------------------------------
# Load wages data from AWS .csv
wages <- read.csv('http://inta.gatech.s3.amazonaws.com/wage2.csv')
# Write the wages data to a .csv in local directory
# write.csv(wages, "exercises/M5_regression/data/wage2.csv", row.names = FALSE)

# Setting the random number generator seed so that our results are reproducible
set.seed(123)

# Summary of wages statistics from data frame
summary_wages <- summary(wages)

# Tabulate education levels in the sample
table_wages <- table(wages$educ)

# Look at the distribution of education and wages in a scatter plot
plot_wages <- ggplot(data=wages, aes(x=educ, y=wage)) + geom_point() + stat_smooth(formula=y~x)
# Print the plot of education v. wages distribution
print(plot_wages)

# Fit a linear model where y is wage and x is education
model.results <- lm(wage ~ educ, data=wages)
# Print the results of the model
print(model.results)
cat(paste("\nmodel.results Summary:"))
print(summary(model.results))
# model.results Summary:
#   Call:
#   lm(formula = wage ~ educ, data = wages)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -877.38 -268.63  -38.38  207.05 2148.26 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  146.952     77.715   1.891   0.0589 .  
# educ          60.214      5.695  10.573   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 382.3 on 933 degrees of freedom
# Multiple R-squared:  0.107,	Adjusted R-squared:  0.106 
# F-statistic: 111.8 on 1 and 933 DF,  p-value: < 2.2e-16
# 
# educ predicted.wage
# 1    1       207.1667
# 2   12       869.5238
# 3   14       989.9524
# 4   50      3157.6666
# `geom_smooth()` using formula = 'y ~ x'

# Predict outcomes for different people with different levels of education, 1,
# 12, 14, and 50 years
data.to.predict <-data.frame(educ=c(1,12,14,50))
data.to.predict$predicted.wage <- predict(model.results, data.to.predict)
cat(paste("\nWage Data to Predict:\n"))
print(data.to.predict)
# Wage Data to Predict:
#   educ predicted.wage
# 1    1       207.1667
# 2   12       869.5238
# 3   14       989.9524
# 4   50      3157.6666
# `geom_smooth()` using formula = 'y ~ x'

# Max education is 18
# Predicted Wage
# Educ = 1: 207 - 115 min
# Educ = 12: 869 - at median for educ. median wage is 905
# Educ = 14: 990 - near mean. mean wage 958
# Educ = 50: 3158 - max is 3078
# P-value is very low: <2e-16 Pr(>|t|)  
# t-value 10.573
# error 5.695
# est. 60.214

# To control for more variables, add them to the Right hand side of the
# 'formula', which is the 'wage ~ educ' piece of the code
model.results.detail <- lm(wage ~ educ + IQ, data=wages)
cat(paste("\nModel 2 (educ + IQ) Summary:"))
print(summary(model.results.detail))
# Note that adding IQ here reduces the coefficient on education, which makes
# sense per the discussion of omitted variables that we have done
# Try using different combinations of variables to see what works and makes sense.

# Model 2 (educ + IQ) Summary:
#   Call:
#   lm(formula = wage ~ educ + IQ, data = wages)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -860.29 -251.00  -35.31  203.98 2110.38 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -128.8899    92.1823  -1.398    0.162    
# educ          42.0576     6.5498   6.421 2.15e-10 ***
#   IQ             5.1380     0.9558   5.375 9.66e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 376.7 on 932 degrees of freedom
# Multiple R-squared:  0.1339,	Adjusted R-squared:  0.132 
# F-statistic: 72.02 on 2 and 932 DF,  p-value: < 2.2e-16
# 
# `geom_smooth()` using formula = 'y ~ x'

# A picture of how the linear model does, with marginal distributions
# Save the picture by uncommenting the line below:
# ggsave('violin.png', width=7, height=5, units = "in")
plot_lm <- ggplot(data = wages, aes(x = educ, y = wage)) +
  geom_violin(aes(group = educ)) +
  stat_smooth(data = wages, 
              aes(x = educ, y = wage), 
              method = 'lm')
print(plot_lm)

# Labor Force Participation ----------------------------------------------------