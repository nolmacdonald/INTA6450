# Instrumental Variables example
# install.packages("sem")
library(sem)
wages<-read.csv('http://inta.gatech.s3.amazonaws.com/wage2.csv')
iv.results<-tsls(lwage ~ educ + age + married + black, ~ feduc + age + married + black, data=wages)
# Run an instrumental variables regression. Note that father's education is
# used for an instrument, and is not in the first set of variables. Age,
# marital, status, and race are assumed to be less related to underlying
# ability and so are controlled for in the "first stage" regression too.

ols.results<-lm(lwage ~ educ + age + married + black, data=wages)
# Run the analogous OLS regression without father's education

print(summary(ols.results))
print(summary(iv.results))
# Note that the point estimate for education has now gone up. This suggests
# that people who have higher education in a way that's correlated with
# their father having higher education, even holding fixed age, marital
# status, and race, receive ~9% higher wages for every year of education
# Also note that the standard error on education is higher for the iv than
# ols results. The reason for this is that in IV, we're using only some of
# the variation in education: only the part related to father's education,
# so it's like there's less variation overall

# Stepwise regression and tree example
# install.packages(c('MASS','sem'))
library(MASS)
start.model<-lm(wage ~ hours + IQ + KWW + educ + exper + tenure + age + married + black + south + urban + sibs, data=wages)
# Give an initial model, which will be the most coefficients we'd want to ever use
summary(start.model)
stepwise.model<- step(start.model)
# The command "step" adds and subtracts coefficients to maximize a measure of
# goodness of fit, by default AIC
summary(stepwise.model)

# install.packages('tree')
library(tree)
wage.tree <- tree(wage ~ married + hours + IQ + KWW + educ + tenure + exper, method="anova", data=wages)
# fit a tree
summary(wage.tree)
# Print the results

plot(wage.tree)
text(wage.tree)
# Plot the tree, and add the text decisions

cross.validation <- cv.tree(wage.tree)
# perform cross-validation, 10-fold. 

cross.validation
# look for the size with the lowest "dev" or deviance
# Why might this be different than the fitted tree?

pruned.wage.tree<-prune.tree(wage.tree,best=5)
# Prune this tree down to 5 terminal nodes, just to show this is how you would
# do it. Here this makes it worse though
summary(pruned.wage.tree)

lmfit<- lm(wage ~ married + hours + IQ + KWW + educ + tenure + exper, data=wages)
# Compute a regression using those same variables:
anova(lmfit)
summary(wage.tree)
# Notice that the mean sum of squared residuals was 130895 using the linear
# model, compared to the residual mean deviance of 130000 using trees. So the
# tree method has less residual error, which means it's a better predictor.
# This is especially striking given that the tree only uses KWW, educ, IQ,
# While the regression needs substantial contributions from tenure, experience,
# and marital status too.

predict(wage.tree, newdata=data.frame(IQ=100, KWW=50, educ=16, married=1, hours=40, tenure=3, exper=2))
predict(lmfit, newdata=data.frame(IQ=100, KWW=50, educ=16, married=1, hours=40, tenure=3, exper=2))
# These predictors give different estimates for the expected wage of a
# particular individual, relatively young, married, well educated, with good
# knowledge of the world of work. Which would you trust and why?

# Extension: try to build another tree, and see what it looks like. 
# You can sample the data with the command:
# install.packages('dplyr')
# library('dplyr')
# wages.sample <- sample(wages, n=500) 

# Takehome exercise from last class
lfp <- read.csv('http://inta.gatech.s3.amazonaws.com/mroz_train.csv')
lfp$inlf<-as.factor(lfp$inlf) # We need the outcome variable to be a 'factor'

inlf.tree <-tree(as.numeric(inlf) - 1 ~huseduc + husage + kidslt6 + kidsge6 + nwifeinc + educ + age, data=lfp, na.action=na.omit) # Fit the model
inlf.lm <- lm(as.numeric(inlf) - 1 ~huseduc + husage + kidslt6 + kidsge6 + nwifeinc + educ + age, data=lfp) # Fit the model
print(inlf.tree)
# compute predictions manually, and save them as lfp$predicted.inlf
lfp$predicted.inlf<-predict(inlf.tree)


library(pROC)
evaluate <- function(y_true, y_predicted, detail=FALSE) {
  curve<-roc(y_true, y_predicted)
  if (detail) {
  print(ci.auc(curve))
  print('Sensitivity (True Positive Rate)')
  tpr<-ci.se(curve)
  print(tpr)
  print('Specificity (True Negative Rate)')
  tnr<-ci.sp(curve)
  print(tnr)
  }
  cat('Point estimate (final word on effectiveness)\n')
  print(auc(curve))
  #print('Point estimate of AUCROC: ' + auc(curve))
}
evaluate(lfp$inlf, lfp$predicted.inlf)


library(randomForest)
lfp <- read.csv('http://inta.gatech.s3.amazonaws.com/mroz_train.csv')
lfp[is.na(lfp)] <- 0
rf <-randomForest(as.factor(inlf) ~ hours  +  kidslt6  , data=lfp)
evaluate(as.factor(lfp$inlf), predict(rf,type="prob")[,1])

lfp.out <- read.csv('http://inta.gatech.s3.amazonaws.com/mroz_test.csv')
lfp.out[is.na(lfp.out)] <- 0
evaluate(as.factor(lfp.out$inlf), predict(rf, newdata=lfp.out, type="prob")[,1])
evaluate(lfp.out$inlf, predict(inlf.lm, newdata=lfp.out))
evaluate(lfp.out$inlf, predict(inlf.tree, newdata=lfp.out))

# Summary of the code:
# The code performs various statistical analyses on wage and labor force participation datasets.
# It includes instrumental variables regression, ordinary least squares regression, stepwise regression, decision tree fitting, cross-validation, pruning, and random forest classification.
# It also evaluates model performance using ROC curves and AUC.

# Possibilities of expanding or improving:
# 1. Add more predictors to the models to see if they improve performance.
# 2. Use different machine learning algorithms like gradient boosting or support vector machines.
# 3. Perform hyperparameter tuning for the random forest model.
# 4. Implement k-fold cross-validation for all models to ensure robustness.
# 5. Compare models using additional metrics like precision, recall, and F1-score.
# 6. Visualize feature importance for tree-based models.

# Potential options to plot for these types of models:
# 1. ROC Curves: Plot ROC curves for different models to compare their performance.
# 2. Feature Importance: Plot feature importance for tree-based models like random forest.
# 3. Decision Trees: Visualize the structure of decision trees.
# 4. Residual Plots: Plot residuals to diagnose linear regression models.
# 5. Partial Dependence Plots: Show the effect of individual predictors on the predicted outcome.

# Code to add ROC curve plotting:
library(ggplot2)

plot_roc_curve <- function(y_true, y_predicted, model_name, save_path=NULL) {
  # Compute the ROC curve
  curve <- roc(y_true, y_predicted)
  
  # Calculate the AUC value
  auc_value <- auc(curve)
  
  # Create the ROC plot using ggplot2
  plot <- ggroc(curve) + 
    ggtitle(paste("ROC Curve for", model_name)) +
    annotate("text", x = 0.6, y = 0.2, label = paste("AUC =", round(auc_value, 2)), size = 5)
  
  # Print the plot to the console
  print(plot)
  
  # Save the plot to a file if a save path is provided
  if (!is.null(save_path)) {
    ggsave(filename = save_path, plot = plot)
  }
}

# Plot ROC curves for the models and save them to files
plot_roc_curve(lfp.out$inlf, predict(rf, newdata=lfp.out, type="prob")[,1], "Random Forest", "exercises/03_r_exercise_3_trees/random_forest_roc.png")
plot_roc_curve(lfp.out$inlf, predict(inlf.lm, newdata=lfp.out), "Linear Model", "exercises/03_r_exercise_3_trees/linear_model_roc.png")
plot_roc_curve(lfp.out$inlf, predict(inlf.tree, newdata=lfp.out), "Decision Tree", "exercises/03_r_exercise_3_trees/decision_tree_roc.png")
