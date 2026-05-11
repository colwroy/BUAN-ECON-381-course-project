library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidymodels)
library(glmnet) #FOR glmnet() REGULARIZATION

#load data
housing1 <- read.csv("https://raw.githubusercontent.com/colwroy/BUAN-ECON-381-course-project/refs/heads/main/Housing.csv")
summary(housing1)

#change price to thousands for readability
housing1$price <- housing1$price / 1000

#binary variables changed from yes/no to 1 and 0
housing1$mainrbinary <- ifelse(housing1$mainroad == 'yes', 1, 0)
housing1$groombinary <- ifelse(housing1$guestroom == 'yes', 1, 0)
housing1$basementbinary <- ifelse(housing1$basement == 'yes', 1, 0)
housing1$hwaterbinary <- ifelse(housing1$hotwaterheating == 'yes', 1, 0)
housing1$acbinary <- ifelse(housing1$airconditioning == 'yes', 1, 0)
housing1$prefareabinary <- ifelse(housing1$prefarea == 'yes', 1, 0)

#create new DF with new binary columns
housing2 <- housing1[,c(1,2,3,4,5,11,14,15,16,17,18,19)]

#split the data first into training (70%) and holdout (30%)
set.seed(123)
split<-initial_split(housing2, .7, strata=price) #CREATE THE SPLIT
housing2_training<-training(split) #TRAINING PARTITION
housing2_holdout<-testing(split) #test PARTITION

#split holdout into testing (50%) and validation (50%)
split<-initial_split(housing2_holdout, .5, strata=price) #CREATE THE SPLIT
housing2_validation<-training(split) #TRAINING PARTITION
housing2_testing<-testing(split) #test PARTITION

#VERIFY STRATIFIED SAMPLING YIELDS EQUALLY SKEWED PARTITIONS
mean(housing2_training$price)
mean(housing2_holdout$price)
mean(housing2_validation$price)
mean(housing2_testing$price)

#export split data for sharing in CSV format
write.csv(housing2_training, "housing2_training.csv", row.names = TRUE)
write.csv(housing2_testing, "housing2_testing.csv", row.names = TRUE)
write.csv(housing2_validation, "housing2_validation.csv", row.names = TRUE)

#correlation matrix
#strongest correlates with price are area, number of bathrooms,
#number of stories, and acbinary?
cor(housing2_training)

#BIVARIATE regression (make model, make predictions, compute RMSE)
############################################################################

#bivariate model using house area and price
priceVarea <- lm(price~area, data = housing2_training)

housing2_training$priceVarea_predONtrain <- predict(priceVarea, housing2_training)
housing2_validation$priceVarea_predONvalidation <- predict(priceVarea, housing2_validation)

(priceVarea_RMSEtrain <- sqrt(mean((housing2_training$price-housing2_training$priceVarea_predONtrain)^2)))
(priceVarea_RMSEvalidation <- sqrt(mean((housing2_validation$price-housing2_validation$priceVarea_predONvalidation)^2)))
priceVarea_RMSE <- c("base model",priceVarea_RMSEtrain, priceVarea_RMSEvalidation)

#level log transform
#B1/100 * change in x1 = change in y
priceVarea_levelLog <- lm(price~log(area), data = housing2_training)

housing2_training$priceVarea_levelLog_predONtrain <- predict(priceVarea_levelLog, housing2_training)
housing2_validation$priceVarea_levelLog_predONvalidation <- predict(priceVarea_levelLog, housing2_validation)

(priceVarea_levelLog_RMSEtrain <- sqrt(mean((housing2_training$price-housing2_training$priceVarea_levelLog_predONtrain)^2)))
(priceVarea_levelLog_RMSEvalidation <- sqrt(mean((housing2_validation$price-housing2_validation$priceVarea_levelLog_predONvalidation)^2)))
priceVarea_levelLog_RMSE <- c("levelLog",priceVarea_levelLog_RMSEtrain, priceVarea_levelLog_RMSEvalidation)

#polynomial model
poly_priceVarea <- lm(price~area+I(area^2), data = housing2_training)

housing2_training$poly_priceVarea_predONtrain <- predict(poly_priceVarea, housing2_training)
housing2_validation$poly_priceVarea_predONvalidation <- predict(poly_priceVarea, housing2_validation)

(poly_priceVarea_RMSEtrain <- sqrt(mean((housing2_training$price-housing2_training$poly_priceVarea_predONtrain)^2)))
(poly_priceVarea_RMSEvalidation <- sqrt(mean((housing2_validation$price-housing2_validation$poly_priceVarea_predONvalidation)^2)))
poly_priceVarea_RMSE <- c("Poly",poly_priceVarea_RMSEtrain, poly_priceVarea_RMSEvalidation)

#spline model
#LOAD LIBRARY
library(mgcv) #FOR gam(), genralized additive models
#default spline with no tuning
#Y ~ S(X), WHERE S() IS A SMOOTH, NONPARAMETRIC FUNCTION
spline_priceVarea <- gam(price ~ s(area), data = housing2_training, family = 'gaussian')

housing2_training$spline_priceVarea_predONtrain <- predict(spline_priceVarea, housing2_training)
housing2_validation$spline_priceVarea_predONvalidation <- predict(spline_priceVarea, housing2_validation)

(spline_priceVarea_RMSEtrain <- sqrt(mean((housing2_training$price-housing2_training$spline_priceVarea_predONtrain)^2)))
(spline_priceVarea_RMSEvalidation <- sqrt(mean((housing2_validation$price-housing2_validation$spline_priceVarea_predONvalidation)^2)))
spline_priceVarea_RMSE <- c("SPLINE",spline_priceVarea_RMSEtrain, spline_priceVarea_RMSEvalidation)

#making a table of model RMSEs
model <- data.frame(matrix(ncol = 3, nrow = 1))
colnames(model) <- c("Model_Name","RMSEtrain", "RMSEvalidation")
model[1, ] <- priceVarea_RMSE
model[nrow(model) + 1, ] <- priceVarea_levelLog_RMSE
model[nrow(model) + 1, ] <- poly_priceVarea_RMSE
model[nrow(model) + 1, ] <- spline_priceVarea_RMSE
model[nrow(model) + 1, ] <- c("baseline", sqrt(sum(((mean(housing2_training$price) - housing2_training$price)^2)/nrow(housing2_training))),
                              sqrt(sum(((mean(housing2_validation$price) - housing2_validation$price)^2)/nrow(housing2_validation))))

###########################################################
#2nd degree poly has the lowest RMSE (~1646) on the validation set
##############################################################

#poly_priceVarea <- lm(price~area+I(area^2), data = housing2_training)

housing2_testing$poly_priceVarea_predONtest <- predict(poly_priceVarea, housing2_testing)

(poly_priceVarea_RMSEtest <- sqrt(mean((housing2_testing$price-housing2_testing$poly_priceVarea_predONtest)^2)))

#making graph of models against training data
ggplot(housing2_training, aes(x = area, y = price)) + 
  geom_point() +
  geom_line(linewidth = 0.6, aes(y = priceVarea_predONtrain, color = "base model")) +
  geom_line(linewidth = 0.6, aes(y = priceVarea_levelLog_predONtrain, color = "levelLog")) +
  geom_line(linewidth = 0.6, aes(y = poly_priceVarea_predONtrain, color = "2nd deg poly")) +
  geom_line(linewidth = 0.6, aes(y = spline_priceVarea_predONtrain, color = "SPLINE")) + 
  labs(title = "Bivariate Plot Training")
  

ggplot(housing2_validation, aes(x = area, y = price)) + 
  geom_point() +
  geom_line(linewidth = 0.6, aes(y = priceVarea_predONvalidation, color = "base model")) +
  geom_line(linewidth = 0.6, aes(y = priceVarea_levelLog_predONvalidation, color = "levelLog")) +
  geom_line(linewidth = 0.6, aes(y = poly_priceVarea_predONvalidation, color = "2nd deg poly")) +
  geom_line(linewidth = 0.6, aes(y = spline_priceVarea_predONvalidation, color = "SPLINE")) + 
  labs(title = "Bivariate Plot Validation")

#MULTI-VARIATE regression (make model, make predictions, compute RMSE)
############################################################################

#basic multivariate model with no transforms/regularization
basemulti <- lm(price~area+bathrooms+stories+acbinary, data = housing2_training)

housing2_training$basemulti_predONtrain <- predict(basemulti, housing2_training)
housing2_validation$basemulti_predONvalidation <- predict(basemulti, housing2_validation)

(basemulti_RMSEtrain <- sqrt(mean((housing2_training$price-housing2_training$basemulti_predONtrain)^2)))
(basemulti_RMSEvalidation <- sqrt(mean((housing2_validation$price-housing2_validation$basemulti_predONvalidation)^2)))
basemulti_RMSE <- c("basemulti", basemulti_RMSEtrain, basemulti_RMSEvalidation)

###########################
#regularization
#############################

#cv.glmnet() AUTOMATICALLY RUNS K-FOLD CROSS-VALIDATION
set.seed(123)
cv.ridge <- cv.glmnet(x=as.matrix(housing2_training[,c(2,4,5,11)]), #INPUT DATA 
                      y=housing2_training[,1], #OUTPUT VECTOR
                      alpha = 0, #1/0 FOR LASSO/RIDGE  
                      nfolds=3, #NUMBER OF FOLDS 
                      type.measure='mse') #LOSS METRIC

#lambda minimizes rmse
(best_lambda <- cv.ridge$lambda.min)

#model using lambda found with cross validation
ridge_model <- glmnet(housing2_training[,c(2,4,5,11)], 
                      y=housing2_training[,1],
                      alpha = 0, #RIDGE
                      lambda = best_lambda)

coef(ridge_model)

#manual calculation of RMSE for RIDGE model
housing2_training$ridge_model_predONtrain <- predict(ridge_model, as.matrix(housing2_training[,c(2,4,5,11)]))
housing2_validation$ridge_model_predONvalidation <- predict(ridge_model, as.matrix(housing2_validation[,c(2,4,5,11)]))

(ridge_RMSEtrain <- sqrt(mean((housing2_training$price-housing2_training$ridge_model_predONtrain)^2)))
(ridge_RMSEvalidation <- sqrt(mean((housing2_validation$price-housing2_validation$ridge_model_predONvalidation)^2)))
ridge_RMSE <- c("ridge", ridge_RMSEtrain, ridge_RMSEvalidation)

##############################
#support vector machine
##############################

library(e1071) #SVM LIBRARY

kern_type<-"radial" #SPECIFY KERNEL TYPE

#BUILD SVM CLASSIFIER
svm <- svm(price~area+bathrooms+stories+acbinary, 
           data = housing2_training, 
           type = "eps-regression", #set to "eps-regression" for numeric prediction
           kernel = kern_type,
           cost=1,                      #REGULARIZATION PARAMETER (lower=more)
           gamma = 1/(ncol(housing2_training)-1), #DEFAULT KERNEL PARAMETER
           coef0 = 0,                    #DEFAULT KERNEL PARAMETER
           degree=3,                     #POLYNOMIAL KERNEL PARAMETER
           scale = FALSE)                #RESCALE DATA? (SET TO TRUE TO NORMALIZE)

print(svm) #DIAGNOSTIC SUMMARY
#coef(svm) #WORKS ONLY FOR "linear" KERNEL

#REPORT IN AND OUT-OF-SAMPLE ERRORS
housing2_training$svm_predONtrain <- predict(svm, housing2_training)
housing2_validation$svm_predONvalidation <- predict(svm, housing2_validation)

(svm_RMSEtrain <- sqrt(mean((housing2_training$price-housing2_training$svm_predONtrain)^2)))
(svm_RMSEvalidation <- sqrt(mean((housing2_validation$price-housing2_validation$svm_predONvalidation)^2)))
svm_RMSE <- c("svm", svm_RMSEtrain, svm_RMSEvalidation)

#TUNING THE SVM BY CROSS-VALIDATION
tune_control<-tune.control(cross=3) #SET K-FOLD CV PARAMETERS
set.seed(123)
TUNE <- tune.svm(x = housing2_training[,c(2,4,5,11)],
                 y = housing2_training[,1],
                 type = "eps-regression",
                 kernel = kern_type,
                 tunecontrol=tune_control,
                 cost=c(.0001,.001,.1, 1, 10, 100,1000), #REGULARIZATION PARAMETER
                 gamma = 1/(ncol(housing2_training)-1), #KERNEL PARAMETER
                 coef0 = 0,           #KERNEL PARAMETER
                 degree = 3)          #POLYNOMIAL KERNEL PARAMETER

print(TUNE) #OPTIMAL TUNING PARAMETERS FROM VALIDATION PROCEDURE

#RE-BUILD MODEL USING OPTIMAL TUNING PARAMETERS
svm_tuned <- svm(price~area+bathrooms+stories+acbinary, 
                 data = housing2_training, 
                 type = "eps-regression", 
                 kernel = kern_type,
                 degree = TUNE$best.parameters$degree,
                 gamma = TUNE$best.parameters$gamma,
                 coef0 = TUNE$best.parameters$coef0,
                 cost = TUNE$best.parameters$cost,
                 scale = FALSE)

print(svm) #DIAGNOSTIC SUMMARY
coef(svm) #WORKS ONLY FOR "linear" KERNEL

#REPORT IN AND OUT-OF-SAMPLE ERRORS ON RETUNED MODEL
housing2_training$svm_tuned_predONtrain <- predict(svm_tuned, housing2_training)
housing2_validation$svm_tuned_predONvalidation <- predict(svm_tuned, housing2_validation)

(svm_tuned_RMSEtrain <- sqrt(mean((housing2_training$price-housing2_training$svm_tuned_predONtrain)^2)))
(svm_tuned_RMSEvalidation <- sqrt(mean((housing2_validation$price-housing2_validation$svm_tuned_predONvalidation)^2)))
svm_tuned_RMSE <- c("svm_tuned",svm_tuned_RMSEtrain, svm_tuned_RMSEvalidation)

#######################
#regression tree/CART model
########################
library(caret)
library(rpart.plot)

tree_spec <- decision_tree(min_n = 20, 
                           tree_depth = 30, 
                           cost_complexity = 0.01) %>%
  set_engine("rpart") %>%
  set_mode("regression")
print(tree_spec)

#estimate the model
tree_fmla <- price~area+bathrooms+stories+acbinary
tree <- tree_spec %>% 
  fit(formula = tree_fmla, data = housing2_training)

#visualizing the CART model
tree$fit %>% 
  rpart.plot(type = 1, extra = 1, roundint = FALSE)
plotcp(tree$fit)

#errors on untuned model
housing2_training$CART_predONtrain <- predict(tree, housing2_training)
housing2_validation$CART_predONvalidation <- predict(tree, housing2_validation)

CART_RMSEtrain <- sqrt(sum(((housing2_training$price - housing2_training$CART_predONtrain)^2)/nrow(housing2_training)))
CART_RMSEvalidation <- sqrt(sum(((housing2_validation$price - housing2_validation$CART_predONvalidation)^2)/nrow(housing2_validation)))
CART_RMSE <- c("CART",CART_RMSEtrain, CART_RMSEvalidation)

###################
#tuning CART
###################

#blank tree
tree_blank <- decision_tree(min_n = tune(),
                            tree_depth = tune(),
                            cost_complexity = tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")

#grid of tuning parameters
tree_grid <- grid_regular(parameters(min_n(),
                                     tree_depth(),
                                     cost_complexity()), levels = 3)

set.seed(123)
tree_tune_results <- tune_grid(tree_blank, 
                          preprocessor = price~area+bathrooms+stories+acbinary, 
                          resamples = vfold_cv(housing2_training, v=3),
                          grid = tree_grid)

#RETRIEVE OPTIMAL PARAMETERS FROM CROSS-VALIDATION
best_params <- select_best(tree_tune_results)

#FINALIZE THE MODEL SPECIFICATION
final_spec <- finalize_model(tree_blank, best_params)

#FIT THE FINALIZED MODEL
tuned_tree <- final_spec %>% fit(price~area+bathrooms+stories+acbinary, 
                                  housing2_training)

#visualizing the tuned CART model
tuned_tree$fit %>% 
  rpart.plot(type = 1, extra = 1, roundint = FALSE)
plotcp(tuned_tree$fit)

#errors on TUNED model
housing2_training$tunedCART_predONtrain <- predict(tuned_tree, housing2_training)
housing2_validation$tunedCART_predONvalidation <- predict(tuned_tree, housing2_validation)

tunedCART_RMSEtrain <- sqrt(sum(((housing2_training$price - housing2_training$tunedCART_predONtrain)^2)/nrow(housing2_training)))
tunedCART_RMSEvalidation <- sqrt(sum(((housing2_validation$price - housing2_validation$tunedCART_predONvalidation)^2)/nrow(housing2_validation)))
tunedCART_RMSE <- c("tunedCART",tunedCART_RMSEtrain, tunedCART_RMSEvalidation)

##########################
##GRADIENT BOOSTED MODEL##
##########################
library(xgboost)


#SPECIFY AND FIT IN ONE STEP:
boosted_forest <- boost_tree(min_n = NULL, #minimum number of observations for split
                             tree_depth = NULL, #max tree depth
                             trees = 100, #number of trees
                             mtry = NULL, #number of predictors selected at each split 
                             sample_size = NULL, #amount of data exposed to fitting
                             learn_rate = NULL, #learning rate for gradient descent
                             loss_reduction = NULL, #min loss reduction for further split
                             stop_iter = NULL)  %>% #maximum iteration for convergence
  set_engine("xgboost") %>%
  set_mode("regression") %>%
  fit(price~area+bathrooms+stories+acbinary, housing2_training)

#errors on untuned gradient boosted model
housing2_training$gradboost_predONtrain <- predict(boosted_forest, housing2_training)
housing2_validation$gradboost_predONvalidation <- predict(boosted_forest, housing2_validation)

gradboost_RMSEtrain <- sqrt(sum(((housing2_training$price - housing2_training$gradboost_predONtrain)^2)/nrow(housing2_training)))
gradboost_RMSEvalidation <- sqrt(sum(((housing2_validation$price - housing2_validation$gradboost_predONvalidation)^2)/nrow(housing2_validation)))
gradboost_RMSE <- c("gradboost",gradboost_RMSEtrain, gradboost_RMSEvalidation)

#############################
#tuning gradient boosted tree
###############################

#blank
boosted_forest_blank <- boost_tree(min_n = tune(), #minimum number of observations for split
                             tree_depth = tune(), #max tree depth
                             trees = tune(), #number of trees
                             mtry = NULL, #number of predictors selected at each split 
                             sample_size = NULL, #amount of data exposed to fitting
                             learn_rate = NULL, #learning rate for gradient descent
                             loss_reduction = tune(), #min loss reduction for further split
                             stop_iter = NULL)  %>% #maximum iteration for convergence
  set_engine("xgboost") %>%
  set_mode("regression")

#create tuning grid
boosted_forest_grid <- grid_regular(parameters(min_n(),
                                     tree_depth(),
                                     trees(),
                                     loss_reduction()), levels = 3)

set.seed(123)
boost_tune_results <- tune_grid(boosted_forest_blank, 
                          preprocessor = price~area+bathrooms+stories+acbinary, 
                          resamples = vfold_cv(housing2_training, v=3),
                          grid = boosted_forest_grid)

#RETRIEVE OPTIMAL PARAMETERS FROM CROSS-VALIDATION
best_params <- select_best(boost_tune_results)

#FINALIZE THE MODEL SPECIFICATION
final_spec <- finalize_model(boosted_forest_blank, best_params)

#FIT THE FINALIZED MODEL
boosted_forest_tuned <- final_spec %>% fit(price~area+bathrooms+stories+acbinary, 
                                 housing2_training)

#errors on TUNED model
housing2_training$boost_tune_predONtrain <- predict(boosted_forest_tuned, housing2_training)
housing2_validation$boost_tune_predONvalidation <- predict(boosted_forest_tuned, housing2_validation)

boost_tune_RMSEtrain <- sqrt(sum(((housing2_training$price - housing2_training$boost_tune_predONtrain)^2)/nrow(housing2_training)))
boost_tune_RMSEvalidation <- sqrt(sum(((housing2_validation$price - housing2_validation$boost_tune_predONvalidation)^2)/nrow(housing2_validation)))
boost_tune_RMSE <- c("boost_tune",boost_tune_RMSEtrain, boost_tune_RMSEvalidation)

##############################
#making a table of model RMSEs
##############################

#RMSE of taking the mean of price data
mean_RMSEtrain <- sqrt(sum(((mean(housing2_training$price) - housing2_training$price)^2)/nrow(housing2_training)))
mean_RMSEvalidation <- sqrt(sum(((mean(housing2_validation$price) - housing2_validation$price)^2)/nrow(housing2_validation)))
baseline_multivar_RMSE <- c("baseline", mean_RMSEtrain, mean_RMSEvalidation)

#making table
multivar_model <- data.frame(matrix(ncol = 3, nrow = 1))
colnames(multivar_model) <- c("Model_Name","RMSEtrain", "RMSEvalidation")
multivar_model[1, ] <- basemulti_RMSE
multivar_model[nrow(multivar_model) + 1, ] <- ridge_RMSE
multivar_model[nrow(multivar_model) + 1, ] <- svm_RMSE
multivar_model[nrow(multivar_model) + 1, ] <- svm_tuned_RMSE
multivar_model[nrow(multivar_model) + 1, ] <- CART_RMSE
multivar_model[nrow(multivar_model) + 1, ] <- tunedCART_RMSE
multivar_model[nrow(multivar_model) + 1, ] <- gradboost_RMSE
multivar_model[nrow(multivar_model) + 1, ] <- boost_tune_RMSE
multivar_model[nrow(multivar_model) + 1, ] <- baseline_multivar_RMSE

############################################################
#tuned gradient boosted tree outperforms base multivariate slightly (RMSE ~1331)

#base multivariate model 2nd (RMSE ~1366), 
#followed by ridge regularized version 3rd (RMSE ~1370)
#on validation set
#############################################################

#tuned gradient boost

housing2_testing$boost_tune_predONtesting <- predict(boosted_forest_tuned, housing2_testing)

(boost_tune_RMSEtesting <- sqrt(sum(((housing2_testing$price - housing2_testing$boost_tune_predONtesting)^2)/nrow(housing2_testing))))

#basemulti <- lm(price~area+bathrooms+stories+acbinary, data = housing2_training)

housing2_testing$basemulti_predONtest <- predict(basemulti, housing2_testing)

(basemulti_RMSEtest <- sqrt(mean((housing2_testing$price-housing2_testing$basemulti_predONtest)^2)))

#BINARY CLASSIFICATION TOOLS
#########################################################################
#ROC curve
library(pROC)

#logistic classification model
logit_bin <- glm(acbinary~price+stories, data=housing2_training, family = "binomial")

housing2_training$logit_bin_predONtrain <- predict(logit_bin, housing2_training, type = "response")
housing2_validation$logit_bin_predONvalidation <- predict(logit_bin, housing2_validation, type = "response")

roc_obj_logitrain <- roc(housing2_training$acbinary, housing2_training$logit_bin_predONtrain)
roc_obj_logivalidation <- roc(housing2_validation$acbinary, housing2_validation$logit_bin_predONvalidation)

train_logi_auc <- auc(roc_obj_logitrain)
validation_logi_auc <- auc(roc_obj_logivalidation)

plot(roc_obj_logitrain, main="ROC Curve train logi", col="#1c61b1", lwd=4, print.auc=TRUE)
plot(roc_obj_logivalidation, main="ROC Curve validation logi", col="#1c61b1", lwd=4, print.auc=TRUE)

#accuracy using a 0.7 decision threshold
(logit_bin_accuracyTRAIN<-mean((housing2_training$logit_bin_predONtrain>0.7)==housing2_training$acbinary))
(logit_bin_accuracyVALIDATION<-mean((housing2_validation$logit_bin_predONvalidation>0.7)==housing2_validation$acbinary))

logit_perf <- c("logit binary", train_logi_auc, validation_logi_auc, 
                 logit_bin_accuracyTRAIN, logit_bin_accuracyVALIDATION)

#probit classification model
probit_bin <- glm(acbinary~price+stories,data=housing2_training, family = binomial(link = "probit"))

housing2_training$probit_bin_predONtrain <- predict(probit_bin, housing2_training, type = "response")
housing2_validation$probit_bin_predONvalidation <- predict(probit_bin, housing2_validation, type = "response")

roc_obj_probitrain <- roc(housing2_training$acbinary, housing2_training$probit_bin_predONtrain)
roc_obj_probivalidation <- roc(housing2_validation$acbinary, housing2_validation$probit_bin_predONvalidation)

train_probi_auc <- auc(roc_obj_probitrain)
validation_probi_auc <- auc(roc_obj_probivalidation)

plot(roc_obj_probitrain, main="ROC Curve train probi", col="#1c61b1", lwd=4, print.auc=TRUE)
plot(roc_obj_probivalidation, main="ROC Curve validation probi", col="#1c61b1", lwd=4, print.auc=TRUE)

#accuracy using a 0.7 decision threshold
(probit_bin_accuracyTRAIN<-mean((housing2_training$probit_bin_predONtrain>0.7)==housing2_training$acbinary))
(probit_bin_accuracyVALIDATION<-mean((housing2_validation$probit_bin_predONvalidation>0.7)==housing2_validation$acbinary))

probit_perf <- c("probit binary", train_probi_auc, validation_probi_auc, 
                 probit_bin_accuracyTRAIN, probit_bin_accuracyVALIDATION)

#BINARY CLASSIFICATION TOOLS (additional tools)
#########################################################################

##############################
#support vector machine
##############################

library(e1071) #SVM LIBRARY

kern_type<-"radial" #SPECIFY KERNEL TYPE

#BUILD SVM CLASSIFIER
svm_classif <- svm(acbinary~price+stories, 
           data = housing2_training, 
           type = "C-classification", #set to "eps-regression" for numeric prediction
           kernel = kern_type,
           cost=1,                      #REGULARIZATION PARAMETER (lower=more)
           gamma = 1/(ncol(housing2_training)-1), #DEFAULT KERNEL PARAMETER
           coef0 = 0,                    #DEFAULT KERNEL PARAMETER
           degree=2,                     #POLYNOMIAL KERNEL PARAMETER
           scale = FALSE)                #RESCALE DATA? (SET TO TRUE TO NORMALIZE)

print(svm_classif) #DIAGNOSTIC SUMMARY
coef(svm_classif) #WORKS ONLY FOR "linear" KERNEL

#REPORT IN AND OUT-OF-SAMPLE ACCURACY
(SVM_classif_accuracyTRAIN<-mean(predict(svm_classif, housing2_training)==housing2_training$acbinary))
(SVM_classif_accuracyVALIDATION<-mean(predict(svm_classif, housing2_validation)==housing2_validation$acbinary))

svm_perf <- c("svm binary", NA, NA, 
                 SVM_classif_accuracyTRAIN, SVM_classif_accuracyVALIDATION)

#TUNING THE SVM BY CROSS-VALIDATION
tune_control<-tune.control(cross=3) #SET K-FOLD CV PARAMETERS
set.seed(123)
TUNE_classif <- tune.svm(x = housing2_training[,c(1, 5)],
                 y = as.factor(housing2_training[,11]),
                 type = "C-classification",
                 kernel = kern_type,
                 tunecontrol=tune_control,
                 cost=c(.0001,.001,.1, 1, 10, 100,1000), #REGULARIZATION PARAMETER
                 gamma = 1/(ncol(housing2_training)-1), #KERNEL PARAMETER
                 coef0 = 0,           #KERNEL PARAMETER
                 degree = 2 )#POLYNOMIAL KERNEL PARAMETER

print(TUNE_classif) #OPTIMAL TUNING PARAMETERS FROM VALIDATION PROCEDURE

#RE-BUILD MODEL USING OPTIMAL TUNING PARAMETERS
svm_classif_tuned <- svm(acbinary~price+stories, 
                 data = housing2_training, 
                 type = "C-classification", 
                 kernel = kern_type,
                 degree = TUNE_classif$best.parameters$degree,
                 gamma = TUNE_classif$best.parameters$gamma,
                 coef0 = TUNE_classif$best.parameters$coef0,
                 cost = TUNE_classif$best.parameters$cost,
                 scale = FALSE)

print(svm_classif_tuned) #DIAGNOSTIC SUMMARY
coef(svm_classif_tuned) #WORKS ONLY FOR "linear" KERNEL

#REPORT IN AND OUT-OF-SAMPLE ACCURACY ON RETUNED MODEL
(SVM_classif_tuned_accuracyTRAIN<-mean(predict(svm_classif_tuned, housing2_training)==housing2_training$acbinary))
(SVM_classif_tuned_accuracyVALIDATION<-mean(predict(svm_classif_tuned, housing2_validation)==housing2_validation$acbinary))

SVM_classif_tuned_perf <- c("SVM tuned binary", NA, NA, 
                 SVM_classif_tuned_accuracyTRAIN, SVM_classif_tuned_accuracyVALIDATION)

#######################
#regression tree/CART model
########################
library(caret)
library(rpart.plot)

classif_tree_spec <- decision_tree(min_n = 20, 
                           tree_depth = 30, 
                           cost_complexity = 0.01) %>%
  set_engine("rpart") %>%
  set_mode("classification")
print(classif_tree_spec)

#estimate the model
classif_tree_fmla <- as.factor(acbinary)~price+stories
classif_tree <- classif_tree_spec %>% 
  fit(formula = classif_tree_fmla, data = housing2_training)

#visualizing the CART model
classif_tree$fit %>% 
  rpart.plot(type = 1, extra = 1, roundint = FALSE)
plotcp(classif_tree$fit)

#accuracy of untuned model
housing2_training$classif_CART_predONtrain <- predict(classif_tree, housing2_training, type = "prob")
housing2_validation$classif_CART_predONvalidation <- predict(classif_tree, housing2_validation, type = "prob")

(classif_CART_accuracyTRAIN<-mean(predict(classif_tree, housing2_training)==housing2_training$acbinary))
(classif_CART_accuracyVALIDATION<-mean(predict(classif_tree, housing2_validation)==housing2_validation$acbinary))

roc_obj_CARTtrain <- roc(housing2_training$acbinary, housing2_training$classif_CART_predONtrain$.pred_1)
roc_obj_CARTvalidation <- roc(housing2_validation$acbinary, housing2_validation$classif_CART_predONvalidation$.pred_1)

train_CART_auc <- auc(roc_obj_CARTtrain)
validation_CART_auc <- auc(roc_obj_CARTvalidation)

classif_CART_perf <- c("CART binary", train_CART_auc, validation_CART_auc, 
                 classif_CART_accuracyTRAIN, classif_CART_accuracyVALIDATION)

###################
#tuning CART
###################

#blank tree
classif_tree_blank <- decision_tree(min_n = tune(),
                            tree_depth = tune(),
                            cost_complexity = tune()) %>%
  set_engine("rpart") %>%
  set_mode("classification")

#grid of tuning parameters
classif_tree_grid <- grid_regular(parameters(min_n(),
                                     tree_depth(),
                                     cost_complexity()), levels = 3)

set.seed(123)
classif_tree_tune_results <- tune_grid(classif_tree_blank, 
                               preprocessor = as.factor(acbinary)~price+stories, 
                               resamples = vfold_cv(housing2_training, v=3),
                               grid = classif_tree_grid)

#RETRIEVE OPTIMAL PARAMETERS FROM CROSS-VALIDATION
c_best_params <- select_best(classif_tree_tune_results)

#FINALIZE THE MODEL SPECIFICATION
c_final_spec <- finalize_model(classif_tree_blank, c_best_params)

#FIT THE FINALIZED MODEL
classif_tuned_tree <- c_final_spec %>% fit(as.factor(acbinary)~price+stories, 
                                 housing2_training)

#visualizing the CART model
classif_tuned_tree$fit %>% 
  rpart.plot(type = 1, extra = 1, roundint = FALSE)
plotcp(classif_tuned_tree$fit)

#accuracy of TUNED model
housing2_training$classif_tunedCART_predONtrain <- predict(classif_tuned_tree, housing2_training, type = "prob")
housing2_validation$classif_tunedCART_predONvalidation <- predict(classif_tuned_tree, housing2_validation, type = "prob")

(classif_tunedCART_accuracyTRAIN<-mean(predict(classif_tuned_tree, housing2_training)==housing2_training$acbinary))
(classif_tunedCART_accuracyVALIDATION<-mean(predict(classif_tuned_tree, housing2_validation)==housing2_validation$acbinary))

roc_obj_tunedCARTtrain <- roc(housing2_training$acbinary, housing2_training$classif_tunedCART_predONtrain$.pred_1)
roc_obj_tunedCARTvalidation <- roc(housing2_validation$acbinary, housing2_validation$classif_tunedCART_predONvalidation$.pred_1)

train_tunedCART_auc <- auc(roc_obj_tunedCARTtrain)
validation_tunedCART_auc <- auc(roc_obj_tunedCARTvalidation)

classif_tunedCART_perf <- c("tuned CART binary", train_tunedCART_auc, validation_tunedCART_auc, 
                 classif_tunedCART_accuracyTRAIN, classif_tunedCART_accuracyVALIDATION)

##########################
##GRADIENT BOOSTED MODEL##
##########################
library(xgboost)


#SPECIFY AND FIT IN ONE STEP:
classif_boosted_forest <- boost_tree(min_n = NULL, #minimum number of observations for split
                             tree_depth = NULL, #max tree depth
                             trees = 100, #number of trees
                             mtry = NULL, #number of predictors selected at each split 
                             sample_size = NULL, #amount of data exposed to fitting
                             learn_rate = NULL, #learning rate for gradient descent
                             loss_reduction = NULL, #min loss reduction for further split
                             stop_iter = NULL)  %>% #maximum iteration for convergence
  set_engine("xgboost") %>%
  set_mode("classification") %>%
  fit(as.factor(acbinary)~price+stories, housing2_training)

#accuracy of untuned gradient boosted model
housing2_training$classif_gradboost_predONtrain <- predict(classif_boosted_forest, housing2_training, type = "prob")
housing2_validation$classif_gradboost_predONvalidation <- predict(classif_boosted_forest, housing2_validation, type = "prob")

(classif_gradboost_accuracyTRAIN<-mean(predict(classif_boosted_forest, housing2_training)==housing2_training$acbinary))
(classif_gradboost_accuracyVALIDATION<-mean(predict(classif_boosted_forest, housing2_validation)==housing2_validation$acbinary))

roc_obj_gradboosttrain <- roc(housing2_training$acbinary, housing2_training$classif_gradboost_predONtrain$.pred_1)
roc_obj_gradboostvalidation <- roc(housing2_validation$acbinary, housing2_validation$classif_gradboost_predONvalidation$.pred_1)

train_gradboost_auc <- auc(roc_obj_gradboosttrain)
validation_gradboost_auc <- auc(roc_obj_gradboostvalidation)

classif_gradboost_perf <- c("XGBOOST binary", train_gradboost_auc, validation_gradboost_auc, 
                 classif_gradboost_accuracyTRAIN, classif_gradboost_accuracyVALIDATION)

#############################
#tuning gradient boosted tree
###############################

#blank
classif_boosted_forest_blank <- boost_tree(min_n = tune(), #minimum number of observations for split
                                   tree_depth = tune(), #max tree depth
                                   trees = tune(), #number of trees
                                   mtry = NULL, #number of predictors selected at each split 
                                   sample_size = NULL, #amount of data exposed to fitting
                                   learn_rate = NULL, #learning rate for gradient descent
                                   loss_reduction = tune(), #min loss reduction for further split
                                   stop_iter = NULL)  %>% #maximum iteration for convergence
  set_engine("xgboost") %>%
  set_mode("classification")

#create tuning grid
classif_boosted_forest_grid <- grid_regular(parameters(min_n(),
                                               tree_depth(),
                                               trees(),
                                               loss_reduction()), levels = 3)

set.seed(123)
classif_boost_tune_results <- tune_grid(classif_boosted_forest_blank, 
                                preprocessor = as.factor(acbinary)~price+stories, 
                                resamples = vfold_cv(housing2_training, v=3),
                                grid = classif_boosted_forest_grid)

#RETRIEVE OPTIMAL PARAMETERS FROM CROSS-VALIDATION
c2_best_params <- select_best(classif_boost_tune_results)

#FINALIZE THE MODEL SPECIFICATION
c2_final_spec <- finalize_model(classif_boosted_forest_blank, c2_best_params)

#FIT THE FINALIZED MODEL
classif_boosted_forest_tuned <- c2_final_spec %>% fit(as.factor(acbinary)~price+stories, 
                                           housing2_training)

#accuracy of TUNED model
housing2_training$classif_boost_tune_predONtrain <- predict(classif_boosted_forest_tuned, housing2_training, type = "prob")
housing2_validation$classif_boost_tune_predONvalidation <- predict(classif_boosted_forest_tuned, housing2_validation, type = "prob")

(classif_boost_tune_accuracyTRAIN<-mean(predict(classif_boosted_forest_tuned, housing2_training)==housing2_training$acbinary))
(classif_boost_tune_accuracyVALIDATION<-mean(predict(classif_boosted_forest_tuned, housing2_validation)==housing2_validation$acbinary))

roc_obj_boosttunetrain <- roc(housing2_training$acbinary, housing2_training$classif_boost_tune_predONtrain$.pred_1)
roc_obj_boosttunevalidation <- roc(housing2_validation$acbinary, housing2_validation$classif_boost_tune_predONvalidation$.pred_1)

train_boosttune_auc <- auc(roc_obj_boosttunetrain)
validation_boosttune_auc <- auc(roc_obj_boosttunevalidation)

classif_boost_tuned_perf <- c("XGBOOST tuned binary", train_boosttune_auc, validation_boosttune_auc, 
                 classif_boost_tune_accuracyTRAIN, classif_boost_tune_accuracyVALIDATION)

##############################
#making a table of model performances
##############################

#no information rate for training: ~0.68 if always choosing '0'
no_info_rate_TRAIN <- sum(housing2_training$acbinary == 0) / nrow(housing2_training)

no_info_rate_VALIDATION <- sum(housing2_validation$acbinary == 0) / nrow(housing2_validation)

baseline <- c("baseline", 0.50, 0.50, no_info_rate_TRAIN, no_info_rate_VALIDATION)

#table
classif_model <- data.frame(matrix(ncol = 5, nrow = 1))
colnames(classif_model) <- c("Model_Name","train auc", "validation auc", 
                     "training accuracy", "validation accuracy")
classif_model[1, ] <- logit_perf
classif_model[nrow(classif_model) + 1, ] <- probit_perf
classif_model[nrow(classif_model) + 1, ] <- svm_perf
classif_model[nrow(classif_model) + 1, ] <- SVM_classif_tuned_perf
classif_model[nrow(classif_model) + 1, ] <- classif_CART_perf
classif_model[nrow(classif_model) + 1, ] <- classif_tunedCART_perf
classif_model[nrow(classif_model) + 1, ] <- classif_gradboost_perf
classif_model[nrow(classif_model) + 1, ] <- classif_boost_tuned_perf
classif_model[nrow(classif_model) + 1, ] <- baseline

##################################################################
#CART performs the best with an accuracy of ~0.756, followed by
#logit ~0.744
#####################################################################

#CART model
housing2_testing$classif_CART_predONtest <- predict(classif_tree, housing2_testing, type = "prob")

(classif_CART_accuracyTEST<-mean(predict(classif_tree, housing2_testing)==housing2_testing$acbinary))

roc_obj_CARTtest <- roc(housing2_testing$acbinary, housing2_testing$classif_CART_predONtest$.pred_1)

(test_CART_auc <- auc(roc_obj_CARTtest))

plot(roc_obj_CARTtest, main="ROC Curve test CART", col="#1c61b1", lwd=4, print.auc=TRUE)
