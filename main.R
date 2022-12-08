##########################################################
#Assy2-Grp-E: u2249614, u2285509, u2262639, u1259089
##########################################################

##########################################################
#Importing Libraries
##########################################################


source("funcs.R")

activate.packages.if.missing("janitor")
activate.packages.if.missing("dplyr")
activate.packages.if.missing("ggplot2")
activate.packages.if.missing("ggExtra")
activate.packages.if.missing("ggcorrplot")
activate.packages.if.missing("schoolmath")
activate.packages.if.missing("PerformanceAnalytics")
activate.packages.if.missing("vcd")
activate.packages.if.missing("grid")
activate.packages.if.missing("Formula")
activate.packages.if.missing("lattice")
activate.packages.if.missing("survival")
activate.packages.if.missing("Hmisc")
activate.packages.if.missing("permute")
activate.packages.if.missing("vegan")
activate.packages.if.missing("car")
activate.packages.if.missing("ggstatsplot")
activate.packages.if.missing("tidyverse")
activate.packages.if.missing("DescTools")
activate.packages.if.missing("ggpubr")
activate.packages.if.missing("modelr")
activate.packages.if.missing("broom")
activate.packages.if.missing("funModeling")
activate.packages.if.missing("Stat2Data")
activate.packages.if.missing("sm")
activate.packages.if.missing("olsrr")
activate.packages.if.missing("glmnet")
activate.packages.if.missing("leaps")
activate.packages.if.missing("plyr")
activate.packages.if.missing("arm")
activate.packages.if.missing("oem")
activate.packages.if.missing("bestglm")
activate.packages.if.missing("GGally")

# Avoid scientific display of numbers
options(scipen = 999)
options(digits=4)

##########################################################
#Data Preparation
##########################################################

load("TeleChurn.Rdata")

head(TeleChurn)
summary(TeleChurn)

#Removing duplicates
TeleChurn1 <- TeleChurn[!duplicated(as.list(TeleChurn))]
summary(TeleChurn1)
#We can confirm that there is no duplicate

#Missing value
TeleChurn1_qual <- status(TeleChurn1)
TeleChurn1_qual %>% 
  mutate(across(where(is.numeric), ~ round(., 3)))
#We can confirm that there is no missing value

TeleChurn_pre <- TeleChurn1 %>%
  mutate(
    Churn = factor(Churn, 
                   levels=c(0,1), 
                   labels=c("Non-churn","Churn")),
    Complains = factor(Complains, 
                       levels=c(0,1), 
                       labels=c("No complaint","Complaint")),
    TariffPlan = factor(TariffPlan,
                        levels=c(1,2),
                        labels=c("PAYG","Contractual")),
    Status = factor(Status,
                    levels=c(1,2),
                    labels=c("Active","Non-Active")),
    ChargeAmount = ordered(ChargeAmount),
    AgeGroup = ordered(AgeGroup)
  )

summary(TeleChurn_pre)

# Attach ready dataset and prepare model initial reg formula


##########################################################
#EDA
##########################################################

attach(TeleChurn_pre)
yvar.name <- "Churn"
yvar <- Churn
xvar.names <- names(TeleChurn_pre) %>% .[. != "Churn"]

#----------------------
# Numeric explanatory variables empirical logit plots
#----------------------
cont.xvars <- list(
  "Number of call failures" = CallFailure,
  "Length of subscription" = SubscriptionLength,
  "Total duration of calls / seconds" = SecondsOfUse,
  "Number of calls" = FrequencyOfUse,
  "Number of text messages" = FrequencyOfSMS,
  "Number of distinct called numbers" = DistinctCalledNumbers)

scs <- c(5, 15, 1, 1, 7, 2)
plot.emplogits(
  yvar, cont.xvars, scs, new.window=T)

plot.conditional.densities(
  yvar, cont.xvars, mfrow=c(3,2), new.window=T)

# NOTES:
# Many variables show non-linearity under logit transformation.
# Transformations of variables will be applied later on to see if we can improve
# this fit.

#----------------------
# Boxplots
#----------------------

plot.boxes(
  yvar, cont.xvars, xlab="Churn", cex.lab=1.5, new.window=T)

# Boxplots show that while the distributions of SubscriptionLength is reasonably 
# symmetric the distribution of other five variables are quite skewed. 

# For skewed variables, attempt to transform them to normal and include - source: (Sheather, 2009)

#----------------------
# Tranformations
#----------------------

# CallFailure: squared (normal)
# Subscription Length: log (normal), log ^ 3 (cubic shape on logit plot)
# SecondsOfUse: log (normal), log^2 (non-constant variance)
# FrequencyOfUse: 1/x (normal), include indicator variable for zero value + interaction
# FrequencyOfSMS: linear (normal), ^2 (non-constant variance)
# DistinctCalledNumbers: 1/x (normal), ^2 (non-constant variance), 
#   include indicator variable for zero value + interaction

xform.xvars <- list(
  "Number of call failures (sq)" = CallFailure^2,
  "Log Length of subscription" = log(SubscriptionLength + 0.1),
  "Log Total duration of calls / seconds" = log(SecondsOfUse + 0.1),
  "1 / Number of calls" = 1/(FrequencyOfUse + 0.1),
  "Number of text messages" = FrequencyOfSMS,
  "1 / Number of distinct called numbers" = 1/(DistinctCalledNumbers + 0.1))

#----------------------
#Conditional Histograms  (Sheather, 2009)
#----------------------

plot.conditional.densities(
  yvar, xform.xvars, mfrow=c(2,3), new.window=T)

#----------------------
# Empirical logit plots of transformed variables
#----------------------

plot.emplogits(
  yvar, xform.xvars, scs, new.window=T)

# These plots show the need for indicator variables for FrequencyOfUse
# and DistinctCalledNumbers

#----------------------
# Correlation in binary and ordinal variables
#----------------------
TeleChurn_ca_names <- names(sapply(
  TeleChurn_pre, is.factor) %>% .[.]) %>% .[.!="Churn"]

TeleChurn_ca <- TeleChurn_pre %>% dplyr::select(all_of(TeleChurn_ca_names))
x11()
ggstatsplot::ggcorrmat(
  data = TeleChurn_ca,
  type = "parametric",
  colors = c("firebrick", "white", "steelblue4")) +
  scale_x_discrete(position = "top", 
                   guide = guide_axis(angle = 45))

# NOTE: No drastic correlation. Highest is negative correlation
# between Age Group 2 and 3.

#----------------------
# Correlation in numerical variables
#----------------------
TeleChurn_num <- TeleChurn_pre %>% 
  dplyr::select(-all_of(TeleChurn_ca_names), -Churn)
x11()
ggstatsplot::ggcorrmat(
  data = TeleChurn_num,
  type = "parametric",
  colors = c("firebrick", "white", "steelblue4")) + 
  scale_x_discrete(position = "top", 
                   guide = guide_axis(angle = 45))

# NOTE: A number of pairs have high correlation:
#
# EXTREMELY HIGH
# CustomerValue, Frequency of SMS
# Frequency of Use, Seconds Of Use
#
# VERY HIGH
# Distinct Called Numbers, Frequency of Use
# Distinct Called Numbers, Seconds of Use
#
# Fitting via LASSO may help avoid issues with multicollinearity

#----------------------
# Histograms
#----------------------

plot.hists(cont.xvars)
plot.hists(xform.xvars)

#----------------------
# Categorical and ordinal variables
#----------------------

#Conditional Histograms  (Sheather, 2009)
cat.xvars <- list(
  "Charge Amount by Churn" = as.numeric(ChargeAmount),
  "Age Group by Churn" = as.numeric(AgeGroup))

plot.conditional.densities(
  yvar, cat.xvars, mfrow=c(2,1), new.window=T)

#----------------------
# Constant covariance check
#----------------------

num.xvar.names <- intersect(names(TeleChurn_num), 
                            xvar.names)
num.xvar.names <- setdiff(num.xvar.names, "FrequencyOfSMS")

TeleChurn_indicators <- TeleChurn_num %>%
  mutate(
    IDistinctCalledNumbers = factor(DistinctCalledNumbers > 0)) %>%
  dplyr::select(IDistinctCalledNumbers)

TeleChurn_trans <- TeleChurn_num %>%
  mutate(
    sqCallFailure = CallFailure^2,
    logSubscriptionLength = log(SubscriptionLength + 0.1),
    logCubeSubscriptionLength = log(SubscriptionLength + 0.1)^3,
    logSecondsOfUse = log(SecondsOfUse + 0.1),
    logSqSecondsOfUse = log(SecondsOfUse + 0.1)^2,
    recipFrequencyOfUse = 1 / (FrequencyOfUse + 1),
    sqFrequencyOfSMS = FrequencyOfSMS^2,
    recipDistinctCalledNumbers = 1 / (DistinctCalledNumbers + 1),
    recipSqDistinctCalledNumbers = 1 / (DistinctCalledNumbers + 1)^2) %>%
  dplyr::select(-CustomerValue)

interactions <- get.interaction.terms(TeleChurn_num %>% 
                                        dplyr::select(-CustomerValue))
interactions <- rbind(interactions,
                      data.frame(var1=names(TeleChurn_num %>% dplyr::select(-CustomerValue)), 
                                 var2="IDistinctCalledNumbers"))

TeleChurn_trans <- cbind(TeleChurn_trans, Churn, TeleChurn_indicators)

# We include the interaction terms above in the model 
# as they exhibit significantly different covariances 
# between the response values (Sheather, 2009)

##########################################################
# Fitting Preparation
##########################################################

# Here we should also consider other transformations
# I just tried the most common one, but how? -discussion
# Look for transformations

# NOTE (GC): Will add full set of transformations and interactions via
# dynamically built formula rather than adding variables to dataset.
# Also will add Yuqi's ratio terms too.

TeleChurn_trans <- cbind(TeleChurn_trans, TeleChurn_ca)
var.names <- names(TeleChurn_trans %>% dplyr::select(-Churn))
formula.full <- as.formula(
  paste("Churn ~", 
        paste(var.names, collapse = " + "),
        " + ",
        paste(
          apply(interactions, 1, paste, collapse = "*"), collapse = " + ")))

#----------------------
# Division of training and validation set
#----------------------
set.seed(100)
index_sub <- sample(
  1:nrow(TeleChurn_trans),
  round(nrow(TeleChurn_trans)*2/3))

#CustomerValue is to be used to assess the model 
#and should not be used as an explanatory variable.
TeleChurn_fit <- TeleChurn_trans
TeleChurn_train <- TeleChurn_fit[index_sub,]
TeleChurn_test <- TeleChurn_fit[-index_sub,]
dim(TeleChurn_train)
dim(TeleChurn_test)

TeleChurn_fit_notrans <- TeleChurn_pre %>% dplyr::select(-CustomerValue)
TeleChurn_train_notrans <- TeleChurn_fit_notrans[index_sub,]
TeleChurn_test_notrans <- TeleChurn_fit_notrans[-index_sub,]

#-------------------------
# Logistic Regression
#-------------------------

# Initial models
# First fit - all variables with transformation
detach(TeleChurn_pre)

##########################################################
# Analysis of Deviance - Manual variable removal
##########################################################
fit_all <- glm(
  formula.full, data = TeleChurn_fit, family = binomial)
summary(fit_all)
full.anova <- Anova(fit_all)
remove.vars <- rownames(full.anova[full.anova$Df == 0,])

reg.step0 <- update(fit_all, as.formula(
  paste("~ . -", paste(remove.vars, collapse = " - "))))
results <- my.backward.stepwise(reg.step0, "deviance")
reg.step1 <- results$model
removed.vars <- append(remove.vars, results$removed.variables)

Anova(reg.step1)
summary(reg.step1)
VIF(reg.step1)
anova(reg.step0, reg.step1)

x11()
par(mfrow=c(2,2))
plot(reg.step1)

# NOTES:
# Initial signs of outliers

# Chisq test retains variables logSecondsOfUse, logSqSecondsOfUse and
# IFrequencyOfUse whereas Anova Likelihood Ratio test considers these
# insignificant.

# Training data only
fit_all_train <- glm(
  formula.full, data = TeleChurn_train, family = binomial)
summary(fit_all_train)
full.anova <- Anova(fit_all_train)
remove.vars <- rownames(full.anova[full.anova$Df == 0,])

reg.step0.train <- update(fit_all_train, as.formula(
  paste("~ . -", paste(remove.vars, collapse = " - "))))
results <- my.backward.stepwise(reg.step0.train, "deviance")
remove.vars <- append(remove.vars, results$removed.variables)

reg.step1.train <- results$model
summary(reg.step1.train)
Anova(reg.step1.train)
VIF(reg.step1.train)
anova(reg.step0.train, reg.step1.train)

x11()
par(mfrow=c(2,2))
plot(reg.step1.train)

# VIF: As expected we see high values for SecondsOfUse and FrequencyOfUse
# given extremely high correlation between them.

# All variables without transformation 
#- just for reference, there is no need for it if the transformations are reliable
fit_all_notrans <- glm(
  Churn ~ ., data = TeleChurn_fit_notrans, family = binomial)
summary(fit_all_notrans)
Anova(fit_all_notrans)
x11()
par(mfrow=c(2,2))
plot(fit_all_notrans)

# Training data only
fit_all_notrans_train <- glm(
  Churn ~ ., data = TeleChurn_train_notrans, family = binomial)
summary(fit_all_notrans_train)
Anova(fit_all_notrans_train)
x11()
par(mfrow=c(2,2))
plot(fit_all_notrans_train)

# Replot residuals (double curve is known issue without binning)
fit_all.res <- resid(reg.step1)
fit_all.yhat <- predict(reg.step1, type="response")
binnedplot(fit_all.yhat, fit_all.res, nclass=20)

fit_all_train.res <- resid(reg.step1.train)
fit_all_train.yhat <- predict(reg.step1.train, type="response")
binnedplot(fit_all_train.yhat, fit_all_train.res, nclass=20)

# Additional diagnostics (QUESTION 6)
#avPlots(reg.step1)
#crPlots(reg.step1) # NOTE: Remove interaction terms for plot!

#avPlots(reg.step1.train)
#crPlots(reg.step1.train) # NOTE: Remove interaction terms for plot!

# We see that none of the variables now fail the chisq score test
# even though some fail the individual Wald tests from summary().
# We double check by computing likelihood ratio confidence intervals 
# via confint() function

#----------------
# Final check via likelihood ratio
#----------------

# 95% confidence interval
con.int <- confint(reg.step1.train)

# Check for 0 in interval
insignificant.variables <- apply(con.int, 1, 
                                 function(x) {between(0, x[1], x[2])}) %>%
  .[.] %>% names
insignificant.variables

# Note all of these point to Charge Amount. HOWEVER, the linear
# term is significant:
con.int[rownames(con.int) == "ChargeAmount.L",]

# So we retain ChargeAmount

# Final check
anova(reg.step1, reg.step0, test="Chisq")
coefs <- coef(fit_all_train)
manual.vars <- names(coefs)[-1]

##########################################################
#Automated Methods of Stepwise
##########################################################

#----------------
#Backward
#----------------
autoback <- step(fit_all_train, 
                 direction= "backward", 
                 test= "Chisq", 
                 k = qchisq(0.05, 1, lower.tail=FALSE))

fit_autoback <- glm(formula(autoback), 
                    data = TeleChurn_train, 
                    family = 'binomial')

summary(fit_autoback)
VIF(fit_autoback)
back.vars <- names(fit_autoback$coefficients)[-1]

# Variables in both backward step and manual
intersect(back.vars, manual.vars)

# Variables removed in backward step but retained in manual
setdiff(manual.vars, back.vars)

# Variables retained in backward step but removed in manual
setdiff(back.vars, manual.vars)

# Here we get different results compared to our manual process.

# REMOVED:
# ChargeAmount is removed due to a higher penalization from its
# 10 degrees of freedom. Interactions with IDistinctCalledNumbers largely
# removed.

#----------------
# Forward
#----------------
fit0 <- glm(Churn ~ 1, 
            data = TeleChurn_train,
            family=binomial)

big <- formula(fit_all_train)
autoforward <- step(fit0,
                    direction='forward', 
                    test="Chisq", 
                    k=qchisq(0.05,1,lower.tail=FALSE), 
                    scope=big)

fit_autoforward <- glm(formula(autoforward), 
                       data = TeleChurn_train, 
                       family = 'binomial')

summary(fit_autoforward)
VIF(fit_autoforward)

# Here we get different results again. 
# All interaction terms except one excluded
# VIF metrics much better.

#----------------
# Both Forward
#----------------
autobothfore <- step(fit0, 
                     direction='both', 
                     test="Chisq", 
                     k=qchisq(0.05,1,lower.tail=FALSE), 
                     scope=big)

fit_autobothfore <- glm(formula(autobothfore), 
                        data = TeleChurn_train,
                        family = 'binomial')

summary(fit_autobothfore)
VIF(fit_autobothfore)

#----------------
# Both Backward
#----------------
autobothback <- step(fit_all, 
                     direction='both', 
                     test="Chisq", 
                     k=qchisq(0.05,1,lower.tail=FALSE))

fit_autobothback <- glm(formula(autobothback), 
                        data = TeleChurn_train,
                        family = 'binomial')

summary(fit_autobothback)
VIF(fit_autobothback)

##########################################################
#Subset Methods
##########################################################

# NOTE (GC): Here.

# Limited to 15 variables in an exhaustive search. As such we will:
# Skip interaction terms
# Remove FrequencyOfUse and its transformations due to its extremely high correlation with SecondsOfUse
# Remove AgeGroup and ChargeAmount as these fail likelihood ratio tests in our previous models
# Remove TariffPlan and logSecondsOfUse as these are the least significant variables in our
#   previous models based on LR test.

TeleChurn_train_subset <- TeleChurn_train %>%
  dplyr::select(-c(FrequencyOfUse, 
                   recipFrequencyOfUse, 
                   AgeGroup, 
                   ChargeAmount,
                   TariffPlan,
                   logSecondsOfUse)) %>%
  relocate(Churn, .after=Status)

fit_exhaustive_AIC <- bestglm(
  TeleChurn_train_subset,
  family=binomial,
  IC="AIC",
  method="exhaustive")

#list best 5 models
fit_exhaustive_AIC$BestModels
#list best model
fit_exhaustive_AIC$BestModel
#summary of best model
summary(fit_exhaustive_AIC$BestModel)  
Anova(fit_exhaustive_AIC$BestModel)

#repeat with BIC
fit_exhaustive_BIC <- bestglm(
  TeleChurn_train_subset,
  family=binomial,
  IC="BIC", 
  method="exhaustive")
fit_exhaustive_BIC$BestModels
fit_exhaustive_BIC$BestModel
summary(fit_exhaustive_BIC$BestModel)
Anova(fit_exhaustive_BIC$BestModel)

##########################################################
# Shrinkage LASSO and elastic net method
##########################################################

# !! NOTE: GC will work on elastic net !!
# Also, elastic net should help solve our multicollinearity issues.

activate.packages.if.missing("Matrix")
activate.packages.if.missing("foreach")

elastic.net.cv.results <- return_min_cvm_lambda(
  TeleChurn_train,
  "Churn") # Note we recurse 5 times. This can be changed with depth parameter.

best.cvm <- min(elastic.net.cv.results$results$CVM)
best.index <- which(elastic.net.cv.results$results$CVM == best.cvm)
elastic.net.best <- elastic.net.cv.results$best.models[[best.index]]
best.lambda <- elastic.net.best$lambda.min
best.alpha <- elastic.net.cv.results$results[13,]$Alpha

lambda.1se.max <- elastic.net.best$lambda.1se
lambda.1se.min <- lambda.1se.max - best.lambda

plot(elastic.net.cv.results$best.models[[1]])
abline(v = log(best.lambda), col="blue")
abline(v = log(lambda.1se.max), col="red")
abline(v = log(lambda.1se.min), col="red")

TeleChurn_X <- model.matrix(Churn ~ ., TeleChurn_train)[,-1]
TeleChurn_X_test <- model.matrix(Churn ~ ., TeleChurn_test)[,-1]

fit_lambda_min <- glmnet(TeleChurn_X,
                         TeleChurn_train$Churn,
                         alpha=best.alpha,
                         lambda=best.lambda,
                         family="binomial")

##########################################################
#Model Evaluation
##########################################################
#some interim checks of residuals, influence and model specification 
#to help pick the most appropriate model

activate.packages.if.missing(caret)
activate.packages.if.missing(pROC)

#----------------
# Confusion Matrix and ROC charts
#----------------

# Manual Model
models <- list(
  "Manual Model" = reg.step1.train,
  "Backward Stepwise" = fit_autoback,
  "Forward Stepwise" = fit_autoforward,
  "Both Stepwise - Full Model Start" = fit_autobothback,
  "Both Stepwise - Empty Model Start" = fit_autobothfore,
  "Subset AIC" = fit_exhaustive_AIC$BestModel,
  "Subset BIC" = fit_exhaustive_BIC$BestModel,
  "Elastic Net" = fit_lambda_min
)

metrics <- get.model.accuracy.metrics(models,
                                      TeleChurn_test)

best.overall.model <- fit_autobothback

TeleChurn_eval <- cbind(TeleChurn_test, 
                        CustomerValue = TeleChurn_pre$CustomerValue[-index_sub])

# Using optimal thresholds for unweighted classification
optimal.thresholds <- sapply(metrics, function(x) {x$optimal.threshold} )
model.customer.values <- measure.model.profit(models, TeleChurn_eval, optimal.thresholds)
model.profits <- sapply(model.customer.values, function(x) { sum(x$CustomerProfit) })
max.profit <- max(model.profits)
max.profit.idx <- which(model.profits == max.profit)
most.profitable.model <- model.profits[max.profit.idx]
most.profitable.model

# Using standard 0.5 threshold
optimal.thresholds <- rep(0.5, 8)
model.customer.values <- measure.model.profit(models, TeleChurn_eval, optimal.thresholds)
model.profits <- sapply(model.customer.values, function(x) { sum(x$CustomerProfit) })
max.profit <- max(model.profits)
max.profit.idx <- which(model.profits == max.profit)
most.profitable.model <- model.profits[max.profit.idx]
most.profitable.model

# Question about practice situation - CustomerValue
# Find best performing model for customer profit
# Training Data

# Prediction for new observation
new.obs <- data.frame(
  CallFailure = 6,
  Complains = 1, # yes
  SubscriptionLength = 35,
  ChargeAmount = 0,
  SecondsOfUse = 3000,
  FrequencyOfUse = 55,
  FrequencyOfSMS = 20,
  DistinctCalledNumbers = 20,
  AgeGroup = 3,
  TariffPlan = 2, # contract
  Status = 1
) %>% mutate(
  Complains = factor(Complains, 
                     levels=c(0,1), 
                     labels=c("No complaint","Complaint")),
  TariffPlan = factor(TariffPlan,
                      levels=c(1,2),
                      labels=c("PAYG","Contractual")),
  Status = factor(Status,
                  levels=c(1,2),
                  labels=c("Active","Non-Active")),
  ChargeAmount = ordered(ChargeAmount, levels=0:10),
  AgeGroup = ordered(AgeGroup, levels=1:5),
  sqCallFailure = CallFailure^2,
  logSubscriptionLength = log(SubscriptionLength + 0.1),
  logCubeSubscriptionLength = log(SubscriptionLength + 0.1)^3,
  logSecondsOfUse = log(SecondsOfUse + 0.1),
  logSqSecondsOfUse = log(SecondsOfUse + 0.1)^2,
  recipFrequencyOfUse = 1 / (FrequencyOfUse + 1),
  sqFrequencyOfSMS = FrequencyOfSMS^2,
  IDistinctCalledNumbers = factor(DistinctCalledNumbers > 0, levels=c(FALSE, TRUE)),
  recipDistinctCalledNumbers = 1 / (DistinctCalledNumbers + 1),
  recipSqDistinctCalledNumbers = 1 / (DistinctCalledNumbers + 1)^2)

new.churn.log.odds <- predict(best.overall.model, 
                              newdata = new.obs)

new.churn.log.odds

new.churn.prob <- predict(best.overall.model, 
                          newdata = new.obs, 
                          type = "response")

new.churn.prob

# Manual calculation of log odds for new customer
ageGroup.contrasts <- contrasts(TeleChurn_train$AgeGroup)
ageGroup.Three <- ageGroup.contrasts[3,]
chargeAmount.contrasts <- contrasts(TeleChurn_train$ChargeAmount)
chargeAmount.Zero <- chargeAmount.contrasts[1,]
best.coefs <- coef(best.overall.model)

log.odds <- 
  best.coefs[1] +
  best.coefs[2] * 6 +
  best.coefs[3] * 35 +
  best.coefs[4] * 3000 +
  best.coefs[5] * 55 +
  best.coefs[6] * 20 +
  best.coefs[7] * 20 +
  best.coefs[8] * log(35 + 0.1) +
  best.coefs[9] * log(35 + 0.1)^3 +
  best.coefs[10] * log(3000 + 0.1) +
  best.coefs[11] * log(3000 + 0.1)^2 +
  best.coefs[12] * (1/ (55 + 1)) +
  best.coefs[13] * 20^2 +
  best.coefs[14] * (1 / (20 + 1)) +
  best.coefs[15] * 1 +
  sum(best.coefs[16:25] * chargeAmount.Zero) +  
  sum(best.coefs[26:29] * ageGroup.Three) +
  best.coefs[30] * 1 +
  best.coefs[32] * 6 * 35 +
  best.coefs[33] * 6 * 55 +
  best.coefs[34] * 6 * 20 +
  best.coefs[35] * 35 * 55 +
  best.coefs[36] * 35 * 20 +
  best.coefs[37] * 3000 * 55 +
  best.coefs[38] * 3000 * 20 +
  best.coefs[39] * 3000 * 20
