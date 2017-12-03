# Home Assignment 1
# Yvonne Schmeisser
# 10.11.2017

rm(list=ls())

# load packages
library(lsr)
library(psych) 
library(lm.beta) 
library(dplyr)
library(gsheet)
library(car)
library(ggplot2) 
library(rgl) 
library(snp.plotter)

### Import data #####

data <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_1.csv")
View(data)

# examine the data
who(TRUE) # sex and ID are factors, all others integer or numeric, nothing needs to be changed

describe(data)
summary(data)
## PROBLEMS:
# Mindfulness should be on a range from 1 to 6, but there are values below 1
# sex has one data point that is coded as three instead of female/male

# Create a new dataset for excluding data, keep the original as "backup"
data1 <- data

# finding the data points in mindfulness that are lower than 1 and excluding them
data1 <- data1[!data1$mindfulness < 1,] # three datapoints

# Trying to find out why one person's sex was coded as three
summary(data1$sex) # 81 female, 75 male, 1 3
data1$sex <- as.numeric(data1$sex) # person with 3 is now a 1!
# shows that the numeric values of the factor sex are 2 and 3
# 2 represents female, 3 is male --> this leads to the assumption, that the person
# labeled as "3" on this variable is male as well.

# recode participant, encoded as three, as male:
class(data1$sex)
summary(data1$sex)

data1$sex[data1$sex == 1] <- 3 # replacing one with 3
summary(data1$sex)

data1$sex <- factor(data1$sex, labels=c("female", "male")) # transform se back into a factor
class(data1$sex)
summary(data1$sex) # result: 81 female, 76 male

View(data1)

### CHECKING FOR OUTLIERS

hist(data1$pain, breaks=40)
hist(data1$age, breaks = 40)
hist(data1$STAI_trait, breaks = 40)
hist(data1$pain_cat, breaks = 40) # one person seems to have a very low value
hist(data1$cortisol_serum, breaks = 40)
hist(data1$cortisol_saliva, breaks = 40) # one person with a very low value
hist(data1$mindfulness, breaks = 40)
hist(data1$weight, breaks = 40) # one person with a low value

data1$ID[data1$pain_cat < 20] # ID_26
data1$ID[data1$cortisol_saliva < 2.5] # ID_69
data1$ID[data1$weight < 45] # ID_122

# Creating a new dataset without these outliers
data_out <- data1
data_out$ID <- as.numeric(data_out$ID)

data_out <- data_out[!data_out$ID == 26,]
data_out <- data_out[!data_out$ID == 69,]
data_out <- data_out[!data_out$ID == 122,]
summary(data_out)


###### Research Question One #########################################################

## Creating the models ----

## first model (model demographic): age and sex as predictors, no interactions
mod1 <- lm(pain ~ age + sex, data = data1)

# values to report for the coefficients
summary(mod1)
lm.beta(mod1)
confint(mod1)

plot(mod1) # Residualplot --> fit is not very good!
plot(pain ~ age + sex, data = data1)

# calculate the model again with univariate outliers excluded
mod1_out <- lm(pain ~ age + sex, data = data_out)
summary(mod1_out)
print(mod1)

## second model  (model psychological)
mod2 <- lm(pain ~ age + sex + STAI_trait + pain_cat 
           + mindfulness + cortisol_serum + cortisol_saliva, data = data1)

# values to report for the coefficient
summary(mod2)
lm.beta(mod2)
confint(mod2)

plot(mod2)


# recalculate without outliers
mod2_out <- lm(pain ~ age + sex + STAI_trait + pain_cat 
           + mindfulness + cortisol_serum + cortisol_saliva, data = data_out)
summary(mod2_out)


## Multivariate outlier -------

## MODEL 1

# Cooks Distance 
plot(cooks.distance(mod1), ponts=data1$ID)
plot(mod1, which = 4)

# cooks distance with >1 cut-off
data1[cooks.distance(mod1) >= 1,]
# no participants with cooks distance above 1

## MODEL 2

# Cooks Distance
cooks.distance(mod2)
plot(mod2, which = 4)
# cooks distance with >1 cut-off
data1[cooks.distance(mod2) >= 1,]
# no influential outliers with cooks distance over 1

# and for the data without the univariate "outliers"
plot(mod1_out, which = 4)
plot(mod2_out, which = 4)


## comparing the different models ----
anova(mod1, mod2)  # second one (psychological) is the better: model two has smaller RSS

# comparing the AICs
AIC(mod1)
AIC(mod2) # fits much better


## Check the Assumptions of Model 2 ----

## Normality of the residuals

hist(x = residuals( mod2 ), # data are the residuals
          xlab = "Value of residual", # x-axis label
          main = "", # no title
          breaks = 20 # lots of breaks
           )
# irregularities! -> two points are far away from the other data points

shapiro.test(residuals(mod2))
# not significant --> residuals are normally distributed



## Linearity
yhat.1 <- preds
yhat.2 <- fitted.values( object = mod2 )
plot( x = yhat.2,
        y = data1$pain,
        xlab = "fitted Values",
        ylab = "Observed Values"
        )
# looks more or less like a line

plot(mod2, which = 1)
# no perfect, straight line, but also not very curvy

windows()
residualPlots(model = mod2)
# if we take a look on the residual plots of the seperate variables, 
# we find curvy lines!
# BUT: nothing in the test is coming up significant --> no problem
# linearity is given

## Homogeneity of variances
plot(x = mod2, which = 3)
# plot shows the standardized residuals
# line should be horizontal and straight

ncvTest(mod2)
# not significant --> varainces are equal


## Multicolliniarity
# Variance inflation factor
vif(mod = mod2)
# serum and saliva do have a VIF over 5 --> problematic

# how much wider would the CIs be, if the variables were uncorrelated?
sqrt(vif(mod = mod2))

# correlation matrix (without the variables that are factors)
dat3 <- data1[-3]
dat3 <- dat3[-1]
cor(dat3)

# correlation of the two cortisol measures
cor.test(data1$cortisol_saliva, data1$cortisol_serum)

### rerun the model twice, each time without one of the highly correlated variables

mod3 <- lm(pain ~ age + sex + STAI_trait + pain_cat 
           + mindfulness + cortisol_serum, data = data1)

mod4 <- lm(pain ~ age + sex + STAI_trait + pain_cat 
           + mindfulness + cortisol_saliva, data = data1)

# multivariate Outliers? --> no
plot(cooks.distance(mod3))
plot(cooks.distance(mod4))

# comparing the models to find out which one is best
summary(mod2)
summary(mod3)
summary(mod4)

anova(mod3, mod4)
anova(mod3, mod2)
anova(mod4, mod2) # not significant

AIC(mod2)
AIC(mod3)
AIC(mod4)

# The smaller the AIC, the better the fit. But: AIC differences up to 2 do not make a real differnce
# So there is no real difference... Also no significant difference in the ANOVA
# But then it's better to take the model with fewer predictors (Ockhams razor)


# -> adjusted R-squared is smaller for mod 2!!

## calculating statistics of the coefficients to report for model4
# confidential intervals
summary(mod4)
confint(mod4)
lm.beta(mod4)



##### Check the assumptions for model4

## Normality of the residuals

hist(x = residuals(mod4), # data are the residuals
     xlab = "Value of residual", # x-axis label
     main = "", # no title
     breaks = 20 # lots of breaks
)

# irregularities! -> two points are far away from the other data points

shapiro.test(residuals(mod4))
# not significant --> residuals are normally distributed

## Checking for linearity
yhat.4 <- fitted.values(object = mod4)
plot(x = yhat.4,
    y = data1$pain,
    xlab = "Fitted Values",
    ylab = "Observed Values"
)


# residual against fitted values
plot(mod4, which = 1)

# residuals against fitted values seperated for every variable
windows()
residualPlots(model = mod4)
# Tukeay test not significant --> linearity


## Homogeneity of variances
#Standardized residuals against fitted values
plot(x = mod4, which = 3)
# no really straight line

ncvTest(mod4)
# not significant --> variances are equal


## Testing for Multicolliniarity
# Variance inflation factors:
vif(mod = mod4)
sqrt(vif(mod = mod4))
# nothing irregular



### Plotting the predictions against the observed values

pred1 <- predict(object=mod1)

ggplot(data1, aes(x = pred1, y = pain))+
  geom_point()+
  geom_smooth(method='lm',formula=y~x+I(x^2))


pred2 <- predict(object=mod2)

ggplot(data1, aes(x = pred2, y = pain))+
  geom_point()+
  geom_smooth(method='lm',formula=y~x+I(x^2))

pred4 <- predict(object=mod4)

ggplot(data1, aes(x = pred4, y = pain))+
  geom_point()+
  geom_smooth(method='lm',formula=y~x+I(x^2))


###### Reserach Question Two ######################################################################

## univariate outliers in the vairable weight?
boxplot(data1$weight)
summary(data1$weight)

data1$ID[data1$weight < 45]
# one person with a very low value, but it could be a realistic value
# I'll keep the person and recheck for multivariate outliers later

## Creating a new model
mod_new <- lm(pain ~ age + sex + weight + STAI_trait + 
               pain_cat + mindfulness + cortisol_serum, data = data1)

summary(mod_new)
AIC(mod_new)
lm.beta(mod_new)
confint(mod_new)

# checking for multivariate outliers
plot(cooks.distance(mod_new))


## model diagnostics 

## Normality of the residuals

hist(x = residuals(mod_new), # data are the residuals
     xlab = "Value of residual", # x-axis label
     main = "", # no title
     breaks = 20 # lots of breaks
)


shapiro.test(residuals(mod_new))
# not significant --> residuals are normally distributed

## Checking for linearity
yhat.new <- fitted.values(object = mod_new)
plot(x = yhat.new,
     y = data1$pain,
     xlab = "Fitted Values",
     ylab = "Observed Values"
)

plot(mod_new, which = 1)

windows()
residualPlots(model = mod_new)


## Homogeneity of variances
plot(x = mod_new, which = 3)

ncvTest(mod_new)
# not significant --> variances are equal


## Testing for Multicolliniarity
vif(mod = mod_new)
sqrt(vif(mod = mod_new))


## backward regression ------
mod_back <- stats::step(object = mod_new, direction = "backward")

summary(mod_new)
summary(mod_back)

# compare back to initial model
anova(mod_back, mod_new) # no significant difference, adj r square is minimally lower for back

AIC(mod_new)
AIC(mod_back)

# report for coefficients
confint(mod_back)
lm.beta(mod_back)


## Comparing the backward model and model from first part ----

# theory-based model is mod4 from research question 1
theory_mod <- mod4
anova(mod_back, theory_mod) # anova is significant, backward models has higher RSS

# backward vs theory based
anova(mod_back, theory_mod)
AIC(mod_back)
AIC(theory_mod) # AIC lower for the theory model 

#window()
#points(pred_back$pain ~ pain, data_new1, col = "red")

## Loading the new data ----

data_new <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_2.csv")
summary(data_new)

# removing participants with a score lower than one on mindfulness
hist(data_new$mindfulness, breaks = 30)
data_new1 <- data_new[!data_new$mindfulness < 1,]
summary(data_new1)

# Outliers
plot(cooks.distance(theory_mod, data=data_new1))
plot(cooks.distance(mod_back, data=data_new1))
# no influential outliers

#### Prediction for the new dataset

pred_back <- predict(mod_back, data_new1)
pred_theory <- predict(theory_mod, data_new1)

length(pred_theory)
length(pred_back)

# plotting predicted values against actual values with a red line indicating the relation
plot(pred_theory ~ data_new1$pain)
abline(lm(pred_theory ~ data_new1$pain), col="red")

plot(pred_back ~ data_new1$pain)
abline(lm(pred_back ~ data_new1$pain), col="red")

#### Taking a look at the residuals
RSS_theory = sum((data_new1["pain"] - pred_theory)^2)
RSS_back = sum((data_new1["pain"] - pred_back)^2)
RSS_theory
RSS_back


##### Research Question 3 #################################################

library(reshape2)
library(psych)
library(cAIC4)
library(influence.ME)
library(ggplot2)
library(longitudinalData)


# importing the data
dat_pain <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_3.csv")

View(dat_pain)
who(TRUE)

summary(dat_pain)

describe(dat_pain)

summary(dat_pain$sex)
hist(dat_pain$age, breaks = 20)
hist(dat_pain$STAI_trait, breaks=30) # 50?
hist(dat_pain$pain_cat, breaks=20)
hist(dat_pain$cortisol_serum, breaks=20)
hist(dat_pain$cortisol_saliva, breaks=20)
hist(dat_pain$mindfulness, breaks=20)
hist(dat_pain$weight, breaks=20)
hist(dat_pain$pain1, breaks=20)
hist(dat_pain$pain2, breaks=20)
hist(dat_pain$pain3, breaks=20)
hist(dat_pain$pain4, breaks=20)

## reshaping the data to long format
repeated_variables <- c("pain1",	"pain2", "pain3",	"pain4")
dat_long <- melt(dat_pain, measure.vars=repeated_variables, variable.name = "day", value.name = "pain_rating")
dat_long <- dat_long[order(dat_long[,"ID"]),]
dat_long$day <- as.numeric(dat_long$day)


## Creating models
# random intercept
mod_int <- lmer(pain_rating ~ day + age + sex + STAI_trait + weight + pain_cat
               + mindfulness + cortisol_serum + (1|ID), data = dat_long)
# random slope
mod_slope <- lmer(pain_rating ~ day + age + sex + STAI_trait + weight + pain_cat
                 + mindfulness + cortisol_serum + (day|ID), data = dat_long)

# outliers?
library(lattice)
library(HLMdiag)

### cooks distance for linear-mixed effect models
library(influence.ME)
infl_inter <- influence(mod_int, group = "ID")
plot(cooks.distance(infl_inter))
# plot
plot(infl_inter, which = "cook")
# no multivariate outliers for the intercept model, when threshold of 1 is applied

infl_slope <- influence(mod_slope, group = "ID")
plot(cooks.distance(infl_slope))
# plot
plot(infl_slope, which = "cook")
# no multivariate outliers for the slope model, when threshold of 1 is applied


### Taking a look at coefficients

# standardized beta coefficients
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}


# Calculating statistics of the coefficients
summary(mod_int)
stdCoef.merMod(mod_int)
confint(mod_int)

summary(mod_slope)
stdCoef.merMod(mod_slope)
confint(mod_slope)


# comparing both models
cAIC(mod_int)$caic # 211.43
cAIC(mod_slope)$caic # 175.83

# calculating marginal R^2
library(r2glmm)
r2beta(mod_int, method = "nsj")
r2beta(mod_slope, method = "nsj")

### Plotting the actual values togethe with the predicted ones

# predicting values and saving them as new variables for the plots
dat_long$pred_int = predict(mod_int)
dat_long$pred_slope = predict(mod_slope)

# random intercept model
windows()
ggplot(dat_long, aes(y = pain_rating, x = day, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_int, x= day))+
  facet_wrap( ~ ID, ncol = 5)

# random slope and intercept model
windows()
ggplot(dat_long, aes(y = pain_rating, x = day, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_slope, x=day))+
  facet_wrap( ~ ID, ncol = 5)

# mod_slope fits the data better --> further calculations will be done with mod_slope


### Model diagnostics mod_slope

# Normality
qqmath(mod_slope, which = 2)
# is approximately normally distributed

# Linearity
predictors=c("weight", "day","sex", "age", "STAI_trait", "pain_cat", "mindfulness", "cortisol_serum")

for(i in 1:length(predictors)){
  predictor_to_test = dat_long[,predictors[i]]
  print(
    ggplot(data.frame(x = predictor_to_test,pearson=residuals(mod_slope,type="pearson")),
           aes(x=x,y=pearson)) +
      geom_point() +
      geom_smooth(method = 'loess') +
      theme_bw()
  )
}
# looks more like a curved relation!!


# Homoscedasdicity
plot(mod_slope) # no specific funnel pattern, there seems to be homoscadasticity
summary(lm(residuals(mod_slope)^2 ~ dat_long[,"ID"]))
# significant! --> might be heteroscedasticity


# Multicollinerity
pairs.panels(dat_long, col = "red", lm = T)
dat_long2 <- dat_long[-1]
dat_long2 <- dat_long2[-1]
cor(dat_long2)

cor(dat_long2) < -.6
cor(dat_long2) > .6


#### new model with quadratic term
mod_slope_2 = lmer(pain_rating ~ day + I(day^2) + age + sex + STAI_trait + weight + pain_cat
                 + mindfulness + cortisol_serum + (day|ID), data = dat_long)


# statistics for the coefficents for mod_slope_2
summary(mod_slope_2)
stdCoef.merMod(mod_slope_2)
confint(mod_slope_2)

# Rsquared
r2beta(mod_slope_2, method = "nsj")


## Comparing quadratic model with slope model
anova(mod_slope, mod_slope_2) # significant, quadratic model is better, chisq: 29.89
cAIC(mod_slope_2) # 121.96


#### plotting the model (errorplot)
dat_long$pred_slope_2 = predict(mod_slope_2) # predicting values for the plot

# creating plot
windows()
ggplot(dat_long, aes(y = pain_rating, x = day, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_slope_2, x= day))+
  facet_wrap( ~ ID, ncol = 5)



###### Model Diagnostics for  the squared slope

# Outliers? cooks distance
infl2 <- influence(mod_slope_2, obs = TRUE)
plot(cooks.distance(infl2))
# plot
plot(infl, which = "cook")
# no outliers when threshold of one is applied

# Normality
qqmath(mod_slope_2, which = 2)

# Linearity

predictors=c("weight", "day", "age", "sex", "STAI_trait", "pain_cat", "mindfulness", "cortisol_serum")

for(i in 1:length(predictors)){
  predictor_to_test = dat_long[,predictors[i]]
  print(
    ggplot(data.frame(x = predictor_to_test,pearson=residuals(mod_slope_2,type="pearson")),
           aes(x=x,y=pearson)) +
      geom_point() +
      geom_smooth(method = 'loess') +
      theme_bw()
  )
}

# Homoscedasdicity
plot(mod_slope_2)
summary(lm(residuals(mod_slope_2)^2 ~ dat_long[,"ID"]))
# not significant


# Multicollinerity
pairs.panels(dat_long, col = "red", lm = T)

plot(mod_slope_2)
predictors2=c("weight", "day", "I(day^2)", "age", "sex", "STAI_trait", "pain_cat", "mindfulness", "cortisol_serum")

for(i in 1:length(predictors2)){
  predictor_to_test = dat_long[,predictors2[i]]
  print(
    ggplot(data.frame(x = predictor_to_test,pearson=residuals(mod_slope_2,type="pearson")),
           aes(x=x,y=pearson)) +
      geom_point() +
      geom_smooth(method = 'loess') +
      theme_bw()
  )
}

#? day^2 not as a predictor?
