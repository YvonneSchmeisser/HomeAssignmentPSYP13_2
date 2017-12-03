###########################################################
#                                                         #
#                 Loading packages                        #
#                                                         #
###########################################################

library(psych) # for describe
library(car) # for residualPlots, vif, pairs.panels, ncvTest
library(ggplot2) # for ggplot
library(cAIC4) # for cAIC
library(r2glmm) # for r2beta
library(influence.ME) # for influence
library(lattice) # for qqmath
library(reshape2) # for melt function

###########################################################
#                                                         #
#                 Custom functions                        #
#                                                         #
###########################################################

# function to extract standardized beta coefficients from mer models, such as one produced by lmer
# source: https://stackoverflow.com/questions/25142901/standardized-coefficients-for-lmer-model

stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}


###########################################################
#                     Load data                          #
###########################################################

# Housing sales dataset from Kaggel
data_house = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/data_house_small_sub.csv")


###########################################################
#                   Model diagnostics                     #
###########################################################


### Model diagnostics
# Fit the final model (containing all predictors)
mod_house2 = lm(price ~ sqft_living + grade, data = data_house[-186,])

# checking for influential outliers

plot(price ~ sqft_living, data = data_house)

# Diagnostic plots> which number corresponds to which plot is in navarro
plot(mod_house2, which = 4) # plots the cooks distance
plot(mod_house2, which = 5) # also Cooks distance
# cooks distance higher than 1 should be excluded as outlier, depens also  on number of participants!
# 4/number ob participants --> 4/200 would produce a lot of outliers here..
# so decide subjectively what you want to do; so in this case we'll have a closer look on 186

## checking assumptions

# normality assumption
# QQ plot
plot(mod_house2, which = 2) # 186 is deviating again
# skew and kurtosis
describe(residuals(mod_house2)) # skew higher than 1 -> normality is violated
# histogram
hist(residuals(mod_house2), breaks = 20)


# Sensitivity anaylsis:
# first calculate everything with all datapoint included, afterwards test again without the outliers
# changes in the significance might be due to influential outcomes/to sample characteristics

# linearity assumption
# predicted values against actual values
pred <- predict( object = mod_house2 )
plot( x = pred, y = data_house[-186, "price"], 
      xlab = "Fitted Values", ylab = "Observed Values") # should fall onto a straight line

# predicted values against residuals
plot(mod_house2, which = 1) # should be a flat line"
# residual plot for each predictor from the car package, returning the result of linearity tests
residualPlots(mod_house2)
# should all fall onto a straight line, if the lines are very curvy, there might be a violation of the linearity
# if the tests are not significant, linearity is still given
# if the tests ar significant, linearity might be violated --> model should be adjusted
# higher order regression, not linear ones should be used then

# homoscedasticty assumption (homogeneity of variance)
plot(mod_house2, which = 3) # of there are patterns above or below the line, homosc. might be violated
ncvTest(mod_house2)n # significant --> variance for higher prices is higher than for lower prices
# therefore segregation of the data would be needed: different regression model for higher and lower prices
# to get a better predictability for the models; but segregation is also somehow a manipulation to make the
# model more significant --> report bith!!!

# multicollinearity (VIF above 5), or a VIF threshold of 3 is recommended in this paper: http://onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2009.00001.x/full
# some info about VIF: 
# https://statisticalhorizons.com/multicollinearity
# http://blog.minitab.com/blog/understanding-statistics/handling-multicollinearity-in-regression-analysis
vif(mod_house2)
pairs.panels(data_house[,c("price", "sqft_living", "grade")], col = "red", lm = T)
# multicollinarity shouldnt be too high --> predictors should not be correlated too high
# they should not explain the same parts of variances
# in most cases, but not always, multicollinarits is caused by highly correlated variables
# vif is a problem if it reaches 3 or 5
# what to do then? --> look at the pairwise correlations, maybe exclude one of the 
# highly correlated variables from the model, but depends on the theoretical assumption




# a more problematic case
# Fit the final model (containing all predictors)
mod_house_geolocation_inter2 = lm(price ~ sqft_living + grade + long * lat, data = data_house[-186,])

# checking for influential outliers

plot(price ~ sqft_living, data = data_house)

plot(mod_house_geolocation_inter2, which = 4)
plot(mod_house_geolocation_inter2, which = 5)

## checking assumptions

# normality assumption
# QQ plot
plot(mod_house_geolocation_inter2, which = 2)
# skew and kurtosis
describe(residuals(mod_house_geolocation_inter2))
# histogram
hist(residuals(mod_house_geolocation_inter2), breaks = 20)



# linearity assumption
# predicted values against actual values
pred <- predict( object = mod_house_geolocation_inter2 )
plot( x = pred, y = data_house$pain, 
      xlab = "Fitted Values", ylab = "Observed Values")
# predicted values against residuals
plot(mod_house_geolocation_inter2, which = 1)
# residual plot for each predictor from the car package, returning the result of linearity tests
residualPlots(mod_house_geolocation_inter2)

# homoscedasticty assumption (homogeneity of variance)
plot(mod_house_geolocation_inter2, which = 3)
ncvTest(mod_house_geolocation_inter2)

# multicollinearity (VIF above 5), or a VIF threshold of 3 is recommended in this paper: http://onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2009.00001.x/full
# some info about VIF: 
# https://statisticalhorizons.com/multicollinearity
# http://blog.minitab.com/blog/understanding-statistics/handling-multicollinearity-in-regression-analysis
vif(mod_house_geolocation_inter2)
pairs.panels(data_house[,c("price", "sqft_living", "grade", "long", "lat")], col = "red", lm = T)



###########################################################
#                     Backward regression                 #
###########################################################

# data is used to tell us, which varibles are important
# first create a model and use all variables that are related to the topic into it
# Backwardregression will excludes variables stepwise and gives out the model with the lowest AIC
# if AIC cannot be decreased significantly anymore, the programm stops and tells us which 
# variables are the most important ones --> whats not significant, is not neede in the model
names(data_house)

# before calculating the backwardsregression, make sure that the used model meets the assumptions of regression!
# exclude variables with high correlations (.8)

# model with potential predictors
mod_house_all_with_sqft_above = lm(price ~ bedrooms + bathrooms + sqft_living + 
                                     sqft_lot + floors + view + condition + grade+
                                     yr_built + yr_renovated + zipcode + sqft_above +
                                     lat + long + sqft_living15 + sqft_lot15, data = data_house)
summary(mod_house_all_with_sqft_above)

# there is some multicollinearity
vif(mod_house_all_with_sqft_above)

# we refit the model without sqft_above (which is highly correlated with sqft_living)
mod_house_all = lm(price ~ bedrooms + bathrooms + sqft_living + 
                     sqft_lot + floors + view + condition + grade+
                     yr_built + yr_renovated + zipcode + 
                     lat + long + sqft_living15 + sqft_lot15, data = data_house)
summary(mod_house_all)
# multicollinearity decreased
vif(mod_house_all)

# backward regression to identify the variables with the highest unique predictive value
mod_house_all_back = step(mod_house_all, direction = "backward")
summary(mod_house_all_back)

# backwards regression not very useful for predicting "real world"
# is fittes highly on the dataset!
# in most cases it's better to rely on the theory and previous data



###########################################################
#                                                         #
#                 Mixed effect models                     #
#                                                         #
###########################################################
###########################################################
#                 Load some more packages                 #
###########################################################


library(lme4) # for lmer
library(lmerTest) # to get singificance test in lmer

###########################################################
#                 Load bullying datasets                  #
###########################################################
# Simulated data from a study on bullying

data_bully_int = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/data_bully_int.csv")
# asign class as a grouping factor
data_bully_int$class = factor(data_bully_int$class)

data_bully_slope = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/data_bully_slope.csv")
data_bully_slope$class = factor(data_bully_slope$class)

###########################################################
#                     Explore data                        #
###########################################################

### an example of random intercept
# descriptive statistics
describe(data_bully_int)

# histograms
hist(data_bully_int$sandwich_taken)
hist(data_bully_int$weight)

# we would like to predict number of sandwitches 
# taken a month from the child's weight

# plot regression line without taking into consideration class
ggplot(data_bully_int, aes(y = sandwich_taken, x = weight))+
  geom_point(aes(color = class), size = 4)+
  geom_smooth(method = "lm", formula = "y ~ x", fill = NA)
# here we can see, that there are different patterns in the different classes
# to make better predictions, several regression lines, one for every class, could help
# "allow the model to fit several regression lines for all the classes"

# use separate regression line for each class
# here we see that in each class there is a different mean (intercept)
# of sandwitches taken, but the effect of weight seems to be similar in all classes
# so we need to use a random intercept model
ggplot(data_bully_int, aes(y = sandwich_taken, x = weight, group = class))+
  geom_point(aes(color = class), size = 4)+
  geom_smooth(aes(color = class), method = "lm", formula = "y ~ x", fill = NA)
# RANDOM INTERCEPT: Baseline of the effect is diefferent, but extend of the effect is similar in every class
# classes have different intercepts, but the slopes are similar
# category of classes has no real effect on the sandwhiches,
# fixed effects variables are those, we are interested in, random effect variables will not
# inlfuence the results, they have some effect but are regarded random.
# usuall yapplies for variables where you have many different categories --> randoms need to be normally distributed
# if there are only 2 categories (sex - male, female), this should not be used as random variable! (cannotbe normally distributed)


### an example of random slope
# descriptive statistics
describe(data_bully_slope)

# histograms
hist(data_bully_slope$sandwich_taken)
hist(data_bully_slope$weight)

# we would like to predict number of sandwitches 
# taken a month from the child's weight
# here we see that in each class there is a different mean (intercept)
# of sandwitches taken, AND also the extent of the effect of weight (its slope)
# seems to be different for each class
# so we need to use a random intercept and slope model
ggplot(data_bully_slope, aes(y = sandwich_taken, x = weight, group = class))+
  geom_point(aes(color = class), size = 4)+
  geom_smooth(aes(color = class), method = "lm", formula = "y ~ x", fill = NA)

# here, class does not only affect the baseline, it also affects the effect of weight itself
# so here, we need random slope to account for the influence of the classes, not only random intercept!!
# if we need random slope or random intercept should be decided by looking at the effects that our categories have
# or which model fits the data better

### fitting linear models on the dataset with random effects

#glm?? something with categorial?

# fixed effects model only
mod_fixed = lm(sandwich_taken ~ weight, data = data_bully_slope)
# random intercept, but no random slope
mod_rnd_int = lmer(sandwich_taken ~ weight + (1|class), data = data_bully_slope)
# 1 is the intercept

# random slope and intercept 
# (there is usually no point in fitting a random slope but not intercept model)
mod_rnd_slope = lmer(sandwich_taken ~ weight + (weight|class), data = data_bully_slope)

# comparing models
AIC(mod_fixed)
AIC(mod_rnd_int)
AIC(mod_rnd_slope)
# sinple AIC is not a good measure for the predictability of mixed model --> use CAIC

# you should use cAIC to compare mixed effects models to each other
cAIC(mod_rnd_int)
cAIC(mod_rnd_slope)


# you can also compare models by the anova function
# to see if one model is significantly better at predicting
# the outcome than the other model
# for this to work, one model needs to contain all variables from
# the other model
anova(mod_rnd_int, mod_rnd_slope) # a Warning message saying: "refitting model(s) with ML (instead of REML)" is natural here, you shouldn't worry about it
# which model is better? likelihood smaller -> better model


# you can also plot the regression lines and see which approach
# results in better fit
data_to_plot_bully = data_bully_slope
data_to_plot_bully$pred_int = predict(mod_rnd_int)
data_to_plot_bully$pred_slope = predict(mod_rnd_slope)

# plot of the random intercept model
ggplot(data_to_plot_bully, aes(y = sandwich_taken, x = weight, group = class))+
  geom_point(aes(color = class), size = 4)+
  geom_line(color='red', aes(y=pred_int, x=weight))+
  facet_wrap( ~ class, ncol = 2)

# plot of the random slope and intercept model
ggplot(data_to_plot_bully, aes(y = sandwich_taken, x = weight, group = class))+
  geom_point(aes(color = class), size = 4)+
  geom_line(color='red', aes(y=pred_slope, x=weight))+
  facet_wrap( ~ class, ncol = 2)


# once you have the final model, you can extract the information
# that you need to report:

# R^2, beta values, CAIC, modelcoefficient (for model and all coefficients),
# teststatistics (t and p from the lmerTest)


# computes the marginal R2 (proportion of variance explained by the fixed factor(s) alone)
# based on Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for obtaining 
# R2 from generalized linear mixed-effects models. Methods in Ecology and Evolution, 4(2), 133-142.
# and Johnson, P. C. (2014). Extension of Nakagawa & Schielzeth's R2GLMM to random slopes 
# models. Methods in Ecology and Evolution, 5(9), 944-946.
r2beta(mod_rnd_slope, method = "nsj", data = data_bully_slope) # will give out Rsquared from the model


# conditional AIC
cAIC(mod_rnd_slope)$caic

# model coefficients and significance test result
summary(mod_rnd_slope)

# compute Beta (standardized coefficients)
# if you have a small number of grouping variables 
# (like we have here, only 4), you may get warning messages like this:
# " In nextpar(mat, cc, i, delta, lowcut, upcut) :
# Last two rows have identical or NA .zeta values: using minstep"
# these messages are safe to ignore, see more info at: 
# https://stat.ethz.ch/pipermail/r-sig-mixed-models/2015q1/023318.html"

# confint(mod_rnd_slope) # this can take a while, requires a lot of computing power

stdCoef.merMod(mod_rnd_slope) # gives out the standardized beta (not for the intercept)
# only report the standardized coefficients




###########################################################
#                                                         #
#                 Repeated measures with                  #
#                  Mixed effect models                    #
#                                                         #
###########################################################

###########################################################
#                 Load wound healing dataset              #
###########################################################

# Person as "class": data from the same persons is more similar to the data of this person than to data from other people

# Simulated data from a study on wound healing over time
data_wound = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/data_woundhealing_repeated.csv")

# asign ID and location as factors
data_wound$ID = factor(data_wound$ID) # clustering variable
data_wound$location = factor(data_wound$location)

# varriables
names(data_wound)

# designate which are the repeated varibales
repeated_variables = c("day_1",	"day_2", "day_3",	"day_4", "day_5",	"day_6",	"day_7")


###########################################################
#                       Explore data                      #
###########################################################

# descriptives
describe(data_wound)
table(data_wound[,"location"])

# histograms
hist(data_wound$day_1)
hist(data_wound$day_2)
hist(data_wound$day_3)
hist(data_wound$day_4)
hist(data_wound$day_5)
hist(data_wound$day_6)
hist(data_wound$day_7)
hist(data_wound$distance_window)

# correlation of repeated variables
cor(data_wound[,repeated_variables])

###########################################################
#           Transform wide to long format                 #
###########################################################

# id.vars should be all non-repeated variables
data_wound_long = melt(data_wound, measure.vars=repeated_variables, variable.name = "time", value.name = "wound_rating")
# order data frame by participant ID(not necessary, just makes the dataframe look more intuitive)
data_wound_long = data_wound_long[order(data_wound_long[,"ID"]),]
# change the time variable to a numerical vector
data_wound_long$time = as.numeric(data_wound_long$time)

# lets look at how the data looks like in long format
data_wound_long


###########################################################
#                        Analysis                         #
###########################################################

# clustering variable: ID(in the parenteses), random intercept -> 1, random slope -> independent variable
mod_rep_int = lmer(wound_rating ~ time + distance_window + location + (1|ID), data = data_wound_long)
mod_rep_slope = lmer(wound_rating ~ time + distance_window + location + (time|ID), data = data_wound_long)
summary(mod_rep_slope) # all predictors are significant


### model comparison to see whether to use random slope or random intercept models
## plot the regression line (prediction)
# save the predictions of bot models to variables
data_wound_long$pred_int = predict(mod_rep_int)
data_wound_long$pred_slope = predict(mod_rep_slope)
# random intercept model
ggplot(data_wound_long, aes(y = wound_rating, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_int, x=time))+
  facet_wrap( ~ ID, ncol = 5)
# random slope and intercept model
ggplot(data_wound_long, aes(y = wound_rating, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_slope, x=time))+
  facet_wrap( ~ ID, ncol = 5)
# the two plotted model look pretty similar

# compare models with cAIC
# not too different
cAIC(mod_rep_int)$caic
cAIC(mod_rep_slope)$caic

# no real difference --> if there is no difference and no theoretical hint, which model to prefer, always go for the simplest model
# so in these cases: the random intercept ones

# compare models with anova
# not significantly different
anova(mod_rep_int, mod_rep_slope)

# if there is not too much benefit for the random slope model,
# use the one with only random intercept in it

### adding a quadratic term of time to the intercept model
# to account for curved relationship between time and wound rating
mod_rep_int_quad = lmer(wound_rating ~ time + I(time^2) + distance_window + location + (1|ID), data = data_wound_long)

## plot the results
# save prediction of the model to new variable
data_wound_long$pred_int_quad = predict(mod_rep_int_quad)

# random intercept model
ggplot(data_wound_long, aes(y = wound_rating, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_int_quad, x=time))+
  facet_wrap( ~ ID, ncol = 5)
# this looks like a better fit than the others

# compare models with cAIC
cAIC(mod_rep_int)$caic
cAIC(mod_rep_int_quad)$caic

# compare models with anova
anova(mod_rep_int, mod_rep_int_quad)

# based on thee results it seems that the random intercept model
# including the quadratic term of time would be the best model to choose



###########################################################
#              Data and model diagnostics                 #
###########################################################
# running model diagnostics on mixed effects models: http://ademos.people.uic.edu/Chapter18.html
#and https://www.ssc.wisc.edu/sscc/pubs/MM/MM_DiagInfer.html

# checking for influential outliers
influence(mod_rnd_slope, group = "class")$alt.fixed
influence(mod_rnd_slope, obs = T)$alt.fixed # this can take a minute or so

## checking assumptions

# normality assumption
# QQ plot
qqmath(mod_rnd_slope, id=0.05) # this might require the lattice package, but you can get the same graph wit the qqnorm() function

# linearity assumption
# linearity of prediction and standardized residuals
plot(mod_rnd_slope)
#linearity of each predictor and the standardized residual
# visualize the linearity of connection of each predictor
predictors=c("weight")

for(i in 1:length(predictors)){
  predictor_to_test = data_bully_slope[,predictors[i]]
  print(
    ggplot(data.frame(x = predictor_to_test,pearson=residuals(mod_rnd_slope,type="pearson")),
           aes(x=x,y=pearson)) +
      geom_point() +
      geom_smooth(method = 'loess') +
      theme_bw()
  )
}


# homoscedasticty assumption (homogeneity of variance)
# look for funnel shape on this graph
plot(mod_rnd_slope)
# Levens model for testing the for heteroscedasticity, from here: http://ademos.people.uic.edu/Chapter18.html
# look at the overall model F and p, if it is significant, there may be heteroscedasticity
summary(lm(residuals(mod_rnd_slope)^2 ~ data_bully_slope[,"class"]))

# multicollinearity
# there are some functions out there, but for now just look at the correlation matrix
# some example of a function designed to extract vif from lmer: https://raw.githubusercontent.com/aufrank/R-hacks/master/mer-utils.R
pairs.panels(data_bully_slope, col = "red", lm = T)


# some additional functions that may be useful for diagnostics in the HLMdiag package: (this is mentioned here: http://ademos.people.uic.edu/Chapter18.html)
#case_delete() #iteratively delete groups corresponding to the levels of a hierarchical linear model, using lmer to fit the models for each deleted case
#covratio() #calculate measures of the change in the covariance matrices for the fixed effects based on the deletion of an observation, or group of observations,
#diagnostics() #is used to compute deletion diagnostics for a hierarchical linear model based on the building blocks returned by case_delete.
#HLMresid() #extracts residuals from a hierarchical linear model fit using lmer. Can provide a variety of different types of residuals based upon the specifications when you call the function
#leverage() #calculates the leverage of a hierarchical linear model fit
#mdffits() #calculate measures of the change in the fixed effects estimates based on the deletetion of an observation, or group of observations

