examdata = read.csv('data.csv', header = T)
library(ggplot2)
library(afex)
library(emmeans)
library(dbplyr)
library(car)
library(corrplot)
summary(examdata)
str(examdata)
table(examdata$in_relationship)
table(examdata$health_conscious)
table(examdata$nut_free)
table(examdata$fruit_free)
# Numeric variable of each categorical variable
examdata$in_relationship = factor(examdata$in_relationship, levels=c("yes", "no"), labels=c("yes", "no"))
examdata$num_in_relationship = factor(examdata$in_relationship, levels=c("yes", "no"), labels=c(1,0))
examdata$num_in_relationship = as.numeric(as.character(examdata$num_in_relationship))
examdata$nut_free =factor(examdata$nut_free, levels=c("yes", "no"), labels=c("yes","no"))
examdata$num_nut_free= factor(examdata$nut_free, levels=c("yes", "no"), labels=c(1,0))
examdata$num_nut_free <- as.numeric(as.character(examdata$num_nut_free))
examdata$fruit_free =factor(examdata$fruit_free, levels=c("yes", "no"), labels=c("yes","no"))
examdata$num_fruit_free= factor(examdata$fruit_free, levels=c("yes", "no"), labels=c(1,0))
examdata$num_fruit_free <- as.numeric(as.character(examdata$num_fruit_free))
examdata$health_conscious = factor(examdata$health_conscious, levels=c("high", "low", "medium"), labels=c("high", "low", "medium"))
examdata$in_relationship = factor(examdata$in_relationship, levels=c("yes", "no"), labels=c("yes", "no"))
# The client wants to understand what variables are related to product satisfaction. Which variables are related to product satisfaction and which are not?
# The outcome variable is product satisfaction. I examine the distribution of satisfaction scores to see if there are any outliers:
g = examdata$product_satisfaction
h = hist(g, breaks = 10, density =10,
         col =  "lightgray", xlab = "x-varriable",main = "observed with normal overlay")
xfit = seq(min(g), max (g), length = 40)
yfit = dnorm(xfit, mean = mean(g), sd = sd(g))
yfit = yfit * diff(h$mids[1:2]) * length(g)

lines(xfit, yfit, col = "black", lwd =2)
# The distribution for product_satisfaction looks fairly symmetric and there are no unusual values in either tail.
# Visulaize distributions of the numeric variables that we will test to see if they are related to product_satisfaction. 
# Display of each numeric variable with a histogram and then in a scatterplot with product_satisfaction.

# age
g = examdata$age
h = hist(g, breaks =10, density = 10,
         col = "lightgray", xlab = "xvariable", main = "observed with normal overlay")
xfit = seq(min(g), max(g), length = 40)
yfit = dnorm(xfit, mean = mean(g), sd = sd(g))
yfit = yfit * diff(h$mids [1 :2]) * length(g)

lines(xfit, yfit, col = "black", lwd = 2)

age_model = lm(product_satisfaction ~ age, data = examdata)
plot(product_satisfaction ~ age, data = examdata)
abline(age_model)

# annual_income
g = examdata$annual_income
h = hist(g, breaks = 10, density =10,
         col = "lightgray", xlab = "x-variable", main = "observed with normal overlay")
xfit = seq(min(g), max(g), length = 40)
yfit = dnorm(xfit, mean = mean(g), sd = sd(g))
yfit = yfit * diff(h$mids[1:2]) * length(g)

lines(xfit, yfit, col = "black", lwd = 2)

annual_income_model = lm(product_satisfaction ~ annual_income, data = examdata)
plot(product_satisfaction ~ annual_income, data = examdata)
abline(annual_income_model)

#conscientiousness_personality
g = examdata$conscientiousness_personality
h = hist(g, breaks = 10, density =10,
         col = "lightgray", xlab = "x-variable", main = "observed with normal overlay")
xfit = seq(min(g), max(g), length = 40)
yfit = dnorm(xfit, mean = mean(g), sd = sd(g))
yfit = yfit * diff(h$mids[1:2]) * length(g)

lines(xfit, yfit, col = "black", lwd = 2)

conscientiousness_personality_model = lm(product_satisfaction ~ conscientiousness_personality, data = examdata)
plot(product_satisfaction ~ conscientiousness_personality, data = examdata)
abline(conscientiousness_personality_model)

# log_relationship_length
g = examdata$log_relationship_length
h = hist(g, breaks = 10, density =10,
         col = "lightgray", xlab = "x-variable", main = "observed with normal overlay")
xfit = seq(min(g), max(g), length = 40)
yfit = dnorm(xfit, mean = mean(g), sd = sd(g))
yfit = yfit * diff(h$mids[1:2]) * length(g)

lines(xfit, yfit, col = "black", lwd = 2)

log_relationship_length_model = lm(product_satisfaction ~ log_relationship_length, data = examdata)
plot(product_satisfaction ~ log_relationship_length, data = examdata)
abline(log_relationship_length_model)

# Test which variables are related to satisfaction with the product using multiple regression. 
# Null hypothesis : The variable is not related to satisfaction.
# Alternative hypothesis : The variable is related to satisfaction.
str(examdata) 
corrplot(cor(examdata[, c(6,7:13)]), method = "ellipse", type = "upper")
model3 <- lm(product_satisfaction ~ age + annual_income +  conscientiousness_personality + log_relationship_length + num_in_relationship + num_nut_free+ num_fruit_free + health_conscious, data=examdata)
summary(model3)
# The p-values for the t statistics for annual_income,conscientiousness_personality,num_in_relationship and num_nut_free, are less than a significance level of .05.
# For these 4 explanatory variables, we reject the null hypothesis and conclude that each relates to satisfaction (taking into account the associations that each variable in the model has with satisfactions)
# The p-values for the t statistics for age, num_fruit_log_relationship_length health_consciouslow and health_consciousmedium are greater than a significance level of .05.
# For these 4 explanatory variables, we accept the null hypothesis and conclude that none of these variables relate to satisfaction (taking into account the associations that each variable in the model has with satisfactions).
# We see that as a set, the explanatory variables account for about 18% of the variation in satisfaction. This indicates that the client may want to invest more effort into finding other variables that are driving satisfaction.
# Model 4 contains variables that were not statiscally significant
model4 = lm(product_satisfaction ~ age + annual_income + conscientiousness_personality + num_in_relationship + num_nut_free, data=examdata)
summary(model4)
anova(model4, model3)
# From the anova test used to compare the fit of these two models, we see that the test statistic has a p value that is greater than .05, so the fit of the full model is statistically different from the fit of the reduced model. In fact, the adjusted R squared value is still at about 18%.
# Satisfaction with the product is related to whether the customer prefers fruit-free chocolate, whether the customer health consciousness is high or low or if the customer is has been in a reelatonship for long.
