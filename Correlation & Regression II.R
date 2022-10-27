examdata = read.csv('data.csv', header = T)
# What is the estimated mean product satisfaction according to a customer's level of health consciousness? 
# Are there important differences in mean satisfaction between these groups?
# Data to be analysed are "health consciousness" and satisfaction
library(ggplot2)
library(afex)
library(emmeans)
library(dbplyr)
library(car)
library(corrplot)
# nut_free, fruit_free, in_relationship = binary
summary(examdata)
str(examdata)
table(examdata$health_conscious)
# # Null hypothesis : Mean satisfaction is equal between the 3 health conscious groups. 
# Alternative hypothesis : Mean satisfaction differs at least between one group and the others. 
# Display 3 levels of health_ conscious
ggplot(examdata, aes(x=product_satisfaction, fill = health_conscious)) + geom_histogram()
aov_oneway = aov_ez(id = "cust.id",
                    dv = "product_satisfaction",
                    between = "health_conscious",
                    data = examdata)
summary(aov_oneway)
aov_oneway
# P Value for the F statistic is <0.001. This means our p.value is lesser than the significance level of .05
# This indicates the test result is highly significant. Thus we can reject the null hypothesis.
# The test results suggests there are differences in mean satisfaction between the 3 health_conscious groups.
# Before we deliver the summary to client, we verify the sample size of each of the health_conscious groups.
table(examdata$health_conscious)
# The sample size of each health_conscious groups is greater than 30, so we disregard the need to test the assumption of normality.
# We must test the assumption that the population variances of two or more samples are considered equal.
library(car)
leveneTest(product_satisfaction ~ health_conscious, data = examdata)
# The test statistic for Levene's is significant as our P value is equal to 0.04 so we do  reject the null hypothesis.
#Executive summary: 
# The three consumer groups based on the three health_conscious types differ on average in their satisfaction with the product. Thus, any effort to improve satisfaction must take into account the health consciousness of the consumers.















