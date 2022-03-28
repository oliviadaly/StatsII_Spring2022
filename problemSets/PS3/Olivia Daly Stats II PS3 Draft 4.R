# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}



lapply(c("tidyverse",
         "nnet",
         "MASS"),  pkgTest)

#set working directory
setwd("C:/Users/olivi/OneDrive/Documents/TCD ASDS/Stats II/PS3")


#####QUESTION 1#####

#read in data


gdpChange <- read.csv("C:/Users/olivi/OneDrive/Documents/TCD ASDS/Stats II/PS3/gdpChange.csv")

summary(gdpChange)


head(gdpChange)

tail(gdpChange)

#Question 1 Part 1- unordered multinomial model

#response variable = GDPWdiff - diff in gdp between
#years t and t-1 - pos, neg, no change 

#reference category = "no change"

#explanatory variables: REG - democracy vs non-democracy, OIL- 1= av. ratio of fuel 
#exports to total exports 1984-6 greater than 50%, =0 otherwise


##################################
#Assigning levels to make variable categorical 
#in the correct way

#if value is no change = 0

#if value is positive 1

#if value is negative 2

gdpChange <- within(gdpChange, {   
  GDPWdiff.cat <- NA # need to initialize variable
  GDPWdiff.cat[GDPWdiff < 0] <- "negative"
  GDPWdiff.cat[GDPWdiff == 0] <- "no change"
  GDPWdiff.cat[GDPWdiff > 0] <- "positive"
} )


gdpChange$GDPWdiff.cat <- factor(gdpChange$GDPWdiff.cat, levels = c("no change", "positive", "negative"))

summary(gdpChange$GDPWdiff.cat)


#Coding categorical variables as factors- 
#Links: https://bookdown.org/carillitony/bailey/chp6.html -
#"R uses factor vectors to to represent dummy or categorical data.
#Factors can be ordered or unordered. Factor vectors are built on top of integer vectors and 
#include a unique label for each integer."


#making sure REG and OIL are also factors as they should be as categorical variables:

gdpChange$REG <- as.factor(gdpChange$REG)

gdpChange$OIL <- as.factor(gdpChange$OIL)


#finally able to move onto setting the reference category
#and creating the regression model

#setting reference category
gdpChange$GDPWdiff2 <- relevel(gdpChange$GDPWdiff.cat, ref = "no change")


multinom_gdp <- multinom(gdpChange$GDPWdiff2 ~ REG + OIL, data = gdpChange)

summary(multinom_gdp)

install.packages(stargazer)
library(stargazer)
stargazer(multinom_gdp)

#as per lecture slides, I am exponentiating
#the coefficient outputs of the model to complete my interpretation


#copying lecture slide format:



#exp(coef(multinom_gdp)[ ,c(1:3)]) #this was giving me crazy values, so I commented it out
#and decided not to exponentiate for interpretation in the end

exp(coef(multinom_gdp)[ ,c(1:3)])

#stargazer(exp(coef(multinom_gdp)[ ,c(1:3)]))

#stargazer(exp(coef(multinom_gdp)[ ,c(1:3)]))

#Interpretation (based on lecture 5 slide 10):


#Predictor/explanatory variables: 

#REG- positive- for a unit change in REG i.e. going from 0 to 1, non-democracy 
#to democracy, the log odds that there
#will be a positive change in GDP from one year to the next increase by 1.769007, when all other variables in the model
#are held constant and the reference category is "no change". 

#REG- negative-  for a unit change in REG i.e. going from 0 to 1, non-democracy 
#to democracy, the log odds that there will be a negative change in GDP from one year to the next increase by 1.379282, when all other variables in the model
#are held constant and the reference category is "no change."

#OIL- positive- when there is a unit change in the OIL variable, i.e. increasing from
#0 to 1, it means the average ratio of fuel exports to total exports in 1984-86 exceeded
#50%. Here, when there is a unit change in the oil variable and
#the average ratio of fuel exports to total exports in 1984-86 exceeded
#50%, the log odds that there will be a 
#positive difference in GDP in a country from one year to the next increase by 4.576321, 
#when all other variables in 
#the model are held constant.

#OIL- negative-  when there is a unit change in the oil variable from 0 to 1, meaning
#the average ratio of fuel exports to total exports in 1984-86 exceeded
#50%, the log odds that there will be a 
#negative difference in GDP in a country from one year to the next increase by 4.783968, 
#when all other variables in 
#the model are held constant.
  
  




###Question 1 Part 2####

#ordered multinomial logit 

ordered_multinom_gdp <- polr(GDPWdiff2 ~ REG + OIL, data = gdpChange, Hess = TRUE)

summary(ordered_multinom_gdp)  

stargazer(ordered_multinom_gdp)


#exponentiating for interpretation, as per lecture slides:
exp(cbind(OR=coef(ordered_multinom_gdp), confint(ordered_multinom_gdp)))

stargazer(exp(cbind(OR=coef(ordered_multinom_gdp), confint(ordered_multinom_gdp))))


#Interpretation - interpreting odds ratios here- based on week 8 slides 8-10-

#REG - where REG = 1, i.e. a country is a democracy, there is a 0.7000737 times increase in the 
#odds that their GDP will differ from the previous year than non-democratic 
#countries, when all other variables in 
#the model are held constant.

#OIL- when OIL =1 , i.e. the average ratio of fuel exports to total exports in a country in 1984-86 exceeded
#50%, the odds of a difference in that country's GDP from one year to the next are 
#1.2593051 times greater than countries with a less than 50% ratio, when all other 
#variables in the model are held constant

####QUESTION 2####


mexico_data <- read.csv("C:/Users/olivi/OneDrive/Documents/TCD ASDS/Stats II/PS3/MexicoMuniData.csv")

##Question 2 Part a- creating the poisson model


#outcome variable:

mexico_data$PAN.visits.06


#Coding categorical variables as factors- 
#Links:

#https://bookdown.org/carillitony/bailey/chp6.html -
#"7.1 Dummy Variables in R. R uses factor vectors to to represent dummy or categorical data.
#Factors can be ordered or unordered. Factor vectors are built on top of integer vectors and 
#include a unique label for each integer.

View(mexico_data)

#predictor variables- making sure they're factors as they should be as dummy/categorical variables: 

mexico_data$PAN.governor.06 = as.factor(mexico_data$PAN.governor.06)

mexico_data$competitive.district = as.factor(mexico_data$competitive.district)

#checking to see how the data looks now
View(mexico_data)


mexico_poisson <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, family = poisson (link = "log"), data=mexico_data)


summary(mexico_poisson)

stargazer(mexico_poisson)

exp(mexico_poisson$coefficients)

stargazer(exp(mexico_poisson$coefficients))

#I used the following links to help me with interpretation:
#https://stats.oarc.ucla.edu/stata/output/poisson-regression/
#"We can interpret the Poisson regression coefficient as follows: for a one unit 
#change in the predictor variable, the difference in the logs of expected counts 
#is expected to change by the respective regression coefficient, given the other predictor 
#variables in the model are held constant.
#NB this website does not exponentiate the coefficients


#Another useful link for intepretation:
#https://www.dataquest.io/blog/tutorial-poisson-regression-in-r/
#"If ?? = 0, then exp(??) = 1, and the expected count is exp(??) and, Y and X are not related."
#"If ?? > 0, then exp(??) > 1, and the expected count is exp(??) times larger than when X = 0"
#"If ?? < 0, then exp(??) < 1, and the expected count is exp(??) times smaller than when X = 0"

#for competitive.district = 1, i.e. a swing district
#?? = -0.08135
#exp(??)= 0.92186932 
#Therefore, based on the above, the expected count is 0.92186932 times smaller
#than when X= 0, with all else held constant.

#This expected count refers to visits by PAN presidential candidates 
#to swing districts, i.e. the count of visits would be 0.92 times smaller, with all else held constant.

#Thus, PAN presidential candidiates are 0.92 times less likely to 
#visit swing districts.

##Question 2 part b-

#Interpreting coefficients-
#Based on website: #https://www.dataquest.io/blog/tutorial-poisson-regression-in-r/


mexico_poisson$coefficients
exp(mexico_poisson$coefficients)

#marginality.06- 
#?? = -2.08014361 
#exp(??) = 0.12491227

#The coefficient is negative, so...

#When there is a unit increase in marginality.06, the expected count decreases by 0.12491227 times 


#PAN.governor.06- 
#?? = -0.31157887
#exp(??) =  0.73228985

#The PAN.governor.06 coefficient is negative, so when there is a unit increase in PAN.governor.06,
#i.e. when PAN.governor.06 = 1 and the state has a PAN-affiliated governor,
#expected count decreases by 0.73228985 times.



#Q2 part c-

#Using method on slide 14:

coeff <- mexico_poisson$coefficients

est_mean <- exp(coeff[1] + coeff[2]*1 + coeff[3]*0 + coeff[4]*1)

est_mean #this seems to be such a low value because there's a lot of zeros in the PAN.visits.06 column

stargazer(est_mean)
