#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd("C:/Users/olivi/OneDrive/Documents/TCD ASDS/Stats II/PS2")

#####################
# Problem 1
#####################

####PART 1#######

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2022/blob/main/datasets/climateSupport.RData?raw=true"))

#climateSupport <- load("C:/Users/olivi/OneDrive/Documents/TCD ASDS/Stats II/PS2/climateSupport.RData") #the problem all along was that I was assigning the object!


summary(climateSupport)

head(climateSupport)

tail(climateSupport)

#converting the factors to numeric values to make the output make sense

climateSupport$choice <- as.numeric(as.factor(climateSupport$choice))-1 # 1 = participant supported policy, 0 = did not support

climateSupport$countries <- as.numeric(as.factor(climateSupport$countries))-1 #  0 = 20 of 192 countries, 1 = 80 of 192, 2 = 160 of 192
climateSupport$sanctions <- as.numeric(as.factor(climateSupport$sanctions))-1 # 0= None, 1 = 5%, 2 = 15% and 3 = 20%

#logit regression because the data is binary - working from lecture 4
ad_model <- glm(choice ~ countries + sanctions, data = climateSupport, family = binomial(logit))

summary(ad_model)

?exp

#default null hypothesis = that a quantity to be measured is zero ie explanatory variables not significant
#global null hypothesis is referring to when a group of hypotheses are being tested together, i.e. multiple variables
#states that in this case, none of the individual null hypotheses is false 
#source: https://journals.sagepub.com/doi/full/10.1177/0962280218768326

#p-value significance: I am taking the column labelled "Pr(>|z|)" as referring to this 

#describing my results:

#Taking alpha as 0.05, I can see that the p value on my table of results for countries is < 2e-16 and for sanctions is 3.15e-10.
#Both of these values are less than 0.05, meaning that we reject the global null hypothesis and take both 
#variables to be statistically significant.


########PART 2#########

#part a

#policy supported by 160 of 192 countries

#I have chosen to use log odds ratios to answer this question based on lecture 4

#Regression equation y = x + bx1 + bx2 + error
#subbing values in to the above to set the conditions we have been given in the question and get an answer
#using formula for log odds from slides 30-36 of lecture 4

odds_1 <- exp((-0.144558) + (0.32436)*2 + (-0.12353)*1)/ (1+ exp((-0.144558) + (0.32436)*2 + (-0.12353)*1)) # this is the model when sanctions are set at 5%
#we add exp() function to make it log odds and not simply odds on its own

odds_2 <- exp((-0.144558) + (0.32436)*2 + (-0.12353)*2)/ (1+ exp((-0.144558) + (0.32436)*2 + (-0.12353)*2)) #going from 5% to 15% sanctions with same model

odds_1
odds_2



#using formula for log odds from slides 66-69 of lecture 4
odds_diff <- odds_1 - odds_2

odds_diff # change from 5% to 15% here causes a 0.03010176 increase in estimated log odds likelihood of
#supporting a climate policy





#part b

#policy supported by only 20 of 192 countries

#Again using regression equation y = x + bx1 + bx2 + error
#again subbing in to the above equation
#exp() function allows us to create logarithmic odds
#using formula for log odds from slides 30-36 of lecture 4

odds_a <- exp((-0.144558) + (0.32436)*0 + (-0.12353)*1)/ (1+ exp((-0.144558) + (0.32436)*0 + (-0.12353)*1)) #sanctions set at 5%

odds_a

exp(odds_a)

odds_b <- exp((-0.144558) + (0.32436)*0 + (-0.12353)*2)/ (1+exp((-0.144558) + (0.32436)*0 + (-0.12353)*2)) #sanctions set at 15%

odds_b

exp(odds_b)

odds_change <- odds_a - odds_b

odds_change

exp()

#In this case there is a 0.03004869 increase in the estimated log odds of participants supporting the policy
#when increasing sanctions from 5% to 15% 


#part c

#80 participating countries, no sanctions

#subbing in to regression equation:

est_prob <- exp((-0.144558) + (0.32436)*1 + (-0.12353)*0)/(1+(exp(-0.144558) + (0.32436)*1 + (-0.12353)*0))

est_prob #answer found = estimated probability of 0.5466251

#part d

#fitting an interactive model

#based on lecture 4 slides, I am comparing the output of this with my additive model to test for significance


int_model <- glm(choice ~ countries*sanctions, data = climateSupport, family = binomial(logit))

summary(int_model) #only slightly different coefficients to additive model

#checking if the answers would change:
#part a- 

odds_3 <- exp((-0.148144) + ((0.328007)*2)*((-0.121111 )*1)) / (1+ exp((-0.148144) + ((0.328007)*2)*((-0.121111 )*1))) # this is the model when sanctions are set at 5%
#we add exp() function to make it log odds and not simply odds on its own

odds_4 <- exp((-0.148144) + ((0.328007)*2)*((-0.121111)*2)) / (1+ exp((-0.148144) + ((0.328007)*2)*((-0.121111)*2))) #going from 5% to 15% sanctions with same model

odds_3
odds_4



#using formula for log odds from slides 66-69 of lecture 4
odds_diff_2 <- odds_3 - odds_4

odds_diff_2 #answer here = 0.01950954 = different to original 







#part b- 

odds_c <- exp((-0.148144) + ((0.328007)*0)*(( -0.121111)*1))/ (1+ exp((-0.148144) + ((0.328007)*0)*(( -0.121111)*1))) #sanctions set at 5%

odds_c



odds_d <- exp((-0.148144) + ((0.328007)*0)*((-0.121111)*2))/ (1+ exp((-0.148144) + ((0.328007)*0)*((-0.121111)*2))) #sanctions set at 15%

odds_d


odds_change_2 <- odds_c - odds_d

odds_change_2 #answer here =  0 = different to original


#I got two different answers here to the original, but I am not sure why.


#not sure if I am meant to do an anova test here? 

#Testing significance
anova_test <- anova(ad_model, int_model, test = "Chisq")

anova_test
#not sure how to interpret the outcome of this