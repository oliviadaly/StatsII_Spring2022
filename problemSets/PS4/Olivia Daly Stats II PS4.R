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

#install.packages("Rtools")

install.packages("ggfortify", dependencies = TRUE, INSTALL_opts = '--no-lock')

#Loading necessary packages

lapply(c("survival", "eha", "tidyverse", "ggfortify", "stargazer"),  pkgTest)



#setwd(C:/Users/olivi/OneDrive/Documents/TCD ASDS/Stats II/PS4)


####QUESTION 1####

data(infants) #calling data w/ eha package


infant_surv <- with(infants, Surv(enter, exit, event)) #creating survival object

#creating additive cox ph model with mother and 
#sex as covariates
infants_cox <- coxph(infant_surv ~ age + sex, data = infants) 

summary(infants_cox) #summary output of model

stargazer(infants_cox, type = "text")
stargazer(infants_cox)

drop1(infants_cox, test = "Chisq") #assessing model quality 
#by using this line to LRT both explanatory variables 

stargazer(drop1(infants_cox, test = "Chisq"))

#Chisq = expected versus observed 

#Interpretation of output:

#plotting
cox_fit <- survfit(infants_cox)

autoplot(cox_fit, main = "Survival Rate Over Time")


# Adding an interaction
#cox.int <- coxph(infant_surv ~ sex * mother, data = infants)
#summary(cox.int)
#drop1(cox.int, test = "Chisq")
#stargazer(cox.int, type = "text")
#stargazer(cox.int) #didn't use this in the end and stuck to an additive
#model

#Interpretation of Output:

# There is a 0.485 decrease in the expected log of the hazard for male babies compared to 
# female babies, holding the age of the mother constant. 

#There is a 0.040 decrease in the expected log of the hazard
# for each time the mother's age increases by 1 year/unit, holding the sex of the infants constant.

#None of the coefficients are statistically significant in their p values 
#according to the output table.

#Hazard Ratios:

coef(infants_cox)

# exponentiate parameter estimates to obtain hazard ratios
exp(coef(infants_cox))
#output value = 0.6156972


# The hazard ratio of male babies is 0.6156972 that of female babies, i.e.
# 62 male babies die for every 100 female babies; male deaths are 38% lower.

#The hazard ratio for the age of the mother is 0.9603673.

#There is a 4% decrease in the expected hazard for infants relative to a one year increase in mother's age 
#(or the expected hazard is 0.9603673 times higher for a mother who is one year older than another), holding sex constant.

#source used to come to this understanding: 
#https://sphweb.bumc.bu.edu/otlt/mph-modules/bs/bs704_survival/BS704_Survival6.html


stargazer(exp(coef(infants_cox)))

#Links for interpreting hazard ratios:

#https://sphweb.bumc.bu.edu/otlt/mph-modules/bs/bs704_survival/BS704_Survival6.html
#https://s4be.cochrane.org/blog/2016/04/05/tutorial-hazard-ratios/
