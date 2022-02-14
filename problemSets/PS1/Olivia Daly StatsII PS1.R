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
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

setwd("C:/Users/olivi/OneDrive/Documents/TCD ASDS/Stats II")


#####################
# Problem 1
#####################

set.seed(123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)
pdf("data_dist.pdf")
plot(data$x, data$y)
dev.off()

head(data)

#generate 1,000 random cauchy variables 

data2 <- rcauchy(1000, location = 0, scale = 1)


#create empirical distribution of observed data 
#using empirical cumulative distribution function

?ecdf


ECDF <- ecdf(data2)
empiricalCDF <- ECDF(data2)

#generate test statistic 

D <- max(abs(empiricalCDF - pnorm(data2)))

D #when we print this it gives us the test statistic 

#getting the p_value 

square_root<- sqrt(2*3.141593)
power_value <- ((1)^2)*((3.141593)^2)/(8*(x)^2)
p_value <- square_root*sum(exp(power_value)) #somehow need to get k to increase with each iteration
                                
p_value

#First attempt at KS function (not used)
KS_function <- function(x){
  ECDF <- ecdf(x)
  empiricalCDF <- ECDF(x) 
  #generate test statistic 
  D <- max(abs(empiricalCDF - pnorm(x)))
  #get p value
  p_value <- sqrt(2*3.141593)*sum(((1)^2)*(3.141593)^2/(8*(x)^2))
  output <- list(D, p_value)
  return(output)
}

#Second, better attempt at KS function:

KS_function <- function(x){
  ECDF <- ecdf(x)
  empiricalCDF <- ECDF(x) 
  #generate test statistic 
  D <- max(abs(empiricalCDF - pnorm(x)))
  #get p value
  square_root<- sqrt(2*3.141593)
  power_value <- ((1)^2)*((3.141593)^2)/(8*(x)^2)
  p_value <- square_root*sum(exp(power_value)) #somehow need to get k to increase with each iteration
  #telling the function what the output is
  output <- list(D, p_value)
  return(output)
}

#where k= 1 in the probability p_value formula

#website used to help me: https://statisticsglobe.com/r-return-multiple-objects-in-function

KS_function(data2)

#we can compare this with the built-in Komolgorov-Smirnov function

ks.test(data2, "pnorm")




#############################
#Problem 2
#############################





set.seed(123)

data <- data.frame(x= runif(200, 1, 10))

data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5) #errors distr w a mean of 0 and sd of 1.5


# this is a log likelihood function for the normal distribution 
norm_likelihood <- function(outcome, input, parameter) {
  n      <- nrow(input)
  k      <- ncol(input)
  beta   <- parameter[1:k]
  sigma2 <- parameter[k+1]^2
  e      <- outcome - input%*%beta
  logl   <- -.5*n*log(2*pi)-.5*n*log(sigma2) - ( (t(e) %*% e)/ (2*sigma2) )
  return(-logl)
}
# returns negative of log l it is a cumulative process assigning a bunch of values to diff args 
# log l is the total maths funciton 
# half of number of rows times log transformation (in the chapters he says)

# show you two different ways to set up same likelihood function
norm_likelihood2 <- function(outcome, input, parameter) {
  n <- ncol(input)
  beta <- parameter[1:n]
  sigma <- sqrt(parameter[1+n])
  -sum(dnorm(outcome, input %*% beta, sigma, log=TRUE))
}
# print our estimated coefficients (intercept and beta_1)
results_norm <- optim(fn=norm_likelihood, outcome=data$y, input=cbind(1, data$x), par=c(1,1,1), hessian=T, method="BFGS")
results_norm2 <- optim(fn=norm_likelihood, outcome=data$y, input=cbind(1, data$x), par=c(1,1,1), hessian=T, method="BFGS")


# print our estimated coefficients (intercept and beta_1)


# get same results regardless of which log-likelihood function we use
results_norm$par; results_norm2$par


# confirm that we get the same thing with lm()
coef(lm(data$y~data$x))

