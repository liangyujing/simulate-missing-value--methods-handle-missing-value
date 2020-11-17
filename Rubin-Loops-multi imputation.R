

library(mice)
data <- airquality
str(data)
summary(data)
data[4:10,3] <- rep(NA,7)
data[1:5,4] <- NA


data <- data[,-c(5,6)]
summary(data)
#have missing values on all the variables


pMiss <- function(x){100*sum(is.na(x))/length(x)}
apply(data,2,pMiss)
apply(data,1,pMiss)

head(data)
###Exploring the missingness patterns
###The mice package provides a nice function md.pattern() to get a better understanding of the pattern of missing data


###install this package using install.packages("mice")
library(mice)
md.pattern(data)  #visaulize


###Visualisation 1
###A perhaps more helpful visual representation can be obtained using the VIM package as follows
install.packages("VIM")
library(VIM)
aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

###Visualisation 2
##Another (hopefully) helpful visual approach is a special box plot
marginplot(data[, c(1,2)])
marginplot(data[, c(1,3)])
marginplot(data[, c(1,4)])
marginplot(data[, c(2,3)])
marginplot(data[, c(2,4)])
marginplot(data[, c(3,4)])


###Doing the imputation proper
##The mice() function takes care of the imputing process

library(mice)
tempData <- mice(data,m=3,maxit=5,meth='pmm',seed=11876)
summary(tempData)
#data: raw data  maxit:cycles, seed, meth: method


### Some notes on the parameters:
## m=5 refers to the number of imputed datasets. Five is the default value.  being replaced 3 times
## meth='pmm' refers to the imputation method. 
## In this case we are using predictive mean matching as imputation method. 
## Other imputation methods can be used, type methods(mice) for a list of the available imputation methods.



##If you would like to check the imputed data, for instance for the variable Ozone, you need to enter the following line of code
#check the data, only show you reolaced ones

tempData$imp$Ozone
## The output shows the imputed data for each observation (first column left) within each imputed dataset (first row at the top).
## If you need to check the imputation method used for each variable, mice makes it very easy to do

tempData$meth

##Now we can get back the completed dataset using the complete() function. 
###0 is for original incomplete data
############complecated data
completedData <- complete(tempData,5)  #can check completedData <- complete(tempData,2)
completedData

### Inspecting the distribution of original and imputed data  

library(lattice)
xyplot(tempData, Ozone ~ Wind+Temp+Solar.R, pch=18, cex=1)
#zone: DV, IV....
# What we would like to see is that the shape of the magenta points (imputed) matches the shape of the blue ones (observed). 
# The matching shape tells us that the imputed values are indeed "plausible values".


### Another helpful plot is the density plot:

densityplot(tempData)
# how the distributions looks iike, blue:true distribution, red, 3 different ones, not far away...

## The density of the imputed data for each imputed dataset is showed in magenta while the density of the observed data is showed in blue. 
## Again, under our previous assumptions we expect the distributions to be similar.

##Another useful visual take on the distributions can be obtained using the stripplot() function
##that shows the distributions of the variables as individual points


stripplot(tempData, pch = 20, cex = 1.2)
#left: raw data; then 2-4 different datasets; see the range, they are laying ...

###Pooling  
# pool everything together.......
# Suppose that the next step in our analysis is to fit a linear model to the data. 
# You may ask what imputed dataset to choose. 
# The mice package makes it again very easy to fit a a model to each of the imputed dataset and then pool the results together


modelFit1 <- with(tempData,lm(Temp~Ozone+Solar.R+Wind))
summary(modelFit1) #liner regression model, 3 different models, I want to pool
summary(pool(modelFit1))   #pooled
# The variable modelFit1 containts the results of the fitting performed over the imputed datasets, while the pool() function pools them all together. 
# Apparently, only the Ozone variable is statistically significant.

# Note that there are other columns aside from those typical of the lm() model: 
# fmi contains the fraction of missing information while lambda is the proportion of total variance that is attributable
#to the missing data.
#riv is for relative inrease in variance due to non-response
##df is residual degreess of freedom



# Remember that we initialized the mice function with a specific seed, therefore the results are somewhat dependent on our initial choice. 
# To reduce this effect, we can impute a higher number of dataset, by changing the default m=5 parameter in the mice() function as follows



# this time 50, check.....50 lines, 50 models....
tempData2 <- mice(data,m=50,seed=20201)
modelFit2 <- with(tempData2,lm(Temp~ Ozone+Solar.R+Wind))
summary(pool(modelFit2))

After having taken into account the random seed initialization, we obtain (in this case) 
more or less the same results as before with only Ozone showing statistical significance.
