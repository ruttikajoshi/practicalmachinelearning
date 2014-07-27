library(caret)
library(rpart)

## Import Training Data
data.orig <- read.csv("C:/Users/FNP2981/Desktop/Coursera/Practical Machine Learning/pml-training.csv")

#----------------------------------
## Generic model for all users
#----------------------------------
# Include all main variables
# Exclude "_X,y,z" variables
data.use <- data.orig[,c(1,6,7,8,9,10,11,21,22,23,24,34,35,36,46,47,48,49,59)]
modfit <- rpart(classe ~ ., method="class", data=data.use)
summary(modfit)
plot(modfit)
text(modfit)

# Exclude "index" and "num_window"
data.use <- data.orig[,c(8,9,10,11,21,22,23,24,34,35,36,46,47,48,49,59)]
modfit <- rpart(classe ~ ., method="class", data=data.use)

plot(modfit, uniform=TRUE, main="Classification Tree")
text(modfit, use.n=FALSE, all=TRUE, cex=.8)

summary(modfit)
plot(modfit)
text(modfit)

#-----------------
## Model by user
#-----------------
names(data.orig)
table(data.orig$user_name)

## Adelmo
data.use <- data.orig[data.orig$user_name=="adelmo",c(8,9,10,11,21,22,23,24,34,35,36,46,47,48,49,59)]
modfit <- rpart(classe ~ ., method="class", data=data.use)
plot(modfit)
text(modfit)
summary(modfit)
plotcp(modfit)

plot(modfit, uniform=TRUE, main="Classification Tree")
text(modfit, use.n=FALSE, all=TRUE, cex=.8)


## Carlitos
data.use <- data.orig[data.orig$user_name=="carlitos",c(8,9,10,11,21,22,23,24,34,35,36,46,47,48,49,59)]
modfit <- rpart(classe ~ ., method="class", data=data.use)

plot(modfit, uniform=TRUE, main="Classification Tree")
text(modfit, use.n=FALSE, all=TRUE, cex=.8)

## Do the same for all users

#----------------------------------
## Validating with Test Dataset
#----------------------------------

data.test <- read.csv("C:/Users/FNP2981/Desktop/Coursera/Practical Machine Learning/pml-testing.csv")
data.test.use <- data.test[,names(data.use[,1:(ncol(data.use)-1)])]

pred <- predict(modfit, data=data.test.use, type = c("class"))

## some issue with R not predicting only for the 20 observations!
