# Removing / Clearing working environment
rm(list=ls())


# Setting working Directory
setwd("/Users/tyagraj/desktop/Casestudywines")


# Loading Data in to R
RedWine <- read.csv("Redwine.csv", sep = ";", stringsAsFactors = FALSE)
WhiteWine <- read.csv("Whitewine.csv", stringsAsFactors = FALSE, sep = ";")

#Merging RedWine and WhiteWine
wine <- rbind(RedWine, WhiteWine)

#Checking the data
str(wine)

#Summary of data
summary(wine)

#dimension of data
dim(wine)

# exploratory, quick histogram plots
hist(wine$fixed.acidity, xlab = "Fixed Acidity", main = "Histogram of Fixed Acidity", col = "slategrey")

hist(wine$volatile.acidity, xlab = "Volatile Acidity", main = "Histogram of Volatile Acidity", col = "slategray")

hist(wine$citric.acid, xlab = "Citric Acid", main = "Histogram of Citric Acid", col = "slategray")

hist(wine$residual.sugar, xlab = "Residual Sugar", main = "Histogram of Residual Sugar", col = "slategray")

hist(wine$chlorides, xlab = "Chlorides", main = "Histogram of Chlorides", col = "slategray")

hist(wine$free.sulfur.dioxide, xlab = "Free Sulfer Dioxide", main = "Histogram of Free Sulfer Dioxide", col = "slategray")


hist(wine$total.sulfur.dioxide, xlab = "Total Sulfer Dioxide", main = "Histogram of Total Sulfer Dioxide", col = "slategray")


hist(wine$density, xlab = "Density", main = "Histogram of Density", col = "slategray")

hist(wine$pH, xlab = "PH", main = "Histogram of  PH", col = "slategray")

hist(wine$sulphates, xlab = "Sulphates", main = "Histogram of Sulphates", col = "slategray")

hist(wine$alcohol, xlab = "Alcohol", main = "Histogram of Alcohol", col = "slategray")

hist(wine$quality, xlab = "Quality", main = "Histogram of Quality", col = "slategray")


#checking for NAs
sum(is.na(wine))

# detecting and replacing outliers with NA's

for ( i in 1:ncol(wine[1:11])){
  val= wine[,i][wine[,i] %in% boxplot.stats(wine[,i])$out]
  #wine= wine[which(!wine[,i] %in% val),]
  wine[,i][wine[,i] %in% val] = NA
}

#Mice imputation method
library(missForest)
library(mice)


wine <- mice(wine,seed = 500)
wine <- complete(wine,1)

# checking Nas after imputation
sum(is.na(wine))


#checkning correlation of variables
library(corrplot)
corrplot(cor(wine))
#checking vif of variables
library(usdm)
vif(wine)

#density variable dropped
wine$density <- NULL

#converting quality as.factor variable
wine$quality <- as.factor(wine$quality)


# dividing data in to training and test 


set.seed(123)
samp <- sample(nrow(wine),0.8* nrow(wine))
train <- wine[samp, ]
test <- wine[-samp, ]

library(MASS)
#1. Ordinance Logistic regression model
model= polr(quality ~. , data = train, Hess=TRUE, method= "logistic")
pred <- predict(model, newdata = test)
table(pred, test$quality)

confusionMatrix(pred, test$quality)

library(caret)

# models#2 # random forest

library(randomForest)
set.seed(212)
model2 <- randomForest(quality~., data = train, ntree =500, importance = TRUE)

pred <- predict(model2, newdata = test)
table(pred, test$quality)

(5 + 300+ 457+108+15) / nrow(test)

library(caret)
confusionMatrix(pred, test$quality)
varImpPlot(model2)

#3. knn model
library(class)
library(dummies)
library(vegan)
model3= knn(train, test, train$quality, k=5)
pred1<- table(model3, test$quality)
confusionMatrix(pred1)

#4.decision tree model
library(rpart)
fit <- rpart(quality ~. , data = train, method= "class")
pred2<- predict(fit,test, type="class")
table(pred2, test$quality)
(233+439+30)/nrow(test)
  