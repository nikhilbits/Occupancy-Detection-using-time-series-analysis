library(dplyr)
library(plyr)
library(e1071)
library("randomForest")
library(TSrepr)
library(tree)
data$month <- as.numeric(format(as.POSIXct(data$date, format = "%Y-%m-%d %H:%M:%S"), "%m"))
data$day <- as.numeric(format(as.POSIXct(data$date, format = "%Y-%m-%d %H:%M:%S"), "%d"))
data$year <- as.numeric(format(as.POSIXct(data$date, format = "%Y-%m-%d %H:%M:%S"), "%Y"))
data$hour <- as.numeric(format(as.POSIXct(data$date, format = "%Y-%m-%d %H:%M:%S"), "%H"))
data$minute <- as.numeric(format(as.POSIXct(data$date, format = "%Y-%m-%d %H:%M:%S"), "%M"))
data$second <- as.numeric(format(as.POSIXct(data$date, format = "%Y-%m-%d %H:%M:%S"), "%S"))
data$minute <- data$minute + round(data$second / 60)
data$htm <- data$hour * 60 + data$minute
data$mtd <- (data$month - 1) * 30 + data$day
print(summary(data))

data <- select(data, -1)
data <- select(data, -c(year))
data_paa <- repr_paa(data$Temperature, q = 5, func = mean)

trainingData <- ldply(data_paa, data.frame)
trainingData$Temperature <- trainingData$X..i.. 
trainingData <- select(trainingData, -c(1))
trainingData$Light <- repr_paa(data$Light, q = 5, func = mean)
trainingData$Humidity <- repr_paa(data$Humidity, q = 5, func = mean)
trainingData$HumidityRatio <- repr_paa(data$HumidityRatio, q = 5, func = mean)
trainingData$CO2 <- repr_paa(data$CO2, q = 5, func = mean)
trainingData$month <- repr_paa(data$month, q = 5, func = mean)
trainingData$htm <- repr_paa(data$htm, q = 5, func = mean)
trainingData$mtd <- repr_paa(data$mtd, q = 5, func = mean)
trainingData$Occupancy <- repr_paa(data$Occupancy, q = 5, func = mean)
trainingData <- transform(trainingData, Occupancy = ifelse(Occupancy > 0.5, 1, 0))
trainingData <- select(trainingData, -c("month", "second"))

test <- read.csv("D:/Academics/Foundations of Data Science/Occupancy/datatest.txt")

test$month <- as.numeric(format(as.POSIXct(test$date, format = "%Y-%m-%d %H:%M:%S"), "%m"))
test$day <- as.numeric(format(as.POSIXct(test$date, format = "%Y-%m-%d %H:%M:%S"), "%d"))
test$year <- as.numeric(format(as.POSIXct(test$date, format = "%Y-%m-%d %H:%M:%S"), "%Y"))
test$hour <- as.numeric(format(as.POSIXct(test$date, format = "%Y-%m-%d %H:%M:%S"), "%H"))
test$minute <- as.numeric(format(as.POSIXct(test$date, format = "%Y-%m-%d %H:%M:%S"), "%M"))
test$second <- as.numeric(format(as.POSIXct(test$date, format = "%Y-%m-%d %H:%M:%S"), "%S"))
test$minute <- test$minute + round(test$second / 60)
test$htm <- test$hour * 60 + test$minute
test$mtd <- (test$month - 1) * 30 + test$day
test <- select(test, -c(1, "year", "month", "day", "minute", "hour", "second"))

dc <- tree(as.factor(Occupancy)~., data = trainingData)
preddc <- predict(dc, test, type = "response")
rdc <- table(test$Occupancy, preddc)



rf <- randomForest(as.factor(Occupancy)~., data = trainingData)
rfpred <- predict(rf, test, type = "response")
rrf <- table(test$Occupancy, rfpred)

logit <- glm(Occupancy~., data = trainingData, family = binomial)
pred <- predict(logit, test, type = "response")
summary(pred)
pred <- transform(pred, pred = ifelse(pred > 0.5, 1,0))
rlog <- table(test$Occupancy, pred$pred)

svmfit <- svm(as.factor(Occupancy)~., data = trainingData, kernel = 'radial')
svmpred <- predict(svmfit, test, type = "response")
rsvmr <- table(test$Occupancy, svmpred) 

svmfit <- svm(as.factor(Occupancy)~., data = trainingData, kernel = 'linear')
svmpred <- predict(svmfit, test, type = "response")
rsvml <- table(test$Occupancy, svmpred)

n <- 2
sampleset <- sample(1:100, n)
index <- c(sampleset, sampleset + 2000)

sample2 <- sortdata[index,]
observedLabels <- sortdata$Occupancy[index]

distMatrix <- dist(as.numeric(unlist(sample2)), method="DTW")

hc <- hclust(distMatrix, method="average")
plot(hc)

