data <- read.csv("Real Estate.csv")

data = na.omit(data)

# Find Outliers and remove them
#house price:
data = data[order(data$Y.house.price.of.unit.area),] #order the value from the least to the greatest
IQR = IQR(data$Y.house.price.of.unit.area)

Q1 = quantile(data$Y.house.price.of.unit.area, probs = .25)
Q3 = quantile(data$Y.house.price.of.unit.area, probs = .75)
leng = length(data$Y.house.price.of.unit.area)

upperfence = Q3 + (1.5 * IQR)
lowerfence = Q1 - (1.5 * IQR)

clean_data = subset(data, data$Y.house.price.of.unit.area < upperfence & data$Y.house.price.of.unit.area > lowerfence)
lengnow = length(clean_data$Y.house.price.of.unit.area)
lengnow

#distance to the nearest MRT station:
data = data[order(data$X3.distance.to.the.nearest.MRT.station),] #order the value from the least to the greatest
IQR = IQR(data$X3.distance.to.the.nearest.MRT.station)


Q1 = quantile(data$X3.distance.to.the.nearest.MRT.station, probs = .25)
Q3 = quantile(data$X3.distance.to.the.nearest.MRT.station, probs = .75)
leng = length(data$X3.distance.to.the.nearest.MRT.station)

upperfence = Q3 + (1.5 * IQR)
lowerfence = Q1 - (1.5 * IQR)

clean_data = subset(clean_data, clean_data$X3.distance.to.the.nearest.MRT.station < upperfence & clean_data$X3.distance.to.the.nearest.MRT.station > lowerfence)
lengnow = length(clean_data$X3.distance.to.the.nearest.MRT.station)
lengnow


unique(data$X4.number.of.convenience.stores)


#latitude:
data = data[order(data$X5.latitude),] #order the value from the least to the greatest
IQR = IQR(data$X5.latitude)

Q1 = quantile(data$X5.latitude, probs = .25)
Q3 = quantile(data$X5.latitude, probs = .75)
leng = length(data$X5.latitude)

upperfence = Q3 + (1.5 * IQR)
lowerfence = Q1 - (1.5 * IQR)

clean_data = subset(clean_data, clean_data$X5.latitude < upperfence & clean_data$X5.latitude > lowerfence)
lengnow = length(clean_data$X5.latitude)
lengnow

#longitude:
data = data[order(data$X6.longitude),] #order the value from the least to the greatest
View(data)
IQR = IQR(data$X6.longitude)

Q1 = quantile(data$X6.longitude, probs = .25)
Q3 = quantile(data$X6.longitude, probs = .75)
leng = length(data$X6.longitude)

upperfence = Q3 + (1.5 * IQR)
lowerfence = Q1 - (1.5 * IQR)

clean_data = subset(clean_data, clean_data$X6.longitude < upperfence & clean_data$X6.longitude > lowerfence)
lengnow = length(clean_data$X6.longitude)
lengnow

table(clean_data$X4.number.of.convenience.stores)
unique(data$X4.number.of.convenience.stores)

pie(table(clean_data$X4.number.of.convenience.stores), main = "Number of Convenience stores near the house", lab = unique(clean_data$X4.number.of.convenience.stores), col =rainbow(unique(clean_data$X4.number.of.convenience.stores)))


up = sort(clean_data$Y.house.price.of.unit.area)

#10000 New Taiwan Dollar/Ping, 1 Ping = 3.3 meter squared
for(x in 1:length(up)){
  up[x] = up[x] * 10000
}

mid = (up[1] + up[length(up)]) / 2

counter <- matrix(, nrow = 2, ncol = 1)
counter[1] = 0
counter[2] = 0
for(x in 1:length(up)){
  if(up[x] < 300000)
  {
    counter[1] = counter[1] + 1
  }
  else if(up[x] > 300000){
    counter[2] = counter[2] + 1
  }
}
pie(counter, main = strwrap("House price above and below 300000 NTD (New Taiwan Dollar)", width = 50), lab = c("Below 300000 NTD", "Above 300000 NTD"), col = c("red", "light blue"))


#plotting the Relationship between number of property and the number of convenience stores nearby
unique(clean_data$X4.number.of.convenience.stores)
interval = c(0:length(unique(clean_data$X4.number.of.convenience.stores)))
unique(interval)

hist(clean_data$X4.number.of.convenience.stores, main = "Relationship between number of property and \nthe number of convenience stores nearby",
     xlab = "number of convenience stores nearby", ylab = "number of property", col = "skyblue", ylim = c(0, 80)
)

clean_data = subset(clean_data, select = -c(clean_data$X1.transaction.date))
str(clean_data)
#install.packages("xgboost")
#install.packages("caTools")
#install.packages("caret")
#install.packages("MLmetrics")
library(MLmetrics)
library(xgboost)
library(caTools)
library(caret)


#split to train and test
datasplit <- sample.split(clean_data$Y.house.price.of.unit.area, SplitRatio = 0.6)
data_train <- subset(clean_data, datasplit == TRUE)
data_test <- subset(clean_data, datasplit == FALSE)
length(data_test)
print(data_test)
length(data_train)

#plotting train Root mean square error (excluding transaction date column)
model_xgb <- xgboost(data = data.matrix(data_train[,c(-1,-2)]),
                     label = data_train$Y.house.price.of.unit.area,
                     nround = 1000,
                     )

plot(model_xgb$evaluation_log)
data_test
#make a prediction
#prediction <- predict(model_xgb, newdata = data.matrix(data_test$Y.house.price.of.unit.area))
prediction <- predict(model_xgb, data.matrix(data_test[,c(-1,-2)]))


#r-squared (the accuracy for regression models)
rsqrt <- R2(prediction, data_test$Y.house.price.of.unit.area)
print(rsqrt)

#rmse
rmse <- RMSE(prediction, data_test$Y.house.price.of.unit.area)
print(rmse)

#mape
mape <- MAPE(prediction, data_test$Y.house.price.of.unit.area)
print(mape)
