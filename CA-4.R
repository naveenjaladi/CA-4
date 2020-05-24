# Reading the data set into the data frame
price_final <- read.csv("price_final.csv")
# displaying the empty rows of data along side of each column
data.frame(sapply(price_final, function(x)sum(length(which(is.na(x)))))) 
# number of rows with incomplete data
nrow(price_final[!complete.cases(price_final),])
# as the dataset is the cleaned from the uncleaned  data set while writing the csv file the rstudio
# appends the first row with column name as x and with unique key identifier so remove the unwanted row
price_final <- price_final[, c(2:9)]
#structure of the data frame
str(price_final)
# importing stringr library to deal with the string data
library(stringr)
# taking the new as area from the postal.code column and removing the string Dublin from all rows.
# as the linear regression model is best model to evaluate the data.
# the data contains of date feild but time series forecasting cannot be a suitable for modeling
# the data is about the year of 2019 so time series is not suitable for data.
price_final$Area <- str_remove_all(price_final$Postal.Code, "Dublin ")
# displaying the empty rows of data along side of each column
data.frame(sapply(price_final, function(x)sum(length(which(is.na(x)))))) 
# number of rows with incomplete data
nrow(price_final[!complete.cases(price_final),])
# once again check for structure before proceeding with predictive modeling
str(price_final)
# plot the independent variable at x and dependent variable at y
plot(price_final$Area, price_final$Price)
# taking the variables into the linear regression model and fitting the model using lm
fit <- lm(Price ~ Area, data=price_final)
# the plot shows that are outliers in the data to get the more detail about outliers qqplot is used.
# loading car library for qqplot
library(car)
# plotting the independent and dependent varibale using qqplot
qqPlot(fit, labels=row.names(price_final), id.method="identify", simulate=TRUE, main="Q-Q Plot")
# the qqplot doesn't shows the all outliers more precisely it only showed up two outliers 4827,5115
# summary of the model shows the summary for each area like area1, area2, ..etc
summary(fit)
# Confidence interval
confint(fit)
# outlier test for detecting all outlier variables in the model
outlierTest(fit)                                          
# outliers being removed
price_final <- price_final[-c(4827,5115,2212,5114,5137,2346,5133,516,5143,5126),]
# again fit the model with the linear regression and remove the outliers 
fit1 <- lm(Price ~ Area, data = price_final)
summary(fit1)
#Researchers evaluate their models based on r-square values or in other words effect sizes. 
# According to Cohen (1992)  r-square value .12 or below indicate low, 
# between .13 to .25 values indicate medium, .26 or above and above values indicate high effect size. 
# In this respect, your models are low and medium effect sizes.

# splitting the data into train and test data with the seed value of 10
set.seed(10)
# number of rows of price_final into no-rows-data
no_rows_data <- nrow(price_final)
# taking sample size as the 70 percentage of a dataset
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)
# giving sample size rows as training data
training_data <- price_final[sample, ]
# test data with remaining rows of data
testing_data <- price_final[-sample, ]
# plotting training data
scatter.smooth(x = training_data$Area,
               y = training_data$Price ,
               main = "Price Vs Area",
               xlab = "Area",
               ylab = "Price")
# now fitting the data with the data with training data 
fit2 <- lm(Price ~ Area, data = training_data)
# summary of the model
summary(fit2)
#  predicting the values from the model
pred_price <- predict(fit2,testing_data)
# taking actual and predicted values into a new data frame act_pred
act_peds <- data.frame(cbind(actual = testing_data$Price, pred = pred_price))
# displaying the actual and predicted price data frame
head(act_peds)
# correlation matrix of act_pred
cor(act_peds)
# estimating the error rate of the model using rse or sigma divided with the mean of the outcome
sigma(fit2)/mean(testing_data$Price)
# Mean absolute percentage error
mape <- mean(abs((act_peds$pred - act_peds$actual)) / act_peds$actual)
mape
# model forecasting
# FOR AREA 1
df <- data.frame(Area = c("1"))
predicted_price <- predict(fit2, df)
predicted_price

# FOR AREA 20
df <- data.frame(Area = c("20"))
predicted_price <- predict(fit2, df)
predicted_price