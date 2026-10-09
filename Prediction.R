#Prediction
# [a]
myData <- read.csv("marketing_sales_data.csv")
dim(myData)
str(myData)
sum(is.na(myData))

# [b]
set.seed(003) 
training_index <- sample(x = nrow(myData),
                               size = 0.70 * nrow(myData),
                               replace = FALSE)

training_data <- myData[training_index, ]                    
validation_data <- myData[-training_index, ]

dim(training_data)
dim(validation_data)

# [c]
Model_1 <- lm(sales ~ spend + price + clicks + promo + emails + reviews, 
                    data = training_data)
summary(Model_1)
flextable::as_flextable(Model_1)

# [d]
Model_2 <- lm(sales ~ spend + price + clicks + promo, 
                    data = training_data)
summary(Model_2)
flextable::as_flextable(Model_2)

# [e]
source("hamR.R")

predicted_Model_1 <- predict(Model_1, newdata = validation_data)
predicted_Model_2 <- predict(Model_2, newdata = validation_data)

performance_Model_1 <- calculate_metrics_prediction(actual = validation_data$sales,
                                                    predicted = predicted_Model_1)
performance_Model_2 <- calculate_metrics_prediction(actual = validation_data$sales,
                                                    predicted = predicted_Model_2)

data.frame(Model_1 = performance_Model_1,
           Model_2 = performance_Model_2) |> round(4)
