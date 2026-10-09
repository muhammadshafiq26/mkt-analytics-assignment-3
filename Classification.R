#Classification Problem
#[a]
myData <- read.csv("marketing_conversion_data_v2.csv")
dim(myData)
str(myData)
sum(is.na(myData))

#[b]
set.seed(003) 
training_index <- sample(x = nrow(myData),
                              size = 0.70 * nrow(myData),
                              replace = FALSE)

training_data <- myData[training_index, ]                    
validation_data <- myData[-training_index, ]

dim(training_data)
dim(validation_data)

#[c]
Model_1<- glm(convert ~ spend + clicks + discount + age + email_len + tenure,
                    family = binomial,
                    data = training_data)

summary(Model_1)
flextable::as_flextable(Model_1)


# [d] Develop Model_2 
Model_2 <- glm(convert ~ spend + clicks + discount,
                    family = binomial,
                    data = training_data)

summary(Model_2)

# [e]
source("hamR.R")

predicted_Model_1 <- predict(Model_1, newdata = validation_data, type = "response")

confusion_matrix(actual = validation_data$convert,
                 predicted = predicted_Model_1)

roc_curve(actual = validation_data$convert,
          predicted = predicted_Model_1)



predicted_Model_2 <- predict(Model_2, newdata = validation_data, type = "response")

confusion_matrix(actual = validation_data$convert,
                 predicted = predicted_Model_2)

roc_curve(actual = validation_data$convert,
          predicted = predicted_Model_2)

