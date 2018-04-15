library(xgboost)
library(tidyverse)
library(dplyr)

train <- read_csv('../input/train.csv')
test <- read_csv('../input/test.csv')

glimpse(train)
glimpse(test)

train %>% summary()

# set a random seed & shuffle data frame
set.seed(1234)
ftrain <- ftrain[sample(1:nrow(dtrain)), ]

ftrain <- ftrain %>% 
  mutate(Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age)) %>%
  mutate(Sex = factor(Sex)) %>%
  mutate(Embarked = ifelse(is.na(Embarked), 'S', Embarked)) %>%
  select(Survived, Sex, Age, Pclass, Embarked)



# One-hot encode categorical
ftrain <- cbind(ftrain, model.matrix(~Embarked-1, ftrain)) %>%
  select(-Embarked)

mtrain <- data.matrix(ftrain %>% select(-Survived))
mlabs <- ftrain$Survived

# get the numb 70/30 training test split
numberOfTrainingSamples <- round(nrow(mtrain) * .7)

# training data
train_data <- mtrain[1:numberOfTrainingSamples,]
train_labels <- mlabs[1:numberOfTrainingSamples]

# testing data
test_data <- mtrain[-(1:numberOfTrainingSamples),]
test_labels <- mlabs[-(1:numberOfTrainingSamples)]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)

# train a model using our training data
model <- xgboost(data = dtrain, # the data   
                 nround = 2, # max number of boosting iterations
                 objective = "binary:logistic")  # the objective function

# generate predictions for our held-out testing data
pred <- predict(model, dtest)

# get & print the classification error
err <- mean(as.numeric(pred > 0.5) != test_labels)
print(paste("test-error=", err))


