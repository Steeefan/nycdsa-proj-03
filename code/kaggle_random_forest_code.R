library(randomForest)
library(caTools)
library(caret)
library(Metrics)
library(MLmetrics)


apartments = readRDS('train-v5.rds')

apartments$price = as.numeric(as.character(apartments$price))

apartments$bathrooms = as.numeric(as.character(apartments$bathrooms))

apartments$bedrooms = as.numeric(as.character(apartments$bedrooms))

apartments$latitude = as.numeric(as.character(apartments$latitude))

apartments$longitude = as.numeric(as.character(apartments$longitude))

cols = c('bathrooms', 'bedrooms', 'latitude', 'longitude', 'price', 'photoCount', 'featureCount', 'mgrHighPct', 'mgrMediumPct', 'mgrLowPct','bldgHighPct', 'bldgMediumPct', 'bldgLowPct')
outcome = 'interest_level'


multiloss <- function(predicted, actual){
  
  predicted <- apply(predicted, c(1,2), function(x) max(min(x, 1-10^(-15)), 10^(-15)))
  
  score <- -sum(actual*log(predicted))/nrow(predicted)
  
  return(score)
}


set.seed(99)
sample <- sample.split(apartments$interest_level, SplitRatio = 0.80)
train <- subset(apartments, sample == T)
test <- subset(apartments, sample == F)

train_data = train[ , cols]
train_outcome = train[ , outcome]
test_data = test[ , cols]
test_outcome = test[ , outcome]

test_outcome_prob = as.data.frame(matrix(0L, nrow = length(test_outcome), ncol = 3))
colnames(test_outcome_prob) = c('high', 'medium', 'low')

for (i in 1:length(test_outcome)){
  if (test_outcome[i] == 'high'){
    test_outcome_prob$high[i] = 1
  }
  if (test_outcome[i] == 'medium'){
    test_outcome_prob$medium[i] = 1
  }
  if (test_outcome[i] == 'low'){
    test_outcome_prob$low[i] = 1
  }
}

#probability forest with train data
apartment_forest_prob <- randomForest(train_data,train_outcome,xtest=train_data,ytest=train_outcome, keep.forest = TRUE)
forest_prob = predict(apartment_forest_prob,test_data,type="prob")

#probability forest with test data
apartment_forest_prob2 <- randomForest(train_data,train_outcome,xtest=test_data,ytest=test_outcome, keep.forest = TRUE)
forest_prob2 = predict(apartment_forest_prob2,test_data,type="prob")

multiloss(forest_prob, test_outcome_prob)

multiloss(forest_prob2, test_outcome_prob)








