library(dplyr)
library(boxcox)
library(xgboost)
library(mlr)

apartments_train = readRDS('train-v6.rds')
apartments_test = readRDS('test-V6.rds')

train_outcomes = apartments_train$interest_level

test_listing_id = as.character(apartments_test$listing_id)

#change low medium high to 2 1 0 respectively, not sure needed

#convert factor columns to numeric 

apartments_train$price = as.numeric(as.character(apartments_train$price))
apartments_train$bathrooms = as.numeric(as.character(apartments_train$bathrooms))
apartments_train$bedrooms = as.numeric(as.character(apartments_train$bedrooms))
apartments_train$latitude = as.numeric(as.character(apartments_train$latitude))
apartments_train$longitude = as.numeric(as.character(apartments_train$longitude))


apartments_test$price = as.numeric(as.character(apartments_test$price))
apartments_test$bathrooms = as.numeric(as.character(apartments_test$bathrooms))
apartments_test$bedrooms = as.numeric(as.character(apartments_test$bedrooms))
apartments_test$latitude = as.numeric(as.character(apartments_test$latitude))
apartments_test$longitude = as.numeric(as.character(apartments_test$longitude))


train_n = nrow(apartments_train)

# drop created and created.Date columns

apartments_train = subset(apartments_train, select = -c(created))
apartments_test = subset(apartments_test, select = -c(created))
apartments_train = subset(apartments_train, select = -c(created.Date))
apartments_test = subset(apartments_test, select = -c(created.Date))

#clean up descriptions, count number of words in description


#clean up display address

#count vectorizor on features

#count managers, do on both train and test? 

train_manager_count = group_by(apartments_train, manager_id) %>% summarise(count = n())
train_manager_count = arrange(train_manager_count, desc(count))

#calculate percentile columns

#Top 1%

top_1_limit = nrow(train_manager_count) - round(nrow(train_manager_count) * .99)
train_manager_count['Manager_Top_1_Perc'] = 0
train_manager_count$Manager_Top_1_Perc[1:top_1_limit] = 1

#Top 5% 

top_5_limit = nrow(train_manager_count) - round(nrow(train_manager_count) * .95)
train_manager_count['Manager_Top_5_Perc'] = 0
train_manager_count$Manager_Top_5_Perc[1:top_5_limit] = 1

#Top 10%

top_10_limit = nrow(train_manager_count) - round(nrow(train_manager_count) * .9)
train_manager_count['Manager_Top_10_Perc'] = 0
train_manager_count$Manager_Top_10_Perc[1:top_10_limit] = 1

#Top 25%

top_25_limit = nrow(train_manager_count) - round(nrow(train_manager_count) * .75)
train_manager_count['Manager_Top_25_Perc'] = 0
train_manager_count$Manager_Top_25_Perc[1:top_25_limit] = 1

#join these columns and drop count column 

apartments_train = left_join(apartments_train, train_manager_count, on = 'manager_id')

apartments_train = subset(apartments_train, select = -c(count))

apartments_train$Manager_Top_1_Perc = ifelse(is.na(apartments_train$Manager_Top_1_Perc), 0, apartments_train$Manager_Top_1_Perc)
apartments_train$Manager_Top_5_Perc = ifelse(is.na(apartments_train$Manager_Top_5_Perc), 0, apartments_train$Manager_Top_5_Perc)
apartments_train$Manager_Top_10_Perc = ifelse(is.na(apartments_train$Manager_Top_10_Perc), 0, apartments_train$Manager_Top_10_Perc)
apartments_train$Manager_Top_25_Perc = ifelse(is.na(apartments_train$Manager_Top_25_Perc), 0, apartments_train$Manager_Top_25_Perc)

apartments_train$manager_id = ifelse(is.na(apartments_train$manager_id), 0, apartments_train$manager_id)

#count building 

train_building_count = group_by(apartments_train, building_id) %>% summarise(count = n())
train_building_count = arrange(train_building_count, desc(count))
train_building_count = train_building_count[2:nrow(train_building_count),]

#Percentiles for buildings

#Top 1%

top_1_limit = nrow(train_building_count) - round(nrow(train_building_count) * .99)
train_building_count['Building_Top_1_Perc'] = 0
train_building_count$Building_Top_1_Perc[1:top_1_limit] = 1

#Top 5%

top_5_limit = nrow(train_building_count) - round(nrow(train_building_count) * .95)
train_building_count['Building_Top_5_Perc'] = 0
train_building_count$Building_Top_5_Perc[1:top_5_limit] = 1

#Top 10%

top_10_limit = nrow(train_building_count) - round(nrow(train_building_count) * .9)
train_building_count['Building_Top_10_Perc'] = 0
train_building_count$Building_Top_10_Perc[1:top_10_limit] = 1

#Top 25%

top_25_limit = nrow(train_building_count) - round(nrow(train_building_count) * .75)
train_building_count['Building_Top_25_Perc'] = 0
train_building_count$Building_Top_25_Perc[1:top_25_limit] = 1

#join these columns onto training set, drop count column 

apartments_train = left_join(apartments_train, train_building_count, on = 'building_id')

apartments_train = subset(apartments_train, select = -c(count))

apartments_train$Building_Top_1_Perc = ifelse(is.na(apartments_train$Building_Top_1_Perc), 0, apartments_train$Building_Top_1_Perc)
apartments_train$Building_Top_5_Perc = ifelse(is.na(apartments_train$Building_Top_5_Perc), 0, apartments_train$Building_Top_5_Perc)
apartments_train$Building_Top_10_Perc = ifelse(is.na(apartments_train$Building_Top_10_Perc), 0, apartments_train$Building_Top_10_Perc)
apartments_train$Building_Top_25_Perc = ifelse(is.na(apartments_train$Building_Top_25_Perc), 0, apartments_train$Building_Top_25_Perc)

apartments_train$building_id = ifelse(is.na(apartments_train$building_id), 0, apartments_train$building_id)

#join to test set
apartments_test = left_join(apartments_test, train_building_count, on = 'building_id')

apartments_test = subset(apartments_test, select = -c(count))

apartments_test$Building_Top_1_Perc = ifelse(is.na(apartments_test$Building_Top_1_Perc), 0, apartments_test$Building_Top_1_Perc)
apartments_test$Building_Top_5_Perc = ifelse(is.na(apartments_test$Building_Top_5_Perc), 0, apartments_test$Building_Top_5_Perc)
apartments_test$Building_Top_10_Perc = ifelse(is.na(apartments_test$Building_Top_10_Perc), 0, apartments_test$Building_Top_10_Perc)
apartments_test$Building_Top_25_Perc = ifelse(is.na(apartments_test$Building_Top_25_Perc), 0, apartments_test$Building_Top_25_Perc)

apartments_test$building_id = ifelse(is.na(apartments_test$building_id), 0, apartments_test$building_id)
apartments_test$building_id = ifelse(is.na(apartments_test$building_id), 0, apartments_test$building_id)

apartments_test = left_join(apartments_test, train_manager_count, on = 'manager_id')

apartments_test = subset(apartments_test, select = -c(count))

apartments_test$Manager_Top_1_Perc = ifelse(is.na(apartments_test$Manager_Top_1_Perc), 0, apartments_test$Manager_Top_1_Perc)
apartments_test$Manager_Top_5_Perc = ifelse(is.na(apartments_test$Manager_Top_5_Perc), 0, apartments_test$Manager_Top_5_Perc)
apartments_test$Manager_Top_10_Perc = ifelse(is.na(apartments_test$Manager_Top_10_Perc), 0, apartments_test$Manager_Top_10_Perc)
apartments_test$Manager_Top_25_Perc = ifelse(is.na(apartments_test$Manager_Top_25_Perc), 0, apartments_test$Manager_Top_25_Perc)

#change NA to 0 in test set
apartments_test$manager_id = ifelse(is.na(apartments_test$manager_id), 0, apartments_test$manager_id)
apartments_test$building_id = ifelse(is.na(apartments_test$building_id), 0, apartments_test$building_id)

#boxcox for price?

#drop columns

apartments_train = subset(apartments_train, select = -c(mgrHighPct, mgrMediumPct, mgrLowPct, bldgHighPct, bldgMediumPct, bldgLowPct))
apartments_test = subset(apartments_test, select = -c(mgrHighPct, mgrMediumPct, mgrLowPct, bldgHighPct, bldgMediumPct, bldgLowPct))
  
#xgboost

apartments_train_outcome = apartments_train$interest_level

#dummy values for outcome labels, 0 for low, 1 for medium, 2 for high

apartments_train_outcome_dummy = ifelse(apartments_train_outcome == 'low', 0, ifelse(apartments_train_outcome == 'medium', 1, 2))

#need to drop all categorical variables

apartments_train = subset(apartments_train, select = -c(aptID, building_id, description, display_address, listing_id, manager_id, street_address, interest_level, created.WDayLbl, Apartment, predOnDesc, X))

apartments_train_data_matrix = xgb.DMatrix(data = as.matrix(apartments_train), label = apartments_train_outcome_dummy) 

apartments_test = subset(apartments_test, select = -c(aptID, building_id, description, display_address, listing_id, manager_id, street_address, created.WDayLbl))

apartments_test_data_matrix = xgb.DMatrix(as.matrix(apartments_test)) 

#train xgboost model, possibly stratify

params = list(
  eta = .06,
  gamma = 0.02,
  max_depth = 6,
  scale_pos_weight = 1,
  min_child_weight = 1,
  colsample_bytree = .8,
  subsample = .8,
  seed = 0,
  nthread = 4,
  objective = 'multi:softprob',
  eval_metric = 'mlogloss',
  num_class = 3,
  maximize = F
)

xgb_train = xgb.cv(params, apartments_train_data_matrix, nrounds = 5000, nfold = 5, early_stopping_rounds = 10)

xgb_mat = as.matrix(xgb_train)

log_loss_df = as.data.frame(xgb_mat[4])

min_log_loss = min(log_loss_df$test_mlogloss_mean)
min_log_loss_idx = which.min(log_loss_df$test_mlogloss_mean)

#.576 test log loss minimum on third attempt
# .57593 test log loss minimum on fourth attempt
# .57590 test log loss minimum on fifth attempt

nround = min_log_loss_idx

#was doing .1 gamma earlier

train_parameters = list(
  eta = .06,
  gamma = 0.02,
  max_depth = 6,
  min_child_weight = 1,
  scale_pos_weight = 1,
  colsample_bytree = .8,
  subsample = .8,
  seed = 0,
  nthread = 4,
  objective = 'multi:softprob',
  eval_metric = 'mlogloss',
  num_class = 3,
  maximize = F
)
 
trained_xgb <- xgb.train(params = train_parameters, data=apartments_train_data_matrix, nrounds=nround)

imp <- xgb.importance(names(apartments_train[-1]),model = trained_xgb)
head(imp)

#make train prediction and check error

train_prediction <- predict(trained_xgb, apartments_train_data_matrix, type = 'prob')

train_prediction_df = as.data.frame(train_prediction)

train_output = as.data.frame(matrix(0L, nrow = train_n, ncol = 3))
colnames(train_output) = c('low', 'medium', 'high')

count = 1

for (i in 1:train_n){
  train_output[i, 'low'] = train_prediction_df[count, 1]
  count = count + 1
  train_output[i, 'medium'] = train_prediction_df[count, 1]
  count = count + 1
  train_output[i, 'high'] = train_prediction_df[count, 1]
  count = count + 1
}


train_outcome_real = as.data.frame(matrix(0L, nrow = train_n, ncol = 3))
colnames(train_outcome_real) = c('high', 'medium', 'low')

for (i in 1:train_n){
  if (train_outcomes[i] == 'high'){
    train_outcome_real$high[i] = 1
  }
  if (train_outcomes[i] == 'medium'){
    train_outcome_real$medium[i] = 1
  }
  if (train_outcomes[i] == 'low'){
    train_outcome_real$low[i] = 1
  }
}

train_outcome_full = cbind(train_output[, 3], train_output[, 2], train_output[, 1], train_outcome_real)

final_train_output = as.data.frame(cbind(train_output[, 3], train_output[, 2], train_output[, 1]))

colnames(final_train_output) = c('high', 'medium', 'low')


multiloss <- function(predicted, actual){
  
  predicted <- apply(predicted, c(1,2), function(x) max(min(x, 1-10^(-15)), 10^(-15)))
  
  score <- -sum(actual*log(predicted))/nrow(predicted)
  
  return(score)
}

multiloss(final_train_output, train_outcome_real)

#fourth attempt multiloss on all training set 0.3722
#fifth attempt multiloss on all training set 0.4428399

#make test prediction
test_prediction <- predict(trained_xgb, apartments_test_data_matrix, type = 'prob')

test_prediction_df = as.data.frame(test_prediction)

#matrix is one column of test * 3 rows need to divide into 3 for each listing id

test_output = as.data.frame(matrix(0L, nrow = length(test_listing_id), ncol = 3))
colnames(test_output) = c('low', 'medium', 'high')

count = 1

for (i in 1:length(test_listing_id)){
  test_output[i, 'low'] = test_prediction_df[count, 1]
  count = count + 1
  test_output[i, 'medium'] = test_prediction_df[count, 1]
  count = count + 1
  test_output[i, 'high'] = test_prediction_df[count, 1]
  count = count + 1
}

#reorder so high is first

final_output = as.data.frame(cbind(as.character(test_listing_id), test_output[, 3], test_output[, 2], test_output[, 1]))

colnames(final_output) = c('listing_id', 'high', 'medium', 'low')

write.csv(final_output, 'kaggle_apartments_predictions_xgboost.csv', row.names = F, quote = F)

final_output$high = as.numeric(as.character(final_output$high))
final_output$medium = as.numeric(as.character(final_output$medium))
final_output$low = as.numeric(as.character(final_output$low))


#first attempt .583 using 500 rounds for xgboost using eta of .01
#second attempt .582 using 710 rounds using eta of .01
#third attempt .5799 using 519 rounds using eta of .05, max depth = 6, gamma = .1
#fourth attempt .57974 using 1366 rounds, eta of .02, max depth = 7, gamma = .05
