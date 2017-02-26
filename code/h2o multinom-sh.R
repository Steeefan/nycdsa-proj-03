library(caret)
library(dplyr)
library(h2o)
library(data.table)
library(lubridate)

localH2O <- h2o.init(ip='localhost', port=54321)

aptsTrain = unique(read.csv('data/v7/train-v7.csv'))
aptsTest = unique(read.csv('data/v7/test-v7.csv'))
aptsTest$price = as.numeric(aptsTest$price)
aptsTest$bathrooms = as.numeric(aptsTest$bathrooms)
aptsTest$bedrooms = as.numeric(aptsTest$bedrooms)

set.seed(0)
train = sample(1:nrow(aptsTrain), 8 * nrow(aptsTrain) / 10)
test = (-train)

preProc = preProcess(
  select(
    aptsTrain,
    price, bathrooms, bedrooms, photoCount, featureCount,
    building_id, sum_vec,
    # created.Hour, latitude, longitude,
    # mgrLowPct, mgrMediumPct, mgrHighPct
    no_fee, hardwood_floors, laundry_in_building, laundry_in_unit,
    pre_war, fitness_center
  )
)

aptsTrainN = predict(
  preProc,
  select(
    aptsTrain,
    price, bathrooms, bedrooms, photoCount, featureCount,
    building_id, sum_vec,
    # created.Hour, latitude, longitude,
    mgrLowPct, mgrMediumPct, mgrHighPct,
    no_fee, hardwood_floors, laundry_in_building, laundry_in_unit,
    pre_war, fitness_center,
    interest_level
  )
)
aptsTrainN$interest_level = factor(aptsTrainN$interest_level, levels(aptsTrainN$interest_level)[c(1, 3, 2)])

preProc = preProcess(
  select(
    aptsTest,
    price, bathrooms, bedrooms, photoCount, featureCount,
    building_id, sum_vec,
    # created.Hour, latitude, longitude,
    mgrLowPct, mgrMediumPct, mgrHighPct,
    no_fee, hardwood_floors, laundry_in_building, laundry_in_unit,
    pre_war, fitness_center
  )
)

aptsTestN = predict(
  preProc,
  select(
    aptsTest,
    price, bathrooms, bedrooms, photoCount, featureCount,
    building_id, sum_vec,
    # created.Hour, latitude, longitude,
    mgrLowPct, mgrMediumPct, mgrHighPct,
    no_fee, hardwood_floors, laundry_in_building, laundry_in_unit,
    pre_war, fitness_center
  )
)

aptsTrainTrainHex = as.h2o(aptsTrainN[train, ])
aptsTrainTestHex = as.h2o(aptsTrainN[test, ])
aptsTestHex = as.h2o(aptsTestN)

# glm1 = h2o.glm(
#   y='interest_level',
#   x=c('price', 'bathrooms', 'bedrooms', 'photoCount', 'featureCount'),
#   training_frame=aptsTrainHex,
#   family='multinomial',
#   nfolds=10,
#   alpha=0
# )
#
# #table(predict(glm1, aptsTestHex))
# curLogLoss = h2o.logloss(h2o.performance(glm1))
# bestLogLossSoFar = min(curLogLoss, bestLogLossSoFar)

################################################################################

hyper_params = list(
  alpha = c(0, 0.01, 0.1, 0.3, 0.5, 0.7, 0.9, 1)
)

glmGrid = h2o.grid(
  algorithm='glm',
  grid_id='glmGrid',
  hyper_params=hyper_params,
  training_frame=aptsTrainTrainHex,
  validation_frame=aptsTrainTestHex,
  y='interest_level',
  # x=c(
  #   'price', 'bathrooms', 'bedrooms', 'photoCount', 'featureCount',
  #   'building_id', 'sum_vec', 'created.Hour', 'latitude', 'longitude',
  #   'mgrLowPct', 'mgrMediumPct', 'mgrHighPct',
  #   'no_fee', 'hardwood_floors', 'laundry_in_building', 'laundry_in_unit',
  #   'pre_war', 'fitness_center'
  # ),
  x=colnames(aptsTrainN)[1:length(aptsTrainN)-1],
  # lambda_search = TRUE,
  family='multinomial',
  nfolds=10
)

stopping_metric = 'accuracy'
sorted_models = h2o.getGrid(
  grid_id='glmGrid',
  sort_by=stopping_metric,
  decreasing=TRUE
)
best_model = h2o.getModel(sorted_models@model_ids[[1]])
h2o.logloss(h2o.performance(best_model))

preds = as.data.table(h2o.predict(best_model, aptsTestHex))
testPreds = data.table(listing_id = unlist(aptsTest$listing_id), preds[,.(high, medium, low)])

tsForSave = paste0(substr(year(now()), 3, 4), paste0(ifelse(month(now()) < 10, '0', ''), month(now())), day(now()), '-', hour(now()), minute(now()))
write.csv(testPreds, paste0('D:/kaggle-mnom-', tsForSave, '.csv'), row.names=F)

best_model@model$model_summary
