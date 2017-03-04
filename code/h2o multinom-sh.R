library(caret)
library(dplyr)
library(h2o)
library(data.table)
library(lubridate)
library(tidyr)
library(VIM)

source('multiloss.R')

localH2O <- h2o.init(ip='localhost', port=54321, nthreads=-1)
h2o.removeAll()

aptsTrain = unique(readRDS('data/v16/train-v16.rds'))
aptsTest = unique(readRDS('data/v16/test-v16.rds'))
aptsTest$price = as.numeric(aptsTest$price)
aptsTest$bathrooms = as.numeric(aptsTest$bathrooms)
aptsTest$bedrooms = as.numeric(aptsTest$bedrooms)

#code to make real outcome data frame
aptsTrain$value = 1
train_outcomes_real = spread(aptsTrain, key=interest_level, value=value)
train_outcomes_real = subset(train_outcomes_real, select=c(high, medium, low))
train_outcomes_real[is.na(train_outcomes_real)] = 0
aptsTrain$value = NULL

set.seed(0)
train = sample(1:nrow(aptsTrain), 8 * nrow(aptsTrain) / 10)
test = (-train)

# preProc = preProcess(
#   select(
#     aptsTrain,
#     price, bathrooms, bedrooms, photoCount, featureCount,
#
#     # bed_price, room_sum, room_diff, room_price, bed_ratio,
#     # avg_price, avg_bed_price, avg_room_price, price_diff,
#     # bed_price_diff, room_price_diff,
#
#     cluster
#   )
#)

# aptsTrainN = predict(
#   preProc,
#   select(
#     aptsTrain,
#     #Y
#     interest_level,
#
#     #Basic apartment stuff
#     price, bathrooms, bedrooms, photoCount, featureCount,
#
#     #Prices, rooms
#     # bed_price, room_price, avg_price, avg_bed_price, avg_room_price,
#     # price_diff, bed_price_diff, room_price_diff,
#     # room_sum, room_diff, bed_ratio,
#
#     #Building
#     building_id, Building_Top_1_Perc, Building_Top_5_Perc, Building_Top_10_Perc,
#
#     #Manager
#     manager_id, Manager_Top_1_Perc, Manager_Top_5_Perc, Manager_Top_10_Perc,
#
#     #Sentiment
#     anger, anticipation, disgust, fear, joy, sadness, surprise, trust, negative,
#     positive,
#
#     #Features
#     no_fee, hardwood_floors, laundry_in_building, laundry_in_unit,
#     pre_war, fitness_center, dishwasher,
#
#     #Location
#     cluster
#   )
# )
#aptsTrainN$interest_level<-as.integer(factor(aptsTrainN$interest_level))

# sapply(select(aptsTrainN, anger, anticipation, disgust, fear, joy, sadness, surprise, trust, negative), sum)
#apply(select(aptsTrainN, anger, anticipation, disgust, fear, joy, sadness, surprise, trust, negative), 2, plyr::count)

# aptsTrainN = select(
#   aptsTrain,
#   #Y
#   interest_level,
#
#   #Basic apartment stuff
#   price, bathrooms, bedrooms, photoCount, featureCount,
#
#   #Building
#   building_id, Building_Top_1_Perc, Building_Top_5_Perc, Building_Top_10_Perc,
#   Building_Top_25_Perc, Building_Top_50_Perc,
#
#   #Manager
#   manager_id, Manager_Top_1_Perc, Manager_Top_5_Perc, Manager_Top_10_Perc,
#   Manager_Top_25_Perc, Manager_Top_50_Perc,
#   mgrHighPct, mgrMediumPct, mgrLowPct,
#
#   #Sentiment
#   # anger, anticipation, disgust, fear, joy, sadness, surprise, trust, negative,
#   # positive
#
#   no_fee, hardwood_floors, laundry_in_building, laundry_in_unit,
#   pre_war, fitness_center
# )
# aptsTrainN$interest_level = factor(aptsTrainN$interest_level, levels(aptsTrainN$interest_level)[c(1, 3, 2)])
#
# preProc = preProcess(
#   select(
#     aptsTest,
#     price, bathrooms, bedrooms, photoCount, featureCount,
#     building_id, sum_vec,
#     # created.Hour, latitude, longitude,
#     mgrLowPct, mgrMediumPct, mgrHighPct,
#     no_fee, hardwood_floors, laundry_in_building, laundry_in_unit,
#     pre_war, fitness_center
#   )
# )
#
# aptsTestN = predict(
#   preProc,
#   select(
#     aptsTest,
#     price, bathrooms, bedrooms, photoCount, featureCount,
#     building_id, sum_vec,
#     # created.Hour, latitude, longitude,
#     mgrLowPct, mgrMediumPct, mgrHighPct,
#     no_fee, hardwood_floors, laundry_in_building, laundry_in_unit,
#     pre_war, fitness_center
#   )
# )

# aptsTestN = select(
#   aptsTest,
#   price, bathrooms, bedrooms, photoCount, featureCount
# )

aptsTrainN = subset(
  aptsTrain,
  select = -c(
    aptID, building_id, description, display_address, listing_id,
    manager_id, street_address, created, created.Date, created.WDayLbl,
    mgrHighPct, mgrMediumPct, mgrLowPct, bldgHighPct, bldgMediumPct, bldgLowPct
  )
)

aptsTestN = subset(
  aptsTest,
  select = -c(
    aptID, building_id, description, display_address, listing_id, manager_id,
    street_address, created, created.Date, mgrHighPct, mgrMediumPct, mgrLowPct,
    bldgHighPct, bldgMediumPct, bldgLowPct, created.WDayLbl
  )
)

#real outcomes split into train and test set
train_outcomes_realTrainTrain = train_outcomes_real[train, ]
train_outcomes_railTrainTest = train_outcomes_real[test, ]

#split training set into trainTrain and trainTest
aptsTrainTrain = aptsTrainN[train, ]
aptsTrainTest = aptsTrainN[test, ]

#data for h2o
aptsTrainTrainHex = as.h2o(aptsTrainTrain)
aptsTrainTestHex = as.h2o(aptsTrainTest)
aptsTrainHex = as.h2o(aptsTrainN)

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
  alpha = seq(0, 1, 0.1),
  lambda = c(1e-4, 1e-5, 1e-6, 1e-7, 1e-8)
)

glmGrid = h2o.grid(
  algorithm='glm',
  grid_id='glmGrid',
  hyper_params=hyper_params,
  training_frame=aptsTrainTrainHex,
  validation_frame=aptsTrainTestHex,
  y='interest_level',
  x=colnames(select(aptsTrainN, -interest_level)),
  family='multinomial',
  nfolds=10,
  solver='L_BFGS'
)

#stopping_metric = 'accuracy'
models = h2o.getGrid(grid_id='glmGrid')
# best_model = h2o.getModel(sorted_models@model_ids[[1]])
# h2o.logloss(h2o.performance(best_model))

for (i in 1:length(models@model_ids)) {
  if (i == 1) {
    results = data.frame()
  }
  predsOnTrain = as.data.table(h2o.predict(h2o.getModel(models@model_ids[[i]]), aptsTrainTrainHex))
  trainPreds = data.table(predsOnTrain[,.(high, medium, low)])
  results = rbind(results, data.frame(i, multiloss(predicted=trainPreds, actual=train_outcomes_realTrainTrain)))
}
names(results) = c('id', 'multiloss')
cat(min(results$multiloss))

best_model = h2o.getModel(sorted_models@model_ids[[which(results$multiloss == min(results$multiloss))]])

predsOnTest = as.data.table(h2o.predict(best_model, aptsTestHex))
testPreds = data.table(listing_id = unlist(aptsTest$listing_id), predsOnTest[,.(high, medium, low)])

tsForSave = paste0(substr(year(now()), 3, 4), paste0(ifelse(month(now()) < 10, '0', ''), month(now())), day(now()), '-', hour(now()), minute(now()))
write.csv(testPreds, paste0('D:/kaggle-mnom-', tsForSave, '.csv'), row.names=F)
