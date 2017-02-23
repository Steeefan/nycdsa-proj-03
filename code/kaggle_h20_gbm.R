#H2O GBM Tuning
library(h2o)
h2o.init(nthreads=-1)

#load training dataset

apartments = readRDS('data/train-v5.rds')

apartments$price = as.numeric(as.character(apartments$price))
apartments$bathrooms = as.numeric(as.character(apartments$bathrooms))
apartments$bedrooms = as.numeric(as.character(apartments$bedrooms))
apartments$latitude = as.numeric(as.character(apartments$latitude))
apartments$longitude = as.numeric(as.character(apartments$longitude))

################################## baseline: 0.4048201 (80/20 split train) 0.7782 Kaggle Score (NO FEATURE OR PHOTO COUNT)
cols = c('bathrooms', 'bedrooms', 'price','latitude', 
         'longitude', 'mgrHighPct', 'mgrMediumPct', 
         'mgrLowPct','bldgHighPct', 'bldgMediumPct', 'bldgLowPct',
         "photoCount","featureCount")
cols = c(cols,colnames(apartments)[34:54])

outcome = 'interest_level'

#Splitting fake training data into training and test sets by an 80% - 20% split.
set.seed(0)
train = sample(1:nrow(apartments), .8*nrow(apartments)) #Training indices.
apartment.train <- apartments[train,c(cols,"interest_level")]
apartment.train.outcome <- apartments[train,"interest_level"]
apartment.test = apartments[-train,c(cols,"interest_level")] #Test dataset yv: the part we don't use for training
interest.level.test = apartments$interest_level[-train] #Test response.

# load training dataset into h2o
train <- as.h2o(apartment.train, destination_frame = "train.hex")

## We only provide the required parameters, everything else is default
gbm <- h2o.gbm(x = cols, y = outcome, training_frame = train)

## Show a detailed model summary
gbm

apartment.test <- as.h2o(apartment.test,destination_frame = "test.hex")
## Get the logloss on the validation set
h2o.logloss(h2o.performance(gbm, newdata = apartment.test)) 

# Prediction on test set + submission

# fully train using entire dataset
train_full <- as.h2o(apartments[c(cols,"interest_level")], destination_frame = "train_full.hex")
gbm_full <- h2o.gbm(x = cols, y = outcome, training_frame = train_full)

# predict on the test dataset
temp <- readRDS("data/test-v6.rds")
apartment.test <- temp[,cols]

# formatting numeric values
apartment.test$price = as.numeric(as.character(apartment.test$price))
apartment.test$bathrooms = as.numeric(as.character(apartment.test$bathrooms))
apartment.test$bedrooms = as.numeric(as.character(apartment.test$bedrooms))
apartment.test$latitude = as.numeric(as.character(apartment.test$latitude))
apartment.test$longitude = as.numeric(as.character(apartment.test$longitude))

# load test set into h2o
test <- as.h2o(apartment.test[cols],destination_frame = "test.hex")

# make prediction using the full model
preds <- as.data.table(h2o.predict(gbm_full, test))
testPreds <- data.table(listing_id = unlist(temp$listing_id), preds[,.(high, medium, low)])

#write file for submission
fwrite(testPreds, "try5.csv")


# 0.403
##################################### TESTING ZONE ##############################

cols = c('bathrooms', 'bedrooms', 'price','latitude', 
         'longitude', 'mgrHighPct', 'mgrMediumPct', 
         'mgrLowPct','bldgHighPct', 'bldgMediumPct', 'bldgLowPct',
         "photoCount","featureCount")
cols = c(cols,colnames(apartments)[34:54])
cols = c(cols, colnames(apartments)[15:24])

outcome = 'interest_level'

#Splitting fake training data into training and test sets by an 80% - 20% split.
set.seed(0)
train = sample(1:nrow(apartments), .8*nrow(apartments)) #Training indices.
apartment.train <- apartments[train,c(cols,"interest_level")]
apartment.train.outcome <- apartments[train,"interest_level"]
apartment.test = apartments[-train,c(cols,"interest_level")] #Test dataset yv: the part we don't use for training
interest.level.test = apartments$interest_level[-train] #Test response.

# load training dataset into h2o
train <- as.h2o(apartment.train, destination_frame = "train.hex")

## We only provide the required parameters, everything else is default
gbm <- h2o.gbm(x = cols, y = outcome, training_frame = train)

## Show a detailed model summary
gbm

apartment.test <- as.h2o(apartment.test,destination_frame = "test.hex")
## Get the logloss on the validation set
h2o.logloss(h2o.performance(gbm, newdata = apartment.test)) 

##################################### TESTING ZONE ##############################











