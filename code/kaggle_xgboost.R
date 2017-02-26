require(xgboost)
require(methods)
require(caret)
library(Ckmeans.1d.dp)

# untunned: performance 0.61870

#######################################Code for submission
seed=1992

# load training dataset
apt = readRDS('data/train-v6.rds')

clean <- function(apartments){
  #fix fomatting
  apartments$price = as.numeric(as.character(apartments$price))
  apartments$bathrooms = as.numeric(as.character(apartments$bathrooms))
  apartments$bedrooms = as.numeric(as.character(apartments$bedrooms))
  apartments$latitude = as.numeric(as.character(apartments$latitude))
  apartments$longitude = as.numeric(as.character(apartments$longitude))
  
  # get rid of created column
  apartments$created = NULL
  
  #convert building and manager id to integer
  apartments$building_id<-as.integer(factor(apartments$building_id))
  apartments$manager_id<-as.integer(factor(apartments$manager_id))
  
  #convert street and display address to integer
  apartments$display_address<-as.integer(factor(apartments$display_address))
  apartments$street_address<-as.integer(factor(apartments$street_address))
  
  ##Length of description in words
  apartments$description <- as.character(apartments$description)
  apartments$description_len<-sapply(strsplit(apartments$description, "\\s+"), length)
  apartments$description = NULL
  
  #price to bedroom ratio
  apartments$bed_price <- apartments$price/apartments$bedrooms
  apartments[which(is.infinite(apartments$bed_price)),]$bed_price = apartments[which(is.infinite(apartments$bed_price)),]$price
  
  #add sum of rooms and price per room
  apartments$room_sum <- apartments$bedrooms + apartments$bathrooms
  apartments$room_diff <- apartments$bedrooms - apartments$bathrooms
  apartments$room_price <- apartments$price/apartments$room_sum
  apartments$bed_ratio <- apartments$bedrooms/apartments$room_sum
  apartments[which(is.infinite(apartments$room_price)),]$room_price = apartments[which(is.infinite(apartments$room_price)),]$price
  
  
  #log transform features, these features aren't normally distributed
  apartments$photoCount <- log(apartments$photoCount + 1)
  apartments$featureCount <- log(apartments$featureCount + 1)
  apartments$price <- log(apartments$price + 1)
  apartments$room_price <- log(apartments$room_price + 1)
  apartments$bed_price <- log(apartments$bed_price + 1)
  
  return(apartments)
}

apt <- clean(apt)

#################################################################################################
#Convert labels of intereset level to integers
apt$interest_level<-as.integer(factor(apt$interest_level))

# Selecting x and y
cols = c('bathrooms', 'bedrooms', "building_id" ,
         'price','latitude','longitude',
         "manager_id","street_address", 
         "created.Day" ,"created.WDay" ,"created.Hour",
         "photoCount","featureCount")
pct = c('mgrHighPct', 'mgrMediumPct', 'mgrLowPct','bldgHighPct', 'bldgMediumPct', 'bldgLowPct')
# Add features
cols = c(cols,colnames(apt)[56:61])
cols = c(cols,colnames(apt)[31:51])
cols = c(cols,pct)

outcome = 'interest_level'

######################### xgboost implementation
# Filter columns
data <- apt[c(outcome,cols)]

# transforming to a 0 to n vector for xgboost redabilitiy
y <- data$interest_level
y = y - 1

#create folds
kfolds<- 20
folds<-createFolds(y, k = kfolds, list = TRUE, returnTrain = FALSE)
fold <- as.numeric(unlist(folds[1]))

x_train<-data[-fold,-1] #Train set
x_val<-data[fold,-1] #Out of fold validation set

y_train<-y[-fold]
y_val<-y[fold]


#convert to xgbmatrix
dtrain = xgb.DMatrix(as.matrix(x_train), label=y_train)
dval = xgb.DMatrix(as.matrix(x_val), label=y_val)

#Parameters for XGB
xgb_params = list(
  colsample_bytree= 0.7,
  subsample = 0.7,
  eta = 0.1,
  objective= 'multi:softprob',
  max_depth= 4,
  min_child_weight= 1,
  eval_metric= "mlogloss",
  num_class = 3,
  seed = seed
)

#perform training
gbdt = xgb.train(params = xgb_params,
                 data = dtrain,
                 nrounds =475,
                 watchlist = list(train = dtrain, val=dval),
                 print_every_n = 25,
                 early_stopping_rounds=50)

####################################
###Generate Feature Importance Plot
imp <- xgb.importance(names(data[-1]),model = gbdt)
xgb.ggplot.importance(imp)

###################### Make predictions on the test dataset
test <- readRDS('data/test-v6.rds')
test <- clean(test)
test_subset <- test[cols]

#convert xgbmatrix
dtest <- xgb.DMatrix(data.matrix(test_subset))


##################
#predict on test set
allpredictions =  (as.data.frame(matrix(predict(gbdt,dtest), nrow=dim(test), byrow=TRUE)))


######################
##Generate Submission
allpredictions = cbind (allpredictions, test$listing_id)
names(allpredictions)<-c("high","medium","low","listing_id")
allpredictions=allpredictions[,c(1,2,3,4)]
write.csv(allpredictions,paste0(Sys.time(),"-XGBModel-20Fold-Seed",seed,".csv"),row.names = FALSE)

####################################TESTING ZONE



