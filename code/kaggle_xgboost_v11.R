require(xgboost)
require(methods)
require(caret)
library(Ckmeans.1d.dp)

# with sum_vec and cluster: 0.606
# with sum_vec(naive bayes on description): 0.60771
# untunned: performance 0.61870
# without mnger id and bldg id pct, logloss  = 0.62429 

# price_diff:  0.61353

#######################################Code for submission
seed=1992

# load training dataset
apt = readRDS('data/train-v11.rds')

clean <- function(apartments){
  # get rid of created column
  apartments$created = NULL
  
  #convert building, manager, listing id to integer
  apartments$building_id<-as.integer(factor(apartments$building_id))
  apartments$manager_id<-as.integer(factor(apartments$manager_id))
  
  #convert street and display address to integer
  apartments$display_address<-as.integer(factor(apartments$display_address))
  apartments$street_address<-as.integer(factor(apartments$street_address))
  
  #log transform features as they aren't normally distributed
  apartments$photoCount <- log(apartments$photoCount + 1)
  apartments$featureCount <- log(apartments$featureCount + 1)
  apartments$price <- log(apartments$price + 1)
  apartments$room_price <- log(apartments$room_price + 1)
  apartments$bed_price <- log(apartments$bed_price + 1)
  
  return(apartments)
}

apt <- clean(apt)

# MUST RUN(to return in the right order later on): Convert labels of intereset level to integers
apt$interest_level<-as.integer(factor(apt$interest_level))

##############################################################################################
# CAN BE EDITTED (add columns)
# do a left join on apt here for the columns you would like to add to the analysis
# eg. apt = left_join(apt, df, by='aptID')

# [INSERT CODE HERE]

# CAN BE EDITTED (selection of columns)
# if you are trying to add more columns (let's call this variables ex)
# cols = c(cols, ex)
# which columns are going to be used on xgboost are selected in cols 

# Selecting x and y
cols = c('bathrooms', 'bedrooms', "building_id" ,
         'price','latitude','longitude',
         "manager_id","street_address", 
         "created.Day" ,"created.WDay" ,"created.Hour",
         "photoCount","featureCount", "sum_vec", 
         "created.Week", "created.Yday", "created.MWeek", 
         "created.Day")
pct = c('mgrHighPct', 'mgrMediumPct', 'mgrLowPct','bldgHighPct', 'bldgMediumPct', 'bldgLowPct')


# dummy variables for the features
cols = c(cols,colnames(apt)[32:52])
# 
cols = c(cols,colnames(apt)[55:61])
# price diff avg-cluster
cols = c(cols,colnames(apt)[66:67])
# sentiment analysis
cols = c(cols,colnames(apt)[76:77])

outcome = 'interest_level'
##########################################################################################

######################### xgboost implementation
# Filter columns
data <- apt[c(outcome,cols)]

# MUST RUN: transforming to a 0 to n vector for xgboost redabilitiy
y <- data$interest_level
y = y - 1

#create folds
kfolds<- 20
folds<-createFolds(y, k = kfolds, list = TRUE, returnTrain = FALSE)
fold <- as.numeric(unlist(folds[1]))

x_train <- data[-fold,-1] #Train set
x_val <- data[fold,-1] #Out of fold validation set

y_train<-y[-fold]
y_val<-y[fold]


#convert to xgbmatrix
dtrain = xgb.DMatrix(as.matrix(x_train), label=y_train)
dval = xgb.DMatrix(as.matrix(x_val), label=y_val)

# full dataset
x_train_full <- data[-1]
y_train_full <- y 
dtrain_full = xgb.DMatrix(as.matrix(x_train_full), label=y_train_full)


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

#train-mlogloss:0.463111	val-mlogloss:0.562324 

#perform training
gbdt = xgb.train(params = xgb_params,
                 data = dtrain,
                 nrounds = 500,
                 watchlist = list(train = dtrain, val=dval),
                 print_every_n = 25,
                 early_stopping_rounds=50)

#train-mlogloss:0.460958	val-mlogloss:0.545883 

gbdt2 =  xgb.train(params = xgb_params,
                   data = dtrain_full,
                   nrounds =475,
                   print_every_n = 25)

####################################
###Generate Feature Importance Plot
imp <- xgb.importance(names(data[-1]),model = gbdt)
head(imp) # returns a table with
xgb.ggplot.importance(imp)

###################### Make predictions on the test dataset
test <- readRDS('data/test-v11.rds')
test <- clean(test)
test_subset <- test[cols]

#convert xgbmatrix
dtest <- xgb.DMatrix(data.matrix(test_subset))


##################
#predict on test set
allpredictions =  (as.data.frame(matrix(predict(gbdt2,dtest), nrow=dim(test), byrow=TRUE)))


######################
##Generate Submission
allpredictions = cbind(allpredictions, test$listing_id)
names(allpredictions)<-c("high","medium","low","listing_id")
allpredictions=allpredictions[,c(1,2,3,4)]
write.csv(allpredictions,paste0(Sys.time(),"-XGBModel-Seed",seed,".csv"),row.names = FALSE)

####################################TESTING ZONE



