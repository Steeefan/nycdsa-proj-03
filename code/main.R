library(rjson)
library(dplyr)
library(purrr)
library(knitr)
library(stringr)
library(VIM)
library(mice)

# Read from JSON
train = fromJSON(file = 'data/train.json')

# Create data frame from list
trainDF = data.frame(
  aptID = names(train$bathrooms),
  bathrooms = unlist(train$bathrooms),
  bedrooms = unlist(train$bedrooms),
  building_id = unlist(train$building_id),
  created = unlist(train$created),
  description = unlist(train$description),
  display_address = unlist(train$display_address),
  latitude = unlist(train$latitude),
  listing_id = unlist(train$listing_id),
  longitude = unlist(train$longitude),
  manager_id = unlist(train$manager_id),
  price = unlist(train$price),
  street_address = unlist(train$street_address),
  interest_level = unlist(train$interest_level)
)

# Convert blank values in complete DF, 0 in building_id to NA
trainDF = as.data.frame(apply(trainDF, 2, function(x) gsub('^[:blank:]*$', NA, trimws(x))))
trainDF$building_id = gsub('^[0]$', NA, trainDF$building_id)

# Create separate features DF
features = data.frame(
  aptID = rep(names(train$features), sapply(train$features, length)),
  feature = tolower(unlist(train$features))
)

# Create separate photos DF
photos = data.frame(
  aptID = rep(names(train$photos), sapply(train$photos, length)),
  photo = tolower(unlist(train$photos))
)

aggr(trainDF)
aggr(trainDF, plot=F)
md.pattern(trainDF)

saveRDS(trainDF, 'data/train-v2.rds')
write.csv(trainDF, 'data/train-v2.csv', row.names=F)

saveRDS(features, 'data/features-v2.rds')
write.csv(features, 'data/features-v2.csv', row.names=F)

saveRDS(photos, 'data/photos-v2.rds')
write.csv(photos, 'data/photos-v2.csv', row.names=F)
