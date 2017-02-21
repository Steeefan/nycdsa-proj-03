library(rjson)
library(dplyr)
library(purrr)
library(knitr)
library(stringr)
library(VIM)
library(mice)
library(lubridate)

# Read from JSON
train = fromJSON(file = 'data/train.json')

# Create data frame from list
trainDF = data.frame(
  aptID = names(train$bathrooms),
  bathrooms = as.numeric(unlist(train$bathrooms)),
  bedrooms = as.numeric(unlist(train$bedrooms)),
  building_id = unlist(train$building_id),
  created = as.character(unlist(train$created)),
  description = as.character(unlist(train$description)),
  display_address = as.character(unlist(train$display_address)),
  latitude = as.numeric(unlist(train$latitude)),
  longitude = as.numeric(unlist(train$longitude)),
  listing_id = as.character(unlist(train$listing_id)),
  manager_id = unlist(train$manager_id),
  price = as.numeric(unlist(train$price)),
  street_address = as.character(unlist(train$street_address)),
  interest_level = unlist(train$interest_level)
)

trainDF$description = as.character(trainDF$description)
trainDF$display_address = as.character(trainDF$display_address)
trainDF$street_address = as.character(trainDF$street_address)
trainDF$listing_id = as.character(trainDF$listing_id)

# Convert blank values in complete DF, 0 in building_id to NA
trainDF = as.data.frame(apply(trainDF, 2, function(x) gsub('^[:blank:]*$', NA, trimws(x))))
trainDF$building_id = gsub('^[0]$', NA, trainDF$building_id)
trainDF$interest_level = factor(trainDF$interest_level, levels(trainDF$interest_level)[c(2, 3, 1)])

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

tz = 'America/New_York'
tsFormat = '%Y-%m-%d %H:%M'
monthweeks <- function(x) {
  UseMethod("monthweeks")
}
monthweeks.Date <- function(x) {
  ceiling(as.numeric(format(x, "%d")) / 7)
}
monthweeks.POSIXlt <- function(x) {
  ceiling(as.numeric(format(x, "%d")) / 7)
}
monthweeks.character <- function(x) {
  ceiling(as.numeric(format(as.Date(x), "%d")) / 7)
}

trainDF = trainDF %>%
  mutate(
    created = as.POSIXct(strptime(created, tz = tz, format = tsFormat)),

    created.Day = day(created),
    created.Month = month(created),
    created.Year = year(created),

    created.Date = make_date(created.Year, created.Month, created.Day),

    created.WDay = wday(created),
    created.WDayLbl = substr(wday(created, label = T), 1, 3),  # week starts on Sun in the US!
    created.Week = week(created),

    created.Hour = hour(created),

    created.Yday = yday(created),
    created.MWeek = monthweeks(created.Date)
  )


trainDF = left_join(
  trainDF,
  photos %>% group_by(aptID) %>% summarise(photoCount = n()),
  by='aptID'
)

trainDF = left_join(
  trainDF,
  features %>% group_by(aptID) %>% summarise(featureCount = n()),
  by='aptID'
)

trainDF$featureCount = ifelse(is.na(trainDF$featureCount), 0, trainDF$featureCount)
trainDF$photoCount = ifelse(is.na(trainDF$photoCount), 0, trainDF$photoCount)


aggr(trainDF)
aggr(trainDF, plot=F)
md.pattern(trainDF)

saveRDS(trainDF, 'D:/train-v3.rds')
write.csv(trainDF, 'D:/train-v3.csv', row.names=F)

saveRDS(features, 'D:/features-v2.rds')
write.csv(features, 'D:/features-v2.csv', row.names=F)

saveRDS(photos, 'D:/photos-v2.rds')
write.csv(photos, 'D:/photos-v2.csv', row.names=F)
