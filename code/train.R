library(rjson)
library(dplyr)
library(purrr)
library(knitr)
library(stringr)
library(VIM)
library(mice)
library(lubridate)
library(tidyr)

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
  latitude = as.numeric(as.character(unlist(train$latitude))),
  longitude = as.numeric(as.character(unlist(train$longitude))),
  listing_id = as.character(unlist(train$listing_id)),
  manager_id = unlist(train$manager_id),
  price = as.numeric(unlist(train$price)),
  street_address = as.character(unlist(train$street_address)),
  interest_level = unlist(train$interest_level)
)

# Casting
trainDF$description = as.character(trainDF$description)
trainDF$display_address = as.character(trainDF$display_address)
trainDF$street_address = as.character(trainDF$street_address)
trainDF$listing_id = as.character(trainDF$listing_id)
trainDF$latitude = as.numeric(as.character(trainDF$latitude))
trainDF$longitude = as.numeric(as.character(trainDF$longitude))

# Convert blank values in complete DF, 0 in building_id to NA
trainDF = as.data.frame(apply(trainDF, 2, function(x) gsub('^[:blank:]*$', NA, trimws(x))))
trainDF$building_id = gsub('^[0]$', NA, trainDF$building_id)
trainDF$interest_level = factor(trainDF$interest_level, levels(trainDF$interest_level)[c(1, 3, 2)])

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

manager = left_join(
  spread(
    trainDF %>%
      group_by(manager_id, interest_level) %>%
      summarise(
        n = n()
      ),

    key=interest_level,
    value=n
  ),

  trainDF %>% group_by(manager_id) %>%
    summarise(
      n = n()
    ),

  by='manager_id'
)

manager = transmute(
  manager,
  mgrHighPct = ifelse(is.na(round(high / n, 4)), 0, round(high / n, 4)),
  mgrMediumPct = ifelse(is.na(round(medium / n, 4)), 0, round(medium / n, 4)),
  mgrLowPct = ifelse(is.na(round(low / n, 4)), 0, round(low / n, 4))
)


building = left_join(
  spread(
    trainDF %>%
      group_by(building_id, interest_level) %>%
      summarise(
        n = n()
      ),

    key=interest_level,
    value=n
  ),

  trainDF %>% group_by(building_id) %>%
    summarise(
      n = n()
    ),
  by='building_id'
)

building = transmute(
  building,
  bldgHighPct = ifelse(is.na(round(high / n, 4)), 0, round(high / n, 4)),
  bldgMediumPct = ifelse(is.na(round(medium / n, 4)), 0, round(medium / n, 4)),
  bldgLowPct = ifelse(is.na(round(low / n, 4)), 0, round(low / n, 4))
)

trainDF = left_join(trainDF, manager, by='manager_id')
trainDF = left_join(trainDF, building, by='building_id')


#add dummy columns for different features
features$dining_room <- NA
features[str_detect(features$feature, 'dining room'),]$dining_room <- 1

features$pre_war <- NA
features[str_detect(features$feature, 'pre-war'),]$pre_war <- 1

features$laundry_in_building <- NA
features[str_detect(features$feature, 'laundry in building'),]$laundry_in_building <- 1

features$dishwasher <- NA
features[str_detect(features$feature, 'dishwasher'),]$dishwasher <- 1

features$hardwood_floors <- NA
features[str_detect(features$feature, 'hardwood floors'),]$hardwood_floors <- 1

features$dogs <- NA
features[str_detect(features$feature, 'dogs allowed'),]$dogs <- 1

features$cats <- NA
features[str_detect(features$feature, 'cats allowed'),]$cats <- 1

features$doorman <- NA
features[str_detect(features$feature, 'doorman'),]$doorman <- 1

features$elevator <- NA
features[str_detect(features$feature, 'elevator'),]$elevator <- 1

features$no_fee <- NA
features[str_detect(features$feature, 'no fee'),]$no_fee <- 1

features$fitness_center <- NA
features[str_detect(features$feature, 'fitness center'),]$fitness_center <- 1

features$laundry_in_unit <- NA
features[str_detect(features$feature, 'laundry in unit'),]$laundry_in_unit <- 1

features$loft <- NA
features[str_detect(features$feature, 'loft'),]$loft <- 1

features$fireplace <- NA
features[str_detect(features$feature, 'fireplace'),]$fireplace <- 1

features$roof_deck <- NA
features[str_detect(features$feature, 'roof deck'),]$roof_deck <- 1

features$outdoor_space <- NA
features[str_detect(features$feature, 'outdoor space'),]$outdoor_space <- 1

features$high_speed_internet <- NA
features[str_detect(features$feature, 'high speed internet'),]$high_speed_internet <- 1

features$balcony <- NA
features[str_detect(features$feature, 'balcony'),]$balcony <- 1

features$swimming_pool <- NA
features[str_detect(features$feature, 'swimming pool'),]$swimming_pool <- 1

features$garden_or_patio <- NA
features[str_detect(features$feature, 'garden/patio'),]$garden_or_patio <- 1

features$wheelchair_access <- NA
features[str_detect(features$feature, 'wheelchair access'),]$wheelchair_access <- 1

features$common_outdoor_space <- NA
features[str_detect(features$feature, 'common outdoor space'),]$common_outdoor_space <- 1

features[is.na(features)] <- 0

cols = colnames(features)[3:length(colnames(features))]
new_features = aggregate(features[cols], by=features['aptID'], FUN=max)
apartments_features <- left_join(trainDF, new_features, by = 'aptID')
apartments_features[, (ncol(trainDF)+1):ncol(apartments_features)][is.na(apartments_features[, (ncol(trainDF)+1):ncol(apartments_features)])] <- 0

# saveRDS(apartments_features, 'D:/train-v5.rds')
# save(apartments_features, file='D:/train-v5.rda')
# write.csv(apartments_features, 'D:/train-v5.csv', row.names=F)
#
# saveRDS(features, 'D:/features-train-v2.rds')
# write.csv(features, 'D:/features-train-v2.csv', row.names=F)
#
# saveRDS(photos, 'D:/photos-train-v2.rds')
# write.csv(photos, 'D:/photos-train-v2.csv', row.names=F)
