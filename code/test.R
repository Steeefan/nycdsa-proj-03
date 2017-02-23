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
test = fromJSON(file = 'data/test.json')

# Create data frame from list
testDF = data.frame(
  aptID = names(test$bathrooms),
  bathrooms = as.numeric(unlist(test$bathrooms)),
  bedrooms = as.numeric(unlist(test$bedrooms)),
  building_id = unlist(test$building_id),
  created = as.character(unlist(test$created)),
  description = as.character(unlist(test$description)),
  display_address = as.character(unlist(test$display_address)),
  latitude = as.numeric(as.character(unlist(test$latitude))),
  longitude = as.numeric(as.character(unlist(test$longitude))),
  listing_id = as.character(unlist(test$listing_id)),
  manager_id = unlist(test$manager_id),
  price = as.numeric(unlist(test$price)),
  street_address = as.character(unlist(test$street_address))
)

# Casting
testDF$description = as.character(testDF$description)
testDF$display_address = as.character(testDF$display_address)
testDF$street_address = as.character(testDF$street_address)
testDF$listing_id = as.character(testDF$listing_id)
testDF$latitude = as.numeric(as.character(testDF$latitude))
testDF$longitude = as.numeric(as.character(testDF$longitude))

# Convert blank values in complete DF, 0 in building_id to NA
testDF = as.data.frame(apply(testDF, 2, function(x) gsub('^[:blank:]*$', NA, trimws(x))))
testDF$building_id = gsub('^[0]$', NA, testDF$building_id)

# Create separate features DF
features = data.frame(
  aptID = rep(names(test$features), sapply(test$features, length)),
  feature = tolower(unlist(test$features))
)

# Create separate photos DF
photos = data.frame(
  aptID = rep(names(test$photos), sapply(test$photos, length)),
  photo = tolower(unlist(test$photos))
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

testDF = testDF %>%
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

# testDF = left_join(
#   testDF,
#   photos %>% group_by(aptID) %>% summarise(photoCount = n()),
#   by='aptID'
# )
#
# testDF = left_join(
#   testDF,
#   features %>% group_by(aptID) %>% summarise(featureCount = n()),
#   by='aptID'
# )

# testDF$featureCount = ifelse(is.na(testDF$featureCount), 0, testDF$featureCount)
# testDF$photoCount = ifelse(is.na(testDF$photoCount), 0, testDF$photoCount)
#
# manager = left_join(
#   spread(
#     testDF %>%
#       group_by(manager_id, interest_level) %>%
#       summarise(
#         n = n()
#       ),
#
#     key=interest_level,
#     value=n
#   ),
#
#   testDF %>% group_by(manager_id) %>%
#     summarise(
#       n = n()
#     ),
#
#   by='manager_id'
# )
#
# manager = transmute(
#   manager,
#   mgrHighPct = ifelse(is.na(round(high / n, 4)), 0, round(high / n, 4)),
#   mgrMediumPct = ifelse(is.na(round(medium / n, 4)), 0, round(medium / n, 4)),
#   mgrLowPct = ifelse(is.na(round(low / n, 4)), 0, round(low / n, 4))
# )
#
#
# building = left_join(
#   spread(
#     testDF %>%
#       group_by(building_id, interest_level) %>%
#       summarise(
#         n = n()
#       ),
#
#     key=interest_level,
#     value=n
#   ),
#
#   testDF %>% group_by(building_id) %>%
#     summarise(
#       n = n()
#     ),
#   by='building_id'
# )
#
# building = transmute(
#   building,
#   bldgHighPct = ifelse(is.na(round(high / n, 4)), 0, round(high / n, 4)),
#   bldgMediumPct = ifelse(is.na(round(medium / n, 4)), 0, round(medium / n, 4)),
#   bldgLowPct = ifelse(is.na(round(low / n, 4)), 0, round(low / n, 4))
# )
#
# testDF = left_join(testDF, manager, by='manager_id')
# testDF = left_join(testDF, building, by='building_id')


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
apartments_features <- left_join(testDF, new_features, by = 'aptID')
apartments_features[, (ncol(testDF)+1):ncol(apartments_features)][is.na(apartments_features[, (ncol(testDF)+1):ncol(apartments_features)])] <- 0

#Joining columns with proportions of High, Medium, Low for BuildingID, ManagerID
# Load train dataset
training_set <- get(load('data/train-v5.rda'))
# include manager Pct information
cols_mgr <- c("mgrHighPct" ,"mgrMediumPct", "mgrLowPct" ,  "manager_id")
training_set_mgr_Pct <- training_set[,cols_mgr]
apartments_features <- left_join(apartments_features, unique(training_set_mgr_Pct), by="manager_id")
# include building Pct information
cols_bldg <- c("bldgHighPct", "bldgMediumPct" ,"bldgLowPct","building_id")
training_set_bldg_Pct <- training_set[,cols_bldg]
apartments_features <- left_join(apartments_features, unique(training_set_bldg_Pct), by="building_id")

#NA values on Pct (set it all equal)
apartments_features[(ncol(apartments_features)-5):ncol(apartments_features)][is.na(apartments_features[(ncol(apartments_features)-5):ncol(apartments_features)])] <- 1/3


# saveRDS(apartments_features, 'data/test-v6.rds')
# save(apartments_features, file='data/test-v6.rda')
# write.csv(apartments_features, 'D:/test-v5.csv', row.names=F)
#
# saveRDS(features, 'D:/features-test-v2.rds')
# write.csv(features, 'D:/features-test-v2.csv', row.names=F)
#
# saveRDS(photos, 'D:/photos-test-v2.rds')
# write.csv(photos, 'D:/photos-test-v2.csv', row.names=F)
