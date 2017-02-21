
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

new_features <- as.data.frame(apartments[,1])

test <- aggregate(dining_room ~ aptID, data = features, max)

cols = colnames(features)[3:length(colnames(features))]

new_features = aggregate(features[cols],by=features['aptID'],FUN=max)

apartments_features <- left_join(apartments, new_features, by = 'aptID')

apartments_features[, 22:54][is.na(apartments_features[, 22:54])] <- 0