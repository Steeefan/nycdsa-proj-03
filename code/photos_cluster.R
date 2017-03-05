library(dplyr)
library(tidyr)
# Adding photos_cluster features

photos_cluster = read.csv(file = 'data/photos_cluster.csv', stringsAsFactors = F)
photos_cluster = photos_cluster %>%
  group_by(listing_id, photos_cluster)%>%
  summarise(n=n())

photos_cluster$photos_cluster <- paste0("photo_cluster_",as.character(photos_cluster$photos_cluster))

photos_cluster = spread(photos_cluster, key = photos_cluster, value = n)
photos_cluster[is.na(photos_cluster)] <- 0

train = readRDS('data/train-v16.rds')
test = readRDS('data/test-v16.rds')

train = left_join(train, photos_cluster, by = 'listing_id')
test  = left_join(test, photos_cluster, by = 'listing_id')

saveRDS(train, 'data/train-v18.rds')
saveRDS(test, 'data/test-v18.rds')
