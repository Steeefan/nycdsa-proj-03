clean <- function(apartments){
  #fix fomatting
  apartments$price = as.numeric(as.character(apartments$price))
  apartments$bathrooms = as.numeric(as.character(apartments$bathrooms))
  apartments$bedrooms = as.numeric(as.character(apartments$bedrooms))
  apartments$latitude = as.numeric(as.character(apartments$latitude))
  apartments$longitude = as.numeric(as.character(apartments$longitude))
  
  ##Count of words in description
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
  
  # by cluster: price, bed_price, room_price
  temp <- apartments %>%
    group_by(cluster)%>%
    summarize(avg_price = mean(price), avg_bed_price  = mean(bed_price), avg_room_price = mean(room_price))
  
  # join avg_cluster stats to the apt dataset  
  apartments <- left_join(apartments, temp, by='cluster')
  
  # compute difference between listing and average of cluster
  apartments$price_diff <- apartments$price - apartments$avg_price
  apartments$bed_price_diff <- apartments$bed_price - apartments$avg_bed_price
  apartments$room_price_diff <- apartments$room_price - apartments$avg_room_price
  
  return(apartments)
}

apt = readRDS('data/train-v8.rds')
train_v9 <- clean(apt)
saveRDS(train_v9, "data/train-v9.rds")

apt2 = readRDS('data/test-v8.rds')
test_v9 <- clean(apt2)
saveRDS(test_v9, "data/test-v9.rds")
