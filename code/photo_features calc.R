library(dplyr)

goldenRatio = 1.61803398875
photoFeat = readRDS('data/photo_features/photo_features.rds')
photoFeat$pxSize = photoFeat$height * photoFeat$width
photoFeat$pxRatio = ifelse(
  (photoFeat$width / photoFeat$height) < 1,
  1 / (photoFeat$width / photoFeat$height),
  photoFeat$width / photoFeat$height
)
photoFeat$diffGR = abs(1.61803398875 - photoFeat$pxRatio)

View(head(photoFeat))

photoFeatures = group_by(photoFeat, listing_id) %>%
  summarise(
    avgR = mean(r),
    avgG = mean(g),
    avgB = mean(b),
    avgBright = mean(brightness),

    avgWidth = mean(width),
    avgHeight = mean(height),
    avgPxRatio = mean(pxRatio),
    avgPxSize = mean(pxSize),
    avgDiffGR = mean(diffGR),

    medR = median(r),
    medG = median(g),
    medB = median(b),
    medBright = median(brightness),

    medWidth = median(width),
    medHeight = median(height),
    medPxRatio = median(pxRatio),
    medPxSize = median(pxSize),
    medDiffGR = median(diffGR)
  )

train = readRDS('data/v13/train-v13.rds')
train$listing_id = as.integer(as.character(train$listing_id))
train = left_join(train, photoFeatures, by='listing_id')

test = readRDS('data/v13/test-v13.rds')
test$listing_id = as.integer(as.character(test$listing_id))
test = left_join(test, photoFeatures, by='listing_id')

saveRDS(train, 'D:/train-v14.rds')
saveRDS(test, 'D:/test-v14.rds')
