multiloss <- function(predicted, actual){
  predicted <- apply(predicted, c(1,2), function(x) max(min(x, 1-10^(-15)), 10^(-15)))
  score <- -sum(actual*log(predicted))/nrow(predicted)
  return(score)
}
