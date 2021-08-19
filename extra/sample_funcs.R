samples_to_density <- function(samples, density=TRUE, binsize=500){
  func <- function(x){
    eps <- (max(samples) - min(samples))/(length(samples)/binsize)
    map_dbl(x, ~mean(between(samples, .x-eps, .x+eps)))
  }
  
  return(func)
}


samples_to_cumulative <- function(samples){
  func <- function(x, lower.tail=TRUE){
    cum_prob <- map_dbl(x, ~mean(samples < .x))
    
    if (lower.tail){
      return(cum_prob)
    } else {
      return(1-cum_prob)
    }
  }
  
  return(func)
}


# dbeta_test <- samples_to_density(rbeta(1e5, 3, 3))
# pbeta_test <- samples_to_cumulative(rbeta(1e5, 3, 3))
# 
# dbeta_test(seq(.1,.9, .1))
# dbeta(seq(.1,.9, .1), 3, 3)

