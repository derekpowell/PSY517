## Functions for lecture 3

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

grid_approx_rbeta <- function(n, shape1, shape2, grid_size=1000){
  p_grid <-  seq(0, 1, length.out=grid_size)
  prob <- dbeta(p_grid, shape1, shape2)
  prob <- prob/sum(prob)
  
  sample(p_grid, prob=prob, size=n, replace=T)
}

gghist <- function(x, ...){
  ggplot() + 
    geom_histogram(aes(x=x), ...) +
    theme_bw()
}

standardize <- function(x){
 (x-mean(x))/sd(x)
}

unstandardize <- function(x, x_orig){
  (x*sd(x_orig))+mean(x_orig)
}
