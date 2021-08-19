## helpers for week 3 code

center <- function(x){
  scale(x, scale=FALSE)
}

add_epred_interval <- function(data, model, values = c(".epred", ".lower", ".upper"), ...){
  # only works/tested for brms models
  preds <- fitted(model, data, ...) %>% 
    as_tibble() %>% 
    select(-Est.Error)
  
  colnames(preds) <- values
  
  bind_cols(data, preds)
}

add_pred_interval <- function(data, model, values = c(".pred", ".lower", ".upper"), ...){
  # only works/tested for brms models
  preds <- predict(model, data, ...) %>% 
    as_tibble() %>% 
    select(-Est.Error)
  
  colnames(preds) <- values
  
  bind_cols(data, preds)
}

gghist <- function(x, ...){
  ggplot() + 
    geom_histogram(aes(x=x), ...)
}


standardize <- function(x){
  (x-mean(x))/sd(x)
}

unstandardize <- function(x, x_orig){
  (x*sd(x_orig))+mean(x_orig)
}

