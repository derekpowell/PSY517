## helpers for PSY517 lectures and exercises

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


standardize <- function (x, ...){
  ## Modified from rethinking package
  x <- scale(x, ...)
  z <- as.numeric(x)
  
  attr(z, "scaled:center") <- attr(x, "scaled:center")
  attr(z, "scaled:scale") <- attr(x, "scaled:scale")
  return(z)
}


unstandardize <- function (x){
  ## From rethinking package
  scale <- attr(x, "scaled:scale")
  center <- attr(x, "scaled:center")
  z <- x * scale + center
  return(as.numeric(z))
}


center <- function(x){
  standardize(x, scale=FALSE)
}


uncenter <- function(x){
  center <- attr(x, "scaled:center")
  z <- x + center
  return(as.numeric(z))
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


pp_check_coverage <- function(data, fit, y, prob = .95){
  y <- enquo(y)
  probs = c((1-prob)/2, 1-(1-prob)/2)
  
  data %>% 
    add_predicted_draws(fit) %>% 
    mutate(.prediction = .prediction) %>% 
    summarize(
      .pred = mean(.prediction),
      .lower = quantile(.prediction, probs[1]),
      .upper = quantile(.prediction, probs[2])
    ) %>% 
    ungroup() %>%
    mutate(accurate = pmap_lgl(list(!! y, .lower, .upper), ~between(..1, ..2, ..3) )) %>%
    summarize(acc = mean(accurate)) %>% 
    .$acc
}