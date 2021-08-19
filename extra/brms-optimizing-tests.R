library(tidyverse)
library(rstan)
library(brms)

### -----

proc_samples <- function(fit, sample_fit){
  samples <- fit$theta_tilde
  col_names <- colnames(samples)
  output <- map(colnames(samples), ~samples[,.x])
  names(output) <- colnames(samples)
  output$lp__ <- fit$log_p
  output$return_code <- 0
  attributes(output) <- attributes(sample_fit@sim$samples[[1]])
  
  return(output)
}

obrm <- function(formula, data, family=gaussian(), prior=NULL, iter=2000, warmup=0, ...){
  
  chains <- 1
  
  stancode <- brms::make_stancode(formula, data, ...)
  standata <- brms::make_standata(formula, data, ...)
  
  print("Compiling Stan model ...")
  stanmodel <- stan_model(model_code=stancode)
  
  print("Fitting ...")
  
  fit_optim <- optimizing(stanmodel, data = standata, hessian=TRUE, draws=iter, importance_resampling=TRUE)
  fit_small <- suppressWarnings(
    sampling(stanmodel, data = standata, chains = 1, cores=1, iter = 10, show_messages=FALSE,refresh=0)
  )
  
  processed_samples <- proc_samples(fit_optim, fit_small)
  
  stanfit <- rstan:::new_empty_stanfit(
    stanmodel = stanmodel, 
    model_pars = names(fit_optim$par),
    stan_args = list(list(chain_id=1, iter=iter, thin=1, seed=123, warmup=warmup, init="random",algorithm="optimizing",method="bfgs")),
    sim = list(
      samples = list(processed_samples),
      chains=1, iter=iter, thin=1, seed=123, warmup=warmup,
      pars_oi=fit_small@sim$pars_oi, dims_oi=fit_small@sim$dims_oi, fnames_oi=fit_small@sim$fnames_oi, 
      n_save=rep(iter,1), warmup2 = rep(warmup, 1)
    ),
    inits = list(fit_optim$par),
    par_dims = fit_small@par_dims,
    mode = 0L
  )
  
  output <- brm(formula, data, family=family, 
                       prior=prior, chains=chains, iter=iter, warmup=warmup, empty=TRUE)
  
  output$fit <- stanfit
  output <- brms:::rename_pars(output) # do the renaming
  return(output)
  
}

## -----

## Some very basic tests
x <- rnorm(40, 10, 5)
y <- x*.5 + rnorm(40,0,1)

d <- tibble(x=x,y=y)

fit_test <- obrm(y~x, d, iter=2000)


loo(fit_test)
predict(fit_test)
fitted(fit_test)
conditional_effects(fit_test)
