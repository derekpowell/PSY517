# modified from: https://coolbutuseless.github.io/2019/04/26/reverse-engineer-the-ggplot-call-from-a-ggplot-object/

library(ggplot2)
library(dplyr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# reverse_mapping ->  "aes(x = ..., y = ...)"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
reverse_mapping <- function(mapping) {
  aes_args <- paste(names(mapping), stringr::str_sub(as.character(mapping), start=2), sep = "=", collapse = ", ")
  aes_text <- glue::glue("aes({aes_args})")
  aes_text
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# reverse aesthetic params ->  "size = 3"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
reverse_aes_params <- function(aes_params) {
  if (length(aes_params) == 0) {
    NULL
  } else {
    paste(names(aes_params), unname(aes_params), sep = "=", collapse = ", ")
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# reverse_layer -> "geom_point(aes(mpg, wt), size = 3)"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

check_inherited_aes <- function(layers){
  map_lgl(layers, ~.x$inherit.aes)
}

reverse_layer <- function(layer) {
  geom_name <- ggplot2:::snakeize(class(layer$geom)[1])
  
  aes_text        <- reverse_mapping(layer$mapping)
  if (aes_text=="aes()"){
    aes_text=""
  }
  aes_params_text <- reverse_aes_params(layer$aes_params)
  geom_args <- paste(c(aes_text, aes_params_text), collapse = ", ")
  
  
  glue::glue("{geom_name}({geom_args})")
}


reverse_plot_aes <- function(p){
  reverse_mapping(p$mapping)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Reverse plot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
reverse_plot <- function(p) {
  layers <- p$layers %>% map_chr(reverse_layer)
  if (check_inherited_aes(p$layers)){
    first_line <- paste0("ggplot(data, ", reverse_plot_aes(p), ")" )
    plot_text <- paste(c(first_line, layers), collapse = "+\n")
  } else {
    plot_text <- paste(c("ggplot(data)", layers), collapse = "+\n")
  }
  # return(plot_text)
  styler::style_text(plot_text)
}
