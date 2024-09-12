library(tidyverse)
library(magrittr)
library(purrr)

# functional programming 
# purrr::negate vs base Negate()
`%!in%` <- purrr::negate(`%in%`)

# test if all elements from a vector of strings are unique in R
is_unique <- function(vector){
  return(!any(duplicated(vector)))
}