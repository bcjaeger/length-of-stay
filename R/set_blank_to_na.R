##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

set_blank_to_na <- function(x){
  x[trimws(x)%in%c("", " ", ".")] = NA_character_
  x
}
