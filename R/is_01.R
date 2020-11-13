##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param .x
is_01 <- function(x) {

  unix = unique(na.omit(x))
  c1 = all(unix %in% c(0, 1))
  c2 = all(c(0, 1) %in% unix)
  c1 & c2

}
