##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param phts
##' @param recipe
##' @param outcome
##' @param n_predictors
make_final_lrm <- function(phts,
                           recipe,
                           outcome,
                           n_predictors) {

  recipe %>%
    prep(training = phts$data) %>%
    juice() %>%
    fit_lrm_step(outcome = outcome,
                 n_predictors_max = n_predictors)

}
