##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data
make_recipe <- function(phts, outcome) {

  recipe_formula <- as.formula(glue("{outcome} ~ ."))

  naming_fun <- function(var, lvl, ordinal = FALSE, sep = '..'){
    dummy_names(var, lvl, ordinal, sep)
  }

  recipe(x = phts$data, formula = recipe_formula) %>%
    step_knnimpute(all_predictors()) %>%
    step_corr(all_numeric(), -matches("^age|^cpbypass")) %>%
    step_nzv(all_predictors(), -matches("^age|^cpbypass")) %>%
    step_novel(all_nominal(), -all_outcomes())
  # step_dummy(all_nominal(), -all_outcomes(), naming = naming_fun)

}
