##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param train
##' @param outcome
fit_lrm_step <- function(train, outcome, n_predictors_max = ncol(train)-1) {

  formula_initial <- as.formula(glue("{outcome} ~ 1"))

  fit_initial <- glm(
    formula = formula_initial,
    data = train,
    family = binomial(link = 'logit')
  )

  predictors <- setdiff(names(train), outcome)
  predictors_collapsed <- glue_collapse(predictors, sep = ' + ')

  formula_maximal <- as.formula(glue("{outcome} ~ {predictors_collapsed}"))

  stepAIC(
    object = fit_initial,
    scope = list(upper = formula_maximal,
                 lower = formula_initial),
    direction = 'both',
    trace = 0,
    k = 1e-5,
    steps = n_predictors_max
  )

}


