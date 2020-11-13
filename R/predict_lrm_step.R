##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param fit
##' @param test
##' @param outcome
predict_lrm_step <- function(fit, test, outcome){

  predictors <- all.vars(fit$formula)[-1]

  output <- tibble(n_predictors = seq_along(predictors),
                   prediction = list(NULL))

  for(i in seq_along(predictors)){

    .predictors <- glue_collapse(predictors[1:i], sep = ' + ')
    .formula <- glue("{outcome} ~ {.predictors}")
    .fit <- glm(formula = .formula,
                data = fit$data,
                family = fit$family)

    output$prediction[[i]] <- predict(.fit, newdata = test, type = 'response')


  }

  output

}
