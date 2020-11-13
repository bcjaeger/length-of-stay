##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param variable
##' @param outcome
##' @param ref_value
##' @param data
##' @param conf.int
##' @param exponentiate
make_or_unadj <- function(variable,
                          outcome,
                          ref_value,
                          data,
                          conf.int = TRUE,
                          exponentiate = TRUE){

  model_formula <- as.formula(glue("{outcome} ~ {variable}"))

  glm(formula = model_formula,
      data = data,
      family = binomial(link = 'logit')) %>%
    tidy(conf.int = conf.int, exponentiate = exponentiate) %>%
    add_ref_rows(data = data, ref_value = ref_value) %>%
    select(term, estimate, conf.low, conf.high) %>%
    filter(term != '(Intercept)')

}
