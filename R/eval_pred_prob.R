##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param prediction
##' @param truth
eval_pred_prob <- function(prediction, truth){

  if(is.factor(truth)) truth <- as.numeric(truth) - 1

  eval <- rms::val.prob(p = prediction, y = truth, pl = FALSE)

  tibble(
    auc = eval['C (ROC)'],
    bri_scaled = eval['R2'],
    cal_slope = eval['Slope'],
    cal_intercept = eval['Intercept'],
    cal_pvalue = eval['U:p']
  )

}
