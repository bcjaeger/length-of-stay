##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param phts
##' @param recipe
make_internal_validation <- function(phts,
                                     recipe,
                                     outcome,
                                     n_predictors_max) {

  vfold_cv(data = phts$data, v = 5, repeats = 3) %>%
    mutate(
      pre_proc = map(splits, ~prep(recipe, training = training(.x))),
      train = map(pre_proc, juice),
      test = map2(pre_proc, splits, ~bake(.x, new_data = testing(.y))),
      lrm_data = map2(
        .x = train,
        .y = test,
        .f = ~ .x %>%
          fit_lrm_step(outcome, n_predictors_max = n_predictors_max) %>%
          predict_lrm_step(test = .y, outcome = outcome) %>%
          mutate(eval = map(.x = prediction,
                            .f = eval_pred_prob,
                            truth = .y$los_gt30)) %>%
          unnest_wider(eval)
      )
    ) %>%
    select(id, id2, lrm_data)

}

# output <- internal_validation %>%
#   mutate(
#     lrm_data = map2(
#       .x = train,
#       .y = test,
#       .f = ~ fit_lrm_step(.x, outcome, n_predictors_max = 20) %>%
#           predict_lrm_step(test = .y, outcome = outcome) %>%
#           mutate(eval = map(.x = prediction,
#                             .f = eval_pred_prob,
#                             truth = .y$los_gt30)) %>%
#           unnest_wider(eval)
#     )
#   )
#
# internal_validation %>%
#   select(id, id2, lrm_data) %>%
#   unnest(cols = lrm_data) %>%
#   group_by(n_predictors) %>%
#   summarize(
#     across(auc:cal_pvalue, mean)
#   ) %>%
#   ggplot(aes(x = n_predictors, y = bri_scaled)) +
#   geom_point()
