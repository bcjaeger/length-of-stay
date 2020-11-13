##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param internal_validation
##' @param phts
visualize_internal_validation <- function(internal_validation, phts) {

  model_data <- internal_validation %>%
    unnest(lrm_data) %>%
    group_by(n_predictors) %>%
    summarize(
      across(
        .cols = c(auc, cal_pvalue),
        .fns = mean
      )
    ) %>%
    pivot_longer(cols = c(auc, cal_pvalue))

  model_data_recoded <- model_data %>%
    mutate(
      name = recode(
        name,
        auc = 'C-statistic',
        cal_pvalue = 'P-value for unreliable calibration'
      ),
      label = table_glue("{name} = {value}")
    )

  n_predictors_best <- model_data %>%
    filter(name == 'cal_pvalue') %>%
    arrange(desc(value)) %>%
    slice(1) %>%
    pull(n_predictors)

  fig <- ggplot(model_data_recoded) +
    aes(x = n_predictors, y = value, fill = name) +
    geom_vline(xintercept = n_predictors_best,
               color = 'grey', linetype = 2) +
    geom_mark_circle(
      aes(filter = n_predictors == n_predictors_best,
          label = label),
      show.legend = FALSE,
      expand = unit(4, 'mm')
    ) +
    geom_point(size = 3.5, shape = 21, col = 'black') +
    theme_bw() +
    labs(fill = 'Model performance metrics',
         x = 'Number of predictors included in the model',
         y = 'Model performance (higher is better)') +
    theme(panel.grid = element_blank(),
          legend.position = 'top',
          legend.justification = 'left',
          legend.direction = 'vertical') +
    scale_fill_manual(values = c('purple', 'orange')) +
    scale_x_continuous(breaks = c(5, n_predictors_best, 10, 15, 20, 25, 30))

  inline_values <- model_data %>%
    mutate(n_predictors = paste0("preds_", n_predictors)) %>%
    as_inline(tbl_variables = c('n_predictors', 'name'),
              tbl_values = 'value')

  list(
    fig = fig,
    inline = inline_values,
    n_predictors_best = n_predictors_best
  )

}
