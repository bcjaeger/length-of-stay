##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param final_lrm
##' @param phts
tabulate_final_lrm <- function(final_lrm, phts, outcome) {

  df_scaled <- final_lrm$data %>%
    mutate(across(where(is.numeric), ~.x / sd(.x)))

  sds <- final_lrm$data %>%
    select(all_of(all.vars(final_lrm$formula)[-1])) %>%
    select(where(~!is.factor(.x))) %>%
    map(sd) %>%
    map(table_value)

  phts$labels$age_txpl %<>%
    str_replace('Years',
                paste('per', sds$age_txpl, 'years'))

  phts$labels$cpbypass %<>%
    str_replace('minutes',
                paste('per', sds$cpbypass, 'minutes'))

  final_lrm_for_table <- glm(
    formula = final_lrm$formula,
    data = df_scaled,
    family = final_lrm$family
  )

  lrm_eqn <- tidy(final_lrm) %>%
    add_ref_rows(data = final_lrm$data) %>%
    filter(estimate != 0) %>%
    mutate(variable = recode(variable, !!!phts$labels),
           estimate = format(round(estimate, 12), nsmall = 12),
           eqn_term = case_when(
             variable == '(Intercept)' ~ glue("{estimate}"),
             is.na(level) ~ glue("{variable} * {estimate}"),
             TRUE ~ glue("({variable} = {level}) * {estimate}")
           )) %>%
    pull(eqn_term) %>%
    glue_collapse(sep = ' + ')

  final_lrm_tidy <- final_lrm_for_table %>%
    tidy(conf.int = TRUE, exponentiate = TRUE) %>%
    add_ref_rows(data = df_scaled, ref_value = 1) %>%
    filter(term != '(Intercept)')

  uni_odds_ratios <- all.vars(final_lrm$formula)[-1] %>%
    set_names() %>%
    map_dfr(make_or_unadj,
            outcome = outcome,
            data = df_scaled,
            ref_value = 1) %>%
    rename_with(.cols = c(estimate, starts_with('conf')),
                .fn = ~paste(.x, 'uni', sep='_'))

  tbl_data_raw <- final_lrm_tidy %>%
    select(variable, level, term, estimate, starts_with('conf')) %>%
    rename_with(.cols = c(estimate, starts_with('conf')),
                .fn = ~paste(.x, 'multi', sep='_')) %>%
    left_join(uni_odds_ratios) %>%
    arrange(variable) %>%
    mutate(variable_label = recode(variable, !!!phts$labels),
           tbv_uni = table_glue(
             "{estimate_uni} ({conf.low_uni}, {conf.high_uni})"
           ),
           tbv_multi = table_glue(
             "{estimate_multi} ({conf.low_multi}, {conf.high_multi})"
           ),
           .before = 1)

  tbl_data_clean <- tbl_data_raw %>%
    mutate(across(where(is.factor), as.character)) %>%
    transmute(
      level = if_else(
        is.na(level),
        variable_label,
        level
      ),
      variable_label = if_else(
        variable_label == level,
        NA_character_,
        variable_label
      ),
      across(starts_with('tbv'),
             str_replace,
             pattern = '1.00, 1.00',
             replacement = 'reference'),
      across(starts_with('tbv'),
             str_replace,
             pattern = '1.00',
             replacement = '1')
    )

  tbl_inline <- tbl_data_raw %>%
    as_inline(tbl_variables = c('variable', 'level'),
              tbl_values = c('tbv_uni', 'tbv_multi'))

  tbl_grouped_data <- tbl_data_clean %>%
    arrange(!is.na(variable_label)) %>%
    as_grouped_data(groups = 'variable_label') %>%
    remove_empty('rows')

  padding_index <- which(
    is.na(tbl_grouped_data$variable_label) &
      tbl_grouped_data$level %in% tbl_data_raw$level
  )

  continuous_variable_index <- which(
    is.na(tbl_grouped_data$variable_label) &
      !(tbl_grouped_data$level %in% tbl_data_raw$level)
  )

  tbl_flex <- tbl_grouped_data %>%
    as_flextable(hide_grouplabel = TRUE)  %>%
    add_header_row(
      values = c('Variable', 'Odds ratio (95% CI)'),
      colwidths = c(1, 2)
    ) %>%
    theme_box() %>%
    align(align = 'center', part = 'all') %>%
    align(j = 1, align = 'left', part = 'body') %>%
    padding(i = padding_index,
            j = 1,
            padding.left = 15) %>%
    width(j = 1, width = 2) %>%
    width(j = c(2, 3), width = 1.5) %>%
    set_header_labels(
      level = 'Variable',
      tbv_uni = 'Unadjusted',
      tbv_multi = 'Adjusted'
    ) %>%
    merge_v(j = 1, part = 'header') %>%
    footnote(
      i = 1,
      j = 1,
      part = 'header',
      ref_symbols = '*',
      value = as_paragraph(
        "Predictor variables included in the table were selected",
        " using stepwise logistic regression. The number of",
        " predictor variables in the final model was optimized",
        " using internal validation of discrimination and calibration."
      )
    ) %>%
    footnote(
      i = continuous_variable_index,
      j = 1,
      ref_symbols = "†",
      value = as_paragraph(
        "Odds ratios for continuous variables correspond to",
        " a one standard deviation change in the variable."
      )
    ) %>%
    footnote(
      i = 2,
      j = 3,
      ref_symbols = "‡",
      part = 'header',
      value = as_paragraph(
        'Adjusted odds ratios account for all variables',
        ' listed in the table, simultaneously.'
      )
    ) %>%
    font(fontname = 'Times New Roman', part = 'all') %>%
    fontsize(size = 12, part = 'all')

  list(
    table = tbl_flex,
    inline = tbl_inline
  )


}
