

the_plan <-
  drake_plan(

    candidate_predictors = make_candidate_predictors(),
    outcome = 'los_gt30',

    phts = clean_phts(file_location = 'data/los_05_29_2019.sas7bdat',
                      candidate_predictors = candidate_predictors,
                      outcome = outcome),

    recipe = make_recipe(phts = phts,
                         outcome = outcome),

    internal_validation = make_internal_validation(
      phts = phts,
      recipe = recipe,
      outcome = outcome,
      n_predictors_max = 15
    ),

    fig_internal_validation = visualize_internal_validation(
      internal_validation,
      phts
    ),

    final_lrm = make_final_lrm(phts = phts,
                               recipe = recipe,
                               outcome = outcome,
                               n_predictors = 10),

    tbl_final_lrm = tabulate_final_lrm(
      final_lrm,
      phts,
      outcome
    ),

    doc = target(
      command = {
        rmarkdown::render(knitr_in("doc/predicting_length_of_stay.Rmd"))
        file_out("doc/predicting_length_of_stay.docx")
      }
    )

)
