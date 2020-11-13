##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param file_location
clean_phts <- function(file_location = "data/los_05_29_2019.sas7bdat",
                       candidate_predictors,
                       outcome) {

  data_in <- read_sas("data/los_05_29_2019.sas7bdat") %>%
    clean_names() %>%
    select(all_of(c(outcome, candidate_predictors)))

  labels <- map(data_in, ~attr(.x, 'label'))

  labels$e_gfr_group <- 'Estimated Glomerular Filtration Rate, mL/min/1.73m2'
  labels$age_txpl <- "Recipient Age, Years"
  labels$cpbypass <- "Cardiopulmonary Bypass Time, minutes"

  data_out <- data_in %>%
    mutate(
      across(where(is.character), set_blank_to_na),
      across(where(is.character), factor),
      across(
        where(~is_01(.x)),
        .fns = ~ factor(.x, levels = c(0, 1), labels = c('No', 'Yes'))
      ),
      txplmec = fct_relevel(txplmec, 'Neither VAD nor ECMO', 'VAD'),
      e_gfr_group = fct_recode(
        e_gfr_group,
        '< 60'     = '1. <60',
        '60 to 90' = '2. 60-90',
        '> 90'     = '3. >90'
      ),
      e_gfr_group = fct_relevel(e_gfr_group, '> 90', '60 to 90')
    )

  list(
    labels = labels,
    data = data_out
  )

}







#across(where(is.character), ~factor(clean_chr(.x))),
#across(where(is.factor), fct_explicit_na, na_level = 'missing'),
# across(
#   .cols = where(~any(is.na(.x))),
#   .fns = list(missing = ~as.numeric(is.na(.x))),
#   .names = "{.col}..{.fn}"
# )
