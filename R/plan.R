

the_plan <-
  drake_plan(

    doc = target(
      command = {
        rmarkdown::render(knitr_in("doc/predicting_length_of_stay.Rmd"))
        file_out("doc/predicting_length_of_stay.docx")
      }
    )

)
