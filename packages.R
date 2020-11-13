
## library() calls go here
library(conflicted)
library(dotenv)
library(drake)
# data management
library(haven)
library(janitor)
library(magrittr)
# data analysis
library(tidyverse)
library(tidymodels)
library(broom)
library(naniar)
library(MASS)
# reporting
library(tibbleOne)
library(table.glue)
library(officer)
library(glue)
library(flextable)
library(ggforce)

conflicted::conflict_prefer("roc",       "pROC")
conflicted::conflict_prefer("filter",    "dplyr")
conflicted::conflict_prefer("slice",     "dplyr")
conflicted::conflict_prefer("select",     "dplyr")
conflicted::conflict_prefer('summarise', 'dplyr')
conflicted::conflict_prefer('summarize', 'dplyr')
conflicted::conflict_prefer("gather",    "tidyr")
conflicted::conflict_prefer("set_names", "purrr")
