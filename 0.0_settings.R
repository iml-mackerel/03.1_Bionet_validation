##### my packages ################################################################################
## CRAN
cran.packages <- c("plyr",
                   "future",
                   "future.apply",
                   "tidyverse",
                   "data.table",
                   "shiny",
                   "knitr",
                   "kableExtra"
                   )

install.this <- cran.packages[!(cran.packages %in% utils::installed.packages()[,"Package"])]
if(length(install.this)>=1) install.packages(install.this)
dummy <- lapply(cran.packages, require, character.only = TRUE)

## github
git.packages <- c('catchR')
install.this <- git.packages[!(git.packages %in% utils::installed.packages()[,"Package"])]
if('catchR' %in% install.this)  devtools::install_github("iml-assess/catchR")
dummy <- lapply(git.packages, require, character.only = TRUE)

##### source R directory  ############################################################################
invisible(sapply(list.files(pattern="[.]R$", path="R/", full.names=TRUE), source))

##### my ggplot theme ################################################################################
theme_set(theme_mackerel())             # theme_mackerel from catchR
update_geom_defaults("line", list(size = 1))  # no idea why somethimes I get fat lines otherwise

