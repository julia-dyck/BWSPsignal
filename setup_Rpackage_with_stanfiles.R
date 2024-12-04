# instructions:
# https://cran.r-project.org/web/packages/rstantools/vignettes/minimal-rstan-package.html


setwd("C:/Users/jdyck/github_office_laptop") # office laptop

# install.packages("rstantools")
library("rstantools")

#rstan_create_package(path = 'BWSPsignal')
getwd()


# update documentation with roxygen2
try(roxygen2::roxygenize(load_code = rstantools_load_code), silent = T)
roxygen2::roxygenize()


# build the package with recompiling the stanfiles
install.packages("BWSPsignal", repos = NULL, type = "source")

# build the package quickly without recompiling the stanfiles
devtools::install(quick = F)
