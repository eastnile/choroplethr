roxygen2::roxygenize()
devtools::load_all()
devtools::document()
devtools::check()

devtools::submit_cran()
devtools::use_cran_comments()

# R CMD check --as-cran ~/git/choroplethr_4.0.0.tar.gz
