.onAttach <- function(...) {
  if (!interactive()) return()
  
  tips <- c(
    "View the Choroplethr documentation at www.Choroplethr.com",
    "Choroplethr has a free course: www.CensusMappingCourse.com"
  )
  
  tip <- sample(tips, 1)
  packageStartupMessage(paste(strwrap(tip), collapse = "\n"))
}