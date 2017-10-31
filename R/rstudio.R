#' Simple check to determine if code is being run in RStudio with the shiny runtime
#' 

is_shiny_runtime = function() {
  runtime = knitr::opts_knit$get("rmarkdown.runtime")
  
  identical(runtime, "shiny")
}