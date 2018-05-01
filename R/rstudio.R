#' Simple check to determine if code is being run in RStudio with the shiny runtime
#' internal function
#' @keywords internal 

allow_shiny = function() {
  runtime = knitr::opts_knit$get("rmarkdown.runtime")
  
  identical(runtime, "shiny") | is.null(runtime)
}