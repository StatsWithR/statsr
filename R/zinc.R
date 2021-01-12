#' Zinc Concentration in Water
#'
#' Trace metals in drinking water affect the flavor and
#' an unusually high concentration can pose a health
#' hazard. Ten pairs of data were taken measuring zinc
#' concentration in bottom water and surface water.
#'
#' @format
#'  A data frame with 10 observations on the following 4 variables.
#'  \describe{
#'    \item{\code{location}}{sample number}
#'    \item{\code{bottom}}{zinc concentration in bottom water}
#'    \item{\code{surface}}{zinc concentration in surface water}
#'    \item{\code{difference}}{difference between zinc concentration at the bottom and surface}
#'  }
#'
#' @source
#'  \href{https://online.stat.psu.edu/stat500/sites/stat500/files/data/zinc_conc.txt}{PennState Eberly College of Science Online Courses}
#'
#' @examples
#'  data(zinc)
#'  str(zinc)
#'  plot(bottom ~ surface, data=zinc)
#'  # use paired t-test to test if difference in means is zero
#'
"zinc"
