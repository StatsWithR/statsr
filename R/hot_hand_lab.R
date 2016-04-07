#' Calculate hit streaks.
#' 
#' @param x A data frame or character vector of hits (\code{"H"}) and misses (\code{"M"}).
#' @return A data frame with one column, \code{length}, containing the length of each hit streak.
#' @examples
#' data(kobe_basket)
#' calc_streak(kobe_basket$shot)
#' 
#' @export

calc_streak = function(x)
{
    if (!is.atomic(x))
        x = x[,1]

    if (any(!x %in% c("H","M")))
        stop('Input should only contain hits ("H") and misses ("M")')
    
    y = rep(0,length(x))
    y[x == "H"] = 1
    y = c(0, y, 0)
    wz = which(y == 0)
    streak = diff(wz) - 1
    
    return(data.frame(length = streak))
}

#' FIXME - Kobe Bryant's shots made
#'
#' Blah blah blah
#'
#' @format A data frame with 133 rows and 6 variables:
#' \describe{
#'   \item{vs}{}
#'   \item{game}{}
#'   \item{quarter}{}
#'   \item{time}{}
#'   \item{description}{}
#'   \item{shot}{}
#' }
#' @source \url{}
"kobe_basket"