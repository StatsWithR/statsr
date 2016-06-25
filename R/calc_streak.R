#' Calculate hit streaks.
#' 
#' @param x A data frame or character vector of hits (\code{"H"}) and misses (\code{"M"}).
#' @return A data frame with one column, \code{length}, containing the length of each hit streak.
#' @examples
#' data(kobe_basket)
#' calc_streak(kobe_basket$shot)
#' test1 <- c("H","M","M","H","H","M","M","M","H","M")
#' test2 <- c("H","M","M","H","H","M","M","M","H","H")
#' calc_streak(test1)
#' calc_streak(test2)
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
    # Need to account for case when last shot is a miss
    y <- if(y[length(y)]==0){
        y[-length(y)]
    }else{
        y
    }
    #
    y = c(0, y, 0)
    wz = which(y == 0)
    streak = diff(wz) - 1
    
    return(data.frame(length = streak))
}