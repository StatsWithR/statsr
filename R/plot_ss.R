#' plot_ss
#'
#' An interactive function that will generate a scatterplot of two variables, then
#' allow the user to click the plot in two locations to draw a best fit line.
#' Residuals are drawn by default; boxes representing the squared residuals are
#' optional.
#'
#' @param x the name of numerical vector 1
#' @param y the name of numerical vector 2
#' @param data the dataframe in which x and y can be found
#' @param showSquares logical option to show boxes representing the squared residuals
#' @param leastSquares logical option to bypass point entry and automatically draw the least squares line
#' @export

plot_ss <- function(x, y, data, showSquares = FALSE, leastSquares = FALSE){
    
    xlab <- paste(substitute(x))
    ylab <- paste(substitute(y))
    
    x <- eval(substitute(x), data)
    y <- eval(substitute(y), data)
    plot(y ~ x, asp = 1, pch = 16, xlab = xlab, ylab = ylab) 
    
    if(leastSquares){
        m1 <- lm(y ~ x)
        y.hat <- m1$fit
    } else{
        cat("Click two points to make a line.")
        pt1 <- locator(1)
        points(pt1$x, pt1$y, pch = 4)
        pt2 <- locator(1)
        points(pt2$x, pt2$y, pch = 4)
        pts <- data.frame("x" = c(pt1$x, pt2$x),"y" = c(pt1$y, pt2$y))
        m1 <- lm(y ~ x, data = pts)
        y.hat <- predict(m1, newdata = data.frame(x))
    }
    r <- y - y.hat
    abline(m1)
    
    oSide <- x - r
    LLim <- par()$usr[1]
    RLim <- par()$usr[2]
    oSide[oSide < LLim | oSide > RLim] <- c(x + r)[oSide < LLim | oSide > RLim] # move boxes to avoid margins
    
    n <- length(y.hat)
    for(i in 1:n){
        lines(rep(x[i], 2), c(y[i], y.hat[i]), lty = 2, col = "#56B4E9")
        if(showSquares){
            lines(rep(oSide[i], 2), c(y[i], y.hat[i]), lty = 3, col = "#E69F00")
            lines(c(oSide[i], x[i]), rep(y.hat[i],2), lty = 3, col = "#E69F00")
            lines(c(oSide[i], x[i]), rep(y[i],2), lty = 3, col = "#E69F00")
        }
    }
    
    SS <- round(sum(r^2), 3)
    cat("\r                                ")
    print(m1)
    cat("Sum of Squares: ", SS)
}