#' Run the interactive Bayes Factor shiny app
#' 
#' This app illustrates how changing the Z score and prior precision
#' affects the Bayes Factor for testing H1 that the mean is zero 
#' versus H2 that the mean is not zero for data arising from a normal
#' population.  Lindley's paradox occurs for large sample sizes
#' when the Bayes factor favors H1 even though the Z score is large or the
#' p-value is small enough to reach statistical significance and the values of 
#' the sample mean do not reflex practical significance based on the prior 
#' distribution.
#' Bartlett's paradox may occur when the prior precision goes to zero, leading 
#' to Bayes factors that favor H1 regardless of the data.
#' A prior precision of one corresponds to the unit information prior.
#' @examples
#' if (interactive()) { 
#' BF.app()
#' }
#' @export
#' 
BF_app = function()
{
  shinyApp(
    ui = pageWithSidebar(
      headerPanel(""),
      sidebarPanel(
        selectInput(inputId = "dist",
                    label = "Prior Distribution Family:",
                    choices = c("Normal" = "norm"),
                    selected = "norm"),
        br(),
        #   sliderInput("n0", "Prior Sample Size n0", min=0, max=4, step=0.01, value=1.0),
        sliderInput("Z", "Z score", 
                    min=-3, max=3, step=.05, value=0.0),
#        sliderInput("mu_2", "mu_2 (in units of  standard deviations)", 
#                     min=-3, max=3, step=.01, value=0.0),
        sliderInput("phi0", "Prior Precision", min=.000001, max=2, step=.01, 
                    value=1.0)
      ),
      mainPanel(
        plotOutput("BF_plot_mu")
      )
    ),
    server = function(input, output, session) 
    {
      
      
      output$BF_plot_mu = renderPlot(
        {   

          d = data.frame(
            n = 1:1000) 
          
            ybar = input$Z*sqrt(d$n)
            d$y = exp(-dnorm(ybar, 0, 1/sqrt(d$n), log=TRUE) +
                       dnorm(ybar, 0, sqrt(1/(input$phi0+.0000001) + 1/d$n), 
                             log=TRUE))
            d$y = BF10.normal(input$Z, n=d$n, n0=input$phi0, 
                                  logBF=FALSE, recip=FALSE)
          
          BF.fav = subset(d, d$y >= 1)
          BF.against = subset(d, d$y < 1)
          if (nrow(BF.against) == nrow(d)) {  # none in favor
            BF.against = rbind(c(1,1),BF.against, 
                               c(max(d$n), 1)) }
          else {
            if (nrow(BF.fav) == nrow(d)) {  # all in favor
              BF.fav =  rbind(c(1,1), BF.fav,c(max(d$n), 1))}
            else {# nrow(BF.fav) > 1 ) { # mix
              BF.fav = rbind(c(1,1), BF.fav, c(max(BF.fav$n),1))
              BF.against = rbind(c(min(BF.against$n),1), BF.against,
                                 c(max(BF.against$n),1))
              }
          }
         
          
          param =  "\u03BC"
          
          
          ggplot(d, aes_string(x='n', y='y')) + 
            ylab("BF[H2:H1]") +
            xlab("Sample Size n") +
            geom_line() +
            geom_abline(slope=0, intercept=0) +
            geom_polygon(data=BF.fav,aes_string(x='n',y='y'),alpha=0.5) +
            geom_polygon(data=BF.against,aes_string(x='n',y='y'),alpha=0.5) +
            scale_y_log10() +
            ggtitle("Bayes Factor H2:H1   H1: mu = 0.0 versus H2: mu = mu_2 ")
        })
      
      output$BF_plot_sd = renderPlot(
        {   
          
          x=10^seq(-2, 6, length=1000)
          n0 =  1/(x^2)   # precision
          Z = input$ybar/sqrt(1/input$n)
          y = BF10.normal(Z, n=input$n, n0=n0, logBF=FALSE, recip=TRUE) 
          d = data.frame(x, y)
          
          ggplot(d, aes_string(x='x', y='y')) + 
            ylab("BF[H1:H2]") +
            xlab("Prior Standard Deviation (in units of sigma)") +
            geom_line() +
            scale_y_log10() +
            scale_x_log10() +
            geom_abline(slope=0, intercept=0) +
            ggtitle(paste0("Bayes Factor H1:H2   H1: mu = 0.0, Z = ", round(Z,2)))
        })
      output$BF_plot_sd = renderPlot(
        {   
          
          x=10^seq(-2, 6, length=1000)
          n0 =  1/(x^2)   # precision
          Z = input$ybar/sqrt(1/input$n)
          y = BF10.normal(Z, n=input$n, n0=n0, logBF=FALSE, recip=TRUE) 
          d = data.frame(x, y)
          
          ggplot(d, aes_string(x='x', y='y')) + 
            ylab("BF[H1:H2]") +
            xlab("Prior Standard Deviation (in units of sigma)") +
            geom_line() +
            scale_y_log10() +
            scale_x_log10() +
            geom_abline(slope=0, intercept=0) +
            ggtitle(paste0("Bayes Factor H1:H2   H1: mu = 0.0, Z = ", round(Z,2)))
        })
      options = list(height = 500)
    }
  )
}

BF10.normal = function(z, n, n0, logBF=TRUE, recip=FALSE) {
  BF10 = .5*(z^2)*n/(n+n0) - .5*log(n + n0) + .5*log(n0)
  if (recip)   BF10 = -BF10
  if (logBF == FALSE)  BF10 = exp(BF10)
  return(BF10)
}

