#' Run the interactive Bayes Factor shiny app
#' 
#' This app illustrates how changing the sample mean and sample size
#' affects the Bayes Factor for testing H1 that the mean is zero 
#' versus H2 that the mean is not zero for data arising from a normal
#' population.
#' @examples
#' \dontrun{
#' BF.app()
#' }

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
                    sliderInput("ybar", "ybar (in units of  standard deviations)", 
                                min=0, max=3, step=.01, value=.25),
                    sliderInput("n", "Sample Size n", min=0, max=1000, step=1, value=100.0)
                ),
            mainPanel(
                plotOutput("BF_plot_mu"),
                plotOutput("BF_plot_sd")
                    )
        ),
        server = function(input, output, session) 
        {
        

            output$BF_plot_mu = renderPlot(
            {   
                 d = data.frame(
                     x = seq(0, 1, length.out = 1000))

                 d$y = exp(-dnorm(input$ybar, 0, 1/sqrt(input$n), log=TRUE) +
                       dnorm(input$ybar, d$x, 1/sqrt(input$n), log=TRUE))
                 
                 BF.fav = subset(d, d$y >= 1)
                 BF.against = subset(d, d$y < 1)
                 if (nrow(BF.fav) == nrow(d))  BF.fav = rbind(BF.fav, c(1,1))
                 if (nrow(BF.against) > 1)  BF.against = rbind(BF.against, c(1,1))
                
                param =  "\u03BC"
                        

                ggplot(d, aes_string(x='x', y='y')) + 
                    ylab("BF[H2:H1]") +
                    xlab("mu_2 under H2 (in units of sigma)") +
                    geom_line() +
                    geom_abline(slope=0, intercept=0) +
                    geom_polygon(data=BF.fav,aes_string(x='x',y='y'),alpha=0.5) +
                    geom_polygon(data=BF.against,aes_string(x='x',y='y'),alpha=0.5) +
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

