#' Run the Credible Interval shiny app
#' 
#' @export

credible_interval_app = function()
{
    shinyApp(
        ui = pageWithSidebar(
                headerPanel(""),
                sidebarPanel(
                    selectInput(inputId = "dist",
                                label = "Posterior Distribution:",
                                choices = c("Normal" = "norm",
                                            "Beta"   = "beta",
                                            "Gamma"  = "gamma"),
                                selected = "norm"),
                    br(),
                    
                    conditionalPanel(
                        "input.dist == 'norm'",
                        numericInput("mu",     HTML("&mu;"), value=0.0),
                        numericInput("sigma2", HTML("&sigma;&sup2;"), value=1.0, min=0)
                    ),
                    conditionalPanel(
                        "input.dist == 'beta' || input.dist == 'gamma'",
                        numericInput("alpha", HTML("&alpha;"), value=1, min=0),
                        numericInput("beta",  HTML("&beta;"),  value=1, min=0)
                    ),
                    br(),
                    
                    sliderInput("ci", "Credible Interval", min=0, max=1, step=0.01, value=0.95)
                    
                ),
                mainPanel(
                    plotOutput("plot"),
                    htmlOutput("calc")
                )
            ),
        server = function(input, output) 
        {
            ci_percent  = reactive(
            {
                c(1-(1-input$ci)/2,(1-input$ci)/2)
            })
            
            ci_interval = reactive(
            {
                data.frame(
                    x = switch(input$dist,
                               norm  = qnorm(ci_percent(), input$mu, sqrt(input$sigma2)),
                               beta  = qbeta(ci_percent(), input$alpha, input$beta),
                               gamma = qgamma(ci_percent(), input$alpha, input$beta)),
                    y = 0
                )
            })
            
            output$calc = renderUI(
            {
                cmd = paste0("c(",paste0(round(rev(ci_percent()),4),collapse=", "),")")
                cmd = paste0("q",input$dist,"(",cmd,",",
                          switch(input$dist,
                                 norm = paste0(" mean=",round(input$mu,3),",",
                                               " sd=",round(sqrt(input$sigma2),3),")"),
                                 beta = paste0(" shape1=",round(input$alpha,3),",",
                                               " shape2=",round(input$beta,3),")"),
                                 gamma = paste0(" shape=",round(input$alpha,3),",",
                                                " rate=",round(input$beta,3),")")
                          )
                      )
                val = eval(parse(text=cmd))
                val = round(val,3)
                val = setNames(val,NULL)
                val = paste0("## [1] ",paste0(val,collapse=" "))
                
                code = function(x) paste0('<div class="sourceCode"><pre class="sourceCode r"><code class="sourceCode r"><span class="dv">',x,'</span></code></pre></div>')
                output = function(x) paste0("<pre><code>",x,"</code></pre>")
                
                HTML(paste(code(cmd),output(val),sep="\n"))
            })
            
            output$plot = renderPlot(
            {
                validate(
                    need(is.numeric(input$mu), "Distribution parameters must be numeric."),
                    need(is.numeric(input$sigma2), "Distribution parameters must be numeric."),
                    need(is.numeric(input$alpha), "Distribution parameters must be numeric."),
                    need(is.numeric(input$beta), "Distribution parameters must be numeric."),
                    need(input$sigma2 > 0, "sigma2 must be > 0."),
                    need(input$alpha > 0, "alpha must be > 0."),
                    need(input$beta > 0, "beta must be > 0."),
                    need(input$ci < 1, "Credible interval width must be < 1.")
                )
                d = data.frame(
                    x = switch(input$dist,
                               norm  = seq(input$mu-3*input$sigma2, input$mu+3*input$sigma2, length.out = 1000),
                               beta  = seq(0, 1, length.out=1000),
                               gamma = seq(0, qgamma(0.995,input$alpha,input$beta), length.out=1000))
                )
                
                d$y = switch(input$dist,
                           norm  = dnorm(d$x, input$mu, sqrt(input$sigma2)),
                           beta  = dbeta(d$x, input$alpha, input$beta),
                           gamma = dgamma(d$x, input$alpha, input$beta))
                
                ci_region = rbind(subset(d, d$x > ci_interval()$x[2] & d$x < ci_interval()$x[1]), ci_interval())
                
                param = switch(input$dist,
                               norm = "\u03BC",
                               beta = "p",
                               gamma = "\u03BB")

                ggplot(d, aes_string(x='x', y='y')) + 
                    ylab("Density") +
                    geom_line() + 
                    geom_polygon(data = ci_region, aes_string(x='x',y='y'),alpha=0.5) +
                    geom_line(data = ci_interval(), size=1.5) +
                    geom_point(data = ci_interval(), size=2) +
                    ggtitle(paste0("Posterior Distribution of ",param," with ",input$ci*100,"% Credible Interval"))
            })
        }
    )
}