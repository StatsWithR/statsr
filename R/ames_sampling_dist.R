#' Simulate Sampling Distribution
#' 
#' Run the interactive ames sampling distribution shiny app to 
#' illustrate sampling distributions using variables from the `ames`
#' dataset.
#' 
#'
#' @examples
#' if (interactive()) { 
#'   ames_sampling_dist()
#' }

#' @export

ames_sampling_dist = function()
{
  if (!allow_shiny())
    stop("Shiny app will only run when built within RStudio.")
  
  ames = statsr::ames
  
  shinyApp(
    ui <- fluidPage(
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
        sidebarPanel(
          selectInput("selected_var", "Variable:",  choices = list("area", "price"), selected = "area"),         
          numericInput("n_samp", "Sample size:", min = 1, max = nrow(ames), value = 30),
          numericInput("n_sim", "Number of samples:", min = 1, max = 30000, value = 15000) 
        ),
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("sampling_plot"),
          verbatimTextOutput("sampling_mean"),
          verbatimTextOutput("sampling_se")
        )
      )
    ),
    
    # Define server logic required to draw a histogram
    server <- function(input, output) {
      
      # create sampling distribution
      sampling_dist <- reactive({
        s = sample(ames[[input$selected_var]], size = input$n_samp * input$n_sim, replace = TRUE)
        m = matrix(s, ncol = input$n_samp)
        data.frame(x_bar = rowMeans(m))
      })
      
      # plot sampling distribution
      output$sampling_plot <- renderPlot({
        x_min <- quantile(ames[[input$selected_var]], 0.1)
        x_max <- quantile(ames[[input$selected_var]], 0.9)
        
        ggplot(sampling_dist(), aes_string(x = "x_bar")) +
          geom_histogram(na.rm=TRUE, bins=50) +
          xlim(x_min, x_max) +
          ylim(0, input$n_sim * 0.35) +
          ggtitle(paste0("Sampling distribution of mean ", 
                         input$selected_var, " (n = ", input$n_samp, ")")) +
          xlab(paste("mean", input$selected_var)) +
          theme(plot.title = element_text(face = "bold", size = 16))
      })
      
      # mean of sampling distribution
      output$sampling_mean <- renderText({
        paste0("mean of sampling distribution = ", round(mean(sampling_dist()$x_bar), 2))
      })
      
      # mean of sampling distribution
      output$sampling_se <- renderText({
        paste0("SE of sampling distribution = ", round(sd(sampling_dist()$x_bar), 2))
      })
    },
    
    options = list(height = 500) 
  )
}