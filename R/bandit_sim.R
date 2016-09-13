#' Run the Bandit Simulation shiny app
#' 
#' @export

bandit_sim = function()
{
  shinyApp(
    ui = fluidPage(
      tags$head(
        tags$style(type="text/css", '.fa-slot-machine:before {content: "\\1f3b0"}')
      ),

      column(width=4,
        fluidRow(
          column(width=5,
            h4("Machine 1:"),
            actionButton("m1_play","Play!", class="btn btn-primary",
                         width="100px", style="white-space: normal;",
                         icon=icon("slot-machine",class="fa-3x"))
          ),
          column(width=1),
          column(width=5,
            h4("Machine 2:"),
            actionButton("m2_play","Play!", class="btn btn-success",
                         width="100px", style="white-space: normal;",
                         icon=icon("slot-machine",class="fa-3x"))
          )
        ),
        br(),
        h4("Results:"),
        tableOutput("tab")
      ),
      column(width=4,
        h4("Events:"),

        tabsetPanel(type = "tabs", 
          tabPanel(
            "Log", 
            wellPanel(
              id = "tPanel",
              style = "overflow-y:scroll; max-height: 220px",
              htmlOutput("res")
            )
          ), 
          tabPanel(
            "Data", 
            wellPanel(
              id = "tPanel",
              style = "overflow-y:scroll; max-height: 220px",
              verbatimTextOutput("data")
            )
          )
        )

      ),
      column(width=4,
        h4("Decision:"),
        actionButton("m1_guess","Machine 1 is good", class="btn btn-primary"),
        br(),br(),
        actionButton("m2_guess","Machine 2 is good", class="btn btn-success"),
        br(),br(),
        h4(textOutput("guess")),
        br(),br(),
        actionButton("reset", "Reset", class="btn btn-danger")
      )
    ),
    server = function(input, output)
    {
        values = reactiveValues(
                    tab = data.frame(Wins=c(0L,0L),
                                     Loses=c(0L,0L),
                                     Plays=c(0L,0L),
                                     row.names = c("Machine 1", "Machine 2")),
                    good = sample(1:2,1),
                    res = data.frame(play=integer(),
                                     machine=integer(),
                                     outcome=character())
                 )
        
        observeEvent(input$m1_play, {
          win = rbinom(1, 1, ifelse(values$good == 1, 1/2, 1/3))
          if (win)
            values$tab[1,c(1,3)] = values$tab[1,c(1,3)] + 1L
          else
            values$tab[1,c(2,3)] = values$tab[1,c(2,3)] + 1L
          
          values$res = rbind(data.frame(play = nrow(values$res)+1L,
                                        machine = 1L,
                                        outcome = ifelse(win,"W","L"),
                                        stringsAsFactors = FALSE),
                             values$res)

          #values$res = c(values$res, 
          #               paste("You", ifelse(win, "won", "lost"), "playing machine 1."))
        })
        
        observeEvent(input$m2_play, {
          win = rbinom(1, 1, ifelse(values$good == 2, 1/2, 1/3))
          if (win)
            values$tab[2,c(1,3)] = values$tab[2,c(1,3)] + 1L
          else
            values$tab[2,c(2,3)] = values$tab[2,c(2,3)] + 1L
          
          values$res = rbind(data.frame(play=nrow(values$res)+1L,
                                        machine = 2L,
                                        outcome = ifelse(win,"W","L"),
                                        stringsAsFactors = FALSE),
                             values$res)

          #values$res = c(paste("You", ifelse(win, "won", "lost"), "playing machine 2."),
          #               values$res)
        })
        
        observeEvent(input$reset, {
          values$tab[] = 0L
          #values$res = c()
          values$res = data.frame(play=integer(),machine=integer(),outcome=character())
          values$good = sample(1:2,1)
          output$guess = renderText("")
        })
        
        observeEvent(input$m1_guess, {
          output$guess = renderText(
                           paste0("You guessed ", 
                                  ifelse(values$good == 1, 
                                         "correctly",
                                         "incorrectly"),
                                  ".")
                         )
        })
        
        observeEvent(input$m2_guess, {
          output$guess = renderText(
                           paste0("You guessed ",
                                  ifelse(values$good == 2, 
                                         "correctly",
                                         "incorrectly"),
                                  ".")
                         )
        })
        
        output$data = renderText({
                        n = nrow(values$res)
                        if (n==0)
                          return("")

                        df = values$res[n:1,]
                        
                        m = paste0(paste0(df$machine,collapse="L, "),"L")
                        o = paste0('"',paste0(df$outcome, collapse='", "'),'"')

                        paste0("data = data.frame(",
                               "machine = c(",m,"), ",
                               "outcome = c(",o,"))")
                      })
        output$tab = renderTable(values$tab, align="ccc")
        output$res = renderTable(values$res,
                                 include.rownames=FALSE,
                                 align="ccc")
    }
  )
}
