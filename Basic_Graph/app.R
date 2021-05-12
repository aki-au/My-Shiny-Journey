library(shiny)
ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = "Choose a number", 
              value = 50, min = 1, max = 100),
  radioButtons("radplot", "Distribution type:",
               c("Normal" = "norm",
                 "Uniform" = "uni",
                 "Log-normal" = "lognorm",
                 "Exponential" = "exp")),
  plotOutput("Plot")
)

server <- function(input, output) {
  output$Plot <- renderPlot({ 
    radplot <- switch(input$radplot,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    hist(radplot(input$num))
  })
}


shinyApp(ui = ui, server = server)