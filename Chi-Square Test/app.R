library(shiny)
library(vcd)
library(ggplot2)
ToothGrowth$size <- ifelse(ToothGrowth$len < median(ToothGrowth$len),"small", "big" )
dosevssize=table(ToothGrowth$dose, ToothGrowth$size)
dosevssupp=table(ToothGrowth$dose, ToothGrowth$supp)
suppvssize=table(ToothGrowth$supp, ToothGrowth$size)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      HTML(("<h2>Chi-Square test</h2>")),
      "The Chi-Square test of Independance is a hypothesis test used in inferential statistics to find out if there is any relationship between different parameters.
         Here, the in-built dataset ToothGrowth has been used. For this test, the growth, i.e, size increase as well as other parameters have been tabulated to see their relationship. In this test, Null Hypothesis is the hypothesis which assumes independance.",
      hr(),

      HTML(("<h3>Input</h3>")),
  radioButtons("chichoice", "Choose the data for Chi-Square Test of Independance",
               choices = c("Dose vs Size" = "dovssi",
                           "Dose vs Supplement" = "dovssu",
                           "Supplement vs Size" = "suvssi" 
               )),

  HTML(("<h3>Mosaic Plot</h3>"),
  HTML("<i> The Mosaic plot has been used to visualize data present in a Contingency table, and its height represents proportion value</i>")),
  plotOutput("mosaicplt")
  
    ),
  mainPanel(
    
  HTML(("<h3>Bar Plot")), 
  plotOutput("barplt"),
  wellPanel(
  HTML(("<h3>Result of Chi-Square test</h3>")),
  verbatimTextOutput("chiresult"))
 

)
))

server <- function(input, output) {
  output$barplt<-renderPlot({
    if(input$chichoice== "dovssi")
    {
      ggplot(ToothGrowth) +
        aes(x = dose, fill = size) +
        geom_bar()
    }
    else if(input$chichoice== "dovvsu")
    {
      ggplot(ToothGrowth) +
        aes(x = dose, fill = supp) +
        geom_bar()
    }
    else
    {
      ggplot(ToothGrowth) +
        aes(x = supp, fill = size) +
        geom_bar()
    }
    
  })
  output$mosaicplt<-renderPlot({
    if(input$chichoice== "dovssi")
    {
      mosaic(~ dose + size,
             direction = c("v", "h"),
             data = ToothGrowth,
             shade = TRUE
      )
    }
    else if(input$chichoice== "dovssu")
    {
      mosaic(~ dose + supp,
             direction = c("v", "h"),
             data = ToothGrowth,
             shade = TRUE
      )
      
    }
    else
    {
      mosaic(~ supp + size,
             direction = c("v", "h"),
             data = ToothGrowth,
             shade = TRUE
      )
    }
    
  })
  output$chiresult<- renderPrint({
    if(input$chichoice== "dovvsi")
    {
      print(chisq.test(dosevssize))
      "We reject the null hypothesis at 0.05 significance level "
      
    }
    else if(input$chichoice== "dovssu")
    {
      print(chisq.test(dosevssupp))
      "We accept the null hypothesis at 0.05 significance level "
    }
    else
    {
      print(chisq.test(suppvssize))
      "We reject the null hypothesis at 0.05 significance level "
    }
  })

  
}

shinyApp(ui = ui, server = server)