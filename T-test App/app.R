library(shiny)
library(psych)
data("PlantGrowth")
print(str(PlantGrowth))
plant <- split(PlantGrowth, PlantGrowth$group)
ctrlplant<- plant[1]
trt1plant<-plant[2]
trt2plant<-plant[3]
plant.data<- data.frame(ctrlplant, trt1plant, trt2plant)
df = subset(plant.data, select = -c(ctrl.group,trt1.group,trt2.group) )
plantctrlvstrt1 = subset(df, select = -c(trt2.weight) )
plantctrlvstrt2 = subset(df, select = -c(trt1.weight) )
planttrt1vstrt2 =  subset(df, select = -c(ctrl.weight) )

ui <- fluidPage(
  sidebarLayout(
      sidebarPanel(
        HTML(("<h2>T test</h2>")),
        "T-test is a hypothesis test used in inferential statistics to find out if there is a difference between the means of the 2 groups.
         Here, the in-built dataset PlantGrowth has been used to compare the yield, measured by dried plant weight between different groups: Control, Treatment 1, and Treatment 2.",
        hr(),
        HTML(("<h3>Input</h3>")),
        radioButtons("ttestchoice", "Choose the data for t-test",
                            choices = c("Control vs Treatment 1" = "ctvst1",
                                        "Control vs Treatment 2" = "ctvst2",
                                        "Treatment 1 vs Treatment 2" = "t1vst2" 
                            )),
        HTML(("<h3>Summary of the 2 Different Groups</h>")),
                   verbatimTextOutput("tblsummary"),
        HTML(("<h3>Result of T-test</h3>")),
                   verbatimTextOutput("Result")
                   
               ),
  mainPanel(

    
  plotOutput("boxplt"),
  wellPanel(
    HTML(("<h4>Histogram for Comparision</h4>")),
  plotOutput("histm"),
  ))
  
 ) )

server <- function(input, output) {
  ttestchoice <- reactive({switch(input$ttestchoice,
                         ctvst1 = plantctrlvstrt1,
                         t1vst2 = planttrt1vstrt2,
                         ctvst2 = plantctrlvstrt2,
                         plantctrlvstrt1)})
  output$boxplt <- renderPlot({ 
    
                
    
    boxplot(ttestchoice(),main="Comparision Boxplot",ylab="Weight of Dried Plant")
    stripchart(ttestchoice(),method="jitter",jitter=.05,vertical=T,add=T) 
  })

  output$histm <- renderPlot({ 
    
    multi.hist(ttestchoice())
     
  })
  output$tblsummary <- renderPrint({summary(ttestchoice())})
  output$Result <- renderPrint({
    if(input$ttestchoice == "ctvst1")
    {
      with(plantctrlvstrt1, t.test(plantctrlvstrt1$ctrl.weight, plantctrlvstrt1$trt1.weight,equal.var=TRUE) )
    }
    else if(input$ttestchoice == "ctvst2")
    {
      with(plantctrlvstrt2, t.test(plantctrlvstrt2$ctrl.weight, plantctrlvstrt2$trt2.weight,equal.var=TRUE) ) 
    }
    else
    {
      with(planttrt1vstrt2, t.test(planttrt1vstrt2$trt1.weight, planttrt1vstrt2$trt2.weight,equal.var=TRUE) ) 
    }
      })
  
}


shinyApp(ui = ui, server = server)