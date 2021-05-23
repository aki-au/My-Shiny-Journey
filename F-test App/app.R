library(shiny)
library(psych)
data("iris")
iris_filtered<- subset(iris, iris$Species!="virginica")
iris_filtered2<- subset(iris, iris$Species!="setosa")
iris_filtered3<- subset(iris, iris$Species!="versicolor")
iris_filtered$Species= iris_filtered$Species[,drop=TRUE]
iris_filtered2$Species= iris_filtered2$Species[,drop=TRUE]
iris_filtered3$Species= iris_filtered3$Species[,drop=TRUE]

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      HTML(("<h2>F test</h2>")),
      "F-test is a hypothesis test used in inferential statistics to find out if the ratio between the variances of two populations is 1.
         Here, the in-built dataset iris has been used. For this test, only Sepal Width has been taken into consideration since its distribution is roughly normal.",
      hr(),
      HTML(("<h3>Summary of the 2 Different Groups</h>")),
      verbatimTextOutput("irisSumm"),
      HTML(("<h3>Input</h3>")),
      
  radioButtons("fchoice", "Choose the data for F-test",
               choices = c("Setosa vs Versicolor" = "sevsve",
                           "Versicolor vs Virginica" = "vevsvi",
                           "Setosa vs Virginica" = "sevsvi" 
               )),


  HTML(("<h3>Result of F-test</h3>")),
  verbatimTextOutput("irisResult")
    ),
  mainPanel(
    
    HTML(("<h3>Comparision of the 2 Species</h3>")),  
  plotOutput("bxxpltt"),
  wellPanel(
  HTML(("<h3>Side-by-side Histograms</h3>")), 
  plotOutput("phist1"))

))
)
server <- function(input, output) {

  output$bxxpltt<-renderPlot({
    if(input$fchoice== "sevsve")
    {
      boxplot(iris_filtered$Sepal.Width~iris_filtered$Species,data=iris_filtered, main="Sepal Width Data",
              xlab="Species Name", ylab="Width")
      stripchart(iris_filtered$Sepal.Width~iris_filtered$Species,method="jitter",jitter=.05,vertical=T,add=T) 
    }
    else if(input$fchoice== "vevsvi")
    {
      boxplot(iris_filtered2$Sepal.Width~iris_filtered2$Species,data=iris_filtered2, main="Sepal Width Data",
              xlab="Species Name", ylab="Width")
      stripchart(iris_filtered2$Sepal.Width~iris_filtered2$Species,method="jitter",jitter=.05,vertical=T,add=T) 
    }
    else
    {
      boxplot(iris_filtered3$Sepal.Width~iris_filtered3$Species,data=iris_filtered3, main="Sepal Width Data",
              xlab="Species Name", ylab="Width")
      stripchart(iris_filtered3$Sepal.Width~iris_filtered3$Species,method="jitter",jitter=.05,vertical=T,add=T)
    }

  })
  output$phist1<-renderPlot({
    if(input$fchoice== "sevsve")
    {
      par(mfrow=c(1,2))
      hist(iris_filtered$Sepal.Width[iris_filtered$Species=="setosa"], main = "Sepal Width Data", xlab = "Setosa", ylab = "Width" )
      hist(iris_filtered$Sepal.Width[iris_filtered$Species=="versicolor"],main = "Sepal Width Data",xlab="Versicolor", ylab = "Width" )
    }
    else if(input$fchoice== "vevsvi")
    {
      par(mfrow=c(1,2))
      hist(iris_filtered2$Sepal.Width[iris_filtered2$Species=="versicolor"], main = "Sepal Width Data", xlab = "Versicolor", ylab = "Width" )
      hist(iris_filtered2$Sepal.Width[iris_filtered2$Species=="virginica"],main = "Sepal Width Data",xlab="Virginica", ylab = "Width" )

    }
    else
    {
      par(mfrow=c(1,2))
      hist(iris_filtered3$Sepal.Width[iris_filtered3$Species=="setosa"], main = "Sepal Width Data", xlab = "Setosa", ylab = "Width" )
      hist(iris_filtered3$Sepal.Width[iris_filtered3$Species=="virginica"],main = "Sepal Width Data",xlab="Virginica", ylab = "Width" )
    }
    
  })
  fchoice <- reactive({switch(input$fchoice,
                                  sevsve = iris_filtered,
                                  vevsvi = iris_filtered2,
                                  sevsvi = iris_filtered3,
                                  iris_filtered)})
  output$irisSumm <- renderPrint({str(fchoice())})
  output$irisResult<- renderPrint({
  if(input$fchoice== "sevsve")
  {
    var.test(iris_filtered$Sepal.Width ~ iris_filtered$Species, data = iris_filtered)
  }
  else if(input$fchoice== "vevsvi")
  {
    var.test(iris_filtered2$Sepal.Width ~ iris_filtered2$Species, data = iris_filtered2)
  }
  else
  {
    var.test(iris_filtered3$Sepal.Width ~ iris_filtered3$Species, data = iris_filtered3)
  }
  })
}

shinyApp(ui = ui, server = server)