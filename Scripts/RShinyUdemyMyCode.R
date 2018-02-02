## A basic shiny app

library(shiny)
library(ggplot2)
library(shinythemes) # adding the shinythemese package
library(DT)



DF<-read.csv("./Data/miningData.csv", header = T,sep=";")

DF$Grade = as.double(1*DF$G1 + 1*DF$G2 + 1*DF$G3)

## Using Input Widgets

server <- function(input,output, session) {
  
  data <- reactive({
    DF$Grade =input$Slider1*DF$G1 + input$Slider2*DF$G2 + input$Slider3*DF$G3
    return(DF)
      })
  
  output$plot <- renderPlot({
    ggplot(data(), aes(Grade,MarketCap.in.M)) + geom_point(shape=1) + geom_smooth(method=lm)
  })

output$tableDT <- DT::renderDataTable(datatable(data()))

Filter.new = reactive({
  
  user_brush <- input$user_brush
  mysel <- brushedPoints(data(), user_brush)
  return(mysel)
  
}
)

output$table = DT::renderDataTable(datatable(Filter.new()))

output$mydownload <- downloadHandler(
  filename = "miningData.csv",
  content = function(file) {
    write.csv(data(), file)})

output$mydownloadfirst <- downloadHandler(
  filename = "miningData.csv",
  content = function(file) {
    write.csv(Filter.new(), file)})

} 



ui <- navbarPage(shinytheme("darkly"), title = h3("The Mining Stock Scale"),


####To make multiple tabs, you call tabPanel and put everything isnide it. Repeat for as many tabs as you want####
   tabPanel(("Adjust your mining Stocks"),

##well panel is excellent for grouping things together
            
             wellPanel(
  sliderInput(inputId = "Slider1",
              label = "Weight on Grade 1",
              value = 1, min = 1, max = 20,step = 0.2),
  sliderInput(inputId = "Slider2",
              label = "Weight on Grade 2",
              value = 1, min = 1, max = 20,step = 0.2),
  sliderInput(inputId = "Slider3",
              label = "Weight on Grade 3",
              value = 1, min = 1, max = 20,step = 0.2)
),
plotOutput("plot", brush= "user_brush"),
DT::dataTableOutput("table"),
downloadButton(outputId = "mydownloadfirst", label = "Download Table")
),

tabPanel("Documentation",
        h4("Video documentation - Embedded from Youtube"),
        tags$iframe(src="https://www.youtube.com/embed/vySGuusQI3Y"),
        style="height:900px;width:900px"),

tabPanel("Data table with the underlying Data",
         DT::dataTableOutput("tableDT"),
         downloadButton(outputId = "mydownload", label = "Download Table")
         )



 )


shinyApp(ui = ui, server = server)

