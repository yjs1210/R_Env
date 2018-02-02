library(shiny)
library(DT)
library(ggplot2)
library(shinythemes)

mydata = read.csv("./Data/miningData.csv", header = T, sep = ";")

attach(mydata)

server = function(input, output, session){
  
  # table for the Data table tab
  
  output$tableDT <- DT::renderDataTable(DT::datatable(mydata) %>% 
                                          formatCurrency("MarketCap.in.M", "$", digits = 0) %>%
                                          formatStyle("Symbol", color = "grey") %>%
                                          formatStyle(c("G3", "G2", "G1"),
                                                      backgroundColor = "lightblue")) 
  
  
  weighted.mydata = reactive(
    cbind(mydata, 
          points = input$w1 * `G1` + input$w2 * `G2` + input$w3 * `G3`)
  )
  
  
  output$scat = renderPlot({
    ggplot(weighted.mydata(), aes(points, MarketCap.in.M)) +
      geom_point() + geom_smooth(method = "lm") +
      xlab("Your Calculated Score") + ylab("Market Capitalization in Million USD")
  })
  
  
  mydata.new = reactive({
    
    user_brush <- input$user_brush
    mysel <- brushedPoints(weighted.mydata(), user_brush)
    return(mysel)
    
  })
  
  
  output$table = DT::renderDataTable(DT::datatable(mydata.new()))
  
  output$mydownload = downloadHandler(
    filename = "selected_miners.csv",
    content = function(file) {
      write.csv(mydata.new(), file)})
  
  
}

ui = navbarPage(theme = shinytheme("sandstone"), title = h3("The Mining Stock Scale"),
                tabPanel(
                  ("Adjust your Mining Stocks"),
                  wellPanel(
                    sliderInput(inputId = "w1",
                                label = "Weight on Grade 1",
                                value = 7, min = 0, max = 20),
                    sliderInput(inputId = "w2",
                                label = "Weight on Grade 2",
                                value = 2, min = 0, max = 20),
                    sliderInput(inputId = "w3",
                                label = "Weight on Grade 3",
                                value = 0.6, min = 0, max = 6, step = 0.2)
                  ),
                  plotOutput("scat", brush = "user_brush"),
                  DT::dataTableOutput("table"),
                  downloadButton(outputId = "mydownload", label = "Download Table")
                ),
                
                tabPanel("Documentation",
                         h4("Video documentation - Embedded from Youtube"),
                         tags$iframe(style="height:700px; width:100%",
                                     src="https://www.youtube.com/embed/vySGuusQI3Y")
                ), 
                
                tabPanel("Data Table with the underlying Data",
                         DT::dataTableOutput("tableDT"))
                
)

shinyApp(ui = ui, server = server)

