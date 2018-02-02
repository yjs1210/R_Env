## Simple datatable

library(shiny)
library(ggplot2)
library(DT)

server <- function(input, output, session) {
  
  output$tableDT <- DT::renderDataTable(diamonds[1:1000,], 
                                        options = list(paging=F), 
                                        rownames=F, 
                                        filter = "top")
  
}

ui <- fluidPage(
  DT::dataTableOutput("tableDT")
)

shinyApp(ui = ui, server = server)





## Datatable styling

library(shiny)
library(ggplot2)
library(DT)

server <- function(input, output, session) {
  
  
  output$tableDT <- DT::renderDataTable(datatable(diamonds[1:1000,],
                                                  options = list(paging=F), 
                                                  rownames=F, 
                                                  filter = "top") %>% 
                                          formatCurrency("price", "$") %>%
                                          formatStyle("price", color = "green") %>%
                                          formatStyle("cut",
                                                      transform = "rotateX(20deg) rotateY(5deg) rotateZ(5deg)",
                                                      backgroundColor = styleEqual(
                                                        unique(diamonds$cut), c("salmon", "lightblue", 
                                                                                "grey", "lightgreen", "lightpink")))) 
  
  
}

ui <- fluidPage(
  DT::dataTableOutput("tableDT")
)

shinyApp(ui = ui, server = server)





## Advanced App - brush

server <- function(input,output, session) {
  
  
  library(ggplot2) # for the diamonds dataset, and ggplot feature
  library(DT) # for the dataTableOutput
  library(shiny) # should always be activated
  
  
  output$plot <- renderPlot({
    ggplot(diamonds, aes(price, carat)) + geom_point()
  })
  
  diam <- reactive({
    
    user_brush <- input$user_brush
    sel <- brushedPoints(diamonds, user_brush)
    return(sel)
    
  })
  
  output$table <- DT::renderDataTable(DT::datatable(diam()))
}

ui <-   fluidPage(
  h1("Using the brush feature to select specific observations"),
  plotOutput("plot", brush = "user_brush"),
  dataTableOutput("table")
)

shinyApp(ui = ui, server = server)

## Advanced App - click

server <- function(input,output, session) {
  
  library(ggplot2) # for the diamonds dataset, and ggplot feature
  library(DT) # for the dataTableOutput
  
  output$plot <- renderPlot({
    ggplot(diamonds, aes(price, carat)) + geom_point() 
  })
  
  diam <- reactive({
    
    user_click <- input$user_click
    sel <- nearPoints(diamonds, user_click, threshold = 10, maxpoints = 5)
    # maxpoints gives the maximum number of observations in the table
    # threshold gives the maximum distance in the dataset
    return(sel)
    
  })
  
  output$table <- DT::renderDataTable(DT::datatable(diam()))
}

ui <-   fluidPage(
  h1("Using the click feature to select specific observations"),
  plotOutput("plot", click = "user_click"),
  dataTableOutput("table")
)

shinyApp(ui = ui, server = server)



## Advanced Plot with csv export

server <- function(input,output, session) {
  
  library(ggplot2) # for the diamonds dataset, and ggplot feature
  library(DT) # for the dataTableOutput
  
  output$plot <- renderPlot({
    
    ggplot(diamonds, aes(price, carat)) + geom_point()
    
  })
  
  diam <- reactive({
    
    user_brush <- input$user_brush
    sel <- brushedPoints(diamonds, user_brush)
    return(sel)
    
  })
  
  output$table <- DT::renderDataTable(DT::datatable(diam()))
  
  output$mydownload <- downloadHandler(
    filename = "plotextract.csv",
    content = function(file) {
      write.csv(diam(), file)})
}

ui <- fluidPage(
  h3("Exporting Data as .csv"),
  plotOutput("plot", brush = "user_brush"),
  dataTableOutput("table"),
  downloadButton(outputId = "mydownload", label = "Download Table")
)

shinyApp(ui = ui, server = server)



## Media Integration

library(shiny)

server = function(input,output) {
}

ui = navbarPage("Integration of different media types",
                
                tabPanel("Image sourced locally",
                         tags$img(src = "logo.png", width = "100px", height = "100px")),
                
                tabPanel("Video sourced locally",
                         tags$video(src = "comist.mp4", type = "video/mp4", controls = T,
                                    width = "900px", height = "800px")),
                
                tabPanel("Pdf sourced online, Iframe",
                         tags$iframe(style="height:600px; width:100%; scrolling=yes", 
                                     src="https://cran.r-project.org/web/packages/shiny/shiny.pdf")), 
                
                tabPanel("Text as .txt",
                         includeText("mytxt.txt"))
)

shinyApp(ui = ui, server = server)