library(shiny)
server <- function(input, output, session) {} #the server
ui = basicPage(
  h1("using textInput and checkboxInput"),
  textInput("mystring", "write here"),
  checkboxInput("mycheckbox", "Factor Y")) #the user interface
shinyApp(ui=ui,server=server) #app launch
