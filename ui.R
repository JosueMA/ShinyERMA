ui<-shinyUI(fluidPage(
  titlePanel("ERMA"),
  sidebarLayout(
    sidebarPanel(
      fileInput('Data','Choose File',accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
      tags$hr(),
      checkboxInput('header','Header',TRUE),
      radioButtons('sep','Separator',c(Comma=',',Semicolon=';',Tab='\t'), ',')),
    mainPanel(
      tabsetPanel(
        tabPanel("Data",tableOutput('Dat')),
        tabPanel("Item Summary",tableOutput('ItemTable'),tableOutput('Item')),
        tabPanel("Person Summary",tableOutput('PersonTable'),tableOutput('Person')),
        #tabPanel("Item Measures",tableOutput('Item')),
        #tabPanel("Person Measures",tableOutput('Person')),
        tabPanel("The Wright Map",plotOutput('HistI'),plotOutput('HistP'),plotOutput('Stem'))
        #tabPanel("The Stem Plot",plotOutput('Stem'))
      ))
  )))