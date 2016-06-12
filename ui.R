ui<-shinyUI(fluidPage(
  titlePanel("ERMA (Dichotomous item responses)"),
  sidebarLayout(
    sidebarPanel(
      "Notes: Headings (first row) and Person ID (first column) are required.
      Please make sure the data structure matches the example data.",
      tags$hr(),
      fileInput('Data','Choose File',accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
      tags$hr(),
      radioButtons('sep','Separator',c(Comma=',',Semicolon=';',Tab='\t',space=""), ',')),
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