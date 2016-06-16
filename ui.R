ui<-shinyUI(fluidPage(
  titlePanel("ERMA"),
  sidebarLayout(
    sidebarPanel(
      fileInput('Data','Choose File (csv/txt)',accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
      tags$hr(),
      radioButtons('sep','Separator',c(Comma=',',Semicolon=';',Tab='\t',space=""), ',')),
    mainPanel(
      tabsetPanel(
        tabPanel("ERMA Description",textOutput('Des1'),tags$hr(),textOutput('Des2'),tags$hr(),textOutput('Des3')),
        tabPanel("Example Data",dataTableOutput('Exdata')),
        tabPanel("Data",dataTableOutput('Dat')),
        tabPanel("Item Summary",tableOutput('ItemTable'),tableOutput('Item')),
        tabPanel("Person Summary",tableOutput('PersonTable'),tableOutput('Person')),
        #tabPanel("Item Measures",tableOutput('Item')),
        #tabPanel("Person Measures",tableOutput('Person')),
        tabPanel("The Wright Map",plotOutput('HistI'),plotOutput('HistP'),plotOutput('Stem'))
        #tabPanel("The Stem Plot",plotOutput('Stem'))
      ))
  )))