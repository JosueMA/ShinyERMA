ui<-shinyUI(fluidPage(
  titlePanel("ERMA"),
  sidebarLayout(
    sidebarPanel(
      fileInput('Data','Choose File (csv/txt)',accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
      tags$hr(),
      radioButtons('sep','Separator',c(Comma=',',Semicolon=';',Tab='\t',Space=""), ',')),
    mainPanel(
      tabsetPanel(
        tabPanel("ERMA Description",
                 br(),
                 h4("ERMA (Everyone's Rasch Measurement Analyzer) for Dichotomous item responses"),
                 hr(),
                 p("ERMA was created using R. As an R program, ERMA provides free and open source code. 
                   ERMA uses a pairwise algorithm for calibrating item parameters, and maximum likelihood 
                   estimation for person measures. The results of ERMA have been validated with other Rasch 
                   software. The simple structure of ERMA makes it a useful teaching tool for users to gain 
                   an understanding of Rasch measurement theory, and also provides the opportunity to modify 
                   the program based on their needs. In addition to the traditional R program, ERMA also is 
                   available as a ShinyApp. ShinyApp is supported by RStudio, and it is a web application 
                   framework for R programs. ShinyERMA provides a user-friendly interface. ShinyERMA uses 
                   click-and-go buttons, and provides major outputs found in typical Rasch analyses. The 
                   development of ERMA program is still in progress. We plan to extend its use for wider 
                   applications in the future, such as rater-mediated assessments."),
                 hr(),
                 p("For the input dataset, headings (first row) and person ID (first column) are required.
                   Please make sure the data structure matches the example data as below."), 
                 br(),
                 p("Person, 1, 2, 3, 4"),
                 p("1, 1, 0, 1, 1"),
                 p("2, 1, 1, 0, 0"),
                 p("3, 1, 0, 0, 0"),
                 p("4, 0, 1, 1, 1"),
                 p("5, 1, 1, 1, 0"),
                 p("6, 1, 1, 0, 1"),
                 p("7, 1, 1, 1, 1"),
                 p("8, 1, 0, 1, 0"),
                 p("9, 1, 1, 1, 1"),
                 p("10, 1, 1, 0, 0"),
                 hr(),
                  p("Engelhard, G. (April 2016). Workshop titled ERMA (Everyone's Rasch Measurement Analyzer) at International Objective Measurement Workshop (IOMW) in Washington D.C.. http://www.iomw.org/pre-conference"),
                 p("Wang, J., & Engelhard, G. (2014). A Pairwise Algorithm in R for Rater-Mediated Assessments.",span("Rasch Measurement Transactions,",style="font-style: italic")," 28(1), 1457-1459."),
                 p("Wang, J. & Engelhard, G. (April 2014).",span("Everyone's Rasch Measurement Analyzer (Erma): An R Package for Rasch Measurement Models.",style="font-style: italic")," Paper presented at the International Objective Measurement Workshop (IOMW) in Philadelphia, PA.")),    
        tabPanel("Data",dataTableOutput('Dat')),
        tabPanel("Item Summary",tags$br(),tableOutput('ItemTable'),tags$br(), tableOutput('Item')),
        tabPanel("Person Summary",tableOutput('PersonTable'),tags$br(),tableOutput('Person')),
        tabPanel("The Wright Map",plotOutput('HistI'),plotOutput('HistP'),tags$br(), plotOutput('Stem'))
                 ))
    )))
