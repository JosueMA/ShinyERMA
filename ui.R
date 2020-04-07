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
      h4("ERMA (Everyone's Rasch Measurement Analyzer) for Dichotomous Item Responses"),
      br(),
      p("ERMA uses a pairwise algorithm for calibrating item parameters, and maximum 
        likelihood estimation for obtaining person measures. The results of ERMA have been 
        validated with other Rasch computer software. The simple structure of ERMA makes it a useful 
        teaching tool for users to gain an understanding of Rasch measurement theory, and also provides 
        the opportunity to modify the R program code based on their needs. This version of ERMA is 
        available as a ShinyApp. ShinyERMA provides a user-friendly interface with click-and-go buttons.
        It provides the basic information found in typical Rasch analyses."),
    br(),
    p("Please make sure the data structure matches the example data shown below with headings in the 
     first row (Person ID (first column) and one column for each item (four items in the example)."), 
    br(),
    p("PersonID, Item1, Item2, Item3, Item4"),
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
  br(),
  p("References"),
  br(),
  p("Engelhard, G. (2013).",span("Invariant measurement: Using Rasch models in the social, behavioral, and health sciences.",
                                 style="font-style: italic"), "New York: Routledge."),
  p("Engelhard, G.& Wang, J. (April 2016). Workshop titled ERMA (Everyone's Rasch Measurement Analyzer) 
    at International Objective Measurement Workshop (IOMW) in Washington D.C.. http://www.iomw.org/pre-conference."),  
  p("Wang, J., & Engelhard, G. (2014). A Pairwise Algorithm in R for Rater-Mediated Assessments.",
    span("Rasch Measurement Transactions, 28(1), 1",style="font-style: italic"),"457-1459."),
  p("Wang, J. & Engelhard, G. (April 2014).",span("Everyone's Rasch Measurement Analyzer (Erma): 
    An R Program for Rasch Measurement Models.",style="font-style: italic")," Paper presented at 
    the International Objective Measurement Workshop (IOMW) in Philadelphia, PA."),
  p("Yen, W. (1981). Using simulation results to choose a latent trait model.", 
     span ("Applied Psychological Measurement, 5,", style="font-style: italic"), "245-262."),
   br(),
   p("Developers: Jue Wang (University of Miami) and George Engelhard Jr. (University of Georgia)")),
      tabPanel("Data",dataTableOutput('Dat')),
      tabPanel("Summary Table",tags$br(),tableOutput('SummaryTable')),
      tabPanel("Item Table", tableOutput('Item')),
      tabPanel("Person Table",tableOutput('Person'),tags$br(),tableOutput('Inttab')),
      tabPanel("The Wright Map",plotOutput('HistI'),plotOutput('HistP'),tags$br(), plotOutput('Stem'))
  ))
)))
