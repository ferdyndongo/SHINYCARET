#' Load a test sample of data from different sources and a fitted model 
#' then compute prediction test of the model
#' @return a small shiny app for random forest test and prediction
#' @importFrom shinydashboard dashboardHeader dashboardBody dashboardSidebar dashboardPage dropdownMenu box
#' @importFrom shiny shinyApp tabsetPanel tabPanel mainPanel fluidRow uiOutput
#' @importFrom purrr %>% 
testApp <- function(){
  
  ui <- dashboardPage(title = "Model Test",
                      header=dashboardHeader( # dashboardHeader ####
                                              title = "TEST",
                                              disable = FALSE,
                                              notificationUi("source")
                                              ),
                      sidebar=dashboardSidebar(datasourceInputUi(id = "source")),
                      body=dashboardBody(shiny::uiOutput("model"), shiny::uiOutput("test"))
  )
  
  server <- function(input, output, session){
    thematic::thematic_shiny()
    
    # import the test sample ####
    data <- datasourceInputServer(id = "source")
    shiny::observeEvent(data(),{
      output$overview <- shiny::renderTable(data())
    })
    
    # import the fitted model of reference ####
    output$model <- shiny::renderUI({
      shiny::req(data())
      shiny::freezeReactiveValue(input,"upload")
      dataFileInput("source")
    })
    model <- dataFileServer("source")
    
    # test the model in the test sample and output the result ####
    output$test <- shiny::renderUI({
      shiny::req(model())
      shiny::tagList(
        predTestUi("source"),
        catVarUi("source"),
        reportUi("source"),
      )
    })
    shiny::observeEvent(model(),{
      shiny::freezeReactiveValue(input,"source-catVar")
      catVarServer("source",data)
    })
    
    pred <- predTestOutput("source", model, data)
    downloadDBtable(id = "source",data = pred)
    reportServer(id = "source",report_script = "TestReport")
  }
  
  shinyApp(ui, server)
}