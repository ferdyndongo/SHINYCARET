#' shiny single page application for rfe and knn testing
#' @return a shiny dashboard page with some plots
#' @importFrom shinydashboard dashboardHeader dashboardBody dashboardSidebar dashboardPage dropdownMenu
#' @importFrom shiny shinyApp tabsetPanel tabPanel
knnApp <- function(){
  
  ui <- dashboardPage(title = "k-Nearest Neighbor",
                      header=dashboardHeader( # dashboardHeader ####
                                              title = "KNN",
                                              disable = FALSE,
                                              dropdownMenu(type = "messages"),
                                              dropdownMenu(type = "notifications"),
                                              dropdownMenu(type = "tasks", badgeStatus = "success")),
                      sidebar=dashboardSidebar(datasourceInputUi(id = "source"),shiny::uiOutput("target"),
                                               shiny::uiOutput("rfe"),shiny::uiOutput("preprocess")),
                      body=dashboardBody(dataVizUi("overview"), shiny::uiOutput("train")
                      )
  )
  
  server <- function(input, output, session){
    thematic::thematic_shiny()
    
    
    data <- datasourceInputServer(id = "source")
    shiny::observeEvent(data(),{
      dataVizOutput("overview",data)
    })
    
    output$target <- shiny::renderUI({
      shiny::req(data())
      catVarUi("source")
    })
    shiny::observeEvent(data(),{
      shiny::freezeReactiveValue(input,"source-catVar")
      catVarServer("source",data)
    })
    
    output$rfe <- shiny::renderUI({
      shiny::req(data())
      rfeUi("source")
    })
    
    output$preprocess <- shiny::renderUI({
      shiny::req(data())
      preProcessUi("source")
    })
    
    fitted_model <- knnServer("source",data)
    
    output$train <- shiny::renderUI({
      shiny::req(fitted_model())
      shiny::tagList(
        trainingUi("source"),
        reportUi("source"),
        downloadUi("source")
      )
    })
    trainingOutput("source",fitted_model)
    reportServer(id = "source",report_script = "knnReport")
    downloadServer("source",fitted_model)
    
  }
  
  shinyApp(ui, server)

}