#' single page application for dbscan outlier test
#' @return a shiny dashboard page with dbscan outlier training tools
#' @importFrom shinydashboard dashboardHeader dashboardBody dashboardSidebar dashboardPage dropdownMenu
#' @importFrom shiny shinyApp tabsetPanel tabPanel
dbscanPredictionApp <- function(){
  
  options(shiny.reactlog = TRUE)
  options(shiny.maxRequestSize = 200 * 1024^2)
  
  ui <- dashboardPage(header=dashboardHeader( # dashboardHeader ####
                                              title = "DBSCAN TEST", 
                                              disable = FALSE,
                                              dropdownMenu(type = "messages"),
                                              dropdownMenu(type = "notifications"),
                                              dropdownMenu(type = "tasks", badgeStatus = "success")),
                      sidebar=dashboardSidebar(datasourceInputUi(id="source"),shiny::uiOutput("preprocess"),shiny::uiOutput("model")),
                      body=dashboardBody(# DBSCAN Outlier Detection ####
                                         dataVizUi("overview"),
                                         shiny::uiOutput("dbscanUI")
                                         
                                         
                      )
  )
  
  server <- function(input, output, session){
    thematic::thematic_shiny()
    
    datatest <- datasourceInputServer(id = "source")
    shiny::observeEvent(datatest(),{
      dataVizOutput("overview",datatest)
    })
    
    output$preprocess <- shiny::renderUI({
      shiny::req(datatest())
      preProcessUi("source")
    })
    
    output$model <- shiny::renderUI({
      shiny::req(datatest(),input$`source-preprocess`)
      shiny::freezeReactiveValue(input,"upload")
      dataFileInput("source")
    })
    
    dbscanData <- dataFileServer("source")
    
    output$dbscanUI <- shiny::renderUI({
      shiny::req(dbscanData())
      shiny::tagList(
        dbscanUi("source"),
        reportUi("source")
      )
    })
    datapred <- dbscanPrediction(id = "source",datatest,dbscanData)
    
    dbscanOutput(id = "source", dbscan = dbscanData()$dbscan, data = dbscanData()$trainingSample,
                 predicted_scores = datapred$predicted_scores,dataclust_pred = datapred$dataclust_pred)
    reportServer(id = "source",report_script = "dbscanTestReport")
  }
  
  shinyApp(ui, server)
  
}