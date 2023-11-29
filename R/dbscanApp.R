#' single page application for dbscan outlier training
#' @return a shiny dashboard page with dbscan outlier training tools
#' @importFrom shinydashboard dashboardHeader dashboardBody dashboardSidebar dashboardPage dropdownMenu
#' @importFrom shiny shinyApp tabsetPanel tabPanel
dbscanApp <- function(){
  options(shiny.reactlog = TRUE)
  options(shiny.maxRequestSize = 200 * 1024^2)
  options(shiny.reactlog = TRUE)
  
  ui <- dashboardPage(title = "DBSCAN",
                      header=dashboardHeader( # dashboardHeader ####
                                              title = "DBSCAN",
                                              disable = FALSE,
                                              dropdownMenu(type = "messages"),
                                              dropdownMenu(type = "notifications"),
                                              dropdownMenu(type = "tasks", badgeStatus = "success")),
                      sidebar=dashboardSidebar(datasourceInputUi(id = "source"),shiny::uiOutput("target"),
                                               shiny::uiOutput("rfe"),shiny::uiOutput("preprocess")),
                      body=dashboardBody(dataVizUi("overview"),
                                         shiny::uiOutput("dbscanOutput"),
                                         shiny::uiOutput("download")
                      )
  )
  
  server <- function(input, output, session){
    thematic::thematic_shiny()
    
    # import train sample ####
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
      shiny::req(data(), input$`source-catVar`)
      rfeUi("source")
    })
    
    output$preprocess <- shiny::renderUI({
      shiny::req(data(),input$`source-catVar`)
      preProcessUi("source")
    })
    
    knn_fit <- knnServer("source", data)
    
    output$dbscanOutput <- shiny::renderUI({
      shiny::req(knn_fit())
      shiny::tagList(
        dbscanInput("source"),
        dbscanUi("source")
      )
    })
    
    dbscan <- dbscanServer("source",knn_fit)
    dbscanOutput("source", dbscan, shiny::reactive(knn_fit()$trainingData))
    
    output$download <- shiny::renderUI({
      shiny::req(dbscan())
      shiny::tagList(
        reportUi("source"),
        downloadUi("source")
      )
    })
    reportServer(id = "source",report_script = "dbscanReport")
    downloadServer("source",list(dbscan=shiny::reactive(dbscan()),
                                 trainingSample=shiny::reactive(knn_fit()$trainingData)))
  }
  
  shinyApp(ui, server)
}