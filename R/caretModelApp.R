#' Shiny WebApp for classificaion model training.
#' @return a small shiny interactive WebApp 
#' @importFrom shinydashboard dashboardHeader dashboardBody dashboardSidebar dashboardPage dropdownMenu box
#' @importFrom shiny shinyApp tabsetPanel tabPanel mainPanel fluidRow uiOutput renderUI
caretModelApp <- function(){
  
  options(shiny.maxRequestSize = 200 * 1024^2)
  
  ui <- dashboardPage(title = "CARET",
                      header=dashboardHeader( # dashboardHeader ####
                                              title = "CARET",
                                              disable = FALSE,
                                              dropdownMenu(type = "messages"),
                                              dropdownMenu(type = "notifications"),
                                              dropdownMenu(type = "tasks", badgeStatus = "success")),
                      sidebar=dashboardSidebar(datasourceInputUi(id = "source"),shiny::uiOutput("target"),shiny::uiOutput("modelUi")),
                      body=dashboardBody(dataVizUi("overview"),shiny::uiOutput("preprocessInput"),
                                         shiny::verbatimTextOutput("fit"),
                                         shiny::verbatimTextOutput("printModel"),shiny::uiOutput("save")
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
    
    output$preprocessInput <- shiny::renderUI({
      shiny::req(input$`source-catVar`)
      preProcessInputUi("source")
    })
    
    preProc <- preProcessMethods("source")
    rfeData <- rfeServer("source",data)
    
    output$modelUi <- shiny::renderUI({
      shiny::req(input$`source-rfe`,input$`source-nzv`,input$`source-center`,input$`source-scale`,input$`source-bagImpute`)
      caretModelUi("source")
    })
    
    fit <- caretModelServer(id = "source",data = rfeData,preProc = preProc)
    
    shiny::observeEvent(fit(),{
      output$fit <- shiny::renderPrint(fit())
      output$printModel <- shiny::renderPrint(fit()$finalModel)
    })
    
    output$save <- shiny::renderUI({
      shiny::req(fit())
      shiny::tagList(
        reportUi("source"),
        downloadUi("source")
      )
    })
    downloadServer(id = "source",data = fit)
  }
  
  shinyApp(ui, server)
}

