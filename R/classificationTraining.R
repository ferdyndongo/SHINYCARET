#' Shiny WebApp for classificaion model training.
#' @return a small shiny interactive WebApp 
#' @importFrom shinydashboard dashboardHeader dashboardBody dashboardSidebar dashboardPage dropdownMenu box
#' @importFrom shiny shinyApp tabsetPanel tabPanel mainPanel fluidRow uiOutput renderUI
#' @importFrom purrr %>% 
classificationApp <- function(){
  
  ui <- dashboardPage(title = "MODEL TRAINING",
                      header=dashboardHeader( # dashboardHeader ####
                                              title = "TRAINING",
                                              disable = FALSE,
                                              dropdownMenu(type = "messages"),
                                              dropdownMenu(type = "notifications"),
                                              dropdownMenu(type = "tasks", badgeStatus = "success")),
                      sidebar=dashboardSidebar(datasourceInputUi(id = "source"),shiny::uiOutput("target"),
                                               shiny::uiOutput("rfe"),shiny::uiOutput("preprocess")),
                      body=dashboardBody(dataVizUi("overview"), shiny::uiOutput("models"), shiny::uiOutput("train")
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
    
    output$models <- shiny::renderUI({
      shiny::req(data(), input$`source-catVar`, input$`source-rfe`, input$`source-preprocess`)
      classModelUi2("source")
    })
    
    fitted_model <- classModelServer2("source", data)
    
    
    output$train <- shiny::renderUI({
      shiny::req(fitted_model())
      shiny::tagList(
        trainingUi("source"),
        reportUi("source"),
        downloadUi("source")
      )
    })
    trainingOutput("source",fitted_model)
    reportServer(id = "source",report_script = "classReport")
    downloadServer("source",fitted_model)
    
  }
  
  shinyApp(ui, server)
  
}

