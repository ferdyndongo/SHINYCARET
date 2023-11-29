#' Shiny WebApp for classificaTion model training.
#' 
#' @importFrom shinydashboard dashboardHeader dashboardBody dashboardSidebar dashboardPage dropdownMenu box
#' @importFrom shiny shinyApp tabsetPanel tabPanel mainPanel fluidRow uiOutput renderUI
classifierModelApp <- function(){
  
  options(shiny.maxRequestSize = 200 * 1024^2)
  
  ui <- dashboardPage(title = "CLASSIFICATION MODEL TRAINING",
                      header=dashboardHeader(title = "CLASS TRAINING",
                                             disable = FALSE,
                                             notificationUi("source")
                                             ),
                      sidebar=dashboardSidebar(fileUi(id = "source"),shiny::uiOutput("target")),
                      body=dashboardBody(dataVizUi("overview"), shiny::uiOutput("models"), shiny::uiOutput("train")
                      )
  )
  
  server <- function(input, output, session){
    thematic::thematic_shiny()
    
    
    data <- fileServer(id = "source")
    shiny::observeEvent(data(),{
      dataVizOutput("overview",data)
    })
    
    output$target <- shiny::renderUI({
      shiny::req(data())
      catVarUi("source")
    })
    shiny::observeEvent(data(),input$catVar,{
      shiny::freezeReactiveValue(input,"source-catVar")
      catVarServer("source",data)
    })
    
    output$models <- shiny::renderUI({
      shiny::req(data(), input$`source-catVar`)
      classModelUi("source")
    })
    
    fitted_model <- classModelServer("source", data)
    
    
    output$train <- shiny::renderUI({
      shiny::req(fitted_model())
      shiny::tagList(
        trainingUi("source"),
        downloadUi("source")
      )
    })
    trainingOutput("source",fitted_model)
    filedownServer("source",fitted_model)
  }
  
  shinyApp(ui, server)
  
}

