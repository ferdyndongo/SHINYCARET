#' Shiny WebApp for classificaion model building.
#' @return a small shiny interactive WebApp 
#' @importFrom shinydashboard dashboardHeader dashboardBody dashboardSidebar dashboardPage dropdownMenu box
#' @importFrom shiny shinyApp tabsetPanel tabPanel mainPanel fluidRow uiOutput renderUI
#' @importFrom purrr %>%
#' @export
ClassModelBuildingApp <- function(){
  
  options(shiny.maxRequestSize = 900 * 1024^2)
  
  ui <- shinydashboard::dashboardPage(title = "CLASSIFICATION MODEL TRAINING",
                                      header=shinydashboard::dashboardHeader(title = "TRAINING/TEST",
                                                                             disable = FALSE,
                                                                             notificationUi("source")
                                      ),
                                      sidebar=shinydashboard::dashboardSidebar(fileUi(id = "source"),shiny::uiOutput("target")),
                                      body=shinydashboard::dashboardBody(dataVizUi("overview"),shiny::uiOutput("models"), 
                                                                         shiny::uiOutput("TrainTest")
                                      )
  )
  
  server <- function(input, output, session){
    
    rawdata <- fileServer(id = "source")
    data <- shiny::reactive({
      if(!is.null(rawdata())){
        if(inherits(rawdata(),"data.frame") && apply(X = rawdata(),MARGIN = 2,function(col){!is.raw(col[[1]])}) %>% all()){
          rawdata()
        }else{
          shiny::showNotification(paste0("ERROR !!! :"," UPLOADED DATA ARE NOT DATASET!!!"),duration = NULL,closeButton = TRUE,type = "error")
          return(NULL)
        }
      }
    })
    # data <- fileServer(id = "source")
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
    
    output$models <- shiny::renderUI({
      shiny::req(data(), input$`source-catVar`)
      classModelUi("source")
    })
    
    
    
    result <- TrainTestModelServer("source", data)
    
    shiny::observeEvent(result,{
      output$TrainTest <- shiny::renderUI({
        shiny::req(data(), input$`source-catVar`, input$`source-caretModel`,result)
        TrainTestModelUi("source")
      })
      TrainTestModelOutput("source", result)
      filedownServer("source",result$model)
    })
    
  }
  
  shinyApp(ui, server)
  
}

