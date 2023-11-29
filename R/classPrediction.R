#' Shiny WebApp for classificaTion model training.
#' 
#' @importFrom shinydashboard dashboardHeader dashboardBody dashboardSidebar dashboardPage dropdownMenu box
#' @importFrom shiny shinyApp tabsetPanel tabPanel mainPanel fluidRow uiOutput renderUI
classPredictionApp <- function(){
  
  options(shiny.maxRequestSize = 200 * 1024^2)
  
  ui <- dashboardPage(title = "Model Test/Predict",
                      header=dashboardHeader(title = "MODEL TEST/PREDICT",titleWidth = 350,
                                             disable = FALSE,
                                             notificationUi("source")
                      ),
                      sidebar=dashboardSidebar(fileUi(id = "source"),width = 350),
                      body=dashboardBody(shiny::uiOutput("source2"), shiny::uiOutput("classPred"))
  )
  
  server <- function(input, output, session){
    thematic::thematic_shiny()
    
    # import model or test sample and visualize it ####
    data1 <- fileServer(id = "source")
    output$source2 <- shiny::renderUI({
      shiny::req(data1())
      fileUi("source2")
    })
    data2 <- fileServer("source2")
    pred <- classPredServer("source",data1,data2)
    
    shiny::observeEvent(pred(),{
      output$classPred <- shiny::renderUI({
        classPredUi("source")
      })
      if("data.frame" %in% class(pred())){
        filedownServer("source",pred)
      }else if("list" %in% class(pred())){
        filedownServer("source",shiny::reactive(pred()$sample))
      }
      classPredOutput("source",pred)
    })
  }
  
  shinyApp(ui, server)
  
}

