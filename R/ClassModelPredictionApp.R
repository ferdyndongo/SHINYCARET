#' Shiny WebApp for classificaTion model prediction.
#' 
#' @importFrom shinydashboard dashboardHeader dashboardBody dashboardSidebar dashboardPage dropdownMenu box
#' @importFrom shiny shinyApp tabsetPanel tabPanel mainPanel fluidRow uiOutput renderUI
#' @export

ClassModelPredictionApp <- function(){
  options(shiny.maxRequestSize = 800 * 1024^2)
  
  
  # Define UI for application that draws a histogram
  ui <- shinydashboard::dashboardPage(title = "Model Prediction",
                                      header=shinydashboard::dashboardHeader(title = "CLASS PREDICTION",titleWidth = 350,
                                                                             disable = FALSE,
                                                                             notificationUi("source")
                                      ),
                                      sidebar=shinydashboard::dashboardSidebar(fileUi(id = "source"),width = 350),
                                      body=shinydashboard::dashboardBody(shiny::uiOutput("source2"), shiny::uiOutput("classPred"))
  )
  
  # Define server logic required to draw a histogram
  server <- function(input, output, session){
    
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
  
  # Run the application 
  shinyApp(ui = ui, server = server)
}