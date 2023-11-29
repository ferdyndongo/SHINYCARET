#' Data Exploration Analysis through descriptive statistics of variables
#' @importFrom shinydashboard dashboardHeader dashboardBody dashboardSidebar dashboardPage dropdownMenu dropdownMenuOutput
#' taskItem messageItem notificationItem
#' @importFrom shiny shinyApp tabsetPanel tabPanel mainPanel uiOutput
#' @export
DataExplorationApp <- function(){
  
  options(shiny.maxRequestSize = 800 * 1024^2, width = 160)
  
  # Define UI for application that draws a EDA
  ui <- shinydashboard::dashboardPage(header=shinydashboard::dashboardHeader( # dashboardHeader ####
                                                                              title = "DATA EXPLORATION",#titleWidth = 250, 
                                                                              disable = FALSE,
                                                                              notificationUi("eda")
  ),
  sidebar=shinydashboard::dashboardSidebar(fileUi(id = "eda")),
  body=shinydashboard::dashboardBody(shiny::uiOutput("rawOutput"))
  )
  
  
  # Define server logic required to build EDA
  server <- function(input, output, session){
    
    data <- fileServer(id = "eda")
    
    output$rawOutput <- shiny::renderUI({
      shiny::req(data())
      shiny::tagList(
        edaUi("eda"),
        reportUi("eda")
      )
    })
    edaOutput("eda",data)
    reportServer("eda", "EDA")
    
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
}
