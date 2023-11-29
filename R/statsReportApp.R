#' Data Exploration Analysis through descriptive statistics of variables
#' @importFrom shinydashboard dashboardHeader dashboardBody dashboardSidebar dashboardPage dropdownMenu dropdownMenuOutput
#' taskItem messageItem notificationItem
#' @importFrom shiny shinyApp tabsetPanel tabPanel mainPanel uiOutput
#' @export
statsReportApp <- function(){
  
  options(shiny.maxRequestSize = 200 * 1024^2, width = 160)
  
  # Define UI for application that draws a histogram
  ui <- shinydashboard::dashboardPage(header=shinydashboard::dashboardHeader( # dashboardHeader ####
                                                                              title = "STATS REPORT",#titleWidth = 250, 
                                                                              disable = FALSE,
                                                                              notificationUi("source")
  ),
  sidebar=shinydashboard::dashboardSidebar(fileUi(id = "source"), shiny::uiOutput("target")),
  body=shinydashboard::dashboardBody(shiny::uiOutput("rawOutput"))
  )
  
  # Define server logic required to draw a histogram
  server <- function(input, output, session){
    
    data <- fileServer(id = "source")
    # data <- shiny::reactive({
    #   shiny::req(rawdata())
    #   rawdata() %>% rename_data() %>%  replace_decimal_separator()  %>% convert_data_type()
    #   })
    output$target <- shiny::renderUI({
      shiny::req(data())
      shiny::tagList(
        catVarUi("source"),
        catVarValueUi("source")
      )
    })
    output$rawOutput <- shiny::renderUI({
      shiny::req(data())
      statsReportUi("source")
    })
    statsReportOutput("source",data)
    catVarServer("source",data)
    catVarValueServer("source",data)
    
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
}
