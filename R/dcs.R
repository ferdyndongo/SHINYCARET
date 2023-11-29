#' Implement our data collection system.
#' Retrieve data from different file sources (excel, csv, mdb, txt) and write to qlite database
#' Read and write from/to database.
#' @importFrom shinydashboard dashboardHeader dashboardBody dashboardSidebar dashboardPage dropdownMenu
#' @importFrom shiny shinyApp tabsetPanel tabPanel mainPanel uiOutput
DataCollectionApp <- function(){
  
  options(shiny.maxRequestSize = 500 * 1024^2)
  
  ui <- dashboardPage(header=dashboardHeader( # dashboardHeader ####
                                              title = "Data Collection System", titleWidth = 250,
                                              disable = FALSE,
                                              notificationUi("source")
  ),
  sidebar=dashboardSidebar(fileUi(id = "source"),width = 250),
  body=dashboardBody(shiny::uiOutput("IO"))
  )
  
  server <- function(input, output, session){
    
    data <- fileServer("source")
    
    output$IO <- shiny::renderUI({
      shiny::req(data())
      shiny::tagList(
        dataVizUi("source"),
        sqliteDSNUi("source"),
        writeToDBUi("source")
      )
    })
    dataVizOutput(id = "source", data)
    sqliteDSNServer("source")
    writeToSQLiteDSN("source",data)
  }
  
  shinyApp(ui, server)
  
}
