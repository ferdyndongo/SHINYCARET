#' Data Pre-Processing raw data through: renaming variable names, detect and remove samples with percentage of 
#' below Detection Limit missingness above a given percentage, fill the below Detection Limit missingness, replacement of
#' wrong decimal separator (the good one is supposed to be the comma ","), conversion of data type, and remove sample with
#' high percentage of missing values
#' @importFrom shinydashboard dashboardHeader dashboardBody dashboardSidebar dashboardPage dropdownMenu dropdownMenuOutput
#' taskItem messageItem notificationItem
#' @importFrom shiny shinyApp tabsetPanel tabPanel mainPanel uiOutput
#' @export
granaPreProcessingApp <- function(){
  
  options(shiny.maxRequestSize = 800 * 1024^2,width = 160)
  
  ui <- dashboardPage(header=dashboardHeader( # dashboardHeader ####
                                              title = "PRE-PROCESSING", titleWidth = 280,
                                              disable = FALSE,
                                              notificationUi("source")
  ),
  sidebar=dashboardSidebar(fileUi(id = "source"),width = 280),
  body=dashboardBody(shiny::uiOutput("dataViz"))
  )
  
  server <- function(input, output, session){
    
    rawdata <- fileServer(id = "source")
    data <- shiny::reactive({
      if(!is.null(rawdata())){
        if(inherits(rawdata(),"data.frame") && apply(X = rawdata(),MARGIN = 2,function(col){!is.raw(col[[1]])}) %>% all()){
          rawdata() %>% data_preprocessing()
        }
      }
    })
    
    output$dataViz <- shiny::renderUI({
      shiny::req(data())
      shiny::tagList(
        edaUi("source"),
        # sqliteDSNUi("source"),
        SQLiteFileUi("source"),
        writeToDBUi("source")
      )
    })
    edaOutput("source",data)
    # sqliteDSNServer("source")
    # writeToSQLiteDSN("source",data)
    SQLiteFileServer("source")
    writeToSQLiteFile("source",data)
  }
  shinyApp(ui, server)
}
