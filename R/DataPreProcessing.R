#' Data Pre-Processing raw data through: renaming variable names, detect and remove samples with percentage of 
#' below Detection Limit missingness above a given percentage, fill the below Detection Limit missingness, replacement of
#' wrong decimal separator (the good one is supposed to be the comma ","), conversion of data type, and remove sample with
#' high percentage of missing values
#' @importFrom shinydashboard dashboardHeader dashboardBody dashboardSidebar dashboardPage dropdownMenu dropdownMenuOutput
#' taskItem messageItem notificationItem
#' @importFrom shiny shinyApp tabsetPanel tabPanel mainPanel uiOutput
DataPreProcessingApp <- function(){
  
  options(shiny.maxRequestSize = 800 * 1024^2,width = 160)
  
  ui <- dashboardPage(header=dashboardHeader( # dashboardHeader ####
                                              title = "DATA PRE-PROCESSING", titleWidth = 280,
                                              disable = FALSE,
                                              notificationUi("source")
                                              ),
                      sidebar=dashboardSidebar(fileUi(id = "source"),width = 280),
                      body=dashboardBody(shiny::uiOutput("dataViz"))
                      )
  
  server <- function(input, output, session){
    thematic::thematic_shiny()
    
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
        dataVizUi("source"),
        catVarUi("source"),
        edaUi("source"),
        downloadUploadUi("source")
      )
    })
    dataVizOutput("source",data)
    catVarServer("source",data)
    edaOutput("source",data)
    downloadUploadServer("source")
    downloadServer("source",data)
    databaseInputServer("source")
    writeToDBServer("source",data)
  }
  shinyApp(ui, server)
}
