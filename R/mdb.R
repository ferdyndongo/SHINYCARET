#' a selectInput object for MS Access database tables or Excel workbook in Input
#'
#' @param id identifier for the datamodule
#' @return a selectInput object with the list of tables stored in the given MS Access file or
#' list of sheetnames in a Excel workbook file
mdbInput <- function(id){
  shiny::tagList(
    shiny::fileInput(shiny::NS(id,"mdb_load"),label = NULL,accept = c(".mdb",".xls",".xlsx")),
    shiny::uiOutput(shiny::NS(id,"mdb_tables")),
    # shinydashboard::box(shiny::selectInput(shiny::NS(id,"mdb_tables"),label = NULL,
    #                                        choices = NULL,selected = NULL,selectize = FALSE))
  )
}

#' Import data from a table in the MS Access database table name given by the mdbInput
#' @param id identifier for the data module
#' @return a reactive dataframe given by the table name in the selectInput object created by mdbInput
#' @importFrom purrr %>% 
mdbServer <- function(id){
  shiny::moduleServer(id,function(input, output, session){
    table_list <- shiny::reactive({
      shiny::req(input$mdb_load$name, input$mdb_load$datapath)
      sheet_list(input$mdb_load$name, input$mdb_load$datapath)
    })
    
    output$mdb_tables <- shiny::renderUI({
      shiny::selectInput(shiny::NS(id,"sheet"),label = "Select sheet",
                         choices = c("",table_list()),selected = NULL,selectize = FALSE)
    })
    shiny::reactive({
      shiny::req(input$mdb_load, input$sheet)
      if(input$sheet %in% table_list()){
        tryCatch({
          load_file(input$mdb_load$name, input$mdb_load$datapath, input$sheet)
        },warning=function(w){
          shiny::showNotification(w,duration = NULL,closeButton = TRUE,type = "warning")
          output$warning <- shinydashboard::renderMenu({
            shinydashboard::dropdownMenu(type="notifications", .list=lapply(X = w,FUN = notificationItem))
          })
        },error=function(e){
          shiny::showNotification(e,duration = NULL,closeButton = TRUE,type = "error")
          output$warning <- shinydashboard::renderMenu({
            shinydashboard::dropdownMenu(type="notifications",#warn$message
                                         shinydashboard::notificationItem(text = e$message,
                                                                          icon = shiny::icon("warning"),
                                                                          status = "danger")
            )
          })
        })
        # load_file(input$mdb_load$name, input$mdb_load$datapath, input$sheet)
      }
    })
  })
}
