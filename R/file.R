#' Ui for file give to fileServer function.
#'
#' @param id module identifier
fileUi <- function(id){
  shiny::tagList(
    shiny::fileInput(shiny::NS(id,"file_load"),label = "Choose file to upload",
                     accept = c(".mdb", ".xls", ".xlsx", ".csv", ".tsv", ".txt", ".RDS", ".db", ".s3db")
    ),
    shiny::uiOutput(shiny::NS(id,"file_tables")
    )
  )
}

#' Server function for fileUi.
#'
#' @param id module identifier
fileServer <- function(id){
  shiny::moduleServer(id,function(input, output, session){
    
    table_list <- shiny::reactive({
      shiny::req(input$file_load$name, input$file_load$datapath)
      if(tools::file_ext(input$file_load$name) %in% c("xls","xlsx","mdb", "db", "s3db")){
        sheet_list(input$file_load$name, input$file_load$datapath)
      }
    })
    
    output$file_tables <- shiny::renderUI({
      shiny::req(table_list())
        shiny::selectInput(shiny::NS(id,"sheet"),label = "Select sheet/table",
                           choices = c("",table_list()),selected = NULL,selectize = FALSE)
    })
    shiny::reactive({
      if(!is.null(table_list())){
        shiny::req(input$file_load, input$sheet)
        id <- shiny::showNotification("DATA LOADING IN PROGRESS ...", duration = NULL, closeButton = FALSE)
        base::on.exit(shiny::removeNotification(id), add = TRUE)
        if(input$sheet %in% table_list()){
          tryCatch({
            load_file(input$file_load$name, input$file_load$datapath, input$sheet)
          },warning=function(w){
            shiny::showNotification(w,duration = NULL,closeButton = TRUE,type = "warning")
            output$warning <- shinydashboard::renderMenu({
              shinydashboard::dropdownMenu(type="notifications",badgeStatus = "warning", icon = shiny::icon("triangle-exclamation"),
                                           .list=lapply(X = w,FUN = shinydashboard::notificationItem))
            })
            load_file(input$file_load$name, input$file_load$datapath, input$sheet)
            # return(NULL)
          },error=function(e){
            shiny::showNotification(e,duration = NULL,closeButton = TRUE,type = "error")
            output$warning <- shinydashboard::renderMenu({
              shinydashboard::dropdownMenu(type="notifications",badgeStatus = "danger", icon = shiny::icon("triangle-exclamation"),
                                           shinydashboard::notificationItem(text = e$message,
                                                                            icon = shiny::icon("triangle-exclamation"),
                                                                            status = "danger")
              )
            })
            return(NULL)
          })
          # load_file(input$file_load$name, input$file_load$datapath, input$sheet)
        }
      }else{
        shiny::updateSelectInput(inputId = "sheet",label = NULL,selected = NULL)
        shiny::req(input$file_load$name, input$file_load$datapath)
        tryCatch({
          load_file(input$file_load$name, input$file_load$datapath)
        },warning=function(w){
          shiny::showNotification(w$message,duration = NULL,closeButton = TRUE,type = "warning")
          output$warning <- shinydashboard::renderMenu({
            shinydashboard::dropdownMenu(type="notifications",badgeStatus = "warning", icon = shiny::icon("triangle-exclamation"),
                                         .list=lapply(X = w$message,FUN = shinydashboard::notificationItem))
          })
          load_file(input$file_load$name, input$file_load$datapath)
          # return(NULL)
        },error=function(e){
          shiny::showNotification(e$message,duration = NULL,closeButton = TRUE,type = "error")
          output$warning <- shinydashboard::renderMenu({
            shinydashboard::dropdownMenu(type="notifications",badgeStatus = "danger", icon = shiny::icon("file"),
                                         shinydashboard::notificationItem(text = e$message,
                                                                          icon = shiny::icon("triangle-exclamation"),
                                                                          status = "danger")
            )
          })
          return(NULL)
        })
      }
    })
  })
}
