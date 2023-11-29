#' A Ui selectInput object selecting the type of datasource and importing data in the App.
#' The supported datasources are: sqlite database and raw data file (csv, xls, xlsx, txt, tsv and mdb).
#' It is the Ui for the dataServer and they are linked by id to compose a module.
#' @param id module identifier
dataUi <- function(id){
  shiny::tagList(
    shiny::selectInput(shiny::NS(id,"datasource"),
                       label = "Pick a datasource",
                       choices = c("","database", "file"),
                       selected = NULL, selectize = FALSE),
    shiny::fluidRow(shiny::uiOutput(shiny::NS(id,"source")))
  )
}

#' Server function for dataUi.
#' @param id module identifier
dataServer <- function(id){
  shiny::moduleServer(id, function(input, output, session){
    
    output$source <- shiny::renderUI({
      shiny::req(input$datasource)
      if(input$datasource=="database"){
        sqliteDSNUi(id = "source")
      }else if(input$datasource=="file"){
        fileUi(id = "source")
      }
    })
    
    available_DSN <- shiny::reactive({
      shiny::req(input$dsn)
      DBI::dbCanConnect(drv = odbc::odbc(), dsn=input$dsn, timeout=10)
    })
    
    dbcon <- shiny::reactive({
      shiny::req(input$dsn)
      if(available_DSN()){
        DBI::dbConnect(drv = odbc::odbc(), dsn=input$dsn, timeout=10)
      }
    })
    
    output$warning <- shinydashboard::renderMenu({
      if(!available_DSN()){
        shiny::showNotification(attributes(available_DSN())$reason,duration = NULL,closeButton = TRUE,type = "error")
        shinydashboard::dropdownMenu(type="notifications", badgeStatus = "danger", icon = shiny::icon("database"),
                                     shinydashboard::notificationItem(text = stringr::str_c(input$dsn, "not found on file .odbc.ini", sep = " "),
                                                                      icon = shiny::icon("database"), status = "danger")
        )
      }else{
        if(purrr::is_empty(table_list())){
          shinydashboard::dropdownMenu(type="notifications", badgeStatus = "success", icon = shiny::icon("database"),
                                       shinydashboard::notificationItem(text = stringr::str_c(input$dsn,"is a empty database",sep = " "),
                                                                        icon=shiny::icon("table"), status = "warning")
          )
        }else{
          shinydashboard::dropdownMenu(type="notifications", badgeStatus = "success", icon = shiny::icon("database"),
                                       shinydashboard::notificationItem(text = stringr::str_c(input$dbtable,"database table",sep = " "),
                                                                        icon=shiny::icon("table"), status = "success")
          )
        }
      }
    })
    
    dbtable_list <- shiny::reactive({
      shiny::req(dbcon())
      DBI::dbListTables(conn = dbcon())
    })
    
    shiny::observeEvent(dbtable_list(),{
      shiny::freezeReactiveValue(input,"dbtable")
      shiny::updateSelectInput(inputId = "dbtable", choices = c("",dbtable_list()),selected = NULL)
    })
    
    table_list <- shiny::reactive({
      shiny::req(input$file_load$name, input$file_load$datapath)
      if(tools::file_ext(input$file_load$name) %in% c("xls","xlsx","mdb","db", "s3db")){
        sheet_list(input$file_load$name, input$file_load$datapath)
      }
    })
    
    output$file_tables <- shiny::renderUI({
      shiny::req(table_list())
      shiny::selectInput(shiny::NS(id,"sheet"),label = "Select sheet",
                         choices = c("",table_list()),selected = NULL,selectize = FALSE)
    })
    
    shiny::reactive({
      if(input$datasource=="database"){
        shiny::req(dbcon(),input$dbtable)
        tryCatch({
          dplyr::tbl(src =dbcon(), input$dbtable) %>% dplyr::collect()
        },warning=function(w){
          shiny::showNotification(w,duration = NULL,closeButton = TRUE,type = "warning")
          output$warning <- shinydashboard::renderMenu({
            shinydashboard::dropdownMenu(type="notifications",badgeStatus = "warning",icon = shiny::icon("database"), .list=lapply(X = w,FUN = notificationItem)
            )
          })
        },error=function(e){
          shiny::showNotification(e,duration = NULL,closeButton = TRUE,type = "error")
          output$warning <- shinydashboard::renderMenu({
            shinydashboard::dropdownMenu(type="notifications", badgeStatus = "danger",icon = shiny::icon("database")
                                         ,.list=lapply(X = e,FUN = notificationItem)
            )
          })
        })
        
      }else if(input$datasource=="file"){
        shiny::req(input$file_load)
        if(!is.null(table_list())){
          shiny::req(input$file_load, input$sheet)
          if(input$sheet %in% table_list()){
            tryCatch({
              load_file(input$file_load$name, input$file_load$datapath, input$sheet)
            },warning=function(w){
              shiny::showNotification(w,duration = NULL,closeButton = TRUE,type = "warning")
              output$warning <- shinydashboard::renderMenu({
                shinydashboard::dropdownMenu(type="notifications",badgeStatus = "warning", icon = shiny::icon("file"),
                                             .list=lapply(X = w,FUN = shinydashboard::notificationItem))
              })
            },error=function(e){
              shiny::showNotification(e,duration = NULL,closeButton = TRUE,type = "error")
              output$warning <- shinydashboard::renderMenu({
                shinydashboard::dropdownMenu(type="notifications",badgeStatus = "danger", icon = shiny::icon("file"),
                                             shinydashboard::notificationItem(text = e$message,
                                                                              icon = shiny::icon("file"),
                                                                              status = "danger")
                )
              })
            })
          }
        }else{
          load_file(input$file_load$name, input$file_load$datapath)
        }
      }
    })
  })
}
