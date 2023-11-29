#' A Ui selectInput object selecting the type of datasource and importing data in the App.
#' The supported datasources are: postgresql database, csv, excel and microsof acces files.
#' It is the Ui for the datasourceInputServer and they are linked by id to compose a module.
#' @param id module identifier
datasourceInputUi <- function(id){
  shiny::tagList(
    shiny::selectInput(shiny::NS(id,"datasource"),
                       label = "Pick a datasource",
                       choices = c("","database", "Access/Excel", "RDS/model"),
                       selected = NULL, selectize = FALSE),
    shiny::fluidRow(shiny::uiOutput(shiny::NS(id,"source")))
  )
}

#' Server function for dataSourceInputUi.
#' @param id module identifier
datasourceInputServer <- function(id){
  shiny::moduleServer(id, function(input, output, session){
    
    output$source <- shiny::renderUI({
      shiny::req(input$datasource)
      if(input$datasource=="database"){
        databaseInputUi(id = "source")
      }else if(input$datasource=="RDS/model"){
        dataFileInput(id = "source")
      }else if(input$datasource=="Access/Excel"){
        mdbInput(id = "source")
      }
    })
    
    available_DSN <- shiny::reactive({
      shiny::req(input$dsn)
      DBI::dbCanConnect(drv = odbc::odbc(), input$dsn, timeout=10)
    })
    
    dbcon <- shiny::reactive({
      shiny::req(input$dsn)
      if(available_DSN()){
        DBI::dbConnect(drv = odbc::odbc(), input$dsn, timeout=10)
      }
    })
    
    output$warning <- shinydashboard::renderMenu({
      if(!available_DSN()){
        shiny::showNotification("DSN NOT AVAILABLE",duration = NULL,closeButton = TRUE,type = "warning")
        shinydashboard::dropdownMenu(type="notifications",
                                     shinydashboard::notificationItem(text = "DSN NOT AVAILABLE",
                                                                      icon = shiny::icon("database"),
                                                                      status = "danger")
        )
      }
    })
    
    shiny::observeEvent(dbcon(),{
      schema_list <- shiny::reactive({
        odbc::dbGetQuery(conn = dbcon(),statement = "SELECT schema_name FROM information_schema.schemata")
      })
      shiny::updateSelectInput(inputId = "schema", choices = c("",schema_list()),selected = NULL)
    })
    
    table_list <- shiny::reactive({
      shiny::req(input$schema)
      if(!is.null(dbcon())) DBI::dbListTables(conn = dbcon(),schema_name=input$schema)
    })
    
    shiny::observeEvent(table_list(),{
      shiny::freezeReactiveValue(input,"dbtable")
      shiny::updateSelectInput(inputId = "dbtable", choices = c("",table_list()),selected = NULL)
    })
    
    mdb_list <- shiny::reactive({
      shiny::req(input$mdb_load$name, input$mdb_load$datapath)
      sheet_list(input$mdb_load$name, input$mdb_load$datapath)
    })
    
    output$mdb_tables <- shiny::renderUI({
      shiny::selectInput(shiny::NS(id,"sheet"),label = "Select sheet",
                         choices = c("",mdb_list()),selected = NULL,selectize = FALSE)
    })
    
    shiny::reactive({
      if(input$datasource=="database"){
        shiny::req(input$schema,input$dbtable)
        if(!is.null(dbcon())){
          tryCatch({
            dplyr::tbl(src =dbcon(), dbplyr::in_schema(schema=input$schema, table = input$dbtable)) %>% dplyr::collect()
          },warning=function(w){
            shiny::showNotification(w,duration = NULL,closeButton = TRUE,type = "warning")
            output$warning <- shinydashboard::renderMenu({
              shinydashboard::dropdownMenu(type="notifications", .list=lapply(X = w,FUN = notificationItem)
                                           # shinydashboard::notificationItem(text = w$message,
                                           #                                  icon = shiny::icon("database"),
                                           #                                  status = "warning")
              )
            })
          },error=function(e){
            shiny::showNotification(e,duration = NULL,closeButton = TRUE,type = "error")
            output$warning <- shinydashboard::renderMenu({
              shinydashboard::dropdownMenu(type="notifications",
                                           shinydashboard::notificationItem(text = e$message,
                                                                            icon = shiny::icon("database"),
                                                                            status = "danger")
              )
            })
          })
        }
        # dplyr::tbl(src =dbcon(), dbplyr::in_schema(schema=input$schema, table = input$dbtable)) %>% dplyr::collect()
      }else if(input$datasource=="RDS/model"){
        shiny::req(input$upload)
        tryCatch({
          load_file(input$upload$name, input$upload$datapath)
        },warning=function(w){
          shiny::showNotification(w,duration = NULL,closeButton = TRUE,type = "warning")
          output$warning <- shinydashboard::renderMenu({
            shinydashboard::dropdownMenu(type="notifications", .list=lapply(X = w,FUN = notificationItem))
          })
        },error=function(e){
          shiny::showNotification(e,duration = NULL,closeButton = TRUE,type = "error")
          output$warning <- shinydashboard::renderMenu({
            shinydashboard::dropdownMenu(type="notifications",
                                         shinydashboard::notificationItem(text = e$message,
                                                                          icon = shiny::icon("file"),
                                                                          status = "danger")
            )
          })
        })
        load_file(input$upload$name, input$upload$datapath)
      }else if(input$datasource=="Access/Excel"){
        shiny::req(input$mdb_load, input$sheet)
        if(input$sheet %in% mdb_list()){
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
                                                                            icon = shiny::icon("file"),
                                                                            status = "danger")
              )
            })
          })
          load_file(input$mdb_load$name, input$mdb_load$datapath, input$sheet)
        }
      }
    })
  })
}
