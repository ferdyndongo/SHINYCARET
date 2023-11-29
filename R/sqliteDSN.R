#' Ui for sqlite connection to .db files
#' @param id  module identifier
sqliteDSNUi <- function(id){
  shiny::tagList(
    shiny::selectInput(shiny::NS(id,"dsn"), "Select a Data Source Name",
                       choices = c("",odbc::odbcListDataSources()$name),selected = NULL,selectize = FALSE),
    shiny::selectInput(shiny::NS(id,"dbtable"), "Select a Table", choices = NULL, selected = NULL,selectize = FALSE)
  )
}

#' server function for sqliteUi.
#' @param id module identifier
sqliteDSNServer <- function(id){
  shiny::moduleServer(id,function(input,output,session){
    
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
                                     shinydashboard::notificationItem(text = attributes(available_DSN())$reason,
                                                                      icon = shiny::icon("database"), status = "danger")
        )
      }else{
        if(purrr::is_empty(table_list())){
          shiny::showNotification(stringr::str_c(input$dsn,"is an empty database",sep = " "),duration = NULL,closeButton = TRUE,type = "warning")
          shinydashboard::dropdownMenu(type="notifications", badgeStatus = "success", icon = shiny::icon("database"),
                                       shinydashboard::notificationItem(text = stringr::str_c(input$dsn,"is an empty database",sep = " "),
                                                                        icon=shiny::icon("table"), status = "warning")
          )
        }else{
          shinydashboard::dropdownMenu(type="notifications", badgeStatus = "success", icon = shiny::icon("database"),
                                       shinydashboard::notificationItem(text = stringr::str_c(input$dbtable,"table in ",input$dsn,sep = " "),
                                                                        icon=shiny::icon("table"), status = "success")
          )
        }
      }
    })
    
    table_list <- shiny::reactive({
      if(!is.null(dbcon())){
        DBI::dbListTables(conn = dbcon())
      }
    })
    
    shiny::observeEvent(table_list(),{
      shiny::freezeReactiveValue(input,"dbtable")
      shiny::updateSelectInput(inputId = "dbtable", choices = c("",table_list()),selected = NULL)
      odbc::dbDisconnect(dbcon())
    })
  })
}

#' Function server in order to write into sqlite database when the writeToDBUi button is clicked.
#' Overwrite a table is not considered, we only implements updating and creating table.
#' @param id module identifier
#' @param dat data to be written into the database
writeToSQLiteDSN <- function(id, dat){
  shiny::moduleServer(id, function(input, output, server){
    
    data <- shiny::reactive({
      shiny::req(dat())
      if(inherits(dat(), "data.frame")){
        dat()
      }else if(inherits(dat(), "train.formula")){
        rawdatamodel <- serialize(object = dat(),connection = NULL)
        data.frame(type=dat()$modelType,method=dat()$method,
                   date=stringr::str_replace_all(paste(Sys.Date()),"-","_"),
                   rawdatamodel=I(list(rawdatamodel)))
      }
    })
    
    shiny::observeEvent(input$submit,{
      shiny::req(input$submit)
      if (!is.null(data())){
        if(input$dsn!=""){
          if(!is.null(input$dbtable) && input$dbtable==""){
            shiny::req(input$dsn, input$file_load)
            ext <- tools::file_ext(input$file_load$name)
            tablename <- base::switch(ext,
                                      RDS = stringr::str_remove_all(strsplit(x = input$file_load$name,split = ".",fixed = TRUE)[[1]][1],"[ ]"),
                                      csv = stringr::str_remove_all(strsplit(x = input$file_load$name,split = ".",fixed = TRUE)[[1]][1],"[ ]"),
                                      txt = stringr::str_remove_all(strsplit(x = input$file_load$name,split = ".",fixed = TRUE)[[1]][1],"[ ]"),
                                      tsv = stringr::str_remove_all(strsplit(x = input$file_load$name,split = ".",fixed = TRUE)[[1]][1],"[ ]"),
                                      xls = stringr::str_remove_all(input$sheet,"[ ]"),
                                      xlsx = stringr::str_remove_all(input$sheet,"[ ]"),
                                      mdb = stringr::str_remove_all(input$sheet,"[ ]"),
                                      db = stringr::str_remove_all(input$sheet,"[ ]"),
                                      s3db = stringr::str_remove_all(input$sheet,"[ ]")
            )
            id <- shiny::showNotification("uploading to db ...", duration = NULL, closeButton = FALSE)
            base::on.exit(shiny::removeNotification(id), add = TRUE)
            tryCatch({
              file_to_sqlite_dsn(input$dsn, data(), tablename, overwrite=FALSE, append=FALSE)
            },warning=function(w){
              shiny::showNotification(w$message,duration = NULL,closeButton = TRUE,type = "warning")
              output$warning <- shinydashboard::renderMenu({
                shinydashboard::dropdownMenu(type="notifications", badgeStatus = "warning",icon = shiny::icon("upload"),
                                             .list=lapply(X = w,FUN = notificationItem))
              })
            },error=function(e){
              shiny::showNotification(e$message,duration = NULL,closeButton = TRUE,type = "error")
              output$warning <- shinydashboard::renderMenu({
                shinydashboard::dropdownMenu(type="notifications", badgeStatus = "danger",icon = shiny::icon("upload"),
                                             .list=lapply(X = e,FUN = notificationItem)
                                             # shinydashboard::notificationItem(text = e$message,
                                             #                                  icon = shiny::icon("upload"),
                                             #                                  status = "danger")
                )
              })
            })
          }else if(!is.null(input$dbtable) && input$dbtable!=""){
            shiny::req(input$dsn, input$dbtable)
            tablename <- stringr::str_remove_all(input$dbtable,"[ ]")
            id <- shiny::showNotification("uploading to db ...", duration = NULL, closeButton = FALSE)
            base::on.exit(shiny::removeNotification(id), add = TRUE)
            tryCatch({
              file_to_sqlite_dsn(input$dsn,data(),tablename,overwrite=FALSE,append=TRUE)
            },warning=function(w){
              shiny::showNotification(w,duration = NULL,closeButton = TRUE,type = "warning")
              output$warning <- shinydashboard::renderMenu({
                shinydashboard::dropdownMenu(type="notifications", badgeStatus = "warning",icon = shiny::icon("upload"),
                                             .list=lapply(X = w,FUN = notificationItem))
              })
            },error=function(e){
              shiny::showNotification(e,duration = NULL,closeButton = TRUE,type = "error")
              output$warning <- shinydashboard::renderMenu({
                shinydashboard::dropdownMenu(type="notifications", badgeStatus = "warning",icon = shiny::icon("upload"),
                                             shinydashboard::notificationItem(text = e$message,
                                                                              icon = shiny::icon("upload"),
                                                                              status = "danger")
                )
              })
            })
            
          }
        }else{
          shiny::showNotification("select the dsn where the data table will be written",duration = NULL,closeButton = TRUE,type = "error")
          output$warning <- shinydashboard::renderMenu({
            shinydashboard::dropdownMenu(type="notifications", badgeStatus = "warning",icon = shiny::icon("upload"),
                                         shinydashboard::notificationItem(text = "The DSN input can't be empty",
                                                                          icon = shiny::icon("upload"),
                                                                          status = "danger")
            )
          })
        }
      }
      
    })
  })
}
