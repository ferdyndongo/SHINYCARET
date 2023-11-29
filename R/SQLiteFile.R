#' Ui for file give to SQLiteServer function.
#' @param id module identifier
SQLiteFileUi <- function(id){
  shiny::tagList(
    shiny::fileInput(shiny::NS(id,"s3db_load"),label = "Choose SQLite Database file (.db or .s3db)",
                     accept = c(".db", ".s3db")
    ),
    shiny::uiOutput(shiny::NS(id,"s3db_tables")
    )
  )
}

#' Server function for SQLiteUi.
#' @param id module identifier
SQLiteFileServer <- function(id){
  shiny::moduleServer(id,function(input, output, session){
    
    table_list <- shiny::reactive({
      shiny::req(input$s3db_load$name, input$s3db_load$datapath)
      if(tools::file_ext(input$s3db_load$name) %in% c("db", "s3db")){
        sheet_list(input$s3db_load$name, input$s3db_load$datapath)
      }
    })
    
    output$s3db_tables <- shiny::renderUI({
      shiny::req(table_list())
      shiny::selectInput(shiny::NS(id,"s3db_table"),label = "table",
                         choices = c("",table_list()),selected = NULL,selectize = FALSE)
    })
    
  })
}

#' Function server in order to write into sqlite database file when the writeToDBUi button is clicked.
#' Overwrite a table is not considered, we only implements updating and creating table.
#' @param id module identifier
#' @param dat data to be written into the database
writeToSQLiteFile <- function(id, dat){
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
        if(!is.null(input$s3db_load)){
          if(is.null(input$s3db_table)){
            shiny::req(input$s3db_load, input$file_load)
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
              write_to_sqlite_file(dbname=input$s3db_load$name, tbname=tablename, data(), overwrite=FALSE, append=FALSE)
            },warning=function(w){
              shiny::showNotification(w,duration = NULL,closeButton = TRUE,type = "warning")
              output$warning <- shinydashboard::renderMenu({
                shinydashboard::dropdownMenu(type="notifications", badgeStatus = "warning",icon = shiny::icon("upload"),
                                             .list=lapply(X = w,FUN = notificationItem))
              })
            },error=function(e){
              shiny::showNotification(e,duration = NULL,closeButton = TRUE,type = "error")
              output$warning <- shinydashboard::renderMenu({
                shinydashboard::dropdownMenu(type="notifications", badgeStatus = "danger",icon = shiny::icon("upload"),
                                             .list=lapply(X = e,FUN = notificationItem)
                                             # shinydashboard::notificationItem(text = e$message,
                                             #                                  icon = shiny::icon("upload"),
                                             #                                  status = "danger")
                )
              })
            })
          }else{
            if(input$s3db_table==""){
              shiny::req(input$s3db_load, input$file_load)
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
                write_to_sqlite_file(dbname=input$s3db_load$name, tbname=tablename, data(), overwrite=FALSE, append=FALSE)
              },warning=function(w){
                shiny::showNotification(w,duration = NULL,closeButton = TRUE,type = "warning")
                output$warning <- shinydashboard::renderMenu({
                  shinydashboard::dropdownMenu(type="notifications", badgeStatus = "warning",icon = shiny::icon("upload"),
                                               .list=lapply(X = w,FUN = notificationItem))
                })
              },error=function(e){
                shiny::showNotification(e,duration = NULL,closeButton = TRUE,type = "error")
                output$warning <- shinydashboard::renderMenu({
                  shinydashboard::dropdownMenu(type="notifications", badgeStatus = "danger",icon = shiny::icon("upload"),
                                               .list=lapply(X = e,FUN = notificationItem)
                                               # shinydashboard::notificationItem(text = e$message,
                                               #                                  icon = shiny::icon("upload"),
                                               #                                  status = "danger")
                  )
                })
              })
            }else{
              shiny::req(input$s3db_load, input$s3db_table)
              tablename <- stringr::str_remove_all(input$s3db_table,"[ ]")
              id <- shiny::showNotification("uploading to db ...", duration = NULL, closeButton = FALSE)
              base::on.exit(shiny::removeNotification(id), add = TRUE)
              tryCatch({
                write_to_sqlite_file(dbname=input$s3db_load$name, tbname=tablename, data(), overwrite=FALSE, append=TRUE)
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
          }
          
        }else{
          shiny::showNotification("select the SQLite Database file where the data table will be written",duration = NULL,closeButton = TRUE,type = "error")
          output$warning <- shinydashboard::renderMenu({
            shinydashboard::dropdownMenu(type="notifications", badgeStatus = "warning",icon = shiny::icon("upload"),
                                         shinydashboard::notificationItem(text = "The SQLite Database can't be empty",
                                                                          icon = shiny::icon("upload"),
                                                                          status = "danger")
            )
          })
        }
        
      }
      
    })
  })
}
