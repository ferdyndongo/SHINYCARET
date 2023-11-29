#' actionButton which writes into the database from an uploaded excel file
#' @param id module identifier
writeToDBUi <- function(id){
  shiny::tagList(
    shiny::actionButton(shiny::NS(id, "submit"),label = "upload to DB") 
  )
}

#' Function server in order to write into postgres database when the writeToDBUi button is clicked.
#' Overwrite a table is not considered, we only implements updating and creating table.
#' @param id module identifier
#' @param dat data to be written into the database
writeToDBServer <- function(id, dat){
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
      if(input$dbtable==""){
        shiny::req(input$dsn,input$schema)
        if(input$datasource=="RDS/model"){
          shiny::req(input$upload)
          id <- shiny::showNotification("uploading to db ...", duration = NULL, closeButton = FALSE)
          base::on.exit(shiny::removeNotification(id), add = TRUE)
          name <- stringr::str_remove_all(strsplit(x = input$upload$name,split = ".",fixed = TRUE)[[1]][1],"[ ]")
          tryCatch({
            write_to_db(input$dsn,data(),input$schema,name,overwrite=FALSE,append=FALSE)
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
        }else if(input$datasource=="Access/Excel"){
          shiny::req(input$mdb_load, input$sheet)
          id <- shiny::showNotification("uploading to db ...", duration = NULL, closeButton = FALSE)
          base::on.exit(shiny::removeNotification(id), add = TRUE)
          name <- stringr::str_remove_all(input$sheet,"[ ]")
          tryCatch({
            write_to_db(input$dsn,data(),input$schema,name,overwrite=FALSE,append=FALSE)
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
        }
        
      }else{
        shiny::req(input$dsn, input$schema, input$dbtable)
        if(input$datasource=="RDS/model" & !is.null(input$upload)){
          id <- shiny::showNotification("uploading to db ...", duration = NULL, closeButton = FALSE)
          base::on.exit(shiny::removeNotification(id), add = TRUE)
          name <- stringr::str_remove_all(input$dbtable,"[ ]")
          tryCatch({
            write_to_db(input$dsn,data(),input$schema,name,overwrite=FALSE,append=TRUE)
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
        }else if(input$datasource=="Access/Excel" & !is.null(input$mdb_load)){
          id <- shiny::showNotification("uploading to db ...", duration = NULL, closeButton = FALSE)
          base::on.exit(shiny::removeNotification(id), add = TRUE)
          name <- stringr::str_remove_all(input$dbtable,"[ ]")
          tryCatch({
            write_to_db(input$dsn,data(),input$schema,name,overwrite=FALSE,append=TRUE)
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
        }
      }
    })
  })
}
