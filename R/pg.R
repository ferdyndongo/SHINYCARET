#' Database User Interface with for inputs used to import a postgres database table
#' @param id module identifier
#' @return a Ui with 3 selectInput object
databaseInputUi <- function(id){
  shiny::tagList(
    shiny::selectInput(shiny::NS(id,"dsn"), "Select a Data Source Name", 
                       choices = c("",odbc::odbcListDataSources()$name),selected = NULL,selectize = FALSE),
    shiny::selectInput(shiny::NS(id,"schema"), "Select a Schema", choices = NULL,selected = NULL,selectize = FALSE),
    shiny::selectInput(shiny::NS(id,"dbtable"), "Select a Table", choices = NULL, selected = NULL,selectize = FALSE)
  )
}

#' Fill inputs needed into the databaseInputUi and import the postgres database table
#' @param id module identifier
#' @importFrom purrr %>% 
#' @return nothing
databaseInputServer <- function(id){
  shiny::moduleServer(id,function(input,output,session){
    dbcon <- shiny::reactive({
      shiny::req(input$dsn)
      if(!is.null(input$dsn)){
        if (DBI::dbCanConnect(drv = odbc::odbc(), input$dsn, timeout=10)){
          DBI::dbConnect(drv = odbc::odbc(), input$dsn, timeout=10)
        }else{
          stop("Data source name not found")
        }
      }
    })
    
    shiny::observeEvent(dbcon(),{
      schema_list <- odbc::dbGetQuery(conn = dbcon(),statement = "SELECT schema_name FROM information_schema.schemata")
      shiny::freezeReactiveValue(input,"schema")
      shiny::updateSelectInput(inputId = "schema", choices = c("",schema_list),selected = NULL)
    })
    
    table_list <- shiny::reactive({
      shiny::req(dbcon(),input$schema)
      DBI::dbListTables(conn = dbcon(),schema_name=input$schema)
    })
    
    shiny::observeEvent(table_list(),{
      shiny::freezeReactiveValue(input,"dbtable")
      shiny::updateSelectInput(inputId = "dbtable", choices = c("",table_list()),selected = NULL)
    })
    
    shiny::reactive({
      shiny::req(input$schema,input$dbtable)
      tryCatch({
        dplyr::tbl(src =dbcon(), dbplyr::in_schema(schema=input$schema, table = input$dbtable)) %>% dplyr::collect()
      },warning=function(w){
        shiny::showNotification(w,duration = NULL,closeButton = TRUE,type = "warning")
        output$warning <- shinydashboard::renderMenu({
          shinydashboard::dropdownMenu(type="notifications", .list=lapply(X = w,FUN = notificationItem)
                                       # shinydashboard::notificationItem(text = w$message,
                                       #                                  icon = shiny::icon("warning"),
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
      # dplyr::tbl(src =dbcon(), dbplyr::in_schema(schema=input$schema, table = input$dbtable)) %>% dplyr::collect()
    })
  })
}