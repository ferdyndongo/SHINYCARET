notificationUi <- function(id){
  shinydashboard::dropdownMenuOutput(shiny::NS(id,"warning"))
}

notificationServer <- function(id,expr){
  shiny::moduleServer(id,function(input,output,session){
    
    tryCatch({
      expr
    },warning=function(w){
      output$warning <- shinydashboard::renderMenu({
        shinydashboard::dropdownMenu(type="notifications", .list=lapply(X = w,FUN = notificationItem))
      })
    },error=function(e){
      output$warning <- shinydashboard::renderMenu({
        shinydashboard::dropdownMenu(type="notifications", .list=lapply(X = e,FUN = notificationItem))
      })
    })
    
  })
}
