notificationsUi <- function(id){
  shinydashboard::dashboardHeader(
    shinydashboard::dropdownMenuOutput(shiny::NS(id,"message")),
    shinydashboard::dropdownMenuOutput(shiny::NS(id,"tasks")),
    shinydashboard::dropdownMenuOutput(shiny::NS(id,"notifications"))
  )
}

headerUi <- function(id, name){
  shinydashboard::dashboardHeader( title = name,
                   disable = FALSE,
                   # dropdownMenu(type = "messages"),
                   shinydashboard::dropdownMenuOutput(shiny::NS(id,"warning")),
                   # dropdownMenu(type = "notifications"),
                   shinydashboard::dropdownMenuOutput(shiny::NS(id,"notifications")),
                   # dropdownMenu(type = "tasks", badgeStatus = "success")
                   shinydashboard::dropdownMenuOutput(shiny::NS(id,"tasks"))
                   )
}

# headerServer <- function(id){
#   shiny::moduleServer(id, function( input, output, session){
#     
#   })
# }