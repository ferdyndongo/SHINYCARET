#' Graphical Exploration Analysis: Explore data with some plots and download a report.
#' @return a small shiny app using data module
#' @importFrom shinydashboard dashboardHeader dashboardBody dashboardSidebar dashboardPage dropdownMenu
#' @importFrom shiny shinyApp tabsetPanel tabPanel mainPanel uiOutput
geApp <- function(){
  
  ui <- dashboardPage(title = "Graphical Exploration Analysis",
                      header=dashboardHeader( # dashboardHeader ####
                                              title = "GEA Dashboard",
                                              disable = FALSE,
                                              dropdownMenu(type = "messages"),
                                              dropdownMenu(type = "notifications"),
                                              dropdownMenu(type = "tasks", badgeStatus = "success")),
                      sidebar=dashboardSidebar(datasourceInputUi(id = "source")),
                      body=dashboardBody(
                        tabsetPanel(
                          tabPanel(value = "parcoord", title = "parallel plot",varSelectionUi("parcoord"), plotUi("parcoord")),
                          tabPanel(value = "box", title = "boxplot",varSelectionUi("box"), plotUi("box"))
                          )
                        )
                      )
  
  server <- function(input, output, session){
    thematic::thematic_shiny()
    
    data <- datasourceInputServer(id = "source")
    
    shiny::observeEvent(data(),{
      varSelectionServer("parcoord", data)
      varSelectionServer("box", data)
      parcoordPlotServer("parcoord", data)
      boxplotServer("box", data)
    })
    
  }
  
  shinyApp(ui, server)
}