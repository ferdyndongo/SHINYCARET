#' Graphical Exploration Analysis: Explore data with some plots and download a report.
#' @return a small shiny app using data module
#' @importFrom shinydashboard dashboardHeader dashboardBody dashboardSidebar dashboardPage dropdownMenu
#' @importFrom shiny shinyApp tabsetPanel tabPanel mainPanel uiOutput
plotVizApp <- function(){
  ui <- dashboardPage(header=dashboardHeader( # dashboardHeader ####
                                              title = "GEA Dashboard",
                                              disable = FALSE,
                                              dropdownMenu(type = "messages"),
                                              dropdownMenu(type = "notifications"),
                                              dropdownMenu(type = "tasks", badgeStatus = "success")),
                      sidebar=dashboardSidebar(datasourceInputUi(id = "source")),
                      body=dashboardBody(shiny::uiOutput("plotUi"),shiny::uiOutput("reportUi"))
  )
  
  
  server <- function(input, output, session){
    # thematic::thematic_shiny()
    
    data <- datasourceInputServer(id = "source")
    
    output$plotUi <- shiny::renderUI({
      shiny::req(data())
      shiny::tagList(
        plotVizUi(id = "source"),
        reportUi("source")
      )
    })
    
    shiny::observeEvent(data(),{
      plotVizServer("source", data)
    })
    
    reportServer(id = "source",report_script = "plotVizReport")
    
  }
  
  shinyApp(ui, server)
  
}