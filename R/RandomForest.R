#' Retrieve data from different sources and train a Random Forest Model
#' @return a small shiny WebApp 
#' @importFrom shinydashboard dashboardHeader dashboardBody dashboardSidebar dashboardPage dropdownMenu box
#' @importFrom shiny shinyApp tabsetPanel tabPanel mainPanel fluidRow uiOutput renderUI
RandomForestApp <- function(){
  
  ui <- dashboardPage(title = "GRANA CLASSIFIER TRAINING",
                      header=dashboardHeader( # dashboardHeader ####
                                              title = "GRANA CLASSIFIER TRAINING",titleWidth = 350,
                                              disable = FALSE,
                                              notificationUi("source")
                      ),
                      sidebar=dashboardSidebar(fileUi(id = "source"),shiny::uiOutput("target"),width = 350),
                      body=dashboardBody(shiny::uiOutput("dataOverview"), shiny::uiOutput("train"))
  )
  
  server <- function(input, output, session){
    thematic::thematic_shiny()
    
    
    rawdata <- fileServer(id = "source")
    data <- shiny::reactive({
      if(!is.null(rawdata())){
        if(inherits(rawdata(),"data.frame") && apply(X = rawdata(),MARGIN = 2,function(col){!is.raw(col[[1]])}) %>% all()){
          rawdata()
        }
      }
    })
    shiny::observeEvent(data(),{
      dataOverview <- dataVizUi("overview")
      dataVizOutput("source",data)
    })
    
    output$target <- shiny::renderUI({
      shiny::req(data())
      catVarUi("source")
    })
    shiny::observeEvent(data(),{
      shiny::freezeReactiveValue(input,"source-catVar")
      catVarServer("source",data)
    })
    
    fitted_model <- rfServer("source",data)
    
    output$train <- shiny::renderUI({
      shiny::req(fitted_model())
      if(inherits(fitted_model(),"train.formula")){
        shiny::tagList(
          trainingUi("source"),
          shiny::fluidRow(shinydashboard::box(downloadUi("source")),
                          shinydashboard::box(sqliteDSNUi("source"),
                                              writeToDBUi("source"))
          )
        )
      }
    })
    trainingOutput("source",fitted_model)
    filedownServer("source",fitted_model)
    sqliteDSNServer("source")
    writeToSQLiteDSN("source", fitted_model)
  }
  
  shinyApp(ui, server)
  
}
