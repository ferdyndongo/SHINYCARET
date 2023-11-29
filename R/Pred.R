#' Load a test sample of data from different sources and a fitted model 
#' then compute prediction test of the model
#' @return a small shiny app for random forest test and prediction
#' @importFrom shinydashboard dashboardHeader dashboardBody dashboardSidebar dashboardPage dropdownMenu box
#' @importFrom shiny shinyApp tabsetPanel tabPanel mainPanel fluidRow uiOutput

PredApp <- function(){
  
  ui <- dashboardPage(title = "GRANA PADANO CLASSIFIER",
                      header=dashboardHeader( # dashboardHeader ####
                                              title = "GRANA PADANO CLASSIFIER",titleWidth = 350,
                                              disable = FALSE,
                                              notificationUi("source")
                      ),
                      sidebar=dashboardSidebar(datasourceInputUi(id = "source"),width = 350),
                      body=dashboardBody(shiny::uiOutput("model"), shiny::uiOutput("test"))
  )
  
  server <- function(input, output, session){
    thematic::thematic_shiny()
    
    # import model or test sample ####
    dat <- datasourceInputServer(id = "source")
    shiny::observeEvent(dat(),{
      if(inherits(dat(),"train.formula")){
        model <- shiny::reactive(dat())
        output$model <- shiny::renderUI({
          shiny::req(dat())
          mdbInput("source")
        })
        data <- mdbServer("source")
        output$test <- shiny::renderUI({
          shiny::req(data(),model())
          shiny::tagList(
            dataVizUi("overview"),
            predTestUi("source"),
          )
        })
        dataVizOutput("overview",data)
        pred <- predTestOutput("source", model, data)
        downloadServer(id = "source", data = pred)
      }else if(inherits(dat(),"data.frame") & 
               apply(X = dat(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% any()){
        raw_index <- apply(X = dat(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% which()
        model <- shiny::reactive(unserialize(dat()[[raw_index]][[1]]))
        output$model <- shiny::renderUI({
          shiny::req(dat())
          mdbInput("source")
        })
        data <- mdbServer("source")
        output$test <- shiny::renderUI({
          shiny::req(data(),model)
          shiny::tagList(
            dataVizUi("overview"),
            predTestUi("source"),
          )
        })
        dataVizOutput("overview",data)
        pred <- predTestOutput("source", model, data)
        downloadServer(id = "source", data = pred)
      }else{
        output$model <- shiny::renderUI({
          shiny::req(dat())
          dataFileInput("source")
        })
        model <- dataFileServer("source")
        data <- shiny::reactive(dat())
        output$test <- shiny::renderUI({
          shiny::req(data(),model())
          shiny::tagList(
            dataVizUi("overview"),
            predTestUi("source"),
          )
        })
        dataVizOutput("overview",data)
        pred <- predTestOutput("source", model, data)
        downloadServer(id = "source", data = pred)
      }
    })
  }
  shinyApp(ui, server)
}