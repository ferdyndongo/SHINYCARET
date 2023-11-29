#' Load a test sample of data from different sources and a fitted model 
#' then compute prediction test of the model
#' @return a small shiny app for random forest test and prediction
#' @importFrom shinydashboard dashboardHeader dashboardBody dashboardSidebar dashboardPage dropdownMenu box
#' @importFrom shiny shinyApp tabsetPanel tabPanel mainPanel fluidRow uiOutput

PredictionApp <- function(){
  options(shiny.maxRequestSize = 200 * 1024^2)
  
  ui <- dashboardPage(title = "Model Test/Predict",
                      header=dashboardHeader( # dashboardHeader ####
                                              title = "MODEL TEST/PREDICT",titleWidth = 350,
                                              disable = FALSE,
                                              notificationUi("source")
                      ),
                      sidebar=dashboardSidebar(fileUi(id = "source"),width = 350),
                      body=dashboardBody(shiny::uiOutput("dataViz"),shiny::uiOutput("source2"), shiny::uiOutput("test")
                      )
  )
  
  server <- function(input, output, session){
    thematic::thematic_shiny()
    
    # import model or test sample and visualize it ####
    data1 <- fileServer(id = "source")
    output$dataViz <- shiny::renderUI({
      shiny::req(data1())
      dataVizUi("dataViz")
    })
    dataVizOutput("dataViz",data1)
    output$source2 <- shiny::renderUI({
      shiny::req(data1())
      fileUi("source2")
    })
    data2 <- fileServer("source2")
    output$test <- shiny::renderUI({
      shiny::req(data1(),data2())
      shiny::tagList(
        predTestUi("source"),
        # reportUi("source"),
      )
    })
    # imported data are  one of 3 options:  a saved .RDS model, a saved model in a database datable or a dataframe 
    shiny::observeEvent(list(data1(),data2()),{
      if(inherits(data1(),"train.formula")){# dat is a saved .RDS model
        pred <- predTestOutput("source", data1, data2)
      }else if(inherits(data1(),"data.frame") &&
               apply(X = data1(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% any()){# data is saved model in a sqlite database datatable
        raw_index <- apply(X = data1(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% which()
        model <- shiny::reactive(unserialize(data1()[[raw_index]][[1]]))
        pred <- predTestOutput("source", model, data2)
      }else{# otherwise dat is a dataframe to test/predict
        pred <- predTestOutput("source", data2, data1)
      }
      filedownServer(id = "source", data = pred)
      # reportServer(id = "source",report_script = "TestReport")
    })
    
  }
  shinyApp(ui, server)
}