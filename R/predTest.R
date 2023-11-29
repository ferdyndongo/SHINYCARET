
#' Ui for random forest model test
#' @param id module object identifier
#' @return a shiny tagList object with different Ui for output visualization
predTestUi <- function(id){
  shiny::tagList(
    # shiny::h3("Refererence Model"),
    # shiny::verbatimTextOutput(shiny::NS(id,"modelPrint")),
    # shiny::h3("Prediction Sample"),
    # DT::dataTableOutput(shiny::NS(id,"sample")),
    # shiny::h3("Predicted class"),
    # DT::dataTableOutput(shiny::NS(id,"data")),
    # downloadUi("source"),
    # shiny::h3("Not Predicted"),
    # DT::dataTableOutput(shiny::NS(id,"na_sample")),
    shiny::h3("Misclassified records"),
    DT::dataTableOutput(shiny::NS(id,"misdata")),
    shiny::h3("TEST METRICS"),
    shiny::verbatimTextOutput(shiny::NS(id,"confMatrix")),
    # shiny::h3("parallel plot of predicted classes"),
    # shiny::plotOutput(shiny::NS(id,"parcoord")),
    # shiny::fluidRow(shinydashboard::box(shiny::plotOutput(shiny::NS(id,"parcoord1"))),
    #                 shinydashboard::box(shiny::plotOutput(shiny::NS(id,"parcoord2")))),
  )
}

#' write the desired Random Forest Test output into the rf_testUi object
#' @param id module object identifier
#' @param model a fitted model object given by knnServer, rfServer etc....
#' @param sample the test sample used
#' @return output test results
#' @importFrom rlang .data
predTestOutput <- function(id, model, sample){
  shiny::moduleServer(id, function(input, output, session){
    
    object <- shiny::reactive({
      shiny::req(model())
      if(inherits(model(),"data.frame") &&
         apply(X = model(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% any()){
        raw_index <- apply(X = model(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% which()
        unserialize(model()[[raw_index]][[1]])
      }else if(inherits(model(),"train.formula")){
        model()
      }
    })
    
    test <- shiny::reactive({
      shiny::req(sample())
      if(inherits(sample(),"data.frame") && apply(X = sample(),MARGIN = 2,function(col){!is.raw(col[[1]])}) %>% all()){
        sample()
      }
    })
    
    na_index <- shiny::reactive({
      shiny::req(test())
      if(!is.null(test())){
        which(test() %>% dplyr::select(object()$coefnames) %>% apply(MARGIN = 1,FUN = anyNA))
      }
    })
    
    test_sample <- shiny::reactive({
      if(!is.null(test()) & !purrr::is_empty(na_index())){
        test() %>% dplyr::slice(-na_index())
      }else if(!is.null(test()) & purrr::is_empty(na_index())){
        test()
      }
    })
    
    shiny::observeEvent(list(object(),test_sample()),{
      output$modelPrint <- shiny::renderPrint({
        shiny::req(object())
        if(!is.null(object()) && inherits(object(),"train.formula")){
          caret::print.train(object())
        }
      })
      output$sample <- DT::renderDataTable({
        shiny::req(test_sample())
        test_sample()
      })
    })
    
    output$warning <- shinydashboard::renderMenu({
      shiny::req(sample())
      if(!(inherits(sample(), "data.frame") && inherits(object(),"train.formula"))){
        shiny::showNotification("WRONG DATA TYPE TO PREDICT!!!",duration = NULL,closeButton = TRUE,type = "error")
        shinydashboard::dropdownMenu(type="notifications",badgeStatus = "warning", icon = shiny::icon("file"),
                                     shinydashboard::notificationItem(text = "wrong data type",
                                                                      icon = shiny::icon("file"),
                                                                      status = "danger")
        )
      }else if(inherits(sample(),"data.frame") &&
               apply(X = sample(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% any()){
        shiny::showNotification("WRONG DATA TYPE TO PREDICT!!!",duration = NULL,closeButton = TRUE,type = "error")
        shinydashboard::dropdownMenu(type="notifications",badgeStatus = "warning", icon = shiny::icon("file"),
                                     shinydashboard::notificationItem(text = "wrong data type",
                                                                      icon = shiny::icon("file"),
                                                                      status = "danger")
        )
      }
    })
    
    output$na_sample <- DT::renderDataTable({
      if(!is.null(test()) & !purrr::is_empty(na_index())){
        test() %>% dplyr::slice(na_index())
      }
    })
    
    # chiamare la predicted_class come era chiamata nel modello di referimento
    varClass <- shiny::reactive(dimnames(attr(x = object()$terms,which = "factors"))[[1]][1])
    # varClass <- names(attr(x = object()$terms,which = "dataClasses"))[1]
    
    data <- shiny::reactive({
      if(!(is.null(test_sample()) | is.null(object()))){
        predicted_data <- base::list()
        prob <- caret::predict.train(object = object(), newdata = test_sample(),type = "prob")
        if(varClass() %in% colnames(test_sample())){
          predicted_data[[paste0("predicted_",varClass())]] <- caret::predict.train(object = object(), newdata = test_sample())
          # %>% dplyr::select(c(object()$coefnames,dplyr::all_of(varClass()),paste0("predicted_",varClass())))
        }else{
          predicted_data[[varClass()]] <- caret::predict.train(object = object(), newdata = test_sample())
          # test_sample() %>% dplyr::bind_cols(predicted_data) 
          # %>% dplyr::select(c(object()$coefnames,dplyr::all_of(varClass())))
        }
        test_sample() %>% dplyr::bind_cols(predicted_data,prob)
      }
    })
    
    output$data <- DT::renderDataTable({
      if(! (is.null(data()) | purrr::is_empty(data())) ) data()
    })
    
    output$confMatrix <- shiny::renderPrint({
      if(!is.null(data())){
        if(varClass() %in% colnames(test_sample())){
          caret::confusionMatrix(data()[[paste0("predicted_",varClass())]],
                                 factor(data()[[varClass()]]),
                                 mode="everything")
        }
      }
    })
    
    output$misdata <- DT::renderDataTable({
      if(!is.null(data())){
        if(varClass() %in% colnames(test_sample())){
          data() %>% dplyr::filter(data()[[varClass()]]!=data()[[paste0("predicted_",varClass())]]) 
          # %>% dplyr::select(c(object()$coefnames,dplyr::all_of(varClass()),paste0("predicted_",varClass())))
        }
      }
    })
    
    # output$parcoord <- shiny::renderPlot({
    #   if(!(is.null(test_sample()) | is.null(object()) | is.null(input$catVar))){
    #     if(input$catVar==""){
    #       GGally::ggparcoord(data() ,columns =which(colnames(data()) %in% object()$coefnames),groupColumn = length(colnames(data())))
    #     }else{
    #       GGally::ggparcoord(data() ,columns =which(colnames(data()) %in% object()$coefnames),groupColumn = length(colnames(data()))) +
    #         ggplot2::facet_wrap(~data()[[input$catVar]])
    #     }
    #   }
    # })
    return(data)
    
  })
}

#' write the desired Random Forest Test output into the rf_testUi object
#' @param id module object identifier
#' @param data1 data read from application
#' @param data2 data read from application
#' @param pred  predicted data from classPredServer
#' @importFrom rlang .data
predTestServer <- function(id, data1, data2, pred){
  shiny::moduleServer(id, function(input, output, session){
    
    # look for the classification variable in the reference model
    varClass <- shiny::reactive({
      if(inherits(data1(),"train.formula")){
        dimnames(attr(x = data1()$terms,which = "factors"))[[1]][1]
      }else if(inherits(data2(),"train.formula")){
        dimnames(attr(x = data2()$terms,which = "factors"))[[1]][1]
      }else if(inherits(data2(),"data.frame") && apply(X = data2(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% any()){
        raw_index <- apply(X = data2(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% which()
        raw_model <- shiny::reactive(unserialize(data2()[[raw_index]][[1]]))
        dimnames(attr(x = raw_model()$terms,which = "factors"))[[1]][1]
      }else if(inherits(data1(),"data.frame") &&
               apply(X = data1(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% any()){# data is saved model in a sqlite database datatable
        raw_index <- apply(X = data1(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% which()
        raw_model <- shiny::reactive(unserialize(data1()[[raw_index]][[1]]))
        dimnames(attr(x = raw_model()$terms,which = "factors"))[[1]][1]
      }
    })
    
    # look for the levels into the classification variable in the reference model
    levClass <- shiny::reactive({
      if(inherits(data1(),"train.formula")){
        data1()$levels
      }else if(inherits(data2(),"train.formula")){
        data2()$levels
      }else if(inherits(data2(),"data.frame") && apply(X = data2(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% any()){
        raw_index <- apply(X = data2(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% which()
        raw_model <- shiny::reactive(unserialize(data2()[[raw_index]][[1]]))
        raw_model()$levels
      }else if(inherits(data1(),"data.frame") &&
               apply(X = data1(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% any()){# data is saved model in a sqlite database datatable
        raw_index <- apply(X = data1(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% which()
        raw_model <- shiny::reactive(unserialize(data1()[[raw_index]][[1]]))
        raw_model()$levels
      }
    })
    
    output$confMatrix <- shiny::renderPrint({
      if(! (is.null(pred()) || is.null(varClass())) ){
        if("data.frame" %in% class(pred())){
          if(!is.null(pred()[[paste0("predicted_",varClass())]])){
            if(varClass() %in% colnames(pred())){
              tryCatch({
                caret::confusionMatrix(pred()[[paste0("predicted_",varClass())]],
                                       factor(x = pred()[[varClass()]],levels = levClass()),
                                       # factor(pred()[[varClass()]]),
                                       mode="everything")
              },warning=function(w){
                shiny::showNotification(paste("ERROR!!!",w$message,sep = "\n"),duration = NULL,closeButton = TRUE,type = "warning")
                return(w$message)
              },error=function(e){
                shiny::showNotification(paste("ERROR!!!",e$message,sep = "\n"),duration = NULL,closeButton = TRUE,type = "error")
                return(e$message)
              })
              
            }
          }
        }else if("list" %in% class(pred())){
          if(!is.null(pred()$sample[[paste0("predicted_",varClass())]])){
            if(varClass() %in% colnames(pred()$sample)){
              tryCatch({
                caret::confusionMatrix(pred()$sample[[paste0("predicted_",varClass())]],
                                       factor(x = pred()$sample[[varClass()]],levels = levClass()),
                                       # factor(pred()$sample[[varClass()]]),
                                       mode="everything")
              },warning=function(w){
                shiny::showNotification(paste("ERROR!!!",w$message,sep = "\n"),duration = NULL,closeButton = TRUE,type = "warning")
                return(w$message)
              },error=function(e){
                shiny::showNotification(paste("ERROR!!!",e$message,sep = "\n"),duration = NULL,closeButton = TRUE,type = "error")
                return(e$message)
              })
              
            }
          }
        }
      }
    })
    
    output$misdata <- DT::renderDataTable({
      if(! (is.null(pred()) || is.null(varClass())) ){
        if("data.frame" %in% class(pred())){
          if(!is.null(pred()[[paste0("predicted_",varClass())]])){
            if(varClass() %in% colnames(pred())){
              pred() %>% dplyr::filter(pred()[[varClass()]]!=pred()[[paste0("predicted_",varClass())]]) 
            }
          }
        }else if("list" %in% class(pred())){
          if(!is.null(pred()$sample[[paste0("predicted_",varClass())]])){
            if(varClass() %in% colnames(pred()$sample)){
              pred()$sample %>% dplyr::filter(pred()$sample[[varClass()]]!=pred()$sample[[paste0("predicted_",varClass())]]) 
            }
          }
        }
      }
    })
    
    
    # output$parcoord <- shiny::renderPlot({
    #   if(!(is.null(test_sample()) | is.null(object()) | is.null(input$catVar))){
    #     if(input$catVar==""){
    #       GGally::ggparcoord(data() ,columns =which(colnames(data()) %in% object()$coefnames),groupColumn = length(colnames(data())))
    #     }else{
    #       GGally::ggparcoord(data() ,columns =which(colnames(data()) %in% object()$coefnames),groupColumn = length(colnames(data()))) +
    #         ggplot2::facet_wrap(~data()[[input$catVar]])
    #     }
    #   }
    # })
    
  })
}

