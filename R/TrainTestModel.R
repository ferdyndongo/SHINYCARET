#' Ui container for classification model Training and Test
#' @param id  module identifier.
TrainTestModelUi <- function(id){
  shiny::tagList(
    shiny::h3("FINAL MODEL"),
    shiny::verbatimTextOutput(shiny::NS(id,"object")),
    shiny::verbatimTextOutput(shiny::NS(id,"finalModel")),
    shiny::downloadButton(shiny::NS(id, "download")),
    shiny::h3("PREDICTIONS ON TEST SAMPLE"),
    DT::dataTableOutput(shiny::NS(id,"dataPred")),
    shiny::h3("SAMPLE NOT PREDICTED"),
    DT::dataTableOutput(shiny::NS(id,"na_sample")),
    shiny::h3("TEST METRICS"),
    shiny::verbatimTextOutput(shiny::NS(id,"confMatrix")),
    shiny::h3("MISCLASSIFIED SAMPLE"),
    DT::dataTableOutput(shiny::NS(id,"misdata")),
  )
}


#' Train and Test classification models such as RF, GBM, XGBTREE
#' @param id module identifier
#' @param data training data used by models
#' @return a list of two objects: the fitted model, and the predictions on test sample
TrainTestModelServer <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    
    trainIndex <- shiny::reactive({
      shiny::req(data(), input$catVar)
      tryCatch({
        set.seed(12345)
        caret::createDataPartition(y = data()[[input$catVar]], p = 0.8, list = FALSE, times = 1)
      },warning=function(w){
        shiny::showNotification(w$message,duration = NULL,closeButton = TRUE,type = "warning")
        set.seed(12345)
        caret::createDataPartition(y = data()[[input$catVar]], p = 0.8, list = FALSE, times = 1)
      },error=function(e){
        shiny::showNotification(e$message,duration = NULL,closeButton = TRUE,type = "error")
        return(NULL)
      })
    })
    
    trainSample <- shiny::reactive({
      shiny::req(trainIndex())
      data()[trainIndex(),]
    })
    
    testSample <- shiny::reactive({
      shiny::req(trainIndex())
      data()[-trainIndex(),]
    })
    
    model_available <- shiny::reactive({
      shiny::req(input$catVar, input$caretModel)
      if(all(caret::getModelInfo()[[input$caretModel]]$library %in% utils::installed.packages())){
        TRUE
      }else{
        not_installed_index <- which(!(caret::getModelInfo()[[input$caretModel]]$library %in% utils::installed.packages()))
        not_installed_library <- caret::getModelInfo()[[input$caretModel]]$library[not_installed_index]
        msg <- paste0("Required packages are missing: ", paste0(not_installed_library,collapse = ", "))
        shiny::showNotification(msg,duration = NULL,closeButton = TRUE,type = "error")
        FALSE
      }
    })
    
    model <- shiny::reactive({
      shiny::req(trainSample())
      if(!is.null(trainSample()) && model_available()){
        shiny::req(input$catVar, input$caretModel)
        id <- shiny::showNotification("MODEL TRAINGING IN PROGRESS ...", duration = NULL, closeButton = FALSE)
        base::on.exit(shiny::removeNotification(id), add = TRUE)
        fitted_model <- fit_class(data = trainSample(), catVar = input$catVar, model=input$caretModel)
        if(!is.null(fitted_model)){
          colnames(fitted_model$trainingData)[which(colnames(fitted_model$trainingData)==".outcome")] <- input$catVar
          fitted_model
        }else{
          fitted_model <- fit_class_na(data = trainSample(), catVar = input$catVar, model=input$caretModel)
          if(!is.null(fitted_model)){
            colnames(fitted_model$trainingData)[which(colnames(fitted_model$trainingData)==".outcome")] <- input$catVar
            fitted_model
          }
        }
      }
    })
    
    pred <- shiny::reactive({
      shiny::req(model(), testSample())
      classPrediction(model(), testSample())
    })
    
    return(list(model=shiny::reactive(model()),
                pred=shiny::reactive(pred()))
    )
    
  })
}


#' Write the results outputs given by TrainTestModelServer in TrainTestModelUi
#' @param id module identifier
#' @param data list of objects given by TrainTestModelServer containing the fitted model from training and the predictions from test 
TrainTestModelOutput <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    model <- shiny::reactive({
      shiny::req(data)
      data$model()
    })
    
    pred <- shiny::reactive({
      data$pred()
    })
    
    output$finalModel <- shiny::renderPrint({
      shiny::req(model())
      model()$finalModel
    })
    
    output$object <- shiny::renderPrint({
      shiny::req(model())
      caret::print.train(model())
    })
    
    output$dataPred <- DT::renderDataTable({
      shiny::req(pred())
      if("data.frame" %in% class(pred())){
        pred()
      }else if("list" %in% class(pred())){
        pred()$sample
      }
    })
    
    output$na_sample <- DT::renderDataTable({
      shiny::req(pred())
      if("list" %in% class(pred())){
        pred()$na_sample
      }
    })
    
    output$misdata <- DT::renderDataTable({
      shiny::req(pred(), input$catVar)
      if("data.frame" %in% class(pred())){
        pred() %>% dplyr::filter(pred()[[input$catVar]]!=pred()[[paste0("predicted_",input$catVar)]])
      }else if("list" %in% class(pred())){
        pred()$sample %>% dplyr::filter(pred()$sample[[input$catVar]]!=pred()$sample[[paste0("predicted_",input$catVar)]])
      }
    })
    
    output$confMatrix <- shiny::renderPrint({
      shiny::req(pred(),input$catVar)
      if(inherits(pred(),"data.frame")){
        tryCatch({
          caret::confusionMatrix(data = pred()[[paste0("predicted_",input$catVar)]],
                                 reference=factor(pred()[[input$catVar]]),
                                 mode="everything")
        },warning=function(w){
          shiny::showNotification(w$message,duration = NULL,closeButton = TRUE,type = "warning")
          caret::confusionMatrix(data = pred()[[paste0("predicted_",input$catVar)]],
                                 reference=factor(pred()[[input$catVar]]),
                                 mode="everything")
        },error=function(e){
          shiny::showNotification(e$message,duration = NULL,closeButton = TRUE,type = "error")
          caret::confusionMatrix(data = pred()[[paste0("predicted_",input$catVar)]],
                                 reference=factor(pred()[[input$catVar]],levels = model()$levels),
                                 mode="everything")
        })
        
      }else if("list" %in% class(pred())){
        tryCatch({
          caret::confusionMatrix(data = pred()$sample[[paste0("predicted_",input$catVar)]],
                                 reference=factor(pred()$sample[[input$catVar]]),
                                 mode="everything")
        },warning=function(w){
          shiny::showNotification(w$message,duration = NULL,closeButton = TRUE,type = "warning")
          caret::confusionMatrix(data = pred()$sample[[paste0("predicted_",input$catVar)]],
                                 reference=factor(pred()$sample[[input$catVar]]),
                                 mode="everything")
        },error=function(e){
          shiny::showNotification(e$message,duration = NULL,closeButton = TRUE,type = "error")
          caret::confusionMatrix(data = pred()[[paste0("predicted_",input$catVar)]],
                                 reference=factor(pred()[[input$catVar]],levels = model()$levels),
                                 mode="everything")
        })
        
      }
    })
    
  })
}

