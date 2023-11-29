#' Predict a model given classification model and prediction sample in input
#' @param model a given classification model
#' @param sample a sample to predict wit respect to the given classificatin model
classPrediction <- function(model, sample){
  
  if(inherits(model,"data.frame") && apply(X = model,MARGIN = 2,function(col){is.raw(col[[1]])}) %>% any()){
    raw_index <- apply(X = model,MARGIN = 2,function(col){is.raw(col[[1]])}) %>% which()
    model <- unserialize(model[[raw_index]][[1]])
  }
  
  if(inherits(model, "train.formula") && 
     (inherits(sample,"data.frame") && apply(X = sample,MARGIN = 2,function(col){!is.raw(col[[1]])}) %>% all())){
    # sample <- sample %>% data_preprocessing()
    na_index <- which(sample %>% dplyr::select(model$coefnames) %>% apply(MARGIN = 1,FUN = anyNA))
    varClass <- dimnames(attr(x = model$terms,which = "factors"))[[1]][1]
    if(!purrr::is_empty(na_index)){
      na_sample <- sample %>% dplyr::slice(na_index)
      sample <- sample %>% dplyr::slice(-na_index)
    }
    prob <- caret::predict.train(object = model, newdata = sample,type = "prob")
    if(varClass %in% colnames(sample)){
      sample[[paste0("predicted_",varClass)]] <- caret::predict.train(object = model, newdata = sample)
    }else{
      sample[[varClass]] <- caret::predict.train(object = model, newdata = sample)
    }
    
    if(purrr::is_empty(na_index)){
      return(sample)
    }else{
      return(list(sample=sample,na_sample=na_sample))
    }
    
  }
}

classPredUi <- function(id){
  shiny::tagList(
    shiny::h3("Predicted class"),
    DT::dataTableOutput(shiny::NS(id,"dataPred")),
    downloadUi("source"),
    shiny::h3("Not Predicted"),
    DT::dataTableOutput(shiny::NS(id,"na_sample"))
  )
}

classPredOutput <- function(id, pred){
  shiny::moduleServer(id, function(input,output,session){
    
    output$dataPred <- DT::renderDataTable({
      if("data.frame" %in% class(pred())){
        pred()
      }else if("list" %in% class(pred())){
        pred()$sample
      }
    })
    output$na_sample <- DT::renderDataTable({
      if("list" %in% class(pred())){
        pred()$na_sample
      }
    })
    
    output$download <- shiny::downloadHandler(
      filename = function(){
        if(!is.null(pred())){
          paste0(input$sheet,"Prediction",".xls")
        }else{
          paste0(input$sheet,"Prediction",".xls")
        }
      },
      content = function(file){
        if(!is.null(pred()) && inherits(pred(),"data.frame") && dim(pred())[1]>0 && 
           apply(X = pred(),MARGIN = 2,function(col){!is.raw(col[[1]])}) %>% all()){
          id <- shiny::showNotification("Downloading database table ...", duration = NULL, closeButton = FALSE)
          base::on.exit(shiny::removeNotification(id), add = TRUE)
          # WriteXLS::WriteXLS(pred(),file)
          writexl::write_xlsx(pred(),file)
        }else if("list" %in% class(pred())){
          # WriteXLS::WriteXLS(pred()$sample,file)
          writexl::write_xlsx(pred()$sample,file)
        }
      }
    )
    
  })
}

#' compute the prediction of classes for the given model and sample in input
#' @param id  module identifier
#' @param data1 sample or model loaded in input
#' @param data2 sample or model loaded in input
classPredServer <- function(id, data1, data2){
  shiny::moduleServer(id,function(input, output, session){
    
    shiny::reactive({
      shiny::req(data1(), data2())
      if(inherits(data1(),"train.formula")){# data1 is a saved .RDS model
        tryCatch({
          # classPrediction(data1(), data2() %>% GranaClassifier:::data_preprocessing())
          classPrediction(data1(), data2())
        },warning=function(w){
          shiny::showNotification(paste0("ERROR!!!",w$message),duration = NULL,closeButton = TRUE,type = "warning")
        },error=function(e){
          shiny::showNotification(paste0("ERROR!!!",e$message),duration = NULL,closeButton = TRUE,type = "error")
        })
        
      }else if(inherits(data1(),"data.frame") &&
               apply(X = data1(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% any()){# data1 is saved model in a sqlite database datatable
        raw_index <- apply(X = data1(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% which()
        model <- shiny::reactive(unserialize(data1()[[raw_index]][[1]]))
        tryCatch({
          # classPrediction(model(), data2() %>% GranaClassifier:::data_preprocessing())
          classPrediction(model(), data2())
        },warning=function(w){
          shiny::showNotification(paste0("ERROR!!!",w$message),duration = NULL,closeButton = TRUE,type = "warning")
        },error=function(e){
          shiny::showNotification(paste0("ERROR!!!",e$message),duration = NULL,closeButton = TRUE,type = "error")
        })
        
      }else{# otherwise data1 is a dataframe to test/predict
        if(inherits(data2(),"data.frame") && apply(X = data2(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% any()){# data2 is saved model in a sqlite database datatable
          raw_index <- apply(X = data2(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% which()
          model <- shiny::reactive(unserialize(data2()[[raw_index]][[1]]))
          tryCatch({
            # classPrediction(model(), data1() %>% GranaClassifier:::data_preprocessing())
            classPrediction(model(), data1())
          },warning=function(w){
            shiny::showNotification(paste0("ERROR!!!",w$message),duration = NULL,closeButton = TRUE,type = "warning")
          },error=function(e){
            shiny::showNotification(paste0("ERROR!!!",e$message),duration = NULL,closeButton = TRUE,type = "error")
          })
          
        }else if(inherits(data2(),"train.formula")){# data2 is a saved .RDS model
          tryCatch({
            # classPrediction(data2(), data1() %>% GranaClassifier:::data_preprocessing())
            classPrediction(data2(), data1())
          },warning=function(w){
            shiny::showNotification(paste0("ERROR!!!",w$message),duration = NULL,closeButton = TRUE,type = "warning")
          },error=function(e){
            shiny::showNotification(paste0("ERROR!!!",e$message),duration = NULL,closeButton = TRUE,type = "error")
          })
          
        }
      }
    })
    
  })
}
