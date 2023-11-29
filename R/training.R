#' Train and fit predictive models of classification and regression from the caret package.
#' @param data a dataset with explanatory variables and response variable
#' @param catVar the response variable
#' @param model A string specifying which classification or regression model to use. 
#' Possible values are found using names(getModelInfo())
#' @param preprocess a character vector specifying the pre-processing methods to be used.
#' @return a fitted knn model
trainModel <- function(data, catVar, model, preprocess=NULL){
  if(!( is.null(data) | is.null(catVar) | is.null(model) )){
    
    ## train and fit the model
    ctrl_fit <- caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75)
    if(base::is.null(preprocess) | purrr::is_empty(preprocess)){
      set.seed(1234)
      caret::train(formula(paste(catVar,"~ .")), data, method=model, trControl=ctrl_fit,verbosity=0)
    }else{
      set.seed(1234)
      caret::train(formula(paste(catVar,"~ .")), data,  method=model, trControl=ctrl_fit, verbosity=0,
                   preProcess=preprocess)
    }
  }
}



trainingUi <- function(id){
  shiny::tagList(
    shiny::verbatimTextOutput(shiny::NS(id,"finalModel")),
    shiny::verbatimTextOutput(shiny::NS(id,"object"))
  )
}

#' write the output of random forest fitted model into the rfTrainingUi
#' @param id module identifier
#' @param object a fitted model object given by knnServer, rfServer etc....

trainingOutput <- function(id, object){
  shiny::moduleServer(id, function(input, output, session){
    
    output$finalModel <- shiny::renderPrint({
      if(!is.null(object()) & inherits(object(),"train.formula")){
        object()$finalModel
      }
    })
    
    output$object <- shiny::renderPrint({
      if(!is.null(object()) & inherits(object(),"train.formula")){
        caret::print.train(object())
      }
    })
  })
}