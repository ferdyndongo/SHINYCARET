#' Fit and train some classification models
#' @param data a dataset with explanatory variables and response variable
#' @param catVar the response variable
#' @param model string specifiying which classification model will be used.
#' @return a trained and fitted classification model
#' @importFrom stats formula
fit_class <- function(data, catVar, model){
  if(!( is.null(data) | is.null(catVar) | is.null(model) )){
    if(catVar %in% colnames(data)){
      data <- data %>% dplyr::select(dplyr::all_of(c(numericDataset(data) %>% colnames(),catVar)))
      ctrl_fit <- caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75)
      if(model=="xgbTree"){
        set.seed(1234)
        tryCatch({
          caret::train(formula(paste(catVar,"~ .")), data, method=model,trControl=ctrl_fit,verbosity=0)
        },warning=function(w){
          shiny::showNotification(w$message,duration = NULL,closeButton = TRUE,type = "warning")
          return(NULL)
        },error=function(e){
          shiny::showNotification(e$message,duration = NULL,closeButton = TRUE,type = "error")
          return(NULL)
        })
      }else{
        set.seed(1234)
        tryCatch({
          caret::train(formula(paste(catVar,"~ .")), data, method=model,trControl=ctrl_fit,verbose=0)
        },warning=function(w){
          shiny::showNotification(w$message,duration = NULL,closeButton = TRUE,type = "warning")
          return(NULL)
        },error=function(e){
          shiny::showNotification(e$message,duration = NULL,closeButton = TRUE,type = "error")
          return(NULL)
        })
      }
      
    }
  }
}

#' Fit and train some classification models with argument na.action=na.omit, 
#' which leads to rejection of cases with missing values on any required variable.
#' @param data a dataset with explanatory variables and response variable
#' @param catVar the response variable
#' @param model string specifiying which classification model will be used.
#' @return a trained and fitted classification model
#' @importFrom stats formula
fit_class_na <- function(data, catVar, model){
  if(!( is.null(data) | is.null(catVar) | is.null(model) )){
    if(catVar %in% colnames(data)){
      data <- data %>% dplyr::select(dplyr::all_of(c(numericDataset(data) %>% colnames(),catVar)))
      ctrl_fit <- caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75)
      if(model=="xgbTree"){
        set.seed(1234)
        tryCatch({
          caret::train(formula(paste(catVar,"~ .")), data, method=model,trControl=ctrl_fit,verbosity=0, na.action=na.omit)
        },warning=function(w){
          shiny::showNotification(w$message,duration = NULL,closeButton = TRUE,type = "warning")
          return(NULL)
        },error=function(e){
          shiny::showNotification(e$message,duration = NULL,closeButton = TRUE,type = "error")
          return(NULL)
        })
      }else{
        set.seed(1234)
        tryCatch({
          caret::train(formula(paste(catVar,"~ .")), data, method=model,trControl=ctrl_fit,verbose=0, na.action=na.omit)
        },warning=function(w){
          shiny::showNotification(w$message,duration = NULL,closeButton = TRUE,type = "warning")
          return(NULL)
        },error=function(e){
          shiny::showNotification(e$message,duration = NULL,closeButton = TRUE,type = "error")
          return(NULL)
        })
      }
      
    }
  }
}

#' Fit and train some classification models with optional recursive feature elimination
#' and preprocessing steps
#' @param data a dataset with explanatory variables and response variable
#' @param catVar the response variable
#' @param rfe logical value whether to apply variable selection with recursive feature elimination algorythm
#' @param model string specifiying which classification model will be used.
#' @param preprocess logical value whether the preprocessing steps have to be done on the explanatory variables
#' @return a trained and fitted classification model
fit_class2 <- function(data, catVar, model, rfe=FALSE, preprocess=FALSE){
  if(!( is.null(data) | is.null(catVar) | is.null(model) )){
    if(catVar %in% colnames(data)){
      if(base::isTRUE(rfe)){
        data <- rfeVar(data, catVar)
      }else{
        data <- data %>% dplyr::select(dplyr::all_of(c(numericDataset(data) %>% colnames(),catVar)))
      }
      ## fitting and tuning the model
      ctrl_fit <- caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75)
      if(base::isTRUE(preprocess)){
        if(model=="gbm"){
          set.seed(1234)
          caret::train(formula(paste(catVar,"~ .")), data,  method=model, trControl=ctrl_fit,verbose=0,
                       verbose=0,preProcess=c("zv", "nzv", "corr" ,"center", "scale", "bagImpute"))
        }else{
          set.seed(1234)
          caret::train(formula(paste(catVar,"~ .")), data,  method=model, trControl=ctrl_fit,verbosity=0,
                       verbosity=0,preProcess=c("zv", "nzv", "corr" ,"center", "scale", "bagImpute"))
        }
      }else{
        if(model=="gbm"){
          set.seed(1234)
          caret::train(formula(paste(catVar,"~ .")), data, method=model, trControl=ctrl_fit,verbose=0)
        }else{
          set.seed(1234)
          caret::train(formula(paste(catVar,"~ .")), data, method=model, trControl=ctrl_fit,verbosity=0)
        }
        
      }
    }
  }
}

#' Fit and train a k nearest neighbors model
#' @param data a dataset with explanatory variables and response variable
#' @param catVar the response variable
#' @param rfe logical value whether to apply variable selection with recursive feature elimination algorythm
#' @param preprocess logical value whether the preprocessing steps have to be done on the explanatory variables
#' @return a fitted knn model
fit_knn <- function(data, catVar, rfe, preprocess){
  if(!( is.null(data) | is.null(catVar) | is.null(rfe) | is.null(preprocess) )){
    
    if(base::isTRUE(rfe)){
      data <- rfeVar(data, catVar)
    }else{
      data <- data %>% dplyr::select(dplyr::all_of(c(numericDataset(data) %>% colnames(),catVar)))
    }
    
    ## fitting and tuning the knn
    ctrl_fit <- caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75)
    if(base::isTRUE(preprocess)){
      set.seed(1234)
      caret::train(formula(paste(catVar,"~ .")), data,  method="knn", trControl=ctrl_fit,
                   preProcess=c("zv", "nzv", "corr" ,"center", "scale", "bagImpute"))
    }else{
      set.seed(1234)
      caret::train(formula(paste(catVar,"~ .")), data, method="knn", trControl=ctrl_fit)
    }
  }
  
}

#' Train a Random Forest model
#' @param data a dataset containing explanatory variables and the categorical variable catVar
#' @param catVar a categorical variable to be taken as classifier
#' @param rfe logical value whether variable selection has to be done with recursive feature elimination algorythm
#' @param preprocess logical value whether the preprocessing steps have to be done on the explanatory variables
#' @return an Random Forest fitted model
fit_rf <- function(data, catVar, rfe, preprocess){
  if(!( is.null(data) | is.null(catVar) | is.null(rfe) | is.null(preprocess) )){
    
    if(base::isTRUE(rfe)){
      # ## recursive feature elimination as feature selection method
      set.seed(1000)
      rfProfile <- caret::rfe(x = numericDataset(data), y=factor(data[[catVar]]), 
                              rfeControl=caret::rfeControl(functions = caret::rfFuncs, method = "cv"))
      data <- data %>% dplyr::select(dplyr::all_of(c(caret::predictors(rfProfile), catVar)))
    }else{
      data <- data %>% dplyr::select(dplyr::all_of(c(numericDataset(data) %>% colnames(),catVar)))
    }
    
    if(base::isTRUE(preprocess)){
      set.seed(1234)
      fitted_rf <- caret::train(formula(paste(catVar,"~ .")), data , preProcess=c("zv", "nzv", "corr" ,"center", "scale", "bagImpute"),
                                method="rf",trControl=caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75))
    }else{
      set.seed(1234)
      fitted_rf <- caret::train(formula(paste(catVar,"~ .")), data , method="rf", 
                                trControl=caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75))
      
    }
    
    return(fitted_rf)
  }
}

#' Train a XGBTREE Model
#' @param data a dataset containing explanatory variables and the categorical variable catVar
#' @param catVar a categorical variable to be taken as classifier
#' @param rfe logical value whether variable selection has to be done with recursive feature elimination algorythm
#' @param preprocess logical value whether the preprocessing steps have to be done on the explanatory variables
#' @return an Random Forest fitted model
fit_xgbTree <- function(data, catVar, rfe, preprocess){
  if(!( is.null(data) | is.null(catVar) | is.null(rfe) | is.null(preprocess) )){
    if(base::isTRUE(rfe)){
      # ## recursive feature elimination as feature selection method
      set.seed(1000)
      rfProfile <- caret::rfe(x = numericDataset(data), y=factor(data[[catVar]]), 
                              rfeControl=caret::rfeControl(functions = caret::rfFuncs, method = "cv"))
      data <- data %>% dplyr::select(dplyr::all_of(c(caret::predictors(rfProfile), catVar)))
    }else{
      data <- data %>% dplyr::select(dplyr::all_of(c(numericDataset(data) %>% colnames(),catVar)))
    }
    
    if(base::isTRUE(preprocess)){
      set.seed(1234)
      fitted_rf <- caret::train(formula(paste(catVar,"~ .")), data , 
                                preProcess=c("zv", "nzv", "corr" ,"center", "scale", "bagImpute"),
                                method="xgbTree", verbosity=FALSE,
                                trControl=caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75))
    }else{
      set.seed(1234)
      fitted_rf <- caret::train(formula(paste(catVar,"~ .")), data , verbosity=FALSE, method="xgbTree",
                                trControl=caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75))
      
    }
    
    return(fitted_rf)
  }
}

#' Train a Linear Discriminant Analysis model
#' @param data a dataset containing explanatory variables and the categorical variable catVar
#' @param catVar a categorical variable to be taken as classifier
#' @param rfe logical value whether variable selection has to be done with recursive feature elimination algorythm
#' @param preprocess logical value whether the preprocessing steps have to be done on the explanatory variables
#' @return a linear discriminant analysis fitted model
fit_lda <- function(data, catVar, rfe, preprocess){
  if(!( is.null(data) | is.null(catVar) | is.null(rfe) | is.null(preprocess) )){
    if(base::isTRUE(rfe)){
      set.seed(1000)
      ldaProfile <- caret::rfe(x = numericDataset(data), y=factor(data[[catVar]]), 
                               rfeControl=caret::rfeControl(functions = caret::ldaFuncs, method = "cv"))
      data <- data %>% dplyr::select(dplyr::all_of(c(caret::predictors(ldaProfile), catVar)))
    }else{
      data <- data %>% dplyr::select(dplyr::all_of(c(numericDataset(data) %>% colnames(),catVar)))
    }
    
    if(base::isTRUE(preprocess)){
      set.seed(1234)
      fitted_lda <- caret::train(formula(paste(catVar,"~ .")), data , preProcess=c("zv", "nzv", "corr" ,"center", "scale", "bagImpute"),
                                 method="lda",trControl=caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75))
    }else{
      set.seed(1234)
      fitted_lda <- caret::train(formula(paste(catVar,"~ .")), data ,
                                 method="lda",trControl=caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75))
    }
    
    return(fitted_lda)
  }
}

#' Train a Decision Tree model
#' @param data a dataset containing explanatory variables and the categorical variable catVar
#' @param catVar a categorical variable to be taken as classifier
#' @param rfe logical value whether variable selection has to be done with recursive feature elimination algorythm
#' @param preprocess logical value whether the preprocessing steps have to be done on the explanatory variables
#' @return a decision tree fitted model
fit_cart <- function(data, catVar, rfe, preprocess){
  if(!( is.null(data) | is.null(catVar) | is.null(rfe) | is.null(preprocess) )){
    
    if(base::isTRUE(rfe)){
      # data <- rfeVar(data, catVar)
      set.seed(1000)
      cartProfile <- caret::rfe(x = numericDataset(data), y=factor(data[[catVar]]), 
                                rfeControl=caret::rfeControl(functions = caret::treebagFuncs, method = "cv"))
      data <- data %>% dplyr::select(dplyr::all_of(c(caret::predictors(cartProfile), catVar)))
    }else{
      data <- data %>% dplyr::select(dplyr::all_of(c(numericDataset(data) %>% colnames(),catVar)))
    }
    
    if(base::isTRUE(preprocess)){
      set.seed(1234)
      fitted_cart <- caret::train(formula(paste(catVar,"~ .")), data , preProcess=c("zv", "nzv", "corr" ,"center", "scale", "bagImpute"),
                                  method="rpart",trControl=caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75))
    }else{
      set.seed(1234)
      fitted_cart <- caret::train(formula(paste(catVar,"~ .")), data ,
                                  method="rpart",trControl=caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75))
    }
    return(fitted_cart)
  }
}

#' Train a Support Vector Machine model
#' @param data a dataset containing explanatory variables and the categorical variable catVar
#' @param catVar a categorical variable to be taken as classifier
#' @param rfe logical value whether variable selection has to be done with recursive feature elimination algorythm
#' @param preprocess logical value whether the preprocessing steps have to be done on the explanatory variables
#' @return a support vector machine fitted model
fit_svm <- function(data, catVar, rfe, preprocess){
  if(!( is.null(data) | is.null(catVar) | is.null(rfe) | is.null(preprocess) )){
    
    if(base::isTRUE(rfe)){
      data <- rfeVar(data, catVar)
    }else{
      data <- data %>% dplyr::select(dplyr::all_of(c(numericDataset(data) %>% colnames(),catVar)))
    }
    
    if(base::isTRUE(preprocess)){
      set.seed(1234)
      fitted_svm <- caret::train(formula(paste(catVar,"~ .")), data , preProcess=c("zv", "nzv", "corr" ,"center", "scale", "bagImpute"),
                                 method="svmRadial",trControl=caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75))
    }else{
      set.seed(1234)
      fitted_svm <- caret::train(formula(paste(catVar,"~ .")), data ,
                                 method="svmRadial",trControl=caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75))
    }
    return(fitted_svm)
  }
}

#' Train a Partial Least Squares model
#' @param data a dataset containing explanatory variables and the categorical variable catVar
#' @param catVar a categorical variable to be taken as classifier
#' @param rfe logical value whether variable selection has to be done with recursive feature elimination algorythm
#' @param preprocess logical value whether the preprocessing steps have to be done on the explanatory variables
#' @return a partial least squares fitted model
fit_pls <- function(data, catVar, rfe, preprocess){
  if(!( is.null(data) | is.null(catVar) | is.null(rfe) | is.null(preprocess) )){
    
    if(base::isTRUE(rfe)){
      data <- rfeVar(data, catVar)
    }else{
      data <- data %>% dplyr::select(dplyr::all_of(c(numericDataset(data) %>% colnames(),catVar)))
    }
    
    if(base::isTRUE(preprocess)){
      set.seed(1234)
      fitted_pls <- caret::train(formula(paste(catVar,"~ .")), data , preProcess=c("zv", "nzv", "corr" ,"center", "scale", "bagImpute"),
                                 method="pls",trControl=caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75))
    }else{
      set.seed(1234)
      fitted_pls <- caret::train(formula(paste(catVar,"~ .")), data ,
                                 method="pls",trControl=caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75))
    }
    return(fitted_pls)
  }
}

