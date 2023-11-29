#' UI selectInput for boolean value activating or not variable selection through recursive feature elimination algorithm
#' @param id module identifier
rfeUi <- function(id){
  shiny::selectInput(shiny::NS(id,"rfe"),label = "variable selection",choices = c("","TRUE","FALSE"))
}

#' Server function for rfe module. Given a boolean input through rfeUi.
#' it computes or skips variable selection steps applying recursive feature elimination algorithm.
#' @param id module identifier
#' @param data dataframe
rfeServer <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    shiny::reactive({
      shiny::req(data(), input$catVar, input$rfe)
      id <- shiny::showNotification("RECURSIVE FEATURE ELIMINATION IN PROGRESS ...", duration = NULL, closeButton = FALSE)
      base::on.exit(shiny::removeNotification(id), add = TRUE)
      if(input$rfe=="TRUE"){
        rfeVar(data(),input$catVar)
      }else{
        data()
      }
    })
  })
}

#' UI selectInput for boolean value activating or not variable selection through recursive feature elimination algorithm
#' @param id module identifier
preProcessUi <- function(id){
  shiny::selectInput(shiny::NS(id,"preprocess"),label = "inputs preprocessing",choices = c("","TRUE","FALSE"))
}

#' UI inputs for pre-processing steps in data analysis. The given options are: 
#' 1. variable selection through recursive feature elimination
#' 2. "nzv" excludes "near zero-variance" predictors.
#' 3. "corr" seeks to filter out highly correlated predictors.
#' 4. "center" subtracts the mean of the predictor's data (again from the data in x) from the predictor value
#' 5. "scale" divides by the standard deviation. 
#' 6. "bagImpute" applies bag imputation to missing values
#' @param id module identifier
preProcessInputUi <- function(id){
  shiny::fluidRow(
    shinydashboard::box(shiny::selectInput(shiny::NS(id,"rfe"),label = "variable selection",choices = c("","TRUE","FALSE"),selected = NULL),width = 2),
    shinydashboard::box(shiny::selectInput(shiny::NS(id,"nzv"),label = "zero variance",choices = c("","TRUE","FALSE"),selected = NULL),width = 2),
    shinydashboard::box(shiny::selectInput(shiny::NS(id,"corr"),label = "correlated inputs",choices = c("","TRUE","FALSE"),selected = NULL),width = 2),
    shinydashboard::box(shiny::selectInput(shiny::NS(id,"center"),label = "zero mean",choices = c("","TRUE","FALSE"),selected = NULL),width = 2),
    shinydashboard::box(shiny::selectInput(shiny::NS(id,"scale"),label = "unit variance",choices = c("","TRUE","FALSE"),selected = NULL),width = 2),
    shinydashboard::box(shiny::selectInput(shiny::NS(id,"bagImpute"),label = "bag imputation",choices = c("","TRUE","FALSE"),selected = NULL),width = 2)
  )
}

#' Server function for pre-processing steps given the selected options given by preProcessInputUi.
#' Predictors that are not numeric are ignored in the calculations 
#' @param id module identifier
#' @param data dataframe
preProcessInputServer <- function(id, data){
  shiny::moduleServer(id,function(input,output,session){
    if(!is.null(data())){
      shiny::req(input$catVar,input$rfe,input$nzv,input$corr,input$center,input$scale,input$bagImpute)
      rfeData <- shiny::reactive({
        if(input$rfe=="TRUE"){
          id <- shiny::showNotification("RECURSIVE FEATURE ELIMINATION IN PROGRESS ...", duration = NULL, closeButton = FALSE)
          base::on.exit(shiny::removeNotification(id), add = TRUE)
          rfeVar(data(),input$catVar)
        }else{
          data()
        }
      })
      shiny::reactive({
        nzv <- input$nzv == "TRUE"
        corr <- input$corr == "TRUE"
        center <- input$center == "TRUE"
        scale <- input$scale == "TRUE"
        bagImpute <- input$bagImpute == "TRUE"
        met <- c("nzv","corr","center","scale","bagImpute")[which(c(nzv,corr,center,scale,bagImpute))]
        if(!is_empty(met)){
          id <- shiny::showNotification("DATA PREPROCESSING IN PROGRESS ...", duration = NULL, closeButton = FALSE)
          base::on.exit(shiny::removeNotification(id), add = TRUE)
          preProc <- caret::preProcess(rfeData(), method=c("zv", "nzv", "corr" ,"center", "scale", "bagImpute"))
          stats::predict(preProc, rfeData())
        }else{
          rfeData()
        }
      })
    }
  })
}

#' Server function for preProcessInputUi.
#' @param id module identifier
#' @return a character vector with the selected methods from preProcessInputUi to be computed in the pre-processing steps.
preProcessMethods <- function(id){
  shiny::moduleServer(id, function(input, output, session){
    shiny::reactive({
      shiny::req(input$catVar,input$rfe,input$nzv,input$corr,input$center,input$scale,input$bagImpute)
      nzv <- input$nzv == "TRUE"
      corr <- input$corr == "TRUE"
      center <- input$center == "TRUE"
      scale <- input$scale == "TRUE"
      bagImpute <- input$bagImpute == "TRUE"
      c("nzv","corr","center","scale","bagImpute")[which(c(nzv,corr,center,scale,bagImpute))]
    })
  })
}

#' Ui selectInput for available classification models
#' @param id module identifier
caretModelUi <- function(id){
  shiny::selectInput(shiny::NS(id,"caretModel"),label = "Choose a model",
                     choices = c("", names(lapply(X = caret::getModelInfo(),FUN = function(x) any(x[["type"]]=="Classification")))),
                     selected = NULL,selectize = FALSE)
}

caretModelServer <- function(id, data,preProc){
  shiny::moduleServer(id, function(input, output, session){
    
    shiny::reactive({
      if(!is.null(data())){
        shiny::req(input$catVar,input$caretModel)
        id <- shiny::showNotification("MODEL TRAINING IN PROGRESS ...", duration = NULL, closeButton = FALSE)
        base::on.exit(shiny::removeNotification(id), add = TRUE)
        tryCatch({
          set.seed(1234)
          fitted_model <- trainModel(data(),input$catVar,input$caretModel,preProc())
          colnames(fitted_model$trainingData)[which(colnames(fitted_model$trainingData)==".outcome")] <- input$catVar
          fitted_model
        },warning=function(w){
          shiny::showNotification(w$message,duration = NULL,closeButton = TRUE,type = "warning")
          set.seed(1234)
          fitted_model <- trainModel(data(),input$catVar,input$caretModel,preProc())
          colnames(fitted_model$trainingData)[which(colnames(fitted_model$trainingData)==".outcome")] <- input$catVar
          fitted_model
        },error=function(e){
          shiny::showNotification(e$message,duration = NULL,closeButton = TRUE,type = "error")
          return(NULL)
        })
        
      }
    })
  })
}


#' Ui selectInput for available classification models
#' @param id module identifier
classModelUi2 <- function(id){
  shiny::selectInput(shiny::NS(id,"caretModel"),label = "Choose a classification model",
                     choices = c("", "RF","LDA","PLS","XGBTREE","KNN","SVM","CART"),
                     selected = NULL,selectize = FALSE)
}

#' Ui selectInput for available classification models
#' @param id module identifier
classModelUi <- function(id){
  shiny::selectInput(shiny::NS(id,"caretModel"),label = "Choose a classification model",
                     choices = c("", "rf", "gbm", "xgbTree"),
                     selected = NULL,selectize = FALSE)
}

#' compute classification models such as Random Forest, Linear Discriminant Analysis,
#' K Nearest Neighbor, Support Vector Machine, Partial Least Squares and Classification
#' And Regression Tree
#' @param id module identifier
#' @param dataset training data used by models
#' @importFrom stats na.omit
classModelServer2 <- function(id, dataset){
  shiny::moduleServer(id, function(input, output, session){
    
    data <- shiny::reactive({
      shiny::req(dataset(), input$catVar)
      dataset() %>% dplyr::select(dplyr::all_of(c(numericDataset(dataset()) %>% colnames(),input$catVar)))
    })
    ctrl_fit <- caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75)
    shiny::reactive({
      shiny::req(data(), input$catVar, input$caretModel)
      id <- shiny::showNotification("MODEL TRAINGING IN PROGRESS ...", duration = NULL, closeButton = FALSE)
      base::on.exit(shiny::removeNotification(id), add = TRUE)
      tryCatch({
        fitted_model <- caret::train(formula(paste(input$catVar,"~ .")), data(), method=input$caretModel,trControl=ctrl_fit)
        colnames(fitted_model$trainingData)[which(colnames(fitted_model$trainingData)==".outcome")] <- input$catVar
        fitted_model
      },warning=function(w){
        shiny::showNotification(w$message,duration = NULL,closeButton = TRUE,type = "warning")
        set.seed(1234)
        fitted_model <- caret::train(formula(paste(input$catVar,"~ .")), data(), 
                                     method=input$caretModel,trControl=ctrl_fit,na.action=na.omit)
        colnames(fitted_model$trainingData)[which(colnames(fitted_model$trainingData)==".outcome")] <- input$catVar
        fitted_model
      },error=function(e){
        shiny::showNotification(e$message,duration = NULL,closeButton = TRUE,type = "error")
        set.seed(1234)
        fitted_model <- caret::train(formula(paste(input$catVar,"~ .")), data(), 
                                     method=input$caretModel,trControl=ctrl_fit,na.action=na.omit)
        colnames(fitted_model$trainingData)[which(colnames(fitted_model$trainingData)==".outcome")] <- input$catVar
        fitted_model
      })
    })
  })
}

#' compute classification models such as Random Forest, Linear Discriminant Analysis,
#' K Nearest Neighbor, Support Vector Machine, Partial Least Squares and Classification
#' And Regression Tree
#' @param id module identifier
#' @param data training data used by models
classModelServer <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    shiny::reactive({
      if(!is.null(data())){
        shiny::req(input$catVar, input$caretModel)
        id <- shiny::showNotification("MODEL TRAINGING IN PROGRESS ...", duration = NULL, closeButton = FALSE)
        base::on.exit(shiny::removeNotification(id), add = TRUE)
        tryCatch({
          fitted_model <- fit_class(data = data(), catVar = input$catVar, model=input$caretModel)
          colnames(fitted_model$trainingData)[which(colnames(fitted_model$trainingData)==".outcome")] <- input$catVar
          fitted_model
        },warning=function(w){
          shiny::showNotification(w$message,duration = NULL,closeButton = TRUE,type = "warning")
          fitted_model <- fit_class_na(data = data(), catVar = input$catVar, model=input$caretModel)
          colnames(fitted_model$trainingData)[which(colnames(fitted_model$trainingData)==".outcome")] <- input$catVar
          fitted_model
        },error=function(e){
          shiny::showNotification(e$message,duration = NULL,closeButton = TRUE,type = "error")
          fitted_model <- fit_class_na(data = data(), catVar = input$catVar, model=input$caretModel)
          colnames(fitted_model$trainingData)[which(colnames(fitted_model$trainingData)==".outcome")] <- input$catVar
          fitted_model
        })
        
      }
    })
  })
}

#' compute classification models such as Random Forest, Linear Discriminant Analysis,
#' K Nearest Neighbor, Support Vector Machine, Partial Least Squares and Classification
#' And Regression Tree
#' @param id module identifier
#' @param data training data used by models
classModelServer3 <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    shiny::reactive({
      if(!is.null(data())){
        shiny::req(input$catVar, input$rfe, input$preprocess, input$caretModel)
        id <- shiny::showNotification("MODEL TRAINGING IN PROGRESS ...", duration = NULL, closeButton = FALSE)
        base::on.exit(shiny::removeNotification(id), add = TRUE)
        rfe <- input$rfe == "TRUE"
        preprocess <- input$preprocess == "TRUE"
        base::switch(input$caretModel,
                     RF=fit_rf(data = data(), catVar = input$catVar, rfe, preprocess),
                     XGBTREE=fit_xgbTree(data = data(), catVar = input$catVar, rfe, preprocess),
                     KNN=fit_knn(data = data(), catVar = input$catVar, rfe, preprocess),
                     LDA=fit_lda(data = data(),catVar = input$catVar, rfe, preprocess),
                     SVM=fit_svm(data = data(), catVar = input$catVar, rfe, preprocess),
                     CART=fit_cart(data = data(), catVar = input$catVar, rfe, preprocess),
                     PLS=fit_pls(data = data(), catVar = input$catVar, rfe, preprocess),
                     shiny::validate("INVALID MODEL !, supported models are: RF, XGBTREE, KNN, LADA, SVM, CART, PLS")
        )
      }
    })
  })
}



#' select the significant variables with recursive feature selection and apply knn
#' @param id identifier of data module
#' @param data a given dataframe with response variable and predictor variable
#' @return a list with a dataframe and a knn fitted object
knnServer <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    shiny::reactive({
      if(!is.null(data())){
        shiny::req(input$catVar, input$rfe, input$preprocess)
        id <- shiny::showNotification("MODEL TRAINING IN PROGRESS ...", duration = NULL, closeButton = FALSE)
        base::on.exit(shiny::removeNotification(id), add = TRUE)
        rfe <- input$rfe == "TRUE"
        preprocess <- input$preprocess == "TRUE"
        fitted_knn <- fit_knn(data(),input$catVar, rfe, preprocess)
        colnames(fitted_knn$trainingData)[which(colnames(fitted_knn$trainingData)==".outcome")] <- input$catVar
        fitted_knn
      }
    })
  })
}

#' Fit and train a k nearest neighbors model given preprocessing inputs
#' @param id module identifier
#' @param data a dataset with explanatory variables and response variable
#' @return a fitted knn model
knnInputServer <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    rfeData <- shiny::reactive({
      shiny::req(input$preprocess)
      if(input$preprocess=="FALSE"){
        data() %>% dplyr::select(dplyr::all_of(c(numericDataset(data()) %>% colnames(),input$catVar)))
      }else{
        if(input$rfe=="TRUE"){
          rfeVar(data(), input$catVar)
        }else{
          data() %>% dplyr::select(dplyr::all_of(c(numericDataset(data()) %>% colnames(),input$catVar)))
        }
      }
    })
    shiny::reactive({
      shiny::req(input$preprocess,input$catVar)
      ctrl_fit <- caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75)
      if(input$preprocess=="FALSE"){
        id <- shiny::showNotification("MODEL TRAINING IN PROGRESS ...", duration = NULL, closeButton = FALSE)
        base::on.exit(shiny::removeNotification(id), add = TRUE)
        set.seed(1234)
        fitted_knn <- caret::train(formula(paste(input$catVar,"~ .")), rfeData(), method="knn", trControl=ctrl_fit)
      }else{
        shiny::req(input$nzv,input$corr,input$center,input$scale,input$bagImpute)
        id <- shiny::showNotification("MODEL TRAINING IN PROGRESS ...", duration = NULL, closeButton = FALSE)
        base::on.exit(shiny::removeNotification(id), add = TRUE)
        nzv <- input$nzv=="TRUE"
        corr <- input$corr=="TRUE"
        center <- input$center=="TRUE"
        scale <- input$scale=="TRUE"
        bagImpute <- input$bagImpute=="TRUE"
        if(!is_empty(c("nzv","corr","center","scale","bagImpute")[c(nzv,corr,center,scale,bagImpute)])){
          set.seed(1234)
          fitted_knn <- caret::train(formula(paste(input$catVar,"~ .")), rfeData(),  method="knn", trControl=ctrl_fit,
                                     preProcess=c("nzv","corr","center","scale","bagImpute")[c(nzv,corr,center,scale,bagImpute)])
        }else{
          set.seed(1234)
          fitted_knn <- caret::train(formula(paste(input$catVar,"~ .")), rfeData(), method="knn", trControl=ctrl_fit)
        }
      }
      colnames(fitted_knn$trainingData)[which(colnames(fitted_knn$trainingData)==".outcome")] <- input$catVar
      fitted_knn
    })
  })
}

#' write the output of random forest training into the rfTrainingUi
#' @param id module object identifier
#' @param data training sample to be used in the random forest fitting process
#' @return random forest fitted model written in the rfTrainingUi

rfServer <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    
    shiny::reactive({
      if(!is.null(data())){
        # shiny::req(input$catVar, input$rfe, input$preprocess)
        shiny::req(input$catVar)
        id <- shiny::showNotification("MODEL TRAINING IN PROGRESS ...", duration = NULL, closeButton = FALSE)
        base::on.exit(shiny::removeNotification(id), add = TRUE)
        rfe <- input$rfe == "TRUE"
        preprocess <- input$preprocess == "TRUE"
        # rfe <- ifelse(is_empty(input$rfe == "TRUE"),FALSE,input$rfe == "TRUE")
        # preprocess <- ifelse(is_empty(input$preprocess == "TRUE"),FALSE,input$preprocess == "TRUE")
        tryCatch({
          fit_rf(data = data(),catVar = input$catVar,rfe, preprocess)
        },warning=function(w){
          shiny::showNotification(w$message,duration = NULL,closeButton = TRUE,type = "warning")
          output$warning <- shinydashboard::renderMenu({
            shinydashboard::dropdownMenu(type="notifications", .list=lapply(X = w,FUN = notificationItem))
          })
        },error=function(e){
          shiny::showNotification(e$message,duration = NULL,closeButton = TRUE,type = "error")
          output$warning <- shinydashboard::renderMenu({
            shinydashboard::dropdownMenu(type="notifications",
                                         shinydashboard::notificationItem(text = e$message,
                                                                          icon = shiny::icon("warning"),
                                                                          status = "danger"
                                         )
            )
          })
        })
      }
    })
  })
}

#' write the output of decsion tree trained model into the rfTrainingUi
#' @param id module object identifier
#' @param data training sample to be used in the decision tree fitting process
#' @return random forest fitted model written in the rfTrainingUi
cartServer <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    
    shiny::reactive({
      if(!is.null(data())){
        shiny::req(input$catVar, input$rfe, input$preprocess)
        id <- shiny::showNotification("MODEL TRAINING IN PROGRESS ...", duration = NULL, closeButton = FALSE)
        base::on.exit(shiny::removeNotification(id), add = TRUE)
        rfe <- input$rfe == "TRUE"
        preprocess <- input$preprocess == "TRUE"
        
        fit_cart(data = data(),catVar = input$catVar,rfe, preprocess)
        
      }
    })
  })
}

#' write the output of linear discriminant analysis training model into the rfTrainingUi
#' @param id module object identifier
#' @param data training sample to be used in the random forest fitting process
#' @return random forest fitted model written in the rfTrainingUi
ldaServer <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    
    shiny::reactive({
      if(!is.null(data())){
        shiny::req(input$catVar, input$rfe, input$preprocess)
        id <- shiny::showNotification("MODEL TRAINING IN PROGRESS ...", duration = NULL, closeButton = FALSE)
        base::on.exit(shiny::removeNotification(id), add = TRUE)
        rfe <- input$rfe == "TRUE"
        preprocess <- input$preprocess == "TRUE"
        
        fit_lda(data = data(),catVar = input$catVar,rfe, preprocess)
        
      }
    })
  })
}

#' write the output of support vector machine training model into the rfTrainingUi
#' @param id module object identifier
#' @param data training sample to be used in the random forest fitting process
#' @return random forest fitted model written in the rfTrainingUi
svmServer <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    
    shiny::reactive({
      if(!is.null(data())){
        shiny::req(input$catVar, input$rfe, input$preprocess)
        id <- shiny::showNotification("MODEL TRAINING IN PROGRESS ...", duration = NULL, closeButton = FALSE)
        base::on.exit(shiny::removeNotification(id), add = TRUE)
        rfe <- input$rfe == "TRUE"
        preprocess <- input$preprocess == "TRUE"
        
        fit_svm(data = data(),catVar = input$catVar,rfe, preprocess)
        
      }
    })
  })
}

#' write the output of partial least squares training model into the rfTrainingUi
#' @param id module object identifier
#' @param data training sample to be used in the random forest fitting process
#' @return random forest fitted model written in the rfTrainingUi
plsServer <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    
    shiny::reactive({
      if(!is.null(data())){
        shiny::req(input$catVar, input$rfe, input$preprocess)
        id <- shiny::showNotification("MODEL TRAINING IN PROGRESS ...", duration = NULL, closeButton = FALSE)
        base::on.exit(shiny::removeNotification(id), add = TRUE)
        rfe <- input$rfe == "TRUE"
        preprocess <- input$preprocess == "TRUE"
        
        fit_pls(data = data(),catVar = input$catVar,rfe, preprocess)
        
      }
    })
  })
}