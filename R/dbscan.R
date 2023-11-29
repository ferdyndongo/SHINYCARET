#' DBSCAN outlier detection
#' @param datatrain dataset with numerical inputs
#' @param eps distance to be considered
#' @param minPts minimum number of points within the considered distance eps
#' @return a dbscan object
dbscan_outlier_detection <- function(datatrain, eps, minPts){
  if(!(is.null(datatrain))){
    X <- datatrain %>% scale()
    ds <- dbscan::dbscan(x = X,eps, minPts)
    return(ds)
  }
  
}


#' Build Ui container for the dbscanOuput plots
#' @param id identifier of dbscan module
#' @return Ui container as tagList object for dbscanOutput plots
dbscanUi <- function(id){
  shiny::tagList(
    shiny::verbatimTextOutput(shiny::NS(id,"dbscanPrint")),
    shiny::verbatimTextOutput(shiny::NS(id,"dbscanTable")),
    shiny::fluidRow(
      shinydashboard::box(shiny::selectInput(shiny::NS(id,"dim1"), label = "",choices = NULL, selected = NULL, selectize = FALSE), width = 2),
      shinydashboard::box(shiny::selectInput(shiny::NS(id,"dim2"), label = "",choices = NULL, selected = NULL, selectize = FALSE), width = 2),
      shinydashboard::box(shiny::selectInput(shiny::NS(id,"ellipse"), label = "ellipse type",
                                             choices = c("convex","confidence","t","norm","euclid"), selectize = FALSE),width = 2)
    ),
    # shiny::plotOutput(shiny::NS(id,"dbscanPca")),
    shiny::plotOutput(shiny::NS(id,"dbscanPlot")),
    shiny::selectInput(shiny::NS(id,"order"), label = "variable order",
                       choices = c("","anyClass","allClass","skewness","Outlying",
                                   "Skewed","Clumpy","Sparse","Striated","Convex","Skinny",
                                   "Stringy","Monotonic"), selected = NULL,selectize = FALSE),
    shiny::plotOutput(shiny::NS(id,"parcoordPlot")),
    # shiny::plotOutput(shiny::NS(id,"parcoord")),
    shiny::tableOutput(shiny::NS(id,"dataclustPrint"))
  )
}

#' Build input sliderInput container for interaction with the dbscanServer
#' @param id identifier of dbscan module
#' @return sliderInput object
dbscanInput <- function(id){
  shiny::sliderInput(shiny::NS(id,"slider"), label = NULL, min = 0, max = 2, value = 1, step = 0.01, round = FALSE)
}

#' a shiny selectInput object with Character specifying frame type for data ellipses. 
#' Possible values are 'convex', 'confidence' or types supported by stat_ellipse 
#' including one of c("t", "norm", "euclid").
#' @param id module identifier
ellipseType <- function(id){
  shiny::selectInput(shiny::NS(id,"ellipse"), label = "ellipse type",
                     choices = c("convex","confidence","t","norm","euclid"), selectize = FALSE)
}

#' a  shiny selectInput object with character string that denotes 
#' how to order the axes (variables) of the parallel coordinate plot
#' @param id module identifier
orderType <- function(id){
  shiny::selectInput(shiny::NS(id,"order"), label = "variable order",
                     choices = c("","anyClass","allClass","skewness","Outlying",
                                 "Skewed","Clumpy","Sparse","Striated","Convex","Skinny",
                                 "Stringy","Monotonic"), selected = NULL,selectize = FALSE)
}


#' Write the dbscanServer outputs into the dbscanUi
#' @param id identifier of the dbscan module
#' @param dbscan the fitted dbscan model object
#' @param data data that has been used for the fitted dbsan clustering model
#' @param predicted_scores projected scores into the first 2 pca dimensions
#' @param dataclust_pred predicted cluster
#' @return nothing
#' @importFrom purrr %>%
dbscanOutput <- function(id,dbscan,data, predicted_scores=NULL, dataclust_pred=NULL){
  shiny::moduleServer(id,function(input,output,session){
    
    output$dbscanPrint <- shiny::renderPrint({
      if(is.null(dataclust_pred)){
        dbscan()
      }else{
        paste0("DBSCAN clustering prediction for ", nrow(dataclust_pred()), " objects.")
      }
    })
    output$dbscanTable <- shiny::renderPrint({
      if(is.null(dataclust_pred)){
        table(cluster=dbscan()$cluster,class=data()[[input$catVar]])
      }else{
        table(dataclust_pred()$cluster)
      }
    })
    
    output$dataclustPrint <- shiny::renderTable({
      if(is.null(dataclust_pred)){
        data() %>% dplyr::bind_cols(cluster=factor(dbscan()$cluster)) %>% 
          dplyr::filter(.data$cluster==0) %>% utils::head()
      }else{
        if(!is.null(dataclust_pred())){
          dataclust_pred() %>% dplyr::filter(.data$cluster==0) %>% utils::head()
        }
      }
    })
    dataParcoord <- shiny::reactive({
      if(is.null(dataclust_pred)){
        data() %>% dplyr::bind_cols(cluster=factor(dbscan()$cluster))
      }else{
        dataclust <- shiny::reactive({data() %>% dplyr::bind_cols(cluster=factor(dbscan()$cluster))})
        if(any(dataclust()$cluster==0)){
          dataclust_pred() %>% 
            dplyr::bind_rows(dataclust() %>% dplyr::filter(!dataclust()$cluster==0) %>% dplyr::select(dplyr::all_of(names(dataclust_pred()))))
        }else{
          dataclust_pred() %>%
            dplyr::bind_rows(dataclust() %>% dplyr::select(dplyr::all_of(names(dataclust_pred()))))
        }
      }
    })
    
    pcaObject <- shiny::reactive({
      shiny::req(data(),input$preprocess)
      if(input$preprocess=="TRUE"){
        stats::prcomp(numericDataset(data()),center=TRUE,scale.=TRUE)
      }else{
        stats::prcomp(numericDataset(data()),center=FALSE,scale.=FALSE)
      }
    })
    
    shiny::observeEvent(pcaObject(),{
      shiny::updateSelectInput(inputId = "dim1",label = "Select the PC in x axis:",
                               choices = stringr::str_split(1:length(pcaObject()[[1]]),pattern = " "),
                               selected = "1")
      shiny::updateSelectInput(inputId = "dim2",label = "Select the PC in y axis:",
                               choices = stringr::str_split(1:length(pcaObject()[[1]]),pattern = " "),
                               selected = "2")
    })
    
    # dbscanPca <- shiny::reactive({
    #   shiny::req(input$dim1, input$dim2)
    #   if(input$preprocess=="TRUE"){
    #     clustViz(dbscanObject = dbscan(),dbscanData = numericDataset(dataclust()) %>% scale(center = TRUE,scale = TRUE),
    #              comp = c(as.numeric(input$dim1),as.numeric(input$dim2)))
    #   }else{
    #     clustViz(dbscanObject = dbscan(),dbscanData = numericDataset(dataclust()),comp = c(as.numeric(input$dim1),as.numeric(input$dim2)))
    #   }
    # })
    
    dbscanPlot <- shiny::reactive({
      shiny::req(data(),input$dim1, input$dim2)
      if(input$preprocess=="TRUE"){
        if(length(colnames(numericDataset(data())))>3){
          factoextra::fviz_cluster(object = dbscan(),data = numericDataset(data()) %>% scale(center = TRUE,scale = TRUE),
                                   stand = FALSE, ellipse.type = input$ellipse,
                                   axes = c(as.numeric(input$dim1),as.numeric(input$dim2)))
        }else{
          factoextra::fviz_cluster(object = dbscan(),data = numericDataset(data()) %>% scale(center = TRUE,scale = TRUE),
                                   stand = FALSE, ellipse.type = input$ellipse,
                                   choose.vars = c(as.numeric(input$dim1),as.numeric(input$dim2)))
        }
      }else{
        if(length(colnames(numericDataset(data())))>3){
          factoextra::fviz_cluster(object = dbscan(),data = numericDataset(data()),
                                   stand = FALSE, ellipse.type = input$ellipse,
                                   axes = c(as.numeric(input$dim1),as.numeric(input$dim2)))
        }else{
          factoextra::fviz_cluster(object = dbscan(),data = numericDataset(data()),
                                   stand = FALSE, ellipse.type = input$ellipse,
                                   choose.vars = c(as.numeric(input$dim1),as.numeric(input$dim2)))
        }
      }
    })
    
    parcoordPlot <- shiny::reactive({parcoord_plot(dataParcoord(),varOrder=input$order)})
    
    if(is.null(predicted_scores)){
      # output$dbscanPca <- shiny::renderPlot({dbscanPca()},res = 96)
      output$dbscanPlot <- shiny::renderPlot({dbscanPlot()},res = 96)
      output$parcoordPlot <- shiny::renderPlot({parcoordPlot() + 
          geom_text(mapping = aes(x = parcoordPlot()$data$variable,
                                  y = round(parcoordPlot()$data$value,2),
                                  label=parcoordPlot()$data$.ID))
      }, res = 96)
      # output$parcoord <- shiny::renderPlot({
      #   parcoord() + 
      #     geom_text(mapping = aes(x = parcoord()$data$variable,
      #                             y = round(parcoord()$data$value,2),
      #                             label=parcoord()$data$.ID))
      # }, res = 96)
    }else{
      # output$dbscanPca <- shiny::renderPlot({predViz(fviz_clust = dbscanPca(),predicted_scores = predicted_scores(),
      #                                                comp = c(as.numeric(input$dim1),as.numeric(input$dim2)))},res = 96)
      output$dbscanPlot <- shiny::renderPlot({predViz(fviz_clust = dbscanPlot(),predicted_scores = predicted_scores(),
                                                      comp = c(as.numeric(input$dim1),as.numeric(input$dim2)))},res = 96)
      output$parcoordPlot <- shiny::renderPlot({
        shiny::req(parcoordPlot())
        parcoordPlot() + 
          geom_text(mapping = aes(x = parcoordPlot()$data$variable,
                                  y = round(parcoordPlot()$data$value,2),
                                  label=parcoordPlot()$data$.ID))
      }, res = 96)
    }
  })
}

#' Compute dbscan
#' @param id identifier for the dbscan module
#' @param knn object resulting from knn analysis
#' @return a list with dbscan object and dataframe with original class and fitted cluster
#' @importFrom purrr %>%
#' @importFrom rlang .data
dbscanServer <- function(id, knn){
  shiny::moduleServer(id, function(input, output, session){
    
    d <- shiny::reactive({
      if(!is.null(knn())){
        if(input$preprocess=="TRUE"){
          dbscan::kNNdist(x = numericDataset(knn()$trainingData) %>% scale(center = TRUE,scale = TRUE),
                          k = knn()[["finalModel"]]$k)
        }else{
          dbscan::kNNdist(x = numericDataset(knn()$trainingData) %>% scale(center = FALSE,scale = FALSE),
                          k = knn()[["finalModel"]]$k)
        }
      }
    })
    shiny::observeEvent(d(),{
      shiny::updateSliderInput(inputId = "slider",
                               label = NULL,
                               min = round(min(d()),1),
                               max = round(max(d()),1),
                               value = round(stats::quantile(x = d(),probs = 0.95)[["95%"]],1),
                               step = 0.01)
    })
    
    shiny::reactive({
      shiny::req(input$slider)
      if(input$preprocess=="TRUE"){
        dbscan::dbscan(x = numericDataset(knn()$trainingData) %>% scale(center = TRUE,scale = TRUE),
                       eps = input$slider,
                       minPts = knn()[["finalModel"]]$k)
      }else{
        dbscan::dbscan(x = numericDataset(knn()$trainingData) %>% scale(center = FALSE, scale = FALSE),
                       eps = input$slider,
                       minPts = knn()[["finalModel"]]$k)
      }
      
    })
    
    # dataclust <- shiny::reactive({
    #   knn()$trainingData %>% dplyr::bind_cols(cluster=factor(dbscan()$cluster))
    # })
    
    # return(list(dbscan=shiny::reactive(dbscan()), dataclust=shiny::reactive(dataclust())))
  })
}

#' Compute dbscan prediction for data in db selected by year
#' @param id identifier for the dbscan module
#' @param test_sample test sample to be predicted
#' @param dbscanData a list with dbscan object and dataclust datafreme resulting from dbscanServer
#' @return a list with dbscan object and dataframe with original class and fitted cluster
#' @importFrom purrr %>% 
dbscanPrediction <- function(id, test_sample, dbscanData){
  shiny::moduleServer(id, function(input, output, session){
    
    coefnames <- shiny::reactive({names(numericDataset(dbscanData()$trainingSample()))})
    predicted_clust <- shiny::reactive({
      if(input$preprocess=="TRUE"){
        stats::predict(object = dbscanData()$dbscan(), data = numericDataset(dbscanData()$trainingSample()) %>% scale(center = TRUE,scale = TRUE), 
                       newdata = test_sample() %>% dplyr::select(dplyr::all_of(coefnames())) %>% scale(center = TRUE,scale = TRUE))
      }else{
        stats::predict(object = dbscanData()$dbscan(), data = numericDataset(dbscanData()$trainingSample()) %>% scale(center = FALSE,scale = FALSE), 
                       newdata = test_sample() %>% dplyr::select(dplyr::all_of(coefnames())) %>% scale(center = FALSE,scale = FALSE))
      }
    })
    dataclust_pred <- shiny::reactive({
      if(!is.null(test_sample())){
        test_sample() %>% dplyr::select(dplyr::all_of(coefnames())) %>% 
          dplyr::bind_cols(cluster=factor(predicted_clust()))
      }
    })
    pca <- shiny::reactive({
      if(input$preprocess=="TRUE" & length(coefnames())>3){
        stats::prcomp(numericDataset(dbscanData()$trainingSample()),center=TRUE,scale.=TRUE)
      }else if(input$preprocess=="FALSE" & length(coefnames())>3){
        stats::prcomp(numericDataset(dbscanData()$trainingSample()),center=FALSE,scale.=FALSE)
      }
    })
    predicted_scores <- shiny::reactive({
      shiny::req(input$preprocess)
      if(length(coefnames())>3){
        if(input$preprocess=="TRUE"){
          cbind.data.frame(dataclust_pred() %>% numericDataset() %>% scale(center = TRUE,scale = TRUE) %*% pca()$rotation, 
                           cluster=factor(predicted_clust())
                           # row.names=paste(test_sample()$numero,test_sample()$matricola,sep="")
          )
        }else{
          cbind.data.frame(dataclust_pred() %>% numericDataset() %>% scale(center = FALSE,scale = FALSE) %*% pca()$rotation, 
                           cluster=factor(predicted_clust())
                           # row.names=paste(test_sample()$numero,test_sample()$matricola,sep="")
          )
        }
      }else{
        if(input$preprocess=="TRUE"){
          data.frame(dataclust_pred() %>% numericDataset() %>% scale(center = TRUE,scale = TRUE), 
                     cluster=factor(predicted_clust())
                     # row.names = paste(test_sample()$numero,test_sample()$matricola,sep="")
          )
        }else{
          data.frame(dataclust_pred() %>% numericDataset() %>% scale(center = FALSE,scale = FALSE), 
                     cluster=factor(predicted_clust())
                     # row.names = paste(test_sample()$numero,test_sample()$matricola,sep="")
          )
        }
      }
    })
    return(list(predicted_scores=shiny::reactive(predicted_scores()), dataclust_pred=shiny::reactive(dataclust_pred()))) 
  })
}


clustViz <- function(dbscanObject, dbscanData,comp=c(1,2)){
  if(length(colnames(dbscanData))>3){
    factoextra::fviz_cluster(object = dbscanObject, data = dbscanData, stand = FALSE, axes = comp)
  }else{
    factoextra::fviz_cluster(object = dbscanObject, data = dbscanData,stand = FALSE,choose.vars=comp)
  }
}



#' visualizzation on the predicted cluster points onto the fitted cluster space given by output of the function clustViz
#' @param fviz_clust ggplot object given by the clustViz function
#' @param predicted_scores dataframe with data used to predict and the predicted cluster column
#' @param comp the chosen axes or dimensions
#' @return a plot with a projection of predicted cluster points into a given cluster space
#' @importFrom ggplot2 ggplot aes geom_point geom_text xlab ylab
#' @importFrom rlang .data
predViz <- function(fviz_clust, predicted_scores,comp=c(1,2)){
  if(any(predicted_scores$cluster==0)){
    outlier.data <- predicted_scores %>% dplyr::filter(predicted_scores$cluster==0)
    ggplot(data = fviz_clust$layers[[1]]$data, mapping = aes(fviz_clust$layers[[1]]$data$x,fviz_clust$layers[[1]]$data$y)) + 
      fviz_clust$layers[[2]] + 
      geom_point(data = predicted_scores, 
                 mapping = aes(.data[[colnames(predicted_scores)[comp[1]]]], 
                               .data[[colnames(predicted_scores)[comp[2]]]], 
                               colour=.data[[colnames(predicted_scores)[length(predicted_scores)]]])) + 
      geom_text(data = predicted_scores,
                mapping = aes(x = .data[[colnames(predicted_scores)[comp[1]]]],
                              y = .data[[colnames(predicted_scores)[comp[2]]]]+0.15,
                              colour=.data[[colnames(predicted_scores)[length(predicted_scores)]]],
                              label=rownames(predicted_scores))) +
      # geom_text(data = outlier.data,
      #           mapping = aes(x = .data[[colnames(outlier.data)[comp[1]]]],
      #                         y = .data[[colnames(outlier.data)[comp[2]]]]+0.15,
      #                         colour=.data[[colnames(outlier.data)[length(outlier.data)]]],
      #                         label=rownames(outlier.data))) +
      xlab(fviz_clust$labels$x) + 
      ylab(fviz_clust$labels$y)
  }else{
    ggplot(data = fviz_clust$layers[[1]]$data, mapping = aes(fviz_clust$layers[[1]]$data$x,fviz_clust$layers[[1]]$data$y)) +
      fviz_clust$layers[[2]] + 
      geom_point(data = predicted_scores, 
                 mapping = aes(.data[[colnames(predicted_scores)[comp[1]]]], 
                               .data[[colnames(predicted_scores)[comp[2]]]], 
                               colour=.data[[colnames(predicted_scores)[length(predicted_scores)]]])) + 
      xlab(fviz_clust$labels$x) + 
      ylab(fviz_clust$labels$y)
  }
}

