#' User Interface for parallel coordinate plot with inputs and outputs
#' @param id module identifier
parcoordUi <- function(id){
  shiny::tagList(
    shiny::plotOutput(outputId = shiny::NS(id,"uniParcoord"),width = "100%",height = "800px"),
    shiny::verbatimTextOutput(outputId = shiny::NS(id,"uniParcoord_info")),
    shiny::tableOutput(outputId = shiny::NS(id,"uniParcoord_data")),
    # shiny::uiOutput(outputId = shiny::NS(id,"uniParcoord")),
    shiny::uiOutput(shiny::NS(id, "multiPlot"))
  )
}

uniParcoordServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # output$uniParcoord <- shiny::renderPlot({
    #   if(length(input$catVar)==1){
    #     parallel_coordinate_plot(data(), input$numVar, input$catVar)
    #   }
    # },res = 96)
    
    uniParcoord <- shiny::reactive({
      if(length(input$catVar)==1){
        parallel_coordinate_plot(data(), input$numVar, input$catVar)
      }
    })
    output$uniParcoord <- shiny::renderPlot({
      shiny::req(uniParcoord())
      uniParcoord() + geom_text(mapping = aes(x = uniParcoord()$data$variable,
                                              y = round(uniParcoord()$data$value,2),label=uniParcoord()$data$.ID))
    })
    
    # output$uniParcoord_info <- shiny::renderPrint({
    #   shiny::req(input$uniParcoord_click)
    #   # cat("[", round(input$uniParcoord_click$x,2), ", ", round(input$uniParcoord_click$y,2), "]", sep = "")
    #   x = uniParcoord()$data$variable
    #   y = round(uniParcoord()$data$value,2)
    # })
    # 
    # uniParcoord_data <- shiny::reactive({
    #   shiny::reactive(input$uniParcoord_click)
    #   shiny::nearPoints(uniParcoord()$data, input$uniParcoord_click)
    # })
    # 
    # output$uniParcoord_data <- shiny::renderTable({
    #   shiny::req(uniParcoord_data())
    #   if(dim(uniParcoord_data())[1]>0){
    #     data() %>% dplyr::slice(as.numeric(uniParcoord_data()$.ID))
    #   }
    # })
    
  })
}

#' Server module rendering a parallel plot coordinate. It can be linked with parcoordUi by id.
#' @param id identifier of the module
#' @param data dataframe

multiplotOutput <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    
    catVar <- shiny::reactive({
      if(base::all(input$catVar=="")){
        "noClass"
      }else if(base::all(input$catVar!="")){
        if(any(is.na(unique(data()[[input$catVar[1]]])))){
          shiny::showNotification(paste0("ERROR !!!: missing values in variable ", input$catVar),duration = NULL,closeButton = TRUE,type = "error")
          unique(data()[[input$catVar[1]]])[!is.na(unique(data()[[input$catVar[1]]]))]
        }else if(all(!is.na(unique(data()[[input$catVar[1]]])))){
          unique(data()[[input$catVar[1]]])
        }
        
      }
    })
    
    output$multiPlot <- shiny::renderUI({
      shiny::req(catVar(), input$catVar)
      if(length(input$catVar)==1 | length(input$catVar)==2){
        purrr::map(catVar(), shiny::plotOutput)
      }
    })
    
    return(shiny::reactive(catVar()))
  })
}

parallel_coordinate_plot <- function(data, numVar=NULL, catVar=NULL, var=NULL){#var=NULL
  if(!is.null(var)){
    data <- data[data[which(colnames(data)==catVar[1])]==var,]
  }
  if(!(is.null(catVar) | is.null(numVar)) && length(catVar)==1){
    if(all(catVar=="") && all(numVar=="")){
      GGally::ggparcoord(data = data, columns = as.integer(numericIndex(data))) + theme(axis.text.x = element_text(angle = 90))
    }else if(all(catVar=="") && all(numVar!="") && length(numVar)>1 ){
      GGally::ggparcoord(data = data, columns = which(colnames(data) %in% numVar)) + theme(axis.text.x = element_text(angle = 90))
    }else if(all(catVar!="") && all(numVar=="")){
      GGally::ggparcoord(data, columns = as.integer(numericIndex(data)), groupColumn = catVar, title = paste("~", catVar)) +
        theme(axis.text.x = element_text(angle = 90))
    }else if(all(catVar!="") && all(numVar!="") && length(numVar)>1 ){
      GGally::ggparcoord(data, columns = which(colnames(data) %in% numVar), groupColumn = catVar,
                         title = paste("~", catVar)) + theme(axis.text.x = element_text(angle = 90))
    }
  }else if(!(is.null(catVar) | is.null(numVar) | is.null(var)) && length(catVar)==2){
    if(all(catVar!="") && all(numVar=="")){
      GGally::ggparcoord(data, columns = as.integer(numericIndex(data)), groupColumn = catVar[2], 
                         title = paste(var, "~", catVar[2])) + theme(axis.text.x = element_text(angle = 90))
    }else if(all(catVar!="") && all(numVar!="") && length(numVar)>1 ){
      GGally::ggparcoord(data, columns = which(colnames(data) %in% numVar), groupColumn = catVar[2], 
                         title = paste(var, "~", catVar[2])) + theme(axis.text.x = element_text(angle = 90))
    }
  }
}

#' Server module rendering a parallel plot coordinate. It can be linked with plotUi by id.
#' @param id identifier of the module
#' @param data dataframe

parcoordPlotServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    output$plot <- shiny::renderPlot({
      if(!is.null(data())){
        parallel_coordinate_plot(data = data(),numVar = input$numVar,catVar = input$catVar)
      }
    },res = 96)
  })
}


#' parallel coordinate plot of data with a column for cluster membership
#' @param dataclust dataframe with a column for cluster membership
#' @param varOrder method used to order variables in the horizontal axis
#' @return a dbscan object
parcoord_plot <- function(dataclust, varOrder=NULL){
  if(!is.null(varOrder)){
    if(any(dataclust$cluster==0)){
      if(varOrder!=""){
        dataclust %>% 
          GGally::ggparcoord(columns = numericIndex(dataclust),
                             groupColumn = categoricIndex(dataclust), 
                             order = varOrder)
      }else{
        dataclust %>% 
          GGally::ggparcoord(columns = numericIndex(dataclust),
                             groupColumn = categoricIndex(dataclust))
      }
    }else{
      if(varOrder!=""){
        dataclust %>% GGally::ggparcoord(columns = numericIndex(dataclust),groupColumn = 1,order = varOrder)
      }else{
        dataclust %>% GGally::ggparcoord(columns = numericIndex(dataclust),groupColumn = 1)
      }
    }
  }else{
    if(any(dataclust$cluster==0)){
      dataclust %>% 
        GGally::ggparcoord(columns = numericIndex(dataclust),
                           groupColumn = categoricIndex(dataclust))
    }else{
      dataclust %>% GGally::ggparcoord(columns = numericIndex(dataclust),groupColumn = 1)
    }
  }
  
}




#' plot outlier if there is some.
#' @param rawdata the raw dataset in input.
#' @param outdata the dataset with logical variable
#' @return a parallel coordinates 
#' @importFrom purrr is_empty
#' @importFrom stats formula
outplot <- function(rawdata, outdata){
  
  dataset <- dplyr::bind_cols(rawdata, outdata)
  # lapply(X = dataset,FUN = is.numeric) %>% data.frame() %>% apply(MARGIN = 2,FUN = isTRUE)
  num_col <- sapply(X = dataset,FUN = is.numeric) %>% which()
  cat_col <- sapply(X = dataset,FUN = is.factor) %>% which()
  bool_col <- sapply(X = dataset,FUN = is.logical) %>% which()
  if(!(is_empty(dataset[bool_col]) & is_empty(dataset[cat_col]))){
    
    if(!(is_empty(dataset[bool_col]) | is_empty(dataset[cat_col]))){
      
      if (!(is_empty(which(dataset[[names(bool_col)]])) | length(unique(dataset[[names(cat_col)]]))<2)){
        GGally::ggparcoord(data = dataset, columns = num_col, groupColumn = names(bool_col)) + 
          ggplot2::facet_wrap(formula(paste("~dataset$",names(cat_col))))
      }else if(is_empty(which(dataset[[names(bool_col)]])) & !length(unique(dataset[[names(cat_col)]]))<2){
        # GGally::ggparcoord(data = dataset, columns = num_col, alphaLines = 0.2, groupColumn = names(cat_col))
        GGally::ggparcoord(data = dataset, columns = num_col, alphaLines = 0.2) +
          ggplot2::facet_wrap(formula(paste("~dataset$",names(cat_col))))
      }else if(!is_empty(which(dataset[[names(bool_col)]])) & length(unique(dataset[[names(cat_col)]]))<2){
        GGally::ggparcoord(data = dataset, columns = num_col, alphaLines = 0.2, groupColumn = names(bool_col))
      }else if(is_empty(which(dataset[[names(bool_col)]])) & length(unique(dataset[[names(cat_col)]]))<2){
        GGally::ggparcoord(data = dataset, columns = num_col, alphaLines = 0.2)
      }
      
    }else if(is_empty(dataset[bool_col]) | !is_empty(dataset[cat_col])){
      GGally::ggparcoord(data = dataset, columns = num_col, alphaLines = 0.2, groupColumn = names(cat_col)) +
        ggplot2::facet_wrap(formula(paste("~dataset$",names(cat_col))))
    }else if(!is_empty(dataset[bool_col]) | is_empty(dataset[cat_col])){
      if(!is_empty(which(dataset[[names(bool_col)]]))){
        GGally::ggparcoord(data = dataset, columns = num_col, groupColumn = names(bool_col)) 
        # +facet_wrap(formula(paste("~dataset$",names(bool_col))))
      }
    }
    
  }else{
    GGally::ggparcoord(data = dataset, columns = num_col, alphaLines = 0.2)
  }
}

