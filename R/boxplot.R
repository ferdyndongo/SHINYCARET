#' User Interface for parallel coordinate plot with inputs and outputs
#' @param id module identifier
boxplotUi <- function(id){
  # shiny::tagList(
  # shiny::uiOutput(outputId = shiny::NS(id,"uniParcoord"),width = "100%",height = "800px"),
  shiny::uiOutput(shiny::NS(id, "boxplot"))
  # )
}

numeric_plotOutput <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    
    numVar <- shiny::reactive({
      if(base::all(input$numVar!="")){
        input$numVar
      }else if(base::all(input$numVar=="")){
        names(numericIndex(data()))
      }
    })
    
    output$boxplot <- shiny::renderUI({
      # shiny::req(numVar())
      purrr::map(numVar(), shiny::plotOutput)
    })
    
    return(shiny::reactive(numVar()))
  })
}

box_plot <- function(data, numVar=NULL, catVar=NULL){
  if(!(is.null(catVar) | is.null(numVar))){
    if(all(catVar=="")){
      # graphics::boxplot(data[[numVar]],horizontal=TRUE,main=numVar)
      ggplot2::ggplot(data = data,mapping = aes(x = get(numVar))) + ggplot2::geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4) +
        ggplot2::xlab(numVar)
    }else if(all(catVar!="") && length(catVar)==1){
      # graphics::boxplot(formula(paste(numVar,"~",catVar)),data=data,horizontal=TRUE,las=1,main=paste(numVar,"~",catVar))
      ggplot2::ggplot(data = data,mapping = aes(x = get(numVar),y = get(catVar))) + ggplot2::geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4) +
        ggplot2::xlab(numVar) + ggplot2::ylab(catVar)
    }
  }
}

# box_plot <- function(data, numVar=NULL, catVar=NULL){
#   if(!(is.null(catVar) | is.null(numVar))){
#     if(all(catVar=="") & all(numVar!="")){
#       if(length(numVar)==1){
#         graphics::boxplot(data[[numVar]],horizontal=TRUE,main=numVar)
#       }else if(length(numVar)>1){
#         graphics::boxplot(data %>% dplyr::select(dplyr::all_of(numVar)) %>% scale(),horizontal = TRUE)
#       }
#     }else if(all(catVar!="") & all(numVar!="")){
#       if(length(catVar)==1){
#         graphics::par(mfrow=c(length(numVar),1))
#         for(var in numVar){
#           graphics::boxplot(formula(paste(var,"~",catVar)),data=data,horizontal=TRUE,las=1,main=paste(var,"~",catVar))
#         }
#       }
#     }
#   }
# }

#' Server module rendering a boxplot. It can be linked with plotUi by id.
#' @param id module identifier
#' @param data dataframe
boxplotServer <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    output$plot <- shiny::renderPlot({
      if(!is.null(data())){
        box_plot(data = data(), numVar = input$numVar, catVar = input$catVar)
      }
    },res = 96)
  })
}


#' Outlier identification with boxplot
#' @param dataset a dataframe of numerical variables
#' @return dataframe with a variable `all` specifying if the observation is outlier for all variables
boxOut <- function(dataset){
  outbox <- list()
  vars <- colnames(dataset)
  for(var in vars) outbox[[var]] <- data.frame(outlier = dataset[[var]] %in% grDevices::boxplot.stats(dataset[[var]])$out)
  if(length(vars)>1) outbox$all <- data.frame(outlier=apply(X = data.frame(outbox),MARGIN = 1,FUN = all))
  # index_out <- which(apply(X = data.frame(outbox),MARGIN = 1,FUN = all))
  # data <- dplyr::bind_cols(dataset,out=outbox$all)
  return (outbox)
}
