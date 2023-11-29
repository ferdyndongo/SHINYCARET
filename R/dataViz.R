#' User interface where the data overview will be written
#' @param id identifier for the data module
#' @return a tagList object
dataVizUi <- function(id){
  shiny::tagList(
    # shiny::h3("Data overview"),
    DT::dataTableOutput(shiny::NS(id,"data")),
    # shiny::uiOutput(shiny::NS(id,"data")),
    shiny::verbatimTextOutput(shiny::NS(id,"model"))
  )
}

#' Visualize overview of data
#' @param id identifier of the data module
#' @param data output of the dataImport
#' @return some plots written in the dataUi
#' @importFrom purrr %>% 
dataVizOutput <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    
    output$data <- DT::renderDataTable({
      if(!is.null(data()) && inherits(data(),"data.frame")){
        if(inherits(data(),"data.frame") && dim(data())[1]>0 && 
           apply(X = data(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% any()){
          data()[1:3]
        }else{
          data()
        }
      }
    })
    output$model <- shiny::renderPrint({
      if(!is.null(data())){
        if(inherits(data(),"data.frame") && dim(data())[1]>0 && 
           apply(X = data(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% any()){
          raw_index <- apply(X = data(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% which()
          model <- unserialize(data()[[raw_index]][[1]])
          caret::print.train(model)
        }else if(inherits(data(), "train.formula")){
          data()
        }
      }
    })
  })
}