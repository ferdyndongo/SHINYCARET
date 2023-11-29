pca_biplot <- function(pcaObject, data, biplotVar=NULL, dim1=NULL, dim2=NULL){
  if(!( is.null(pcaObject) || is.null(data) || is.null(biplotVar) || is.null(dim1) || is.null(dim2) )){
    if(biplotVar=="" || (biplotVar!="" && length(unique(data[[biplotVar]]))==1)){
      tryCatch({
        factoextra::fviz_pca_biplot(X = pcaObject,addEllipses = TRUE,axes = c(as.numeric(dim1),as.numeric(dim2)))
      },warning=function(w){
        shiny::showNotification(w$message,duration = NULL,closeButton = TRUE,type = "warning")
      },error=function(e){
        shiny::showNotification(e$message,duration = NULL,closeButton = TRUE,type = "error")
      })
    }else if(biplotVar!="" && length(unique(data[[biplotVar]]))>1){
      if(is.null(pcaObject[["na.action"]])){
        tryCatch({
          factoextra::fviz_pca_biplot(X = pcaObject, habillage = data[[biplotVar]], addEllipses = TRUE,
                                      axes = c(as.numeric(dim1),as.numeric(dim2)))
        },warning=function(w){
          shiny::showNotification(w$message,duration = NULL,closeButton = TRUE,type = "warning")
        },error=function(e){
          shiny::showNotification(e$message,duration = NULL,closeButton = TRUE,type = "error")
        })
      }else{
        tryCatch({
          factoextra::fviz_pca_biplot(X = pcaObject, habillage = data[-pcaObject[["na.action"]],][[biplotVar]], addEllipses = TRUE,
                                      axes = c(as.numeric(dim1),as.numeric(dim2)))
        },warning=function(w){
          shiny::showNotification(w$message,duration = NULL,closeButton = TRUE,type = "warning")
        },error=function(e){
          shiny::showNotification(e$message,duration = NULL,closeButton = TRUE,type = "error")
        })
      }
      
    }
  }
}

#' Server module rendering a pca biplot. It can be linked with plotUi by id.
#' @param id identifier of the module
#' @param pcaObject an list object with results of pca
#' @param data original data passed to pca. It is used to group individuals in biplot.
pca_biplotServer <- function(id, pcaObject, data) {
  shiny::moduleServer(id, function(input, output, session) {
    output$pca_biplot <- shiny::renderPlot({
      if(!( is.null(pcaObject()) || is.null(data()))){
        pca_biplot(pcaObject(), data(), biplotVar=input$biplotVar, dim1=input$dim1, dim2=input$dim2)
      }
    },res = 96)
  })
}