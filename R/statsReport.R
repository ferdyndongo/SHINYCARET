#' User interface where for descriptive statistics
#' @param id identifier for the data module
#' @return a tagList object
statsReportUi <- function(id){
  shiny::tagList(
    shiny::h3("Data overview"),
    DT::dataTableOutput(shiny::NS(id,"data")),
    # shiny::uiOutput(shiny::NS(id,"data")),
    shiny::verbatimTextOutput(shiny::NS(id,"model")),
    shiny::h3("Descriptive Statistics"),
    DT::dataTableOutput(shiny::NS(id,"stats")),
    shiny::fluidRow(
      shinydashboard::box(shiny::downloadButton(shiny::NS(id, "download")),width = 2),
      # shinydashboard::box(shiny::downloadButton(outputId = shiny::NS(id,"report"),label =  "Generate report"),width = 2),
      # shinydashboard::box(shiny::radioButtons(inputId = shiny::NS(id,"format"),label = "Document format",
      #                                         choices = c("RNotebook","HTML","PDF"),
      #                                         selected = NULL,inline = TRUE))
    )
  )
}

#' Visualize overview of data and descriptive statistics in the edaUi
#' @param id identifier of the data module
#' @param data output of the dataImport
#' @return descriptive statistics and missing values distribution
#' @importFrom skimr skim_with sfl
#' @importFrom purrr %>%
statsReportOutput <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    
    sum_stats <- skimr::skim_with(numeric=skimr::sfl(sample_size=not_NA,hist=NULL))
    shiny::observeEvent(data(),{
      
      if(!is.null(data()) && inherits(data(),"data.frame") && dim(data())[1]>0 && 
         apply(X = data(),MARGIN = 2,function(col){!is.raw(col[[1]])}) %>% all()){
        
        used_data <- shiny::reactive({
          if(!is.null(input$catVar)){
            if(all(input$catVar=="") && all(input$catVarValue=="")){
              data()
            }else if(all(input$catVar!="") && length(input$catVar)==1 && all(input$catVarValue=="")){
              if(input$catVar %in% names(data())){
                data()  %>% dplyr::group_by(.data[[input$catVar[1]]])
              }
            }else if(all(input$catVar!="") && length(input$catVar)==1 && all(input$catVarValue!="") && length(input$catVarValue)==1 ){
              if(input$catVar %in% names(data()) && input$catVarValue %in% unique(data()[[input$catVar]])){
                data() %>% dplyr::group_by(.data[[input$catVar[1]]]) %>% 
                  dplyr::filter(.data[[input$catVar[1]]]==input$catVarValue)
              }
            }
            
          }
        })
        
        output$data <- DT::renderDataTable({
          shiny::req(used_data())
          used_data()
        })
        
        statistics <- shiny::reactive({
          shiny::req(used_data())
          used_data() %>% sum_stats()
        })
        
        stats <- shiny::reactive({
          shiny::req(statistics())
          if("numeric" %in% statistics()$skim_type){
            if(!is.null(input$catVar)){
              if(all(input$catVar=="") && all(input$catVarValue=="")){
                statistics() %>% dplyr::filter(.data$skim_type=="numeric") %>% 
                  dplyr::select(c(.data$skim_variable, .data$numeric.sample_size, .data$numeric.mean, .data$numeric.p0, 
                                  .data$numeric.p25, .data$numeric.p50, .data$numeric.p75, .data$numeric.p100, .data$numeric.sd)) %>% 
                  dplyr::rename(Variable=.data$skim_variable, Max=.data$numeric.p100,  Mean=.data$numeric.mean, SD=.data$numeric.sd, Min=.data$numeric.p0,
                                P25=.data$numeric.p25, P50=.data$numeric.p50, P75=.data$numeric.p75, N=.data$numeric.sample_size) %>%
                  # dplyr::rename(Parametro=.data$skim_variable, Massimo=.data$numeric.p100,  Media=.data$numeric.mean, 
                  #               `Deviazione Standard`=.data$numeric.sd, Minimo=.data$numeric.p0, `1° Quartile`=.data$numeric.p25, 
                  #               Mediana=.data$numeric.p50, `3° Quartile`=.data$numeric.p75,`Numero Campioni`=.data$numeric.sample_size) %>% 
                  tibble::as_tibble()
              }else if(all(input$catVar!="") && length(input$catVar)==1 && all(input$catVarValue=="")){
                if(input$catVar %in% names(statistics())){
                  statistics() %>% dplyr::filter(.data$skim_type=="numeric") %>% 
                    dplyr::select(c(.data[[input$catVar]],.data$skim_variable, .data$numeric.sample_size, .data$numeric.mean, 
                                    .data$numeric.p0, .data$numeric.p25, .data$numeric.p50, .data$numeric.p75, .data$numeric.p100, 
                                    .data$numeric.sd)) %>% dplyr::arrange(.data[[input$catVar]]) %>%
                    dplyr::rename(Variable=.data$skim_variable, Max=.data$numeric.p100,  Mean=.data$numeric.mean, SD=.data$numeric.sd, Min=.data$numeric.p0,
                                  P25=.data$numeric.p25, P50=.data$numeric.p50, P75=.data$numeric.p75, N=.data$numeric.sample_size) %>%
                    # dplyr::rename(Parametro=.data$skim_variable, Massimo=.data$numeric.p100,  Media=.data$numeric.mean, 
                    #               `Deviazione Standard`=.data$numeric.sd, Minimo=.data$numeric.p0, `1° Quartile`=.data$numeric.p25, 
                    #               Mediana=.data$numeric.p50, `3° Quartile`=.data$numeric.p75,`Numero Campioni`=.data$numeric.sample_size) %>%
                    tibble::as_tibble()
                }
              }else if(all(input$catVar!="") && length(input$catVar)==1 && all(input$catVarValue!="") && length(input$catVarValue)==1 ){
                if(input$catVar %in% names(statistics()) && input$catVarValue %in% unique(statistics()[[input$catVar]])){
                  statistics() %>% dplyr::filter(.data$skim_type=="numeric") %>% 
                    dplyr::select(c(.data[[input$catVar]],.data$skim_variable, .data$numeric.sample_size, .data$numeric.mean, 
                                    .data$numeric.p0, .data$numeric.p25, .data$numeric.p50, .data$numeric.p75, 
                                    .data$numeric.p100, .data$numeric.sd)) %>%
                    dplyr::rename(Variable=.data$skim_variable, Max=.data$numeric.p100,  Mean=.data$numeric.mean, SD=.data$numeric.sd, Min=.data$numeric.p0,
                                  P25=.data$numeric.p25, P50=.data$numeric.p50, P75=.data$numeric.p75, N=.data$numeric.sample_size) %>%
                    # dplyr::rename(Parametro=.data$skim_variable, Massimo=.data$numeric.p100,  Media=.data$numeric.mean, 
                    #               `Deviazione Standard`=.data$numeric.sd, Minimo=.data$numeric.p0, `1° Quartile`=.data$numeric.p25, 
                    #               Mediana=.data$numeric.p50, `3° Quartile`=.data$numeric.p75,`Numero Campioni`=.data$numeric.sample_size) %>%
                    tibble::as_tibble()
                }
              }
              
            }
          }
          else{
            statistics()
          }
          
        })
        
        output$stats <- DT::renderDataTable({
          shiny::req(stats())
          if(!is.null(input$catVar)){
            if(all(input$catVar=="") && all(input$catVarValue=="")){
              stats()
            }else if(all(input$catVar!="") && length(input$catVar)==1 && all(input$catVarValue=="")){
              if(input$catVar %in% names(data())){
                stats() %>% dplyr::arrange(stats()[input$catVar])
              }
            }else if(all(input$catVar!="") && length(input$catVar)==1 && all(input$catVarValue!="") && length(input$catVarValue)==1 ){
              if(input$catVar %in% names(data()) && input$catVarValue %in% unique(data()[[input$catVar]])){
                stats()
              }
            }
            
          }
          
        })
        
        
        
        output$download <- shiny::downloadHandler(
          filename = function(){
            if(!is.null(statistics()) && inherits(statistics(),"data.frame") && dim(statistics())[1]>0) {
              if(!is.null(input$catVarValue) && input$catVarValue!=""){
                shiny::req(input$catVarValue)
                paste0("STATS_",input$catVarValue, ".xlsx")
              }else if(!is.null(input$catVar) && input$catVar!=""){
                shiny::req(input$catVar)
                paste0("STATS_" , input$catVar, ".xlsx")
              }else if(!is.null(input$sheet) && input$sheet!=""){
                paste0("STATS_" , input$sheet, ".xlsx")
              }else{
                paste0("STATS_REPORT",".xlsx")
              }
              
            }
          },
          content = function(file){
            if(!is.null(stats()) && inherits(stats(),"data.frame") && dim(stats())[1]>0) {
              if(!is.null(input$catVarValue) && input$catVarValue!=""){
                shiny::req(input$catVarValue)
                id <- shiny::showNotification("WAIT WHILE DOWNLOADING STATISTICS REPORT ...", duration = NULL, closeButton = FALSE)
                base::on.exit(shiny::removeNotification(id), add = TRUE)
                # download each categorical variable in its own sheet
                # ls_stats <- split(x = stats(), f = input$catVarValue)
                tryCatch({
                  # WriteXLS::WriteXLS(x = ls_stats, file)
                  # WriteXLS::WriteXLS(x = stats(), file)
                  writexl::write_xlsx(x = stats(), file)
                },warning=function(w){
                  shiny::showNotification(w$message,duration = NULL,closeButton = TRUE,type = "warning")
                },error=function(e){
                  shiny::showNotification(e$message,duration = NULL,closeButton = TRUE,type = "error")
                })
              }else if(!is.null(input$catVar) && input$catVar!=""){
                shiny::req(input$catVar)
                id <- shiny::showNotification("WAIT WHILE DOWNLOADING STATISTICS REPORT ...", duration = NULL, closeButton = FALSE)
                base::on.exit(shiny::removeNotification(id), add = TRUE)
                # ls_stats <- split(x = stats(), f = stats()[input$catVar])
                tryCatch({
                  # WriteXLS::WriteXLS(x = ls_stats, file)
                  # WriteXLS::WriteXLS(x = stats(), file)
                  writexl::write_xlsx(x = stats(), file)
                },warning=function(w){
                  shiny::showNotification(w$message,duration = NULL,closeButton = TRUE,type = "warning")
                },error=function(e){
                  shiny::showNotification(e$message,duration = NULL,closeButton = TRUE,type = "error")
                })
                
              }else if(!is.null(input$sheet) && input$sheet!=""){
                shiny::req(input$sheet)
                id <- shiny::showNotification("WAIT WHILE DOWNLOADING STATISTICS REPORT ...", duration = NULL, closeButton = FALSE)
                base::on.exit(shiny::removeNotification(id), add = TRUE)
                tryCatch({
                  # WriteXLS::WriteXLS(x = stats(), file)
                  writexl::write_xlsx(x = stats(), file)
                },warning=function(w){
                  shiny::showNotification(w$message,duration = NULL,closeButton = TRUE,type = "warning")
                },error=function(e){
                  shiny::showNotification(e$message,duration = NULL,closeButton = TRUE,type = "error")
                })
                
              }else{
                id <- shiny::showNotification("WAIT WHILE DOWNLOADING STATISTICS REPORT ...", duration = NULL, closeButton = FALSE)
                base::on.exit(shiny::removeNotification(id), add = TRUE)
                # WriteXLS::WriteXLS(x = stats(), file)
                writexl::write_xlsx(x = stats(), file)
              }
              
            }
          }
        )
      }
    })
  })
}
