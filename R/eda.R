#' User interface where for descriptive statistics
#' @param id identifier for the data module
#' @return a tagList object
edaUi <- function(id){
  shiny::tagList(
    shiny::h3("Data overview"),
    DT::dataTableOutput(shiny::NS(id,"data")),
    shiny::verbatimTextOutput(shiny::NS(id,"model")),
    shiny::h3("Descriptive Stats by variable"),
    shiny::verbatimTextOutput(shiny::NS(id,"stats")),
    # DT::dataTableOutput(shiny::NS(id,"stats")),
    shiny::h3("Number of missing values by record"),
    shiny::plotOutput(outputId = shiny::NS(id,"sample_na"),width = "100%",height = "800px"),
    shiny::verbatimTextOutput(shiny::NS(id,"na")),
    DT::dataTableOutput(shiny::NS(id,"na_data")),
    shiny::h3("Number of Below Detection Limit missingness by record"),
    shiny::verbatimTextOutput(shiny::NS(id,"dl")),
    DT::dataTableOutput(shiny::NS(id,"dl_data")),
    shiny::h3("Data with wrong decimal separator"),
    DT::dataTableOutput(shiny::NS(id,"dec_sep")),
    # DT::dataTableOutput(shiny::NS(id,"na_dl_data")),
  )
}

#' Visualize overview of data and descriptive statistics in the edaUi
#' @param id identifier of the data module
#' @param data output of the dataImport
#' @return descriptive statistics and missing values distribution
#'@importFrom skimr skim_with sfl
#'@importFrom purrr %>%
edaOutput <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    
    n_LoD <- skimr::skim_with(character=skimr::sfl(n_bDL),numeric=skimr::sfl(hist=NULL))
    
    shiny::observeEvent(data(),{
      
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
      
      if(!is.null(data()) && inherits(data(),"data.frame") && dim(data())[1]>0 && 
         apply(X = data(),MARGIN = 2,function(col){!is.raw(col[[1]])}) %>% all()){
        
        used_data <- shiny::reactive({
          if(!is.null(input$catVar)){
            if(all(input$catVar=="") && all(input$catVarValue=="")){
              data()
            }else if(all(input$catVar!="") && length(input$catVar)==1 && all(input$catVarValue=="")){
              if(input$catVar %in% names(data())){
                data() %>% dplyr::group_by(.data[[input$catVar[1]]])
              }
            }else if(all(input$catVar!="") && length(input$catVar)==1 && all(input$catVarValue!="") && length(input$catVarValue)==1 ){
              if(input$catVar %in% names(data()) && input$catVarValue %in% unique(data()[[input$catVar]])){
                data() %>% dplyr::group_by(.data[[input$catVar[1]]]) %>% dplyr::filter(.data[[input$catVar[1]]]==input$catVarValue)
              }
            }
            
          }else{
            data()
          }
        })
        
        output$data <- DT::renderDataTable({used_data()})
        
        output$stats <- shiny::renderPrint({ #DT::renderDataTable({})
          shiny::req(used_data())
          used_data() %>% n_LoD()
        })
        
        n_mis <- shiny::reactive({
          shiny::req(used_data())
          apply(X = used_data(),MARGIN = 1,FUN = n_missing)
        })
        
        output$sample_na <- shiny::renderPlot({
          shiny::req(used_data())
          visdat::vis_dat(used_data())
        })
        
        output$na <- shiny::renderPrint({
          shiny::req(n_mis())
          n_mis()
        })
        
        output$na_data <- DT::renderDataTable({
          shiny::req(n_mis())
          if(any(n_mis() >= floor(length(numericIndex(used_data()))/2))){
            used_data() %>% dplyr::filter(n_mis() >= floor(length(numericIndex(used_data()))/2))
          }
        })
        
        n_dl <- shiny::reactive({
          shiny::req(used_data())
          apply(X = used_data(),MARGIN = 1,FUN = n_bDL)
        })
        
        output$dl <- shiny::renderPrint({
          shiny::req(n_dl())
          if(!is.null(n_dl())) n_dl()
        })
        
        output$dl_data <- DT::renderDataTable({
          shiny::req(n_dl())
          if(any(n_dl() >= floor(length(numericIndex(used_data()))/2))){
            used_data() %>% dplyr::filter(n_dl() >= floor(length(numericIndex(used_data()))/2))
          }
        })
        
        output$dec_sep <- DT::renderDataTable({
          shiny::req(used_data())
          wrong_decimal_separator_data(used_data())
        })
        
      }
    })
  })
}

#' Visualize overview of data and descriptive statistics in the edaUi
#' @param id identifier of the data module
#' @param data output of the dataImport
#' @return descriptive statistics and missing values distribution
#'@importFrom skimr skim_with sfl
#'@importFrom purrr %>%
edaOutput2 <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    
    n_LoD <- skimr::skim_with(character=skimr::sfl(n_bDL),numeric=skimr::sfl(hist=NULL))
    
    output$stats <- shiny::renderPrint({
      if(!is.null(data()) && inherits(data(),"data.frame") && dim(data())[1]>0 &&
         apply(X = data(),MARGIN = 2,function(col){!is.raw(col[[1]])}) %>% all()){
        if(!is.null(input$catVar)){
          if(all(input$catVar=="")){
            n_LoD(data()) #%>% dplyr::select(-.data$numeric.hist)
          }else if(all(input$catVar!="") && length(input$catVar)==1){
            if(input$catVar %in% names(data())){
              data() %>% dplyr::group_by(.data[[input$catVar[1]]]) %>% n_LoD() #%>% dplyr::select(-.data$numeric.hist)
            }
          }
        }
      }
    })
    
    n_mis <- shiny::reactive({
      if(!is.null(data())){
        apply(X = data(),MARGIN = 1,FUN = n_missing)
      }
    })
    
    output$na <- shiny::renderPrint({
      if(inherits(data(),"data.frame") && apply(X = data(),MARGIN = 2,function(col){!is.raw(col[[1]])}) %>% all()){
        if(!is.null(n_mis())){
          n_mis()
        }
      }
    })
    
    output$na_data <- DT::renderDataTable({
      if(inherits(data(),"data.frame") && apply(X = data(),MARGIN = 2,function(col){!is.raw(col[[1]])}) %>% all()){
        if(any(n_mis() >= floor(length(numericIndex(data()))/2))){
          data() %>% dplyr::filter(n_mis() >= floor(length(numericIndex(data()))/2))
        }
      }
    })
    
    n_dl <- shiny::reactive({
      if(!is.null(data())){}
      apply(X = data(),MARGIN = 1,FUN = n_bDL)
    })
    output$dl <- shiny::renderPrint({
      if(inherits(data(),"data.frame") && apply(X = data(),MARGIN = 2,function(col){!is.raw(col[[1]])}) %>% all()){
        if(!is.null(n_dl())){
          n_dl()
        }
      }
    })
    
    output$dl_data <- DT::renderDataTable({
      if(inherits(data(),"data.frame") && apply(X = data(),MARGIN = 2,function(col){!is.raw(col[[1]])}) %>% all()){
        if(any(n_dl() >= floor(length(numericIndex(data()))/2))){
          data() %>% dplyr::filter(n_dl() >= floor(length(numericIndex(data()))/2))
        }
      }
    })
    output$dec_sep <- DT::renderDataTable({
      if(inherits(data(),"data.frame") && apply(X = data(),MARGIN = 2,function(col){!is.raw(col[[1]])}) %>% all()){
        if(!is.null(data())){
          wrong_decimal_separator_data(data())
        }
      }
    })
    
    n_mis_dl <- shiny::reactive({
      shiny::req(data())
      apply(X = data(),MARGIN = 1,FUN = n_missing_bDL)
    })
    output$na_dl_data <- shiny::renderTable({
      if(any(n_mis_dl() >= floor(length(numericIndex(data()))/2))){
        data() %>% dplyr::filter(n_mis_dl() >= floor(length(numericIndex(data()))/2))
      }
    })
    
  })
}