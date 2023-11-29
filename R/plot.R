#' Generic container object for plot
#' @param id module identifier
plotUi <- function(id){
  shiny::plotOutput(outputId = shiny::NS(id,"plot"), width = "100%",height = "800px")
}

#' Ui container for some plots in Graphical Exploration Analysis
#' @param id module identifier
plotVizUi <- function(id){
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(shiny::selectInput(shiny::NS(id,"catVar"),label = "", multiple = TRUE,
                                             choices = NULL, selected = NULL, selectize = FALSE),width = 2),
      shinydashboard::box(shiny::selectInput(shiny::NS(id,"numVar"),label = "", multiple = TRUE,
                                             choices = NULL, selected = NULL, selectize = FALSE),width = 2)
    ),
    shiny::h3("parallel coordinates plot"),
    shiny::uiOutput(shiny::NS(id,"plotPar"))
    # shiny::plotOutput(shiny::NS(id,"par_coord_plot")),
    # shiny::fluidRow(
    #   shinydashboard::box(shiny::selectInput(shiny::NS(id,"boxcatVar"),label = "", multiple = TRUE, choices = NULL, selected = NULL, selectize = FALSE),width = 2),
    #   shinydashboard::box(shiny::selectInput(shiny::NS(id,"boxnumVar"),label = "", multiple = TRUE, choices = NULL, selected = NULL, selectize = FALSE),width = 2)
    # ),
    # shiny::h3("Boxplot"),
    # shiny::plotOutput(shiny::NS(id,"boxplot")),
  )
}


#' Server for plotVizUi in charge of fill Ui with plots in the App
#' @param id module identifier
#' @param data data used
#' @importFrom ggplot2 theme element_text
plotVizServer <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    cat_col <- shiny::reactive({
      shiny::req(data)
      (sapply(X = data(),FUN = is.factor) | sapply(X = data(),FUN = is.character)) %>% which() %>% names()
    })
    num_col <- shiny::reactive({
      shiny::req(data())
      names(numericDataset(data()))
    })
    
    shiny::observeEvent(cat_col(),{
      shiny::updateSelectInput(inputId = "catVar",label = "Categorical Variable", choices = c("",cat_col()),selected = "")
    })
    shiny::observeEvent(num_col(),{
      shiny::updateSelectInput(inputId = "numVar",label = "Numeric Variable",choices = c("",num_col()),selected = "")
    })
    
    shiny::observeEvent(input$catVar,{
      catVar <- ifelse(base::all(input$catVar==""),"noClass",unique(data()[[input$catVar[1]]]))
      output$plotPar <- shiny::renderUI({
        if(length(input$catVar)==1 | length(input$catVar)==2){
          purrr::map(catVar,shiny::plotOutput)
        }
      })
    })
    shiny::observeEvent(input$catVar,{

      if(!(is.null(input$catVar)) | is.null(input$numVar)){
        
        # catVar <- shiny::reactive({
        #   if(base::all(input$catVar=="")){
        #     "noClass"
        #   }else if(base::all(input$catVar!="")){
        #     unique(data()[[input$catVar[1]]])
        #   }
        # })
        # output$plotPar <- shiny::renderUI({
        #   shiny::req(catVar())
        #   if(length(input$catVar)==1 | length(input$catVar)==2){
        #     purrr::map(catVar(),plotOutput)
        #   }
        # })
        
        for(i in 1:length(unique(data()[[input$catVar[1]]]))){
          base::local({
            var <- shiny::reactive({unique(data()[[input$catVar[1]]])[i]})
            output[[var()]] <- shiny::renderPlot({
              if(all(input$catVar=="") & all(input$numVar=="")){
                GGally::ggparcoord(data(), columns = numericIndex(data())) + theme(axis.text.x = element_text(angle = 90))
              }else if(all(input$catVar!="")  & all(input$numVar=="")){
                # coordcatVar <-shiny::isolate(input$`source-coordcatVar`)
                if(length(input$catVar)==1){
                  if(!is.na(var())){
                    # shiny::freezeReactiveValue(input,"catVar")
                    GGally::ggparcoord(data()[data()[which(colnames(data())==input$catVar)]==var(),],
                                       columns = numericIndex(data()[data()[which(colnames(data())==input$catVar)]==var(),]),
                                       title = var())+
                      theme(axis.text.x = element_text(angle = 90))
                  }
                }else if(length(input$catVar)==2){
                  if(!is.na(var())){
                    # shiny::freezeReactiveValue(input,"catVar")
                    GGally::ggparcoord(data()[data()[which(colnames(data())==input$catVar[1])]==var(),],
                                       columns = numericIndex(data()[data()[which(colnames(data())==input$catVar[1])]==var(),]),
                                       groupColumn = input$catVar[2],
                                       title = paste(var(),"~",input$catVar[2]))+
                      theme(axis.text.x = element_text(angle = 90))
                  }
                }
                
              }else if(all(input$catVar=="")  & all(input$numVar!="")){
                if(length(input$numVar)>1){
                  GGally::ggparcoord(data(), columns = which(colnames(data()) %in% input$numVar)) + theme(axis.text.x = element_text(angle = 90))
                }
              }else{
                if(length(input$numVar)>1 & length(input$catVar)==1){
                  if(!is.na(var())){
                    # shiny::freezeReactiveValue(input,"catVar")
                    GGally::ggparcoord(data()[data()[which(colnames(data())==input$catVar)]==var(),],
                                       columns = which(colnames(data()) %in% input$numVar),
                                       title = var())+
                      theme(axis.text.x = element_text(angle = 90))
                  }
                }else if(length(input$numVar)>1 & length(input$catVar)==2){
                  GGally::ggparcoord(data()[data()[which(colnames(data())==input$catVar[1])]==var(),],
                                     columns = which(colnames(data()) %in% input$numVar),
                                     groupColumn = input$catVar[2],title = paste(var(),"~",input$catVar[2]))+
                    theme(axis.text.x = element_text(angle = 90))
                }
              }
            },res = 96)
          })
        }
        
      }
    })
    
    
    
    
    
    
    
    # shiny::observeEvent(cat_col(),{
    #   shiny::updateSelectInput(inputId = "boxcatVar",label = "Categorical Variable", choices = c("",cat_col()),selected = "")
    # })
    # shiny::observeEvent(num_col(),{
    #   shiny::updateSelectInput(inputId = "boxnumVar",label = "Numeric Variable",choices = c("",num_col()),selected = "")
    # })
    
    
    
    
    
    # output$par_coord_plot <- shiny::renderPlot({parallel_coordinate_plot(data(),numVar = input$coordnumVar,catVar = input$coordcatVar)},res = 96)
    
    # output$boxplot <- shiny::renderPlot({box_plot(data(), numVar = input$boxnumVar, catVar = input$boxcatVar)}, res = 96)
    
  })
}
