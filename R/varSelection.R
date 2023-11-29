#' selectInput object for numeric variables. It is the UI for numVarServer and they are linked by id
#' @param id module identifier
numVarUi <- function(id){
  shiny::selectInput(shiny::NS(id,"numVar"),label = "", multiple = TRUE, choices = NULL, selected = NULL, selectize = FALSE)
}

#' Fills the numVarUi with numeric variables names found in given data. It is linked with numVarUi by id
#' @param id module identifier
#' @param data data where the numeric variables names are found.
numVarServer <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    num_col <- shiny::reactive({
      if(!is.null(data())){
        names(numericDataset(data()))
      }
    })
    shiny::observeEvent(num_col(),{
      shiny::updateSelectInput(inputId = "numVar",label = "Numeric Variable",choices = c("",num_col()),selected = "")
    })
  })
}

#' SelectInput object with factor or character variables. It is the UI for catVarserver and they are linked by id.
#' @param id module identifier
catVarUi <- function(id){
  shiny::selectInput(shiny::NS(id,"catVar"),label = "", choices = NULL, selected = NULL, selectize = FALSE)
}

#' Server for catVarUi. Fills the catVarUi with the categorical variable names.
#' @param id module identifier
#' @param data data where the categorical variable names are found
catVarServer <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    cat_col <- shiny::reactive({
      if(!is.null(data())){
        (sapply(X = data(),FUN = is.factor) | sapply(X = data(),FUN = is.character)) %>% which() %>% names()
      }
    })
    shiny::observeEvent(cat_col(),{
      shiny::updateSelectInput(inputId = "catVar",label = "Categorical Variable", choices = c("",cat_col()),selected = "")
    })
  })
}


#' SelectInput object with factor or character variables. It is the UI for catVarserver and they are linked by id.
#' @param id module identifier
catVarValueUi <- function(id){
    shiny::selectInput(shiny::NS(id,"catVarValue"),label = "", choices = NULL, selected = NULL, selectize = FALSE )
}

#' Server for catVarUi. Fills the catVarUi with the categorical variable names.
#' @param id module identifier
#' @param data data where the categorical variable names are found
catVarValueServer <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    catVarVal <- shiny::reactive({
      shiny::req(data(), input$catVar)
      if( !(is.null(data()) | is.null(input$catVar)) ){
        if(inherits(data()[[input$catVar]],"factor")){
          levels(data()[[input$catVar]])
        }else if(inherits(data()[[input$catVar]],"character")){
          unique(data()[[input$catVar]])
        }
      }
    })
    shiny::observeEvent(catVarVal(),{
      shiny::req(input$catVar)
      shiny::updateSelectInput(inputId = "catVarValue",label = paste0("Choose one ",input$catVar), choices = c("",catVarVal()),selected = "")
    })
  })
}

#' Input module for variable selection in a dataset
#' @param id identifier of module object
#' @return a taglist with 2 selectInput for categorical and numerical variables, and the output container for the plot
varSelectionUi <- function(id){
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(shiny::selectInput(shiny::NS(id,"catVar"),label = "", multiple = TRUE, choices = NULL, selected = NULL, selectize = FALSE),width = 2),
      shinydashboard::box(shiny::selectInput(shiny::NS(id,"numVar"),label = "", multiple = TRUE, choices = NULL, selected = NULL, selectize = FALSE),width = 2)
    )
  )
}

#' Server module for varPlotUi in charge of loading the selectInput with the corresponding variables
#' @param id module identifier
#' @param data dataframe for variable selection
varSelectionServer <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    
    cat_col <- shiny::reactive({
      if(!is.null(data())){
        (sapply(X = data(),FUN = is.factor) | sapply(X = data(),FUN = is.character)) %>% which() %>% names()
      }
    })
    num_col <- shiny::reactive({
      if(!is.null(data())){
        names(numericDataset(data()))
      }
    })
    
    shiny::observeEvent(cat_col(),{
      shiny::updateSelectInput(inputId = "catVar",label = "Categorical Variable", choices = c("",cat_col()),selected = "")
    })
    shiny::observeEvent(num_col(),{
      shiny::updateSelectInput(inputId = "numVar",label = "Numeric Variable",choices = c("",num_col()),selected = "")
    })
  })
}
