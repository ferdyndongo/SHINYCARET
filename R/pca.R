#' User interface where the Principal Component Analysis outputs will be written with variable selection inputs
#' @param id data module identifier
#' @return a tagList object
pcaUi <- function(id){
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(shiny::selectInput(shiny::NS(id,"numVar"),label = "", multiple = TRUE, choices = NULL, selected = NULL, selectize = FALSE),width = 2),
      shinydashboard::box(shiny::selectInput(shiny::NS(id,"center"),label = "zero mean",choices = c("","TRUE","FALSE")),width = 2),
      shinydashboard::box(shiny::selectInput(shiny::NS(id,"scale"),label = "unit variance",choices = c("","TRUE","FALSE")),width = 2),
      shiny::actionButton(shiny::NS(id,"run"),label = "run pca", class = "btn btn-primary btn-lg active")
    ),
    shiny::h3("PRINCIPAL COMPONENTS"),
    shiny::p("Visualize the eigenvalues/variances of Principal Components/dimensions"),
    shiny::verbatimTextOutput(shiny::NS(id,"eig")),
    # shiny::tableOutput(shiny::NS(id,"eig"))
    shiny::selectInput(shiny::NS(id,"ncomp"),label = "", choices = NULL, selected = NULL, selectize = FALSE),
    shiny::h3("SCREE PLOT"),
    shiny::p("Barplot of Eigenvalues/Variances against the number of dimensions"),
    shiny::plotOutput(shiny::NS(id,"screeplot")),
    shiny::h3("CONTRIBUTION OF VARIABLES TO PCs/DIMENSIONS"),
    shiny::verbatimTextOutput(shiny::NS(id,"pcaPrint")),
    shiny::selectInput(shiny::NS(id,"dim"),label = "", multiple = TRUE, choices = NULL, selected = NULL, selectize = FALSE),
    shiny::plotOutput(shiny::NS(id,"pcaVar")),
    shiny::h3("BIPLOT"),
    shiny::p("Biplot of individuals and variables"),
    shiny::fluidRow(
      shinydashboard::box(shiny::selectInput(shiny::NS(id,"dim1"), label = "",choices = NULL, selected = NULL, selectize = FALSE), width = 2),
      shinydashboard::box(shiny::selectInput(shiny::NS(id,"dim2"), label = "",choices = NULL, selected = NULL, selectize = FALSE), width = 2),
      shinydashboard::box(shiny::selectInput(shiny::NS(id,"biplotVar"),label = "", choices = NULL, selected = NULL, selectize = FALSE), width = 2)
    ),
    shiny::plotOutput(shiny::NS(id,"pca_biplot"),width = "100%",height = "800px"),
    # shiny::plotOutput(shiny::NS(id, "classPCA"),width = "100%",height = "800px"),
    # shiny::plotOutput(shiny::NS(id, "robPCA"),width = "100%",height = "800px"),
  )
}

#' Compute Principal Component Analyis with variable selection inputs and outputs summary with plots
#' @param id identifier of data module
#' @param  data a given dataframe whose numeric variables will be extracted
#' @return a pca object
pcaServer <- function(id, data){
  shiny::moduleServer(id,function(input, output, session){
    
    num_col <- shiny::reactive({
      shiny::req(data())
      names(numericDataset(data()))
    })
    shiny::observeEvent(num_col(),{
      shiny::updateSelectInput(inputId = "numVar",label = "PCA Numeric Variables ",choices = c("",num_col()),selected = "")
    })
    
    pcaData <- shiny::eventReactive(input$run,{
      shiny::req(data(),input$center,input$scale)
      zero_mean <- input$center=="TRUE"
      unit_variance <- input$scale=="TRUE"
      if(all(input$numVar=="")){
        numericDataset(data()) %>% scale(center = zero_mean,scale = unit_variance) %>% data.frame()
      }else if(all(input$numVar!="") && length(input$numVar)>=3){
        data() %>% dplyr::select(dplyr::all_of(input$numVar)) %>% scale(center = zero_mean,scale = unit_variance) %>% data.frame()
      }
    }) 
    
    shiny::reactive({
      shiny::req(pcaData())
      id <- shiny::showNotification("PCA IS RUNNING ...", duration = NULL, closeButton = FALSE)
      base::on.exit(shiny::removeNotification(id), add = TRUE)
      tryCatch({
        stats::prcomp(x = pcaData(),center=FALSE,scale.=FALSE)
        # stats::prcomp(formula(paste0("~",paste(names(pcaData()),collapse = "+"))),data=pcaData(),
        #               center=FALSE,scale.=FALSE)
      },warning=function(w){
        shiny::showNotification(w$message,duration = NULL,closeButton = TRUE,type = "warning")
        stats::prcomp(formula(paste0("~",paste(names(pcaData()),collapse = "+"))),data=pcaData(),na.action=na.omit,
                      center=FALSE,scale.=FALSE)
      },error=function(e){
        shiny::showNotification(e$message,duration = NULL,closeButton = TRUE,type = "error")
        stats::prcomp(formula(paste0("~",paste(names(pcaData()),collapse = "+"))),data=pcaData(),na.action=na.omit,
                      center=FALSE,scale.=FALSE)
      })
    })
    
  })
}

#' output summary of pca
#' @param id identifier of data module
#' @param data data used to fit the pca. It only necessary for biplot in order to classify the graphical visualization of scores.
#' @param  pcaObject a given fitted pca model
pcaOutput <- function(id, data, pcaObject){
  shiny::moduleServer(id,function(input, output, session){
    
    cat_col <- shiny::reactive({
      if(!is.null(data())){
        shiny::req(data())
        (sapply(X = data(),FUN = is.factor) | sapply(X = data(),FUN = is.character)) %>% which() %>% names()
      }
    })
    
    shiny::observeEvent(pcaObject(),{
      shiny::req(pcaObject())
      shiny::updateSelectInput(inputId = "ncomp",label = "Select the number of PCs to explore:",
                               choices = stringr::str_split(1:length(pcaObject()[[1]]),pattern = " "),
                               selected = "2")
      shiny::updateSelectInput(inputId = "dim",label = "Select the PCs to explore:",
                               choices = stringr::str_split(1:length(pcaObject()[[1]]),pattern = " "),
                               selected = c("1","2"))
      shiny::updateSelectInput(inputId = "dim1",label = "Select the PC in x axis:",
                               choices = stringr::str_split(1:length(pcaObject()[[1]]),pattern = " "),
                               selected = "1")
      shiny::updateSelectInput(inputId = "dim2",label = "Select the PC in y axis:",
                               choices = stringr::str_split(1:length(pcaObject()[[1]]),pattern = " "),
                               selected = "2")
      shiny::updateSelectInput(inputId = "biplotVar",label = "Categorical Variable", choices = c("",cat_col()),selected = "")
    })
    
    output$eig <- shiny::renderPrint({
      if(!is.null(pcaObject())){
        shiny::req(pcaObject(), input$ncomp)
        factoextra::get_eigenvalue(pcaObject()) %>% utils::head(as.numeric(input$ncomp))
      }
    })
    # output$eig <- shiny::renderTable({factoextra::get_eigenvalue(pcaObject()) %>% utils::head(as.numeric(input$ncomp))})
    output$screeplot <- shiny::renderPlot({
      if(!is.null(pcaObject())){
        shiny::req(pcaObject(), input$ncomp)
        factoextra::fviz_eig(X = pcaObject(),addlabels = TRUE,ncp = as.numeric(input$ncomp))
      }
    })
    output$pcaPrint <- shiny::renderPrint({
      if(!is.null(pcaObject())){
        shiny::req(pcaObject(), input$dim)
        factoextra::facto_summarize(X = pcaObject(), element = "var", axes = as.numeric(input$dim))
      }
    })
    output$pcaVar <- shiny::renderPlot({
      if(!is.null(pcaObject())){
        shiny::req(pcaObject(), input$dim)
        factoextra::fviz_contrib(X = pcaObject(),choice = "var", axes = as.numeric(input$dim))
      }
    })
    
    output$pca_biplot <- shiny::renderPlot({
      if(!( is.null(pcaObject()) | is.null(data()) | is.null(input$biplotVar))){
        pca_biplot(pcaObject(), data(), biplotVar=input$biplotVar, dim1=input$dim1, dim2=input$dim2)
      }
    })
    
    
  })
}

#' User interface where the Principal Component Analysis outputs will be written with variable selection inputs
#' @param id data module identifier
#' @return a tagList object
pcaInputUi <- function(id){
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(shiny::selectInput(shiny::NS(id,"numVar"),label = "", multiple = TRUE, choices = NULL, selected = NULL, selectize = FALSE),width = 2),
      shinydashboard::box(shiny::selectInput(shiny::NS(id,"center"),label = "zero mean",choices = c("","TRUE","FALSE")),width = 2),
      shinydashboard::box(shiny::selectInput(shiny::NS(id,"scale"),label = "unit variance",choices = c("","TRUE","FALSE")),width = 2),
      shiny::actionButton(shiny::NS(id,"run"),label = "run pca", class = "btn btn-primary btn-lg active")
    )
  )
}

#' Ui for outlier detection with robust pca plots
#' @param id module object identifier
#' @return a tagList shiny object
RobustPCAUi <- function(id){
  shiny::tagList(
    # shiny::plotOutput(shiny::NS(id, "biplot_clas"),width = "100%",height = "800px"),
    shiny::plotOutput(shiny::NS(id, "classPCA"),width = "100%",height = "800px"),
    shiny::plotOutput(shiny::NS(id, "robPCA"),width = "100%",height = "800px"),
    # shiny::plotOutput(shiny::NS(id, "dist")),
    # shiny::plotOutput(shiny::NS(id, "biplot"))
    # shiny::plotOutput(shiny::NS(id, "biplot_class")),
    # shiny::plotOutput(shiny::NS(id, "biplot_rob"))
  )
}

#' build robust pca
#' @param data dataframe
#' @param id module identifier
#' @importFrom rrcov plot PcaHubert PcaClassic biplot
RobustPCAServer <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    
    pcaData <- shiny::eventReactive(input$run,{
      shiny::req(data(),input$center,input$scale)
      zero_mean <- input$center=="TRUE"
      unit_variance <- input$scale=="TRUE"
      if(all(input$numVar=="")){
        numericDataset(data()) %>% scale(center = zero_mean,scale = unit_variance) %>% data.frame()
      }else if(all(input$numVar!="") && length(input$numVar)>=3){
        data() %>% dplyr::select(dplyr::all_of(input$numVar)) %>% scale(center = zero_mean,scale = unit_variance) %>% data.frame()
      }
    })
    
    pcaRob <- shiny::reactive({
      shiny::req(pcaData())
      tryCatch({
        rrcov::PcaHubert(pcaData(), scale=FALSE)
        # rrcov::PcaHubert(formula(paste0("~",paste(names(pcaData()),collapse = "+"))),data=pcaData(),scale=FALSE)
      },warning=function(w){
        shiny::showNotification(w$message,duration = NULL,closeButton = TRUE,type = "warning")
        rrcov::PcaHubert(formula(paste0("~",paste(names(pcaData()),collapse = "+"))), data=pcaData(),scale=FALSE,na.action=na.omit)
      },error=function(e){
        shiny::showNotification(e$message,duration = NULL,closeButton = TRUE,type = "error")
        rrcov::PcaHubert(formula(paste0("~",paste(names(pcaData()),collapse = "+"))),data=pcaData(),scale=FALSE,na.action=na.omit)
      })
    })
    
    pcaClas <- shiny::reactive({
      shiny::req(pcaData())
      tryCatch({
        rrcov::PcaClassic(pcaData(),scale=FALSE)
        # rrcov::PcaClassic(formula(paste0("~",paste(names(pcaData()),collapse = "+"))), data=pcaData(),scale=FALSE)
      },warning=function(w){
        shiny::showNotification(w$message,duration = NULL,closeButton = TRUE,type = "warning")
        rrcov::PcaClassic(formula(paste0("~",paste(names(pcaData()),collapse = "+"))),data=pcaData(),scale=FALSE,na.action=na.omit)
      },error=function(e){
        shiny::showNotification(e$message,duration = NULL,closeButton = TRUE,type = "error")
        rrcov::PcaClassic(formula(paste0("~",paste(names(pcaData()),collapse = "+"))),data=pcaData(),scale=FALSE,na.action=na.omit)
      })
    })
    
    output$classPCA <- shiny::renderPlot({
      if(!is.null(pcaClas())){
        shiny::req(pcaClas())
        rrcov::plot(x = pcaClas())
      }
    },res = 96)
    
    output$robPCA <- shiny::renderPlot({
      if(!is.null(pcaRob())){
        shiny::req(pcaRob())
        rrcov::plot(x = pcaRob())
      }
    },res = 96)
    
    
  })
}

