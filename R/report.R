#' Ui rendering report
#' @param id module identifier

reportUi <- function(id){
  shiny::tagList(
    shiny::radioButtons(inputId = shiny::NS(id,"format"),label = "Document format",#choices = c("RNotebook","HTML","PDF")
                        choices = "RNotebook",selected = NULL,inline = TRUE),
    shiny::downloadButton(outputId = shiny::NS(id,"report"),label =  "Generate report")
  )
}

#' Server function for reportUi in charge of generate report
#' @param id module identifier
#' @param report_script file with extension .Rmd in charge of parameterized report
#' @importFrom rmarkdown render

reportServer <- function(id, report_script){
  shiny::moduleServer(id, function(input, output, session){
    output$report <- shiny::downloadHandler(
      filename = function(){
        paste(report_script,sep = ".", input$format
              # switch(input$format, RNotebook="html", PDF="pdf",HTLM="html")
        )
      },
      content = function(file){
        # params <- list(input=input)
        # tempReport <- file.path(tempdir(), paste(report_script, "Rmd",sep = "."))
        # file.copy(from = paste(report_script, "Rmd",sep = "."), to = tempReport,overwrite = TRUE)
        
        id <- shiny::showNotification("Rendering report ...", duration = NULL, closeButton = FALSE)
        base::on.exit(shiny::removeNotification(id), add = TRUE)
        
        render(input = paste(report_script, "Rmd",sep = "."),
               # output_format = rmarkdown::html_notebook(),
               switch(input$format,
                                    RNotebook=rmarkdown::html_notebook(),
                                    PDF=rmarkdown::pdf_document(),
                                    HTML=rmarkdown::html_document()
                                    ),
               output_file = file,
               envir = new.env(globalenv()),
               params = list(input=input))
      }
    )
  })
}
