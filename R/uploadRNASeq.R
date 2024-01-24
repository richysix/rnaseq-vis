#' Create UI components to upload sample and count files
#'
#' `uploadRNASeqInput()` produces the buttons for uploading sample and count data 
#' files. It also provides a checkbox to use the package test data. If sample and
#' count files are subsequently uploaded, this is used instead of the test data.
#' 
#' @param id namespace id for the UI components. Must match the id provided to the 
#' [uploadRNASeqServer()] function.
#'
#' @returns a [htmltools::tagList()] containing two [shiny::fileInput()] controls and 
#' a [shiny::checkboxInput()].
#' 
#' @export
#'
#' @examples
#' 
#' uploadRNASeqInput("rnaseqData")
#' 
uploadRNASeqInput <- function(id) {
  tagList(
    fileInput(NS(id, "sampleFile"), "Sample File"),
    fileInput(NS(id, "countFile"), "Count File"),
    checkboxInput(NS(id, "testdata"), 'Use test data', value = FALSE, width = NULL)
  )
}

#' Server function to upload sample and count data files
#'
#' `uploadRNASeqServer()` implements uploading a sample file and a count data
#' file. It also handles using the package test data.
#' 
#' @param id namespace id for the UI components. Must match the id provided to the 
#' [uploadRNASeqInput()] function.
#'
#' @returns a list containing two [shiny::reactive()] objects
#' * sampleInfo a data.frame of sample metadata
#' * counts a data.frame of RNAseq count data
#' 
#' @export
#'
#' @examples
#' 
#' uploadRNASeqServer("rnaseqData")
#' 
uploadRNASeqServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observe({
      updateCheckboxInput(session, "testdata", value = FALSE)
    }) |>
      bindEvent(input$sampleFile, input$countFile)
    
    sample_file <- reactive({
      if (input$testdata) {
        file_path <- system.file("extdata", "zfs-rnaseq-sampleInfo.tsv", package = "rnaseqVis")
        print(file_path)
        return(file_path)
      } else {
        print(input$sampleFile)
        return(input$sampleFile$datapath)
      }
    })

    sampleInfo <- reactive({
      req(sample_file())
      rnaseqtools::load_rnaseq_samples(sample_file())
    })
    
    
    counts_file <- reactive({
      if (input$testdata) {
        file_path <- system.file("extdata", "counts.shield-subset.tsv", package = "rnaseqVis")
        print(file_path)
        return(file_path)
      } else {
        print(input$countFile)
        return(input$countFile$datapath)
      }
    })
    counts <- reactive({
      req(counts_file())
      rnaseqtools::load_rnaseq_data(counts_file())
    })
    
    list(
      sampleInfo = sampleInfo,
      counts = counts
    )
  })

}

#' A test shiny app for the uploadRNASeq module
#'
#' `uploadRNASeqApp()` creates a small test app for testing the [uploadRNASeqInput()] and
#' [uploadRNASeqServer()] functions. A subset of the returned sample and count data.frames 
#' are displayed in two [shiny::tableOutput()]s
#' 
#' @return a [shiny::shinyApp()] object
#'
#' @examples
#' uploadRNASeqApp()
uploadRNASeqApp <- function() {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        uploadRNASeqInput("rnaseqData")
      ),
      mainPanel(
        tableOutput('samples'),
        tableOutput('counts')
      )
    )
  )
  
  server <- function(input, output, session) {
    data_list <- uploadRNASeqServer("rnaseqData")
    output$samples <- renderTable(head(data_list$sampleInfo()))
    output$counts <- renderTable(data_list$counts()[1:5,c(5,7,9:13)])
  }
  shinyApp(ui, server)
} 
