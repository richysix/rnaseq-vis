#' Create UI components to upload a file to use to subset the data
#' 
#' `SubsetByGeneIDsInput()` produces upload and reset buttons to provide a file of 
#' gene IDs to use to subset the data with. 
#' 
#' @param id namespace id for the UI components. Must match the id provided to the 
#' [SubsetByGeneIDsServer()] function.
#' 
#' @returns a [htmltools::tagList()] containing a [shiny::fileInput()] control and 
#' a [shiny::actionButton()].
#' 
#' @examples 
#' SubsetByGeneIDsInput("geneIds")
#' 
#' @export
SubsetByGeneIDsInput <- function(id) {
  tagList(
    fileInput(NS(id, "geneIdsFile"), "Upload file to subset genes with"),
    actionButton(NS(id, "subsetReset"), "Reset Subset Gene IDs"),
  )
}

#' Server function to upload a file to use to subset the data
#' 
#' `SubsetByGeneIDsServer()` implements uploading a file of gene IDs to use to 
#' subset the data with. It also deals with resetting the gene IDs with the 
#' reset button.
#' 
#' @param id namespace id for the UI components. Must match the id provided to the 
#' [SubsetByGeneIDsInput()] function.
#' @param debug logical Turns on debugging messages. default: FALSE
#' 
#' @returns a [shiny::reactive()] object which is a vector of gene IDs if a file has been uploaded.
#' If a file hasn't been uploaded or the reset button has been clicked it returns NULL
#' 
#' @examples 
#' SubsetByGeneIDsServer("geneIds")
#' 
#' # turn on debugging
#' SubsetByGeneIDsServer("geneIds", debug = TRUE)
#' 
#' @export
#' 
SubsetByGeneIDsServer <- function(id, debug = FALSE) {
  stopifnot(!is.reactive(debug))
  moduleServer(id, function(input, output, session) {
    upload_state <- reactiveValues(
      state = NULL
    )
    observe({
      if(debug) message("Gene IDs file uploaded")
      upload_state$state <- 'uploaded'
    }) |> bindEvent(input$geneIdsFile)
    observe({
      if(debug) message("Gene IDs file reset")
      upload_state$state <- 'reset'
    }) |> bindEvent(input$subsetReset)
    
    geneIdsFile <- reactive({
      req(input$geneIdsFile)
      req(upload_state$state)
      if (upload_state$state == "uploaded") {
        return(input$geneIdsFile$datapath)
      } else {
        return(NULL)
      }
    })

    reactive({
      load_gene_ids(geneIdsFile())
    })
  })
}

#' Load a file of gene ids
#'
#' `load_gene_ids()` takes a file name and returns the first column of the
#' file.
#' 
#' @param gene_ids_file path to file of gene ids.
#'
#' @returns a vector of the first column of the file.
#' If `gene_ids_file` is NULL, then it returns NULL
#' 
#' @examples
#' ids_file <- system.file("extdata", "gene-ids.txt", package = "rnaseqVis")
#' load_gene_ids(ids_file)
#' 
load_gene_ids <- function(gene_ids_file) {
  if(is.null(gene_ids_file)){
    return(NULL)
  } else {
    data <- readr::read_tsv(gene_ids_file, show_col_types = FALSE,
                            col_names = FALSE)
    return(data[[1]])
  }
}

#' A test shiny app for the SubsetByGeneIDs module
#'
#' `SubsetByGeneIDsApp()` creates a small test app for testing the [SubsetByGeneIDsInput()] and
#' [SubsetByGeneIDsServer()] functions. A subset of the returned gene IDs and count data.frames 
#' are displayed in two [shiny::tableOutput()]s
#' 
#' @param debug logical for turning on debugging messages. default: FALSE
#' 
#' @return a [shiny::shinyApp()] object.
#'
#' @examples
#' SubsetByGeneIDsApp()
#' 
#' SubsetByGeneIDsApp(debug = TRUE)
#' 
SubsetByGeneIDsApp <- function(debug = FALSE) {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        SubsetByGeneIDsInput("geneIds")
      ),
      mainPanel(
        tableOutput('geneIds')
      )
    )
  )
  
  server <- function(input, output, session) {
    geneIds <- SubsetByGeneIDsServer("geneIds", debug)
    output$geneIds <- renderTable(head(geneIds()))
    output$counts <- renderTable(data_list$counts()[1:5,1:6])
  }
  shinyApp(ui, server)
} 
