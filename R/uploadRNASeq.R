# raw_counts <- rnaseqtools::load_rnaseq_data('data/counts.shield-subset.tsv')

# sample_info <- rnaseqtools::load_rnaseq_samples('data/zfs-rnaseq-sampleInfo.tsv')

uploadRNASeqInput <- function(id) {
  tagList(
    fileInput(NS(id, "sampleFile"), "Sample File"),
    fileInput(NS(id, "countFile"), "Count File"),
    checkboxInput(NS(id, "testdata"), 'Use test data', value = FALSE, width = NULL)
  )
}

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

uploadRNASeqApp <- function() {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        uploadRNASeqInput("rnaseq-data")
      ),
      mainPanel(
        tableOutput('samples'),
        tableOutput('counts')
      )
    )
  )
  
  server <- function(input, output, session) {
    data_list <- uploadRNASeqServer("rnaseq-data")
    output$samples <- renderTable(head(data_list$sampleInfo()))
    output$counts <- renderTable(data_list$counts()[1:5,1:6])
  }
  shinyApp(ui, server)
} 

uploadRNASeqApp()
