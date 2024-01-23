# raw_counts <- rnaseqtools::load_rnaseq_data('data/counts.shield-subset.tsv')

# sample_info <- rnaseqtools::load_rnaseq_samples('data/zfs-rnaseq-sampleInfo.tsv')

uploadRNASeqInput <- function(id) {
  tagList(
    fileInput(NS(id, "sampleFile"), "Sample File"),
    fileInput(NS(id, "countFile"), "Count File")
  )
}

uploadRNASeqServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    sampleInfo <- reactive({
      req(input$sampleFile)
      rnaseqtools::load_rnaseq_samples(input$sampleFile$datapath)
    })
    
    counts <- reactive({
      req(input$countFile)
      rnaseqtools::load_rnaseq_data(input$countFile$datapath)
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
