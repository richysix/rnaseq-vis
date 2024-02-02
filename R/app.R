library(shiny)

rnaseqVisApp <- function(debug = FALSE, ...) {
  ui <- fluidPage(
    theme = "flatly.bootstrap.min.css",
    navbarPage(
      "rnaseqVis",
      tabPanel(
        "Input",
        sidebarLayout(
          sidebarPanel(
            uploadRNASeqInput("upload"),
            SubsetByGeneIDsInput("subset"),
            transformInput("transform"),
            clusterInput("cluster"),
            width = 3
          ),
          mainPanel(
            SubsetByGeneIDsOutput("subset"),
            uploadRNASeqOutput("upload"),
          )
        )
      ),
      tabPanel(
        "Heatmap",
        heatmapOutput("hmap")
      ),
      tabPanel("Help", includeMarkdown("README.md"))
    )
  )
  server <- function(input, output, session) {
    data_list <- uploadRNASeqServer("upload", debug)
    counts_subset <- SubsetByGeneIDsServer(
      "subset", 
      "counts" = reactive(data_list$counts()),
      "gene_metadata" = reactive(data_list$gene_metadata()),
      debug
    )

    # Transform counts
    transformed_counts <- transformServer("transform", counts = reactive(counts_subset$counts()))
    
    # Cluster counts
    clustered_counts <- clusterServer(
      "cluster", counts = reactive(transformed_counts$counts()), 
      gene_metadata = reactive(counts_subset$gene_metadata()),
      debug = debug
    )
    
    heatmapServer("hmap", counts = reactive(clustered_counts$counts()),
                  sample_info = reactive(data_list$sample_info()),
                  gene_metadata = reactive(clustered_counts$gene_metadata()),
                  transform = reactive(transformed_counts$transform()),
                  debug = debug)
    
    # Outputs
    output$samples <- renderTable(head(data_list$sample_info()))
    output$counts <- renderTable(clustered_counts$counts()[1:5,1:5])
    output$metadata <- renderTable(clustered_counts$gene_metadata()[1:5,])
  }
  shinyApp(ui, server, ...)
}
