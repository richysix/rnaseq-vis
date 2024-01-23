library(shiny)

rnaseqVisApp <- function(...) {
  ui <- fluidPage(
    theme = "flatly.bootstrap.min.css",
    navbarPage(
      "geneExpr",
      tabPanel("Input",
               sidebarLayout(
                 sidebarPanel(
                   uploadRNASeqInput("rnaseq-data"),
                   width = 3
                 ),
                 mainPanel(
                   tableOutput("samples"),
                   tableOutput("counts")
                 )
               )
      )
    )
  )
  server <- function(input, output, session) {
    data_list <- uploadRNASeqServer("rnaseq-data")
    output$samples <- renderTable(head(data_list$sampleInfo()))
    output$counts <- renderTable(data_list$counts()[1:5,c(5,7,9:13)])
  }
  shinyApp(ui, server, ...)
}

