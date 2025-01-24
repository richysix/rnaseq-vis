library(shinyBS)

shinyBSAlertInput <- function(id) {
  selectInput(NS(id, "column"), label = "Pick a column", choices = colnames(ggplot2::mpg))
}

shinyBSAlertOutput <- function(id) {
  tagList(
    shinyBS::bsAlert(NS(id, "selected_column")),
    tableOutput(NS(id, "column"))
  )
}

shinyBSAlertServer <- function(id, data = ggplot2::mpg, debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    column_values <- reactive({
      col_name <- req(input$column)
      if (debug) warning(glue::glue("The selected column is {col_name}"))
      shinyBS::createAlert(session, anchorId = NS(id, "selected_column"),
                           alertId = "ColNameAlert", title = "Selected Column Name",
                           content = glue::glue("The selected column is {col_name}"),
                           append = FALSE, style = "danger")
                           
      data[[input$column]]
    })
    
    output$column <- renderTable(column_values())
    
    return(list(values = column_values))
  })
}

shinyBSAlertApp <- function(debug = FALSE) {
  ui <- fluidPage(
    shinyBSAlertInput("test"),
    shinyBSAlertOutput("test"),
  )
  server <- function(input, output, session) {
    data_list <- shinyBSAlertServer(id = "test", debug = debug)
  }
  shinyApp(ui, server)
}

shinyBSAlertTestApp <- function(debug = FALSE) {
  ui <- fluidPage(
    selectInput("column", label = "Pick a column", choices = colnames(ggplot2::mpg)),
    shinyBS::bsAlert("selected_column"),
    tableOutput("col_values")
  )
  server <- function(input, output, session) {
    column_values <- reactive({
      col_name <- req(input$column)
      if (debug) warning(glue::glue("The selected column is {col_name}"))
      shinyBS::createAlert(session, anchorId = "selected_column",
                           alertId = "ColNameAlert", title = "Selected Column Name",
                           content = glue::glue("The selected column is {col_name}"),
                           append = FALSE, style = "danger")
      
      ggplot2::mpg[[input$column]]
    })
    
    output$col_values <- renderTable(head(column_values()))

  }
  shinyApp(ui, server)
}
