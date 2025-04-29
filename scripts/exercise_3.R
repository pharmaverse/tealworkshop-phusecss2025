library(teal)
library(dplyr)
library(ggplot2)

my_custom_module_ui <- function(id) {
  ns <- NS(id)
  tags$div(
    # variable selector
    selectInput(
      inputId = ns("variable"),
      label = "Select variable",
      # initialize empty - to be updated from within server
      choices = NULL
    ),
    plotOutput(ns("plot"))
  )
}

my_custom_module_srv <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    # update variable selector by names of data
    updateSelectInput(
      inputId = "variable",
      choices = data()[["ADSL"]] |> select(where(is.numeric)) |> names()
    )

    # add plot call to qenv
    result <- reactive({
      req(input$variable)
      within(
        data(),
        {
          my_plot <- ggplot(ADSL, aes(x = input_var)) +
            geom_histogram()
          my_plot
        },
        input_var = as.name(input$variable)
      )
    })

    # render to output the object from qenv
    output$plot <- renderPlot({
      result()[["my_plot"]]
    })

  })
}

my_custom_module <- module(
  label = "My Custom Module",
  ui = my_custom_module_ui,
  server = my_custom_module_srv
)


data <- teal_data()
data <- within(data, {
  ADSL <- rADSL
})

app <- init(
  data = data,
  modules = list(
    my_custom_module
  )
)

if (Sys.getenv("QUARTO_ROOT") == "") {
  shinyApp(app$ui, app$server)
}
