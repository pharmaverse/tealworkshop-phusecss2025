library(teal)
library(teal.widgets) # optional extension for UI convinience functions
library(teal.reporter)
library(dplyr)
library(ggplot2)

my_custom_module_ui <- function(id) {
  ns <- NS(id)

  standard_layout(
    output = div(fluidRow(column(
      width = 12,
      br(), hr(),
      plotOutput(ns("plot"))
    ))),
    encoding = div(
      # Reporter buttons
      simple_reporter_ui(ns("simple_reporter")),
      br(),
      tags$label('Encodings', class = 'text-primary'),
      helpText('Analysis Data:', tags$code('ADSL')),
      selectInput(
        inputId = ns("variable"),
        label = "Select variable",
        choices = NULL
      ),
      hr(),
      # Show R Code buttom
      actionButton(
        inputId = ns("src"),
        label = "Show R code",
        width = "100%"
      )
    )
  )
}

my_custom_module_srv <- function(id, data, reporter, filter_panel_api) {
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

    # reproducibility
    observeEvent(input$src, {
      showModal(
        ui = modalDialog(
          title = "Reproducible R code",
          tags$pre(
            get_code(result())
          )
        ),
        session = session
      )
    })

    # reporter
    simple_reporter_srv(
      "simple_reporter",
      reporter = reporter,
      card_fun = function(card = TealReportCard$new(), comment) {
        card$set_name("Histogram Plot")
        card$append_plot(result()[["my_plot"]])
        card$append_fs(filter_panel_api$get_filter_state())
        card$append_encodings(list(param = input$variable))
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(get_code(result()))
        card
      }
    )

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
