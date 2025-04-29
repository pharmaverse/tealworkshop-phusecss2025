library(teal.modules.general)
data <- teal_data() |> within(ADSL <- rADSL)

app <- init(
  data = data,
  modules = list(
    tm_g_scatterplot(
      x = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          label = "Select variable:",
          choices = variable_choices(data[["ADSL"]], "AGE"),
          selected = "AGE"
        )
      ),
      y = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          label = "Select variable:",
          choices = variable_choices(data[["ADSL"]], "BMRKR1"),
          selected = "BMRKR1"
        )
      ),
      # Modify here!
      decorators = list()
    )
  )
)

if (Sys.getenv("QUARTO_ROOT") == "") {
  shinyApp(app$ui, app$server)
}
