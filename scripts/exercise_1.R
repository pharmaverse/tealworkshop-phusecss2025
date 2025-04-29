# load teal and all dependent packages
library(teal)

# create empty `teal_data` object
data <- teal_data()

# execute code within it
data <- within(data, {
  IRIS <- iris
  MTCARS <- mtcars
})

app <- init(
  data = data,
  modules = list(
    example_module()
  )
)

if (Sys.getenv("QUARTO_ROOT") == "") {
  shinyApp(app$ui, app$server)
}
