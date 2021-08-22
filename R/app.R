# Define UI ----
ui <- shiny::fluidPage(
  shiny::titlePanel("DAMN"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::textInput(
        "extension",
        "Enter input data extension:",
        value = "tab"
      ),
      shinyFiles::shinyDirButton(
        "dir",
        "Select a folder",
        "Upload",
        FALSE
      ),
      shiny::radioButtons(
        "step",
        "Select normalization step:",
        c("Raw" = "raw",
          "MA-Plot" = "maplot",
          "Normalization" = "norm",
          "Centering" = "cent",
          "Scaling" = "scale",
          "Distribution normalization" = "distr")
      )
    ),
    shiny::mainPanel(
      shiny::h2("Box plot(s)", align = "center"),
      shiny::plotOutput("barchart"))
  )
)

# Define server logic ----
server <- function(input, output) {
  root_dir = c(wd = if(.Platform$OS.type == "unix") '/' else "C:/")
  shinyFiles::shinyDirChoose(
    input,
    'dir',
    roots = root_dir
  )
  output$barchart <- shiny::renderPlot({
    directory <- shinyFiles::parseDirPath(root_dir, input$dir)
    switch(
      input$step,
      "raw" = ggplot2::ggplot(
        data = merge_datasets(directory = directory, file_ext = input$extension, step = "raw"),
        ggplot2::aes(x = CH1, y = CH2, color = name)
        ) +
        ggplot2::geom_point() +
        ggplot2::facet_wrap(~ name),
      "maplot" = ggplot2::ggplot(
        data = mean_average(merge_datasets(directory = directory, file_ext = input$extension, step = "raw")),
        ggplot2::aes(x = A, y = M, color = name)
        ) +
        ggplot2::geom_point() +
        ggplot2::facet_wrap(~ name),
      "norm" = ggplot2::ggplot(
        data = normalize(mean_average(merge_datasets(directory = directory, file_ext = input$extension, step = "raw"))),
        ggplot2::aes(x = A, y = M, color = name)
        ) +
        ggplot2::geom_point() +
        ggplot2::facet_wrap(~ name),
      "cent" = ggplot2::ggplot(
        data = center_norm(merge_datasets(directory = directory, file_ext = input$extension)),
        ggplot2::aes(x = name, y = M, color = name)
        ) +
        ggplot2::geom_boxplot(varwidth = TRUE) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)),
      "scale" = ggplot2::ggplot(
        data = scale_norm(center_norm(merge_datasets(directory = directory, file_ext = input$extension))),
        ggplot2::aes(x = name, y = M, color = name)
        ) +
        ggplot2::geom_boxplot(varwidth = TRUE) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)),
      "distr" = ggplot2::ggplot(
        data = distribution_norm(scale_norm(center_norm(merge_datasets(directory = directory, file_ext = input$extension)))),
        ggplot2::aes(y = M)
        ) +
        ggplot2::geom_boxplot(varwidth = TRUE) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
    )
  })
}

# Run the app ----
# shiny::shinyApp(ui = ui, server = server)


#' Run App
#'
#' Run Shiny App for more comfortable plotting.
#'
#' @param gui ui argument of a shiny app.
#' @param gserver server argument of a shiny app.
#' @export
run_app <- function(gui = ui, gserver = server) {
  shiny::shinyApp(ui = gui, server = gserver)
}

run_app()
