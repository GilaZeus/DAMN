library(shiny)
library(shinyFiles)

# Define UI ----
ui <- fluidPage(
  titlePanel("DAMN"),
  sidebarLayout(
    sidebarPanel(
      textInput(
        "extension",
        "Enter input data extension:",
        value = "tab"
      ),
      shinyDirButton(
        "dir",
        "Select a folder",
        "Upload"
      ),
      radioButtons(
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
    mainPanel(
      h2("Bar chart(s)", align = "center"),
      plotOutput("barchart"))
  )
)

# Define server logic ----
server <- function(input, output) {
  root_dir = c(wd = if(.Platform$OS.type == "unix") '/' else "C:")
  shinyDirChoose(
    input,
    'dir',
    roots = root_dir
  )
  output$barchart <- renderPlot({
    directory <- parseDirPath(root_dir, input$dir)
    switch(
      input$step,
      "raw" = ggplot(data = merge_datasets(directory = directory, file_ext = input$extension, step = "raw"), aes(x = CH1, y = CH2, color = name)) + geom_point() + facet_wrap(~ name),
      "maplot" = ggplot(data = mean_average(merge_datasets(directory = directory, file_ext = input$extension, step = "raw")), aes(x = A, y = M, color = name)) + geom_point() + facet_wrap(~ name),
      "norm" = ggplot(data = normalize(mean_average(merge_datasets(directory = directory, file_ext = input$extension, step = "raw"))), aes(x = A, y = M, color = name)) + geom_point() + facet_wrap(~ name),
      "cent" = ggplot(data = center_norm(merge_datasets(directory = directory, file_ext = input$extension)), aes(x = name, y = M, color = name)) + geom_boxplot(varwidth = TRUE) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)),
      "scale" = ggplot(data = scale_norm(center_norm(merge_datasets(directory = directory, file_ext = input$extension))), aes(x = name, y = M, color = name)) + geom_boxplot(varwidth = TRUE) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)),
      "distr" = ggplot(data = distribution_norm(scale_norm(center_norm(merge_datasets(directory = directory, file_ext = input$extension)))), aes(y = M)) + geom_boxplot(varwidth = TRUE) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    )
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)