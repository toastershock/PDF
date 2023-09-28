library(shiny)
library(raster)
library(gridExtra)
library(imager)
library(shinyWidgets)
ui <- fluidPage(
  tags$style(
    HTML(
      "
      body, html {
        height: 100%;
        margin: 0;
      }
      #matrix-iframe-container {
        position: absolute;
        width: 100%;
        height: 100%; /* Adjust the height as needed */
        top: 0;
        left: 0;
        background-color: white; /* Set background color for upper half */
        z-index: 2;
      }
      #matrix-iframe {
        position: absolute;
        width: 100%;
        height: 100%; /* Adjust the height as needed */
        bottom: 0;
        left: 0;
        border: none;
        z-index: ;
      }
      "
    )
  ),
  tags$iframe(id = "matrix-iframe", src = "matrix_background.html", frameborder = "0"),
  titlePanel("Image to PDF"),
  sidebarLayout(
    sidebarPanel(width = 4,
      #fileInput("file", "Choose a File", multiple = TRUE),
      actionBttn("file_choose", "Choose Files", style = "bordered", color = "success", icon = icon("file")),
      actionBttn("create", "Create PDF", style = "fill", color = "danger", icon = icon("hammer")),
      actionBttn("reset", "Reset", style = "unite", color = "primary", icon = icon("sync")),
      fluidRow(
        column(2,uiOutput("id")
               ),
        column(4,uiOutput("delete")
               )
      ),
      uiOutput("browse")
    ),
    mainPanel(
      dataTableOutput("files_table")
    )
  )
)

server <- function(input, output, session) {
  session$onSessionEnded({
    stopApp
  })
  # ReactiveValues
  files_reactive <- reactiveValues(files = character(0))
  
  observeEvent(input$reset,{
    files_reactive$files <- NULL
  })
  
  output$id <- renderUI({
    if (length(files_reactive$files) > 0) {
      numericInput("id", label = NULL, value = 1, min = 1, max = length(files_reactive$files), width = 100)
    }
  })
    
  output$delete <- renderUI({
    if (length(files_reactive$files) > 0) {actionBttn("delete", "Delete ID", style = "jelly", color = "primary", icon = icon("remove"))}
  })
  observeEvent(input$delete,{
    files_reactive$files <- files_reactive$files[-input$id]
  })
  
  observeEvent(input$file_choose, {
    file <- choose.files()
    # Append the selected file paths to the reactiveValues
    files_reactive$files <- c(files_reactive$files, file)
  })
  
  output$files_table <- renderDataTable(
    if (length(files_reactive$files) > 0) {
    data.frame(ID = seq(1,length(files_reactive$files)),Files = files_reactive$files)
      }, options = list(
        pageLength = 20, autoWidth = TRUE,
        columnDefs = list(list( targets = 0, width = '100px'))
      )
  )
  
  observeEvent(input$create, {
    if (length(files_reactive$files) > 0) {
      shiny::withProgress(expr = {
        dir <- choose.dir()
        pdf(paste0(dir,"/Test.pdf"))
      for (i in 1:length(files_reactive$files)) {
        image <- imager::load.image(files_reactive$files[i])
        plot(image, axes = F)
      }
      dev.off()}, message = "Creating PDF...")
      if(input$browse == "Yes"){browseURL(paste0(dir,"/Test.pdf"))}
    }
  })
  
  output$browse <- renderUI({
    if (length(files_reactive$files) > 0) {
    radioGroupButtons(
      inputId = "browse",
      label = "Open PDF after creation",
      choices = c("Yes", "No"),
      status = "primary",
      checkIcon = list(
        yes = icon("ok", 
                   lib = "glyphicon"),
        no = icon("remove",
                  lib = "glyphicon"))
    )}
  })
  
}

shinyApp(ui, server)
