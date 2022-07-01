library(shiny)
library(readxl)
library(tidyverse)
library(shinythemes)

ui <- fluidPage(theme = shinytheme('sandstone'),
                navbarPage(
                  'QurvE',

                  # load input file
                  tabPanel('Data',
                           sidebarPanel(
                             fileInput(inputId = 'file1',
                                       label = 'Choose xlsx file',
                                       accept = c(".xlsx"))
                           ), # sidebar panel

                           mainPanel(
                             h1("Custom data layout"),

                             img(src = 'data_instruction.png',
                                 heigt = '100%',
                                 width = '100%'),

                             h4("Output 1"),
                           ) # main panel

                  ), # Navbar 1
                  tabPanel("Computation", "under construction"),
                  # display contents of infile
                  # tableOutput('contents'),
                  tabPanel("Visualize", "under construction"),
                  tabPanel("Report", "under construction")
                )

                # show plots
                # TODO: Which plots, which options, ...


)

server <- function(input, output){
  # load input file
  data <- reactive({
    req(input$file1)

    inFile <- input$file1

    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))

    df <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)

    return(df)
  })

  # render input data
  output$contents <- renderTable({
    data()
  })


}


shinyApp(ui = ui, server = server)
