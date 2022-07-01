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
                             # select file type
                             selectInput(inputId = "input_file_type",
                                         label = "Select file type:",
                                         choices = c("Excel (.xlsx)" = "xlsx",
                                                     "csv" = "csv")),

                             # upload excel
                             conditionalPanel(
                               condition = "input.input_file_type == 'xlsx'",
                               fileInput(inputId = 'excel_file',
                                         label = 'Choose Excel file',
                                         accept = c('.xlsx', '.xls'))
                             ),

                             # upload csv
                             conditionalPanel(
                               condition = "input.input_file_type == 'csv'",
                               fileInput(inputId = 'csv_file',
                                         label = 'Choose csv file',
                                         accept = c('.csv'))
                             )
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
    req(input$excel_file)

    inFile <- input$excel_file

    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))

    df <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)

    return(df)
  })

  data <- reactive({
    req(input$csv_file)

    inFile <- input$csv_file

    if(is.null(inFile))
      return(NULL)

    df <- read.csv(inFile$datapath, ".xlsx", sep="")

    return(df)
  })

  # render input data
  output$contents <- renderTable({
    data()
  })


}


shinyApp(ui = ui, server = server)
