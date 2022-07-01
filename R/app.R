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
                                                     "csv (under construction...)" = "csv")),

                             # upload excel: conditional
                             conditionalPanel(
                               condition = "input.input_file_type == 'xlsx'",
                               fileInput(inputId = 'excel_file',
                                         label = 'Choose Excel file',
                                         accept = c('.xlsx', '.xls')),

                               selectInput(inputId = "sheet",
                                           label = "Select Sheet",
                                           choices = c("Sheet 1" = "Sheet1",
                                                       "Sheet 2" = "Sheet2",
                                                       "Sheet 3" = "Sheet3")),

                               selectInput(inputId = "format",
                                           label = "Select Format",
                                           choices = c("Data in columns" = "data_in_columns",
                                                       "Data in rows" = "data_in_rows")),

                               checkboxInput(inputId = 'subtract_blanc',
                                             label = 'Subtract blank'),

                               checkboxInput(inputId = 'calibration',
                                             label = 'Calibration (under construction)')

                             ),

                             # upload csv: conditional
                             conditionalPanel(
                               condition = "input.input_file_type == 'csv'",
                               fileInput(inputId = 'csv_file',
                                         label = 'Choose csv file (under construction...)',
                                         accept = c('.csv'))
                             )
                           ), # sidebar panel

                           mainPanel(
                             h1("Custom data layout"),
                             img(src = 'data_instruction.png',
                                 heigt = '100%',
                                 width = '100%')
                           ) # main panel

                  ), # Navbar 1
                  tabPanel("Computation", h1("under construction")),
                  # display contents of infile
                  # tableOutput('contents'),
                  tabPanel("Visualize",  h1("under construction")),
                  tabPanel("Report",  h1("under construction")),
                  tabPanel("About Us",
                           mainPanel(
                             h1("Authors"),
                             'Nicolas Wirth, Jonathan Funk, Stefano Donatino',
                             h1("Publications"),
                             'featuring publications which use this tool'
                           )
                  )

                )

                # show plots
                # TODO: Which plots, which options, ...


)

server <- function(input, output){
  # load excel file
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

  # load csv file TODO: untested
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
