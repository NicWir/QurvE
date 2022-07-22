list.of.packages <- c("ggplot2", "shiny", "readxl", "tidyverse", "shinythemes")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(readxl)
library(tidyverse)
library(shinythemes)

source("general_misc_utils.R")
source("growth_computation.R")
source("growth_plots.R")
source("growth_summaries.R")
source("fluorescence_computation.R")
source("fluorescence_plots.R")
source("fluorescence_summaries.R")

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
                                 width = '100%'),
                             h1("Your Data"),
                             tableOutput("excel_data")
                           ) # main panel

                  ), # Navbar 1
                  tabPanel("Computation",
                           fluidRow(
                             column(4,
                                    mainPanel(
                                      h2('Growth fit'),
                                      h4('Options'),
                                      checkboxInput(inputId = 'linear_regression',
                                                    label = 'linear regression'),
                                      checkboxInput(inputId = 'parametric_fit',
                                                    label = 'parametric fit'),
                                      checkboxInput(inputId = 'nonparametric_fit',
                                                    label = 'nonparametric fit'),
                                      checkboxInput(inputId = 'run_interactive_mode',
                                                    label = 'run interactive mode')
                                    ), # Growth fit


                                    mainPanel(
                                      h2('Dose-response Analysis'),
                                      checkboxInput(inputId = 'perform_ec50',
                                                    label = 'perform EC50 Analysis'),

                                      selectInput(inputId = "format",
                                                  label = "Response Parameter",
                                                  choices = c("mu.linfit" = "mu.linfit",
                                                              "other.fit" = "other.fit")),

                                      checkboxInput(inputId = 'log_transform_concentration',
                                                    label = 'log transform concentration'),

                                      checkboxInput(inputId = 'log_transform_response',
                                                    label = 'log transform response'),

                                      checkboxInput(inputId = 'run_interactive_mode',
                                                    label = 'run interactive mode'),

                                      selectInput(inputId = "smoothing_factor",
                                                  label = "smoothing factor",
                                                  choices = c("NULL" = "NULL",
                                                              "other" = "other")),

                                      actionButton(inputId = "run",
                                                   label = "Run computation")

                                    )
                             ),

                             column(4,
                                    mainPanel(
                                      h2('Linear fit'),

                                      checkboxInput(inputId = 'log_transform_data',
                                                    label = 'log-transform data'),

                                      checkboxInput(inputId = 'log_transform_time',
                                                    label = 'log-transform time'),

                                      numericInput(
                                        inputId = 't0_linear_fit',
                                        label = 't0',
                                        value = 0,
                                        min = NA,
                                        max = NA,
                                      ),

                                      numericInput(
                                        inputId = 'minimum_density_linear_fit',
                                        label = 'minimum density',
                                        value = 0,
                                        min = NA,
                                        max = NA,
                                      ),

                                      numericInput(
                                        inputId = 'R2_threshold',
                                        label = 'R2 threshold',
                                        value = 0.95,
                                        min = NA,
                                        max = NA,
                                      ),

                                      numericInput(
                                        inputId = 'RSD_threshold',
                                        label = 'RSD threshold',
                                        value = 0.1,
                                        min = NA,
                                        max = NA,
                                      ),

                                      checkboxInput(inputId = 'custom_sliding_window_size',
                                                    label = 'custom sliding window size'),

                                      numericInput(
                                        inputId = 'custum_sliding_window_size_value',
                                        label = '',
                                        value = 0.1,
                                        min = NA,
                                        max = NA,
                                      ),
                                    )

                             ),

                             column(4,
                                    mainPanel(
                                      h2('Parametric fit'),
                                      h4('Models:'),

                                      checkboxInput(inputId = 'logistic',
                                                    label = 'logistic'),

                                      checkboxInput(inputId = 'richards',
                                                    label = 'Richards'),

                                      checkboxInput(inputId = 'gompertz',
                                                    label = 'Gompertz'),

                                      checkboxInput(inputId = 'extended_gompertz',
                                                    label = 'extended Gompertz')

                                    ),

                                    mainPanel(
                                      checkboxInput(inputId = 'log_transform_data_parametric',
                                                    label = 'Log-transform data'),

                                      checkboxInput(inputId = 'log_transform_time_parametric',
                                                    label = 'Log-transform time')
                                    )
                             ),
                             column(4,
                                    mainPanel(
                                      h2('Nonparametric fit'),

                                      checkboxInput(inputId = 'log_transform_data_nonparametric',
                                                    label = 'Log-transform data'),

                                      checkboxInput(inputId = 'log_transform_time_nonparametric',
                                                    label = 'Log-transform time'),

                                      numericInput(
                                        inputId = 't0_nonparametric_fit',
                                        label = 't0',
                                        value = 0,
                                        min = NA,
                                        max = NA,
                                      ),

                                      numericInput(
                                        inputId = 'minimum_density_nonparametric_fit',
                                        label = 'minimum density',
                                        value = 0,
                                        min = NA,
                                        max = NA,
                                      ),

                                      numericInput(
                                        inputId = 'smoothing_factor',
                                        label = 'smoothing factor',
                                        value = 0.55,
                                        min = NA,
                                        max = NA,
                                      ),

                                      numericInput(
                                        inputId = 'number_of_bootstrappings',
                                        label = 'number of bootstrappings',
                                        value = 0,
                                        min = NA,
                                        max = NA,
                                      ),

                                    ))
                           ),

                  ),
                  # display contents of infile
                  # tableOutput('contents'),
                  tabPanel("Visualize",
                           h1("under construction"),
                           h3("Display parameter plot, when computation is run"),
                           plotOutput("console")),

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
  output$excel_data <- renderTable({
    inFile <- input$excel_file

    if(is.null(inFile))
      return(NULL)

    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep = ""))

    read_excel(paste(inFile$datapath, ".xlsx", sep=""))
  })

  results <- eventReactive(input$run,{
      inFile <- input$excel_file

      if(is.null(inFile))
        return(NULL)
      file.rename(inFile$datapath,
                  paste(inFile$datapath, ".xlsx", sep=""))

      grodata <- read_data(paste(inFile$datapath, ".xlsx", sep=""))

      growth.workflow(grodata = grodata)
  })

  output$console <- renderPlot({
    results <- results()
    plot.parameter(results)
    })

}


shinyApp(ui = ui, server = server)
