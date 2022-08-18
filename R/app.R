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
                           tabsetPanel(type = "tabs",
                                       tabPanel(title = "Custom",
                                                sidebarPanel(
                                                  # select file type
                                                  selectInput(inputId = "input_file_type_custom",
                                                              label = "Select file type:",
                                                              choices = c("Excel (.xlsx)" = "xlsx",
                                                                          "csv" = "csv")),

                                                  selectInput(inputId = "format",
                                                              label = "Select Format",
                                                              choices = c("Data in columns" = "data_in_columns",
                                                                          "Data in rows" = "data_in_rows")),

                                                  fileInput(inputId = 'growth_file',
                                                            label = 'Choose growth data file',
                                                            accept = c('.xlsx', '.xls', '.csv')),

                                                  conditionalPanel(
                                                    condition = "input.input_file_type_custom == 'xlsx'",
                                                    selectInput(inputId = "sheet",
                                                                label = "Select Sheet",
                                                                choices = c("Sheet 1" = "Sheet1",
                                                                            "Sheet 2" = "Sheet2",
                                                                            "Sheet 3" = "Sheet3")),
                                                  ), # select sheet: conditional

                                                  conditionalPanel(
                                                    condition = "input.input_file_type_custom == 'csv'",
                                                    selectInput(inputId = "seperator",
                                                                label = "Select separator",
                                                                choices = c("," = "comma_seperator",
                                                                            ";" = "semicolon_seperator")
                                                    ),

                                                    selectInput(inputId = "decimal_seperator",
                                                                label = "Select Decimal separator",
                                                                choices = c("." = "dot_decimal_seperator",
                                                                            "," = "comma_decimal_seperator")
                                                    ),
                                                  ),

                                                  checkboxInput(inputId = 'subtract_blanc',
                                                                label = 'Subtract blank'),

                                                  checkboxInput(inputId = 'calibration',
                                                                label = 'Calibration (under construction)'),

                                                  fileInput(inputId = 'fluorescence_file_1',
                                                            label = 'Fluorescence data 1',
                                                            accept = c('.xlsx', '.xls', '.csv')),

                                                  fileInput(inputId = 'fluorescence_file_2',
                                                            label = 'Fluorescence data 2',
                                                            accept = c('.xlsx', '.xls', '.csv')),

                                                ),# sidebar panel
                                       ), # Custom tabPanel

                                       tabPanel(title = "Plate reader",
                                                sidebarPanel(
                                                  # select file type
                                                  selectInput(inputId = "input_file_type_plate_reader",
                                                              label = "Select file type:",
                                                              choices = c("Excel (.xlsx)" = "xlsx",
                                                                          "csv" = "csv")),

                                                  selectInput(inputId = "format",
                                                              label = "Select Format",
                                                              choices = c("Data in columns" = "data_in_columns",
                                                                          "Data in rows" = "data_in_rows")),

                                                  fileInput(inputId = 'growth_file',
                                                            label = 'Choose growth data file',
                                                            accept = c('.xlsx', '.xls', '.csv')),

                                                  conditionalPanel(
                                                    condition = "input.input_file_type_plate_reader == 'xlsx'",
                                                    selectInput(inputId = "sheet",
                                                                label = "Select Sheet",
                                                                choices = c("Sheet 1" = "Sheet1",
                                                                            "Sheet 2" = "Sheet2",
                                                                            "Sheet 3" = "Sheet3")),
                                                  ), # select sheet: conditional

                                                  conditionalPanel(
                                                    condition = "input.input_file_type_plate_reader == 'csv'",
                                                    selectInput(inputId = "seperator",
                                                                label = "Select separator",
                                                                choices = c("," = "comma_seperator",
                                                                            ";" = "semicolon_seperator")
                                                    ),

                                                    selectInput(inputId = "decimal_seperator",
                                                                label = "Select Decimal separator",
                                                                choices = c("." = "dot_decimal_seperator",
                                                                            "," = "comma_decimal_seperator")
                                                    ),
                                                  ),

                                                  selectInput(inputId = "platereader_software",
                                                              label = "Platereader software",
                                                              choices = c("Biotek - Gen5/Gen6" = "biotek_gen5_gen6"
                                                              ),
                                                  ),

                                                  selectInput(inputId = "density_data_read",
                                                              label = "Density data",
                                                              choices = c("Read 3:630" = "read_3_630"
                                                              ),
                                                  ),

                                                  selectInput(inputId = "fluorescence_data_1_read",
                                                              label = "Fluorescence data 1",
                                                              choices = c("Read 4:485,528" = "read_4_485_528"
                                                              ),
                                                  ),

                                                  selectInput(inputId = "fluorescence_data_2_read",
                                                              label = "Fluorescence data 1",
                                                              choices = c("Read 4:485,528[2]" = "read_4_485_528_2"
                                                              ),
                                                  ),

                                                  checkboxInput(inputId = 'subtract_blanck_plate_reader',
                                                                label = 'subtract blank',
                                                                value = TRUE),

                                                  checkboxInput(inputId = 'convert_time_values_plate_reader',
                                                                label = 'Convert time values',
                                                                value = TRUE),

                                                  checkboxInput(inputId = 'calibration_plate_reader',
                                                                label = 'Calibration'),

                                                ),# sidebar panel
                                       ), # Plate reader tabPanel
                           ), # tabSet Panel

                           mainPanel(
                             h1("Custom data layout"),
                             img(src = 'data_instruction.png',
                                 heigt = '100%',
                                 width = '100%'),
                             h1("Your Data"),
                             tableOutput("excel_data")
                           ) # main panel

                  ), # Navbar 1

                  navbarMenu('Computation',
                             tabPanel("Growth",
                                      fluidRow(
                                        column(4,
                                               mainPanel(
                                                 h2('Growth fit'),
                                                 h4('Options'),
                                                 checkboxInput(inputId = 'linear_regression_growth',
                                                               label = 'linear regression',
                                                               value = TRUE),

                                                 checkboxInput(inputId = 'nonparametric_fit_growth',
                                                               label = 'parametric fit',
                                                               value = TRUE),

                                                 checkboxInput(inputId = 'log_transform_time_growth',
                                                               label = 'Log-transform time'),

                                                 checkboxInput(inputId = 'biphasic_growth_growth',
                                                               label = 'Biphasic growth'),

                                                 numericInput(
                                                   inputId = 'growth_threshold_growth',
                                                   label = 'growth threshold',
                                                   value = 1.5,
                                                   min = NA,
                                                   max = NA,
                                                 ),

                                                 numericInput(
                                                   inputId = 'minimum_density_growth',
                                                   label = 'minimum density',
                                                   value = 0,
                                                   min = NA,
                                                   max = NA,
                                                 ),

                                                 numericInput(
                                                   inputId = 't0_growth',
                                                   label = 't0',
                                                   value = 0,
                                                   min = NA,
                                                   max = NA,
                                                 )
                                               ), # Growth fit


                                               mainPanel(
                                                 h2('Dose-response Analysis'),
                                                 checkboxInput(inputId = 'perform_ec50_growth',
                                                               label = 'perform EC50 Analysis',
                                                               value = FALSE),

                                                 selectInput(inputId = "response_parameter_growth",
                                                             label = "Response Parameter",
                                                             choices = c("mu.linfit" = "mu.linfit",
                                                                         "other.fit" = "other.fit")),

                                                 checkboxInput(inputId = 'log_transform_concentration_growth',
                                                               label = 'log transform concentration'),

                                                 checkboxInput(inputId = 'log_transform_response_growth',
                                                               label = 'log transform response'),


                                                 selectInput(inputId = "smoothing_factor_growth",
                                                             label = "smoothing factor",
                                                             choices = c("NULL" = "NULL",
                                                                         "other" = "other")),

                                                 numericInput(
                                                   inputId = 'minimum_number_of_different_values_growth',
                                                   label = 'Minimum number of different values',
                                                   value = 6,
                                                   min = NA,
                                                   max = NA,
                                                 ),

                                                 numericInput(
                                                   inputId = 'number_of_bootstrappings_dr_growth',
                                                   label = 'Number of bootstrappings',
                                                   value = 0,
                                                   min = NA,
                                                   max = NA,
                                                 ),


                                                 actionButton(inputId = "run_growth",
                                                              label = "Run computation")

                                               )
                                        ),

                                        column(4,
                                               mainPanel(
                                                 h2('Linear fit'),

                                                 numericInput(
                                                   inputId = 'R2_threshold_growth',
                                                   label = 'R2 threshold',
                                                   value = 0.95,
                                                   min = NA,
                                                   max = NA,
                                                 ),

                                                 numericInput(
                                                   inputId = 'RSD_threshold_growth',
                                                   label = 'RSD threshold',
                                                   value = 0.1,
                                                   min = NA,
                                                   max = NA,
                                                 ),

                                                 numericInput(
                                                   inputId = 'dY_threshold_growth',
                                                   label = 'dY threshold',
                                                   value = 0.05,
                                                   min = NA,
                                                   max = NA,
                                                 ),

                                                 checkboxInput(inputId = 'custom_sliding_window_size_growth',
                                                               label = 'custom sliding window size',
                                                               value = TRUE),

                                                 numericInput(
                                                   inputId = 'custum_sliding_window_size_value_growth',
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

                                                 checkboxInput(inputId = 'logistic_growth',
                                                               label = 'logistic',
                                                               value = TRUE),

                                                 checkboxInput(inputId = 'richards_growth',
                                                               label = 'Richards',
                                                               value = TRUE),

                                                 checkboxInput(inputId = 'gompertz_growth',
                                                               label = 'Gompertz',
                                                               value = TRUE),

                                                 checkboxInput(inputId = 'extended_gompertz_growth',
                                                               label = 'extended Gompertz',
                                                               value = TRUE),

                                                 checkboxInput(inputId = 'log_transform_data_parametric_growth',
                                                               label = 'Log-transform data',
                                                               value = TRUE),

                                               ),

                                        ),

                                        column(4,
                                               mainPanel(
                                                 h2('Nonparametric fit'),

                                                 checkboxInput(inputId = 'log_transform_data_nonparametric_growth',
                                                               label = 'Log-transform data',
                                                               value = TRUE),

                                                 numericInput(
                                                   inputId = 'smoothing_factor_nonparametric_growth',
                                                   label = 'smoothing factor',
                                                   value = 0.55,
                                                   min = NA,
                                                   max = NA,
                                                 ),

                                                 numericInput(
                                                   inputId = 'number_of_bootstrappings_growth',
                                                   label = 'number of bootstrappings',
                                                   value = 0,
                                                   min = NA,
                                                   max = NA,
                                                 ),

                                                 checkboxInput(inputId = 'remove_negative_values_in_bootstrapping_growth',
                                                               label = 'Remove negative values in bootstrapping growth',
                                                               value = TRUE)
                                               ))
                                      ),
                             ), # Growth Tab Panel




                             tabPanel("Fluorescence",
                                      fluidRow(
                                        column(4,
                                               mainPanel(
                                                 h2('Fluorescence fit'),
                                                 h4('Options'),
                                                 checkboxInput(inputId = 'linear_regression_fluorescence',
                                                               label = 'linear regression'),

                                                 checkboxInput(inputId = 'parametric_fit_fluorescence',
                                                               label = 'parametric fit'),

                                                 checkboxInput(inputId = 'nonparametric_fit_fluorescence',
                                                               label = 'nonparametric fit'),

                                                 checkboxInput(inputId = 'run_interactive_mode_fluorescence',
                                                               label = 'run interactive mode'),

                                                 checkboxInput(inputId = 'biphasic_fluorescence',
                                                               label = 'Biphasic'),

                                                 selectInput(inputId = 'data_type_x_fluorescence',
                                                             label = 'Data type x',
                                                             choices = c('density', 'time')),

                                                 checkboxInput(inputId = 'normalize_fluorescence',
                                                               label = 'Normalize fluorescence'),

                                                 numericInput(
                                                   inputId = 'growth_threshold_in_percent_fluorescence',
                                                   label = 'growth threshold (in %)',
                                                   value = 1.5,
                                                   min = NA,
                                                   max = NA,
                                                 ),

                                                 numericInput(
                                                   inputId = 'minimum_density_fluorescence',
                                                   label = 'minimum_density',
                                                   value = 0,
                                                   min = NA,
                                                   max = NA,
                                                 ),

                                                 numericInput(
                                                   inputId = 't0_fluorescence',
                                                   label = 't0',
                                                   value = 0,
                                                   min = NA,
                                                   max = NA,
                                                 ),

                                               ), # Fluorescence fit


                                               mainPanel(
                                                 h2('Dose-response Analysis'),
                                                 checkboxInput(inputId = 'perform_ec50_fluorescence',
                                                               label = 'perform EC50 Analysis'),

                                                 selectInput(inputId = "format_fluorescence",
                                                             label = "Response Parameter",
                                                             choices = c("max_slope_spline" = "max_slope_spline")
                                                 ),

                                                 checkboxInput(inputId = 'log_transform_concentration_fluorescence',
                                                               label = 'log transform concentration'),

                                                 checkboxInput(inputId = 'log_transform_response_fluorescence',
                                                               label = 'log transform response'),

                                                 numericInput(
                                                   inputId = 'smoothing_factor_fluorescence',
                                                   label = 'smoothing factor',
                                                   value = NULL,
                                                   min = NA,
                                                   max = NA,
                                                 ),

                                                 numericInput(
                                                   inputId = 'minimum_number_of_different_values_fluorescence',
                                                   label = 'smoothing factor',
                                                   value = 6,
                                                   min = NA,
                                                   max = NA,
                                                 ),

                                                 numericInput(
                                                   inputId = 'number_of_bootrappings_fluorescence',
                                                   label = 'number of bootrappings',
                                                   value = 0,
                                                   min = NA,
                                                   max = NA,
                                                 ),


                                                 actionButton(inputId = "run_fluorescence",
                                                              label = "Run computation")
                                               )
                                        ), # Dose response analysis

                                        column(4,
                                               mainPanel(
                                                 h2('Linear fit'),

                                                 checkboxInput(inputId = 'log_transform_x',
                                                               label = 'log-transform x'),

                                                 checkboxInput(inputId = 'log_transform_fluorescence',
                                                               label = 'log-transform fluorescence'),

                                                 numericInput(
                                                   inputId = 'R2_threshold',
                                                   label = 'R2_threshold',
                                                   value = 0.97,
                                                   min = NA,
                                                   max = NA,
                                                 ),

                                                 numericInput(
                                                   inputId = 'RSD_threshold',
                                                   label = 'RSD threshold',
                                                   value = 0.05,
                                                   min = NA,
                                                   max = NA,
                                                 ),

                                                 numericInput(
                                                   inputId = 'dY_threshold',
                                                   label = 'dY threshold',
                                                   value = 0.05,
                                                   min = NA,
                                                   max = NA,
                                                 ),

                                                 checkboxInput(inputId = 'custom_sliding_window_size',
                                                               label = 'custom sliding window size'),

                                                 numericInput(
                                                   inputId = 'custum_sliding_window_size_value',
                                                   label = '',
                                                   value = 8,
                                                   min = NA,
                                                   max = NA,
                                                 ),
                                               )

                                        ), # Linear fit: Fluorescence

                                        column(4,
                                               mainPanel(
                                                 h2('Nonparametric fit'),

                                                 checkboxInput(inputId = 'log_transform_x',
                                                               label = 'Log-transform x'),

                                                 checkboxInput(inputId = 'log_transform_fluorescence',
                                                               label = 'Log-transform fluorescence'),

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

                                                 checkboxInput(inputId = 'remove_neg_values_in_bootstrap',
                                                               label = 'remove negative values in bootstrapping'),

                                               ),

                                               mainPanel(
                                                 checkboxInput(inputId = 'log_transform_data_parametric',
                                                               label = 'Log-transform data'),

                                                 checkboxInput(inputId = 'log_transform_time_parametric',
                                                               label = 'Log-transform time')
                                               )
                                        ),

                                      ),


                             ),


                  ),

                  navbarMenu("Results",
                             tabPanel(title = "Growth",
                                      tableOutput('results_table_growth')),

                             tabPanel(title = "Computation",
                                      h1('Under construction'))
                  ),

                  # display contents of infile
                  # tableOutput('contents'),
                  tabPanel("Visualize",
                           h1("under construction"),
                           tabsetPanel(type = "tabs",
                                       tabPanel(title = "Group plots",
                                                plotOutput("plot")),
                                       tabPanel(title = "Dose-response analysis",
                                                plotOutput("dose_response")),
                                       tabPanel(title = "Parameter plots",
                                                plotOutput("parameter_plot"))
                           ),
                           h3("Display parameter plot, when computation is run"),
                           #plotOutput("console")
                  ),

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

  results <- reactiveValues()

  # load excel file
  output$excel_data <- renderTable({
    inFile <- input$growth_file

    if(is.null(inFile))
      return(NULL)

    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep = ""))

    read_excel(paste(inFile$datapath, ".xlsx", sep=""),  col_names = F)
  })

  observeEvent(input$run_growth,{
    inFile <- input$growth_file

    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))

    grodata <- read_data(paste(inFile$datapath, ".xlsx", sep=""))

    if (input$smoothing_factor_growth == "NULL") {
      smooth.dr = NULL
    }

    results$growth <- growth.workflow(grodata = grodata,
                    log.x.gc = input$log_transform_time_growth,
                    biphasic = input$biphasic_growth_growth,
                    growth.thresh = input$growth_threshold_growth,
                    min.density = input$minimum_density_growth,
                    t0 = input$t0_growth,
                    ec50 = input$perform_ec50_growth,
                    dr.parameter = input$response_parameter_growth,
                    log.x.dr = input$log_transform_concentration_growth,
                    log.y.dr = input$log_transform_response_growth,
                    smooth.dr = smooth.dr,
                    dr.have.atleast = input$minimum_number_of_different_values_growth,
                    nboot.dr = input$number_of_bootstrappings_dr_growth,
                    #lin.R2 = input$R2_threshold_growth, ## Here seem to be problems
                    #lin.RSD = input$RSD_threshold_growth,
                    #lin.dY = input$dY_threshold_growth,
                    #lin.h = input$custum_sliding_window_size_value_growth
                    # PARAMETRIC FIT VARIABLES
                    smooth.gc = input$smoothing_factor_nonparametric_growth,
                    nboot.gc = input$number_of_bootstrappings_growth,
                    neg.nan.act = input$remove_negative_values_in_bootstrapping_growth

    )
  })

  output$parameter_plot <- renderPlot({
    results <- results$growth
    plot.parameter(results)
  })

  output$dose_response <- renderPlot({
    results <- results$growth
    plot(results$drFit$spline)
  })

  output$results_table_growth <- renderTable({
    results <- results$growth
    results$gcFit$gcTable
  })

  #output$console <- renderPlot({
  #  results <- results()
  #  plot.parameter(results)
  #  })

}


shinyApp(ui = ui, server = server)
