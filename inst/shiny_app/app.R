list.of.packages <- c("ggplot2", "shiny", "readxl", "tidyverse", "shinythemes")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(readxl)
library(tidyverse)
library(shinythemes)
library(DT)

source("../../R/general_misc_utils.R")
source("../../R/growth_computation.R")
source("../../R/growth_plots.R")
source("../../R/growth_summaries.R")
source("../../R/fluorescence_computation.R")
source("../../R/fluorescence_plots.R")
source("../../R/fluorescence_summaries.R")
gc_parameters <- c('mu.linfit', 'lambda.linfit', 'dY.linfit', 'A.linfit', 'mu2.linfit', 'lambda2.linfit',
                   'mu.model', 'lambda.model', 'A.model', "tD.linfit", "tD2.linfit", "tD.spline", "tD2.spline",
                   'mu.spline', 'lambda.spline', 'A.spline', 'dY.spline', 'integral.spline', 'mu2.spline', 'lambda2.spline',
                   'mu.bt', 'lambda.bt', 'A.bt', 'integral.bt', 'max_slope.linfit', 'max_slope.spline')
ui <- fluidPage(theme = shinytheme('sandstone'),
                navbarPage(
                  'QurvE',

                  # load input file
                  tabPanel('Data',
                           tabsetPanel(type = "tabs",
                                       tabPanel(title = "Custom",
                                                sidebarPanel(
                                                  style='border-color: #ADADAD',
                                                  wellPanel(
                                                    style='background-color:#F0EBE4; padding: 1; border-color: #ADADAD; padding: 1; padding-bottom: 0',
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
                                                            accept = c('.xlsx', '.xls', '.csv'))
                                                  ),

                                                  wellPanel(
                                                    style='background-color:#F0EBE4; padding: 1; border-color: #ADADAD; padding: 1; padding-bottom: 0',
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
                                                  )
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
                                                  style='border-color: #ADADAD',
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
                                        sidebarLayout(
                                          column(4,
                                                 sidebarPanel( width = 12,
                                                               style='border-color: #ADADAD',
                                                               wellPanel(
                                                                 style='background-color:#F0EBE4; padding: 1; border-color: #ADADAD; padding-top: 0; padding-bottom: 0',
                                                                 h2(strong('Growth fit')),
                                                                 h4('Options'),
                                                                 checkboxInput(inputId = 'linear_regression_growth',
                                                                               label = 'linear regression',
                                                                               value = TRUE),

                                                                 checkboxInput(inputId = 'parametric_fit_growth',
                                                                               label = 'parametric fit',
                                                                               value = FALSE),

                                                                 checkboxInput(inputId = 'nonparametric_fit_growth',
                                                                               label = 'non parametric fit',
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


                                                               wellPanel(
                                                                 style='background-color:#F0EBE4; padding: 1; border-color: #ADADAD; padding-top: 0; padding-bottom: 0',
                                                                 h2(strong('Dose-response Analysis')),
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
                                                                   inputId = 'number_of_bootstrappings_dr_growth',
                                                                   label = 'Number of bootstrappings',
                                                                   value = 0,
                                                                   min = NA,
                                                                   max = NA,
                                                                 )
                                                               ),


                                                               actionButton(inputId = "run_growth",
                                                                            label = "Run computation")


                                                 ) # sidebarPanel(
                                          ), # column
                                          column(6,
                                                 conditionalPanel(
                                                   condition = "input.linear_regression_growth",
                                                   sidebarPanel(
                                                     width = 4,
                                                     style='border-color: #ADADAD; padding-top: 0',
                                                     h3(strong('Linear fit')),

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
                                                                   value = FALSE),

                                                     numericInput(
                                                       inputId = 'custom_sliding_window_size_value_growth',
                                                       label = NULL,
                                                       value = 0,
                                                       min = NA,
                                                       max = NA,
                                                     ),
                                                   )
                                                 ), # conditionalPanel
                                                 conditionalPanel(
                                                   condition = "input.parametric_fit_growth",
                                                   sidebarPanel(
                                                     style='border-color: #ADADAD; padding-top: 0',
                                                     h3(strong('Parametric fit')),
                                                     wellPanel(
                                                       h4(strong('Models:')),
                                                       style='background-color:#F0EBE4; padding: 1; padding-top: 0; padding-bottom: 0',
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

                                                       checkboxInput(inputId = 'huang_growth',
                                                                     label = 'Huang',
                                                                     value = TRUE)
                                                     ),
                                                     checkboxInput(inputId = 'log_transform_data_parametric_growth',
                                                                   label = 'Log-transform data',
                                                                   value = TRUE)

                                                   )
                                                 ),  # conditionalPanel

                                                 conditionalPanel(
                                                   condition = "input.nonparametric_fit_growth",
                                                   sidebarPanel(
                                                     style='border-color: #ADADAD; padding-top: 0',
                                                     h3(strong('Nonparametric fit')),

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
                                                     )
                                                   )
                                                 )  # conditionalPanel
                                          ) # column
                                        ) # sidebarLayout
                                      ), # fluidRow
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
                                                   inputId = 'custom_sliding_window_size_value_growth',
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
                                                 checkboxInput(inputId = 'log_transform_data_nonparametric',
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
                                      tabsetPanel(type = "tabs",
                                                  tabPanel(title = "Linear Fit",
                                                           DT::dataTableOutput('results_table_growth_linear')
                                                  ),
                                                  tabPanel(title = "Nonparametric Fit",
                                                           DT::dataTableOutput('results_table_growth_spline')
                                                  ),
                                                  tabPanel(title = "Parametric Fit",
                                                           DT::dataTableOutput('results_table_growth_model')
                                                  )
                                      )
                             ),
                             tabPanel(title = "Fluorescence",
                                      h1('Under construction'))
                  ),

                  # Visualize
                  navbarMenu("Visualize",
                             tabPanel(title = "Growth Plots",
                                      h1("Growth Plots"),
                                      tabsetPanel(type = "tabs",
                                                  tabPanel(title = "Group plots",
                                                           sidebarPanel(

                                                             selectInput(inputId = "data_type_growth_group_plots",
                                                                         label = "Data type",
                                                                         choices = c("Raw density" = "raw",
                                                                                     "Spline fits" = "spline")
                                                             ),

                                                             textInput(inputId = "select_samples_based_on_string_growth_group_plots",
                                                                       label = "Select sample based on string (separate by ;)"
                                                             ),

                                                             textInput(inputId = "select_samples_based_on_concentration_growth_group_plots",
                                                                       label = "Select sample based on concentration (separate by ;)"
                                                             ),

                                                             textInput(inputId = "exclude_samples_based_on_string_growth_group_plots",
                                                                       label = "Exclude sample based on string (separate by ;)"
                                                             ),

                                                             textInput(inputId = "exclude_samples_based_on_concentration_growth_group_plots",
                                                                       label = "Exclude sample based on concentration (separate by ;)"
                                                             ),

                                                             checkboxInput(inputId = "plot_group_averages_growth_group_plots",
                                                                           label = "Plot group averages",
                                                                           value = TRUE),

                                                             checkboxInput(inputId = "plot_derivative_growth_group_plots",
                                                                           label = "Plot derivative",
                                                                           value = TRUE),

                                                             h3("Customize plot appearance"),

                                                             checkboxInput(inputId = "log_transform_y_axis_growth_group_plots",
                                                                           label = "Log-transform y-axis",
                                                                           value = TRUE),

                                                             strong("x-Range"),
                                                             fluidRow(
                                                               column(3,
                                                                      textInput(inputId = "x_range_min_growth_group_plot",
                                                                                label = NULL,
                                                                                value = "min"
                                                                      )
                                                               ),

                                                               column(3,
                                                                      textInput(inputId = "x_range_max_growth_group_plot",
                                                                                label = NULL,
                                                                                value = "max"
                                                                      )
                                                               )
                                                             ),

                                                             strong("y-Range"),
                                                             fluidRow(
                                                               column(3,
                                                                      textInput(inputId = "y_range_min_growth_group_plot",
                                                                                label = NULL,
                                                                                value = "min"
                                                                      )
                                                               ),

                                                               column(3,
                                                                      textInput(inputId = "y_range_max_growth_group_plot",
                                                                                label = NULL,
                                                                                value = "max"
                                                                      )
                                                               )
                                                             ),

                                                             strong("y-Range (derivative)"),
                                                             fluidRow(
                                                               column(3,
                                                                      textInput(inputId = "y_range_min_derivative_growth_group_plot",
                                                                                label = NULL,
                                                                                value = "min"
                                                                      )
                                                               ),

                                                               column(3,
                                                                      textInput(inputId = "y_range_max_derivative_growth_group_plot",
                                                                                label = NULL,
                                                                                value = "max"
                                                                      )
                                                               )
                                                             ),

                                                             textInput(inputId = "y_axis_title_growth_group_plot",
                                                                       label = "y-axis title",
                                                                       value = "Growth [y(t)]"
                                                             ),

                                                             textInput(inputId = "x_axis_title_growth_group_plot",
                                                                       label = "x-axis title",
                                                                       value = "Time"
                                                             ),

                                                             textInput(inputId = "y_axis_title_derivative_growth_group_plot",
                                                                       label = "y-axis title derivative",
                                                                       value = "Growth rate"
                                                             ),

                                                             sliderInput(inputId = "line_width_growth_group_plot",
                                                                         label = "Line width",
                                                                         min = 0.01,
                                                                         max = 5,
                                                                         value = 1.1)

                                                           ), # Side panel growth group plots

                                                           mainPanel(
                                                             plotOutput("growth_group_plot")
                                                           )


                                                  ),

                                                  tabPanel(title = "Dose-response analysis",
                                                           sidebarPanel(
                                                             #
                                                           ),
                                                           mainPanel(
                                                             plotOutput("growth_dose_response_plot")
                                                           )
                                                  ),

                                                  tabPanel(title = "Parameter plots",
                                                           sidebarPanel(
                                                             selectInput(inputId = "parameter_growth_parameter_growth_plot",
                                                                         label = "Parameter",
                                                                         choices = ""
                                                             ),

                                                             textInput(inputId = "select_sample_based_on_string_growth_parameter_plot",
                                                                       label = "Select sample based on string (separated by ;)"
                                                             ),

                                                             textInput(inputId = "select_sample_based_on_concentration_growth_parameter_plot",
                                                                       label = "Select sample based on concentration (separated by ;)"
                                                             ),

                                                             textInput(inputId = "exclude_sample_based_on_strings_growth_parameter_plot",
                                                                       label = "Exclude sample based on strings (separated by ;)"
                                                             ),

                                                             textInput(inputId = "exclude_sample_based_on_concentration_growth_parameter_plot",
                                                                       label = "Exclude sample based on concentration (separated by ;)"
                                                             ),

                                                             checkboxInput(inputId = 'normalize_to_reference_growth_parameter_plot',
                                                                           label = 'normalize to reference',
                                                                           value = FALSE),

                                                             # Conditional Panel
                                                             conditionalPanel(condition = "input.normalize_to_reference_growth_parameter_plot",
                                                                              # reactive selection
                                                                              selectInput(inputId = 'reference_condition_growth_parameter_plot',
                                                                                          label = 'Reference condition',
                                                                                          choices = ""
                                                                              ),

                                                                              # reactive selection
                                                                              selectInput(inputId = 'reference_concentration_growth_parameter_plot',
                                                                                          label = 'Reference concentration',
                                                                                          choices = ""
                                                                              ),
                                                             ),


                                                             sliderInput(inputId = "shape.size_growth_parameter_plot",
                                                                         label = "Shape size",
                                                                         min = 1,
                                                                         max = 10,
                                                                         value = 2.5),

                                                             sliderInput(inputId = "basesize_growth_parameter_plot",
                                                                         label = "Base size",
                                                                         min = 1,
                                                                         max = 25,
                                                                         value = 12)


                                                           ),

                                                           mainPanel(
                                                             plotOutput("growth_parameter_plot")
                                                           )
                                                           # TEST CONSOL OUTPUT
                                                           #verbatimTextOutput('test')
                                                  )
                                      )
                             ),


                             tabPanel(title = "Flourescence Plots",
                                      h1("Flourescence Plots"),
                                      tabsetPanel(type = "tabs",
                                                  tabPanel(title = "Group plots",
                                                           sidebarPanel(

                                                             selectInput(inputId = "data_type_fluorescence_group_plots",
                                                                         label = "Data type",
                                                                         choices = c("Raw density" = "raw",
                                                                                     "Spline fits" = "spline")
                                                             ),

                                                             textInput(inputId = "select_samples_based_on_string_fluorescence_group_plots",
                                                                       label = "Select sample based on string (separate by ;)"
                                                             ),

                                                             textInput(inputId = "select_samples_based_on_concentration_fluorescence_group_plots",
                                                                       label = "Select sample based on string (separate by ;)"
                                                             ),

                                                             textInput(inputId = "exclude_samples_based_on_string_fluorescence_group_plots",
                                                                       label = "Select sample based on string (separate by ;)"
                                                             ),

                                                             textInput(inputId = "exclude_samples_based_on_concentration_fluorescence_group_plots",
                                                                       label = "Select sample based on string (separate by ;)"
                                                             ),

                                                             checkboxInput(inputId = "plot_group_averages_fluorescence_group_plots",
                                                                           label = "Plot group averages",
                                                                           value = TRUE),

                                                             checkboxInput(inputId = "plot_derivative_fluorescence_group_plots",
                                                                           label = "Plot derivative",
                                                                           value = TRUE),

                                                             h3("Customize plot appearance"),

                                                             checkboxInput(inputId = "log_transform_y_axis_fluorescence_group_plots",
                                                                           label = "Log-transform y-axis",
                                                                           value = TRUE),

                                                             textInput(inputId = "x_range_fluorescence_group_plot",
                                                                       label = "x-Range (separated by ;)",
                                                                       value = "lower;upper"
                                                             ),

                                                             textInput(inputId = "y_range_fluorescence_group_plot",
                                                                       label = "y-Range (separated by ;)",
                                                                       value = "lower;upper"
                                                             ),

                                                             textInput(inputId = "y_range_derivative_fluorescence_group_plot",
                                                                       label = "y-Range derivative (separated by ;)",
                                                                       value = "lower;upper"
                                                             ),

                                                             textInput(inputId = "y_axis_title_fluorescence_group_plot",
                                                                       label = "y-axis title",
                                                                       value = "Growth [y(t)]"
                                                             ),

                                                             textInput(inputId = "x_axis_title_fluorescence_group_plot",
                                                                       label = "x-axis title",
                                                                       value = "Time"
                                                             ),

                                                             textInput(inputId = "y_axis_title_derivative_fluorescence_group_plot",
                                                                       label = "y-axis title derivative",
                                                                       value = "Growth rate"
                                                             ),

                                                             sliderInput(inputId = "line_width_fluorescence_group_plot",
                                                                         label = "Line width",
                                                                         min = 0.01,
                                                                         max = 5,
                                                                         value = 1.1)

                                                           ), # Side panel growth fluorescence plots
                                                           mainPanel(
                                                             plotOutput("fluorescence_group_plot")
                                                           )
                                                  ),

                                                  tabPanel(title = "Dose-response analysis",
                                                           sidebarPanel(
                                                             # add sibar stuff
                                                           ),
                                                           mainPanel(
                                                             plotOutput("fluorescence_dose_response_plot")
                                                           )
                                                  ),

                                                  tabPanel(title = "Parameter plots",
                                                           sidebarPanel(
                                                             selectInput(inputId = "parameter_growth_parameter_fluorescence_plot",
                                                                         label = "Parameter",
                                                                         choices = c("mu.linfit" = "mu.linfit_fluorescence_parameter_plot",
                                                                                     "other" = "other_fluorescence_parameter_plot")
                                                             ),

                                                             textInput(inputId = "select_sample_based_on_string_fluorescence_parameter_plot",
                                                                       label = "Select sample based on string (separated by ;)"
                                                             ),

                                                             textInput(inputId = "select_sample_based_on_concentration_fluorescence_parameter_plot",
                                                                       label = "Select sample based on concentration (separated by ;)"
                                                             ),

                                                             textInput(inputId = "exclude_sample_based_on_strings_fluorescence_parameter_plot",
                                                                       label = "Exclude sample based on strings (separated by ;)"
                                                             ),

                                                             textInput(inputId = "exclude_sample_based_on_concentration_fluorescence_parameter_plot",
                                                                       label = "Exclude sample based on concentration (separated by ;)"
                                                             ),

                                                             checkboxInput(inputId = 'normalize_to_reference_fluorescence_parameter_plot',
                                                                           label = 'normalize to reference',
                                                                           value = TRUE),

                                                             sliderInput(inputId = "shape.size_fluorescence_parameter_plot",
                                                                         label = "Shape size",
                                                                         min = 1,
                                                                         max = 10,
                                                                         value = 2.5),

                                                             sliderInput(inputId = "basesize_fluorescence_parameter_plot",
                                                                         label = "Base size",
                                                                         min = 1,
                                                                         max = 25,
                                                                         value = 12),

                                                             selectInput(inputId = 'reference_condition_fluorescence_parameter_plot',
                                                                         label = 'Reference condition',
                                                                         choices = ""
                                                             ),

                                                             selectInput(inputId = 'reference_concentration_fluorescence_parameter_plot',
                                                                         label = 'Reference concentration',
                                                                         choices = ""
                                                             ),
                                                           ),
                                                           mainPanel(
                                                             plotOutput("fluorescence_parameter_plot")
                                                           )
                                                  )
                                      )
                             ),

                             tabPanel(title = "Growth & Flourescence Plots",
                                      h1("Growth & Flourescence Plots"),
                                      tabsetPanel(type = "tabs",
                                                  tabPanel(title = "Group plots",
                                                           sidebarPanel(
                                                             # add sibar stuff
                                                           ),
                                                           plotOutput("growth_and_fluorescence_group_plot")),
                                                  tabPanel(title = "Dose-response analysis",
                                                           sidebarPanel(
                                                             # add sibar stuff
                                                           ),
                                                           plotOutput("growth_and_fluorescence_dose_response_plot")),
                                                  tabPanel(title = "Parameter plots",
                                                           sidebarPanel(
                                                             # add sibar stuff
                                                           ),
                                                           plotOutput("growth_and_fluorescence_parameter_plot"))
                                      )
                             ),
                  ),

                  tabPanel("Report",  h1("under construction")),

                  tabPanel("About Us",
                           mainPanel(
                             h1("Authors"),
                             'Nicolas Wirth, Jonathan Funk',
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

    fit.opt <- c()
    if(input$linear_regression_growth){
      fit.opt <- c(fit.opt,
                   'l')
    }
    if(input$parametric_fit_growth){
      fit.opt <- c(fit.opt,
                   'm')
    }
    if(input$nonparametric_fit_growth){
      fit.opt <- c(fit.opt,
                   's')
    }

    results$growth <- growth.workflow(grodata = grodata,
                                      ec50 = input$perform_ec50_growth,
                                      fit.opt = fit.opt,
                                      t0 = input$t0_growth,
                                      min.density = input$minimum_density_growth,
                                      log.x.gc = input$log_transform_time_growth,
                                      log.y.model = input$log_transform_data_parametric_growth,
                                      log.y.spline = input$log_transform_data_nonparametric_growth,
                                      biphasic = input$biphasic_growth_growth,
                                      lin.h = input$custom_sliding_window_size_value_growth,## Here seem to be problems
                                      lin.R2 = input$R2_threshold_growth,
                                      lin.RSD = input$RSD_threshold_growth,
                                      lin.dY = input$dY_threshold_growth, ##
                                      interactive = FALSE, ### TODO (popups)
                                      nboot.gc = input$number_of_bootstrappings,
                                      smooth.gc = input$smoothing_factor_nonparametric_growth,
                                      model.type = c("logistic", "richards", "gompertz", "gompertz.exp", "huang"), ### TODO: implement like model and huang button
                                      growth.thresh = input$growth_threshold_growth,
                                      dr.parameter = input$response_parameter_growth, ### TODO if else bla
                                      smooth.dr = input$smooth.dr,
                                      log.x.dr = input$log_transform_concentration_growth,
                                      log.y.dr = input$log_transform_response_growth,
                                      nboot.dr = input$number_of_bootstrappings_dr_growth,
                                      report = NULL,
                                      out.dir = "Test"
    )
    gc_parameters <- c()
    if("s" %in% results$growth$control$fit.opt || "a" %in% results$growth$control$fit.opt){
      gc_parameters <- c(gc_parameters,
                         'mu.spline' = 'mu.spline',
                         "tD.spline" = "tD.spline",
                         'lambda.spline' = 'lambda.spline',
                         'A.spline' = 'A.spline',
                         'dY.spline' = 'dY.spline',
                         'integral.spline' = 'integral.spline')
    }
    if("l" %in% results$growth$control$fit.opt || "a" %in% results$growth$control$fit.opt){
      gc_parameters <- c(gc_parameters,
                         'mu.linfit' = 'mu.linfit',
                         "tD.linfit" = "tD.linfit",
                         'lambda.linfit' = 'lambda.linfit',
                         'A.linfit' = 'A.linfit',
                         'dY.linfit' = 'dY.linfit')
    }
    if("m" %in% results$growth$control$fit.opt || "a" %in% results$growth$control$fit.opt){
      gc_parameters <- c(gc_parameters,
                         'mu.model' = 'mu.model',
                         'lambda.model' = 'lambda.model',
                         'A.model' = 'A.model',
                         'dY.model' = 'dY.model')
    }
  })

  # Group Plots:
  ## Growth
  output$growth_group_plot <- renderPlot({
    results <- results$growth
    plot.grofit(results,
                data.type = input$data_type_growth_group_plots,
                names = input$select_samples_based_on_string_growth_group_plots,
                conc = input$select_samples_based_on_concentration_growth_group_plots,
                exclude.nm = input$exclude_samples_based_on_string_growth_group_plots,
                exclude.conc = input$exclude_samples_based_on_concentration_growth_group_plots,
                mean = input$plot_group_averages_growth_group_plots,
                deriv = input$plot_derivative_growth_group_plots,
                log.y = input$log_transform_y_axis_growth_group_plots,
                x.lim = c(input$x_range_min_growth_group_plot, input$x_range_max_growth_group_plot),
                y.lim = c(input$y_range_min_growth_group_plot,input$y_range_max_growth_group_plot),
                y.lim.deriv = c(input$y_range_min_derivative_growth_group_plot, input$y_range_max_derivative_growth_group_plot),
                y.title = input$y_axis_title_growth_group_plot,
                x.title = input$x_axis_title_growth_group_plot,
                y.title.deriv = input$y_axis_title_derivative_growth_group_plot,
                lwd = input$line_width_growth_group_plot
    )
  })

  output$test <- renderText({
    print(ifelse(is.null(input$reference_condition_growth_parameter_plot), "Null", input$reference_condition_growth_parameter_plot))
  })

  output$dose_response_plot <- renderPlot({
    results <- results$growth
    plot(results$drFit)
  })

  output$growth_parameter_plot <- renderPlot({
    results <- results$growth

    if (input$normalize_to_reference_growth_parameter_plot){
      reference.conc <- as.numeric(input$reference_concentration_growth_parameter_plot)
      reference.nm <- input$reference_condition_growth_parameter_plot
    } else {
      reference.conc <- NULL
      reference.nm <- NULL
    }



    plot.parameter(results,
                   param = input$parameter_growth_parameter_growth_plot,
                   names = input$select_sample_based_on_string_growth_parameter_plot,
                   conc = input$select_sample_based_on_concentration_growth_parameter_plot,
                   exclude.nm = input$exclude_sample_based_on_strings_growth_parameter_plot,
                   exclude.conc = input$exclude_sample_based_on_concentration_growth_parameter_plot,
                   reference.nm = reference.nm,
                   reference.conc = reference.conc,
                   shape.size = input$shape.size_growth_parameter_plot,
                   basesize = input$basesize_growth_parameter_plot
    )
  })


  output$results_table_growth_linear <- DT::renderDT({
    res.table.gc <- results$growth$gcFit$gcTable

    table_linear <- datatable(data.frame("Sample|Replicate|Conc." = paste(res.table.gc$TestId, res.table.gc$AddId, res.table.gc$concentration, sep = "|"),
                              "µ<sub>max</sub>" = ifelse(res.table.gc$mu.linfit==0 | is.na(res.table.gc$mu.linfit), "", ifelse(is.na(res.table.gc$mu2.linfit), round(as.numeric(res.table.gc$mu.linfit), 3), paste0("<strong>", round(as.numeric(res.table.gc$mu.linfit), 3), "</strong>", " (", round(as.numeric(res.table.gc$mu2.linfit), 3), ")"))),
                              "t<sub>D</sub>" = ifelse(res.table.gc$mu.linfit==0 | is.na(res.table.gc$mu.linfit), "",  ifelse(is.na(res.table.gc$mu2.linfit), round(log(2)/as.numeric(res.table.gc$mu.linfit), 2), paste0("<strong>", round(log(2)/as.numeric(res.table.gc$mu.linfit), 2), "</strong>", " (", round(log(2)/as.numeric(res.table.gc$mu2.linfit), 2), ")"))),
                              "λ" = round(as.numeric(res.table.gc$lambda.linfit), 2),
                              "Δy" = round(as.numeric(res.table.gc$dY.linfit), 3),
                              "y<sub>max</sub>" = round(as.numeric(res.table.gc$A.linfit), 3),
                              "t<sub>start</sub><br>(µ<sub>max</sub>)" = ifelse(is.na(res.table.gc$mu2.linfit), round(as.numeric(res.table.gc$tmu.start.linfit), 2), paste0("<strong>", round(as.numeric(res.table.gc$tmu.start.linfit), 2), "</strong>", " (", round(as.numeric(res.table.gc$tmu2.start.linfit), 2), ")")),
                              "t<sub>end</sub><br>(µ<sub>max</sub>)" = ifelse(is.na(res.table.gc$mu2.linfit), round(as.numeric(res.table.gc$tmu.end.linfit), 2), paste0("<strong>", round(as.numeric(res.table.gc$tmu.end.linfit), 2), "</strong>", " (", round(as.numeric(res.table.gc$tmu2.end.linfit), 2), ")")),
                              "R<sup>2</sup><br>(linear fit)" = ifelse(is.na(res.table.gc$mu2.linfit), round(as.numeric(res.table.gc$r2mu.linfit), 3), paste0("<strong>", round(as.numeric(res.table.gc$r2mu.linfit), 3), "</strong>", " (", round(as.numeric(res.table.gc$r2mu2.linfit), 3), ")")),
                              stringsAsFactors = F, check.names = F),
                              options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, -1), c("15","25", "50", "All")) ),
                              escape = FALSE)
    # table_linear <- res.table.gc
    table_linear
  })

  output$results_table_growth_spline <- DT::renderDT({
    res.table.gc <- results$growth$gcFit$gcTable
    table_spline <- datatable(data.frame("Sample|Replicate|Conc." = paste(res.table.gc$TestId, res.table.gc$AddId, res.table.gc$concentration, sep = "|"),
                                         "µ<sub>max</sub>" = ifelse(res.table.gc$mu.spline==0 | is.na(res.table.gc$mu.spline), "", ifelse(is.na(res.table.gc$mu2.spline), round(as.numeric(res.table.gc$mu.spline), 3), paste0("<strong>", round(as.numeric(res.table.gc$mu.spline), 3), "</strong>", " (", round(as.numeric(res.table.gc$mu2.spline), 3), ")"))),
                                         "t<sub>D</sub>" = ifelse(res.table.gc$mu.spline==0 | is.na(res.table.gc$mu.spline), "",  ifelse(is.na(res.table.gc$mu2.spline), round(log(2)/as.numeric(res.table.gc$mu.spline), 2), paste0("<strong>", round(log(2)/as.numeric(res.table.gc$mu.spline), 2), "</strong>", " (", round(log(2)/as.numeric(res.table.gc$mu2.spline), 2), ")"))),
                                         "λ" = round(as.numeric(res.table.gc$lambda.spline), 2),
                                         "y<sub>max</sub>" = round(as.numeric(res.table.gc$A.spline), 3),
                                         "Δy" = round(as.numeric(res.table.gc$dY.spline), 3),
                                         "t<sub>max</sub>" = ifelse(is.na(res.table.gc$mu2.spline), round(as.numeric(res.table.gc$tmax.spline), 2), paste0("<strong>", round(as.numeric(res.table.gc$tmax.spline), 2), "</strong>", " (", round(as.numeric(res.table.gc$tmax2.spline), 2), ")")),
                                         "smooth.<br>fac" = res.table.gc$smooth.spline, check.names = F),
                              options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, -1), c("15","25", "50", "All")) ),
                              escape = FALSE)
    table_spline
  })

  output$results_table_growth_model <- DT::renderDT({
    res.table.gc <- results$growth$gcFit$gcTable
    table_model <- data.frame("Sample|Replicate|Conc." = paste(res.table.gc$TestId, res.table.gc$AddId, res.table.gc$concentration, sep = "|"),
                     "Model" = res.table.gc$used.model,
                     "µ<sub>max</sub>" = ifelse(res.table.gc$mu.model==0 | is.na(res.table.gc$mu.model), "", paste(round(as.numeric(res.table.gc$mu.model), 3),"\u00B1", round(as.numeric(res.table.gc$stdmu.model),3))),
                     "t<sub>D</sub>" = paste(ifelse(res.table.gc$mu.model==0 | is.na(res.table.gc$mu.model), "", paste(round(log(2)/as.numeric(res.table.gc$mu.model), 2), "\u00B1", round(sqrt(((-log(2)*as.numeric(res.table.gc$stdmu.model))/(as.numeric(res.table.gc$mu.model))^2)^2), 2)))),
                     "λ" = paste(round(as.numeric(res.table.gc$lambda.model), 2), "\u00B1", round(as.numeric(res.table.gc$stdlambda.model),3)),
                     "A" = paste(round(as.numeric(res.table.gc$A.model), 3), "\u00B1", round(as.numeric(res.table.gc$stdA.model),3)),
                     stringsAsFactors = F, check.names = F)
    if(!is.null(res.table.gc)){
      if ( "richards" %in% res.table.gc$used.model  ){
        table_model <- suppressWarnings(cbind(table_model, data.frame("ν" = round(as.numeric(res.table.gc$parameter_nu.model), 3), stringsAsFactors = F, check.names = F)))
      }
      if ( "gompertz" %in% res.table.gc$used.model ){
        table_model <- suppressWarnings(cbind(table_model, data.frame("α" = round(as.numeric(res.table.gc$parameter_alpha.model), 3), stringsAsFactors = F, check.names = F)))
      }
      if ( "gompertz.exp" %in% res.table.gc$used.model ){
        table_model <- suppressWarnings(cbind(table_model, data.frame("t<sub>shift</sub>" = round(as.numeric(res.table.gc$parameter_t_shift.model), 3), stringsAsFactors = F, check.names = F)))
      }
    }
    table_model <- datatable(table_model,
                              options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, -1), c("15","25", "50", "All")) ),
                              escape = FALSE)
    table_model
  })

  # conditional selections:
  ## Growth plots
  ### growth.parameter()

  selected_inputs_parameter_growth_parameter_plot <- reactive({
    results <- results$growth
    gc_parameters <- c()
    if("s" %in% results$control$fit.opt || "a" %in% results$control$fit.opt){
      if(results$control$biphasic){
        gc_parameters <- c(gc_parameters,
                           'Growth rate (Spline)' = 'mu.spline',
                           'Growth rate phase 2 (Spline)' = 'mu.spline',
                           "Doubling time (Spline)" = "tD.spline",
                           "Doubling time phase 2 (Spline)" = "tD.spline",
                           'Lag time (Spline)' = 'lambda.spline',
                           'Maximum density (Spline)' = 'A.spline',
                           'ΔDensity (Spline)' = 'dY.spline',
                           'Area under the curve (Spline)' = 'integral.spline')
      } else {
        gc_parameters <- c(gc_parameters,
                           'Growth rate (Spline)' = 'mu.spline',
                           "Doubling time (Spline)" = "tD.spline",
                           'Lag time (Spline)' = 'lambda.spline',
                           'Maximum density (Spline)' = 'A.spline',
                           'ΔDensity (Spline)' = 'dY.spline',
                           'Area under the curve (Spline)' = 'integral.spline')
      }
    }
    if("l" %in% results$control$fit.opt || "a" %in% results$control$fit.opt){
      if(results$control$biphasic){
        gc_parameters <- c(gc_parameters,
                           'Growth rate (linear fit)' = 'mu.linfit',
                           'Growth rate phase 2 (linear fit)' = 'mu.linfit',
                           "Doubling time (linear fit)" = "tD.linfit",
                           "Doubling time phase 2 (linear fit)" = "tD.linfit",
                           'Lag time (linear fit)' = 'lambda.linfit',
                           'Maximum density (linear fit)' = 'A.linfit',
                           'ΔDensity (linear fit)' = 'dY.linfit')
      } else {
        gc_parameters <- c(gc_parameters,
                           'Growth rate (linear fit)' = 'mu.linfit',
                           "Doubling time (linear fit)" = "tD.linfit",
                           'Lag time (linear fit)' = 'lambda.linfit',
                           'Maximum density (linear fit)' = 'A.linfit',
                           'ΔDensity (linear fit)' = 'dY.linfit')
      }
    }
    if("m" %in% results$control$fit.opt || "a" %in% results$control$fit.opt){
      gc_parameters <- c(gc_parameters, 'mu.model' = 'mu.model', 'lambda.model' = 'lambda.model', 'A.model' = 'A.model', 'dY.model' = 'dY.model')
    }
    gc_parameters
  })

  selected_inputs_reference_condition_growth_parameter_plot <- reactive({
    results <- results$growth
    results$expdesign$condition
  })

  select_inputs_reference_concentration_growth_parameter_plot <- reactive({
    results <- results$growth
    results$expdesign$concentration
  })

  observe({
    updateSelectInput(inputId = "parameter_growth_parameter_growth_plot",
                      choices = selected_inputs_parameter_growth_parameter_plot()
    )})

  observe({
    updateSelectInput(inputId = "reference_condition_growth_parameter_plot",
                      choices = selected_inputs_reference_condition_growth_parameter_plot()
    )})

  observe({
    updateSelectInput(inputId = "reference_concentration_growth_parameter_plot",
                      choices = select_inputs_reference_concentration_growth_parameter_plot()
    )})


  ## Fluorescence
  selected_inputs_reference_condition_fluorescence_parameter_plot <- reactive({
    results <- results$growth
    results$expdesign$condition
  })

  select_inputs_reference_concentration_fluorescence_parameter_plot <- reactive({
    results <- results$growth
    results$expdesign$concentration
  })

  observe({
    updateSelectInput(inputId = "reference_condition_fluorescence_parameter_plot",
                      choices = selected_inputs_reference_condition_fluorescence_parameter_plot()
    )})

  observe({
    updateSelectInput(inputId = "reference_concentration_fluorescence_parameter_plot",
                      choices = select_inputs_reference_concentration_fluorescence_parameter_plot()
    )})



  # in case you want to print a consol output:
  #output$console <- renderPlot({
  #  results <- results()
  #  plot.parameter(results)
  #  })

}


shinyApp(ui = ui, server = server)
