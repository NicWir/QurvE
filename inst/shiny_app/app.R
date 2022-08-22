list.of.packages <- c("ggplot2", "shiny", "readxl", "tidyverse", "shinythemes")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(readxl)
library(tidyverse)
library(shinythemes)

source("../../R/general_misc_utils.R")
source("../../R/growth_computation.R")
source("../../R/growth_plots.R")
source("../../R/growth_summaries.R")
source("../../R/fluorescence_computation.R")
source("../../R/fluorescence_plots.R")
source("../../R/fluorescence_summaries.R")

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
                                                               value = FALSE),

                                                 numericInput(
                                                   inputId = 'custum_sliding_window_size_value_growth',
                                                   label = NULL,
                                                   value = 1,
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
                                                                         choices = c("mu.linfit" = "mu.linfit",
                                                                                     "other" = "other_growth_parameter_plot")
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
                                                                           value = TRUE),

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
                                      neg.nan.act = input$remove_negative_values_in_bootstrapping_growth,
                                      report = NULL,
                                      out.dir = "Test"
    )
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

    plot.parameter(results,
                   param = input$parameter_growth_parameter_growth_plot,
                   names = input$select_sample_based_on_string_growth_parameter_plot,
                   conc = input$select_sample_based_on_concentration_growth_parameter_plot,
                   exclude.nm = input$exclude_sample_based_on_strings_growth_parameter_plot,
                   exclude.conc = input$exclude_sample_based_on_concentration_growth_parameter_plot,
                   reference.nm = input$reference_condition_growth_parameter_plot,
                   reference.conc = input$reference_concentration_growth_parameter_plot,
                   shape.size = input$shape.size_growth_parameter_plot,
                   basesize = input$basesize_growth_parameter_plot
                   )
  })


  output$results_table_growth <- renderTable({
    results <- results$growth
    results$gcFit$gcTable
  })

  # conditional selections:
  ## Growth plot
  selected_inputs_reference_condition_growth_parameter_plot <- reactive({
    results <- results$growth
    results$expdesign$condition
  })

  select_inputs_reference_concentration_growth_parameter_plot <- reactive({
    results <- results$growth
    results$expdesign$concentration
  })

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
