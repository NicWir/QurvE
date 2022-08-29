list.of.packages <- c("ggplot2", "shiny", "readxl", "tidyverse", "shinythemes", "shinyFiles", "shinyjs", "shinyBS", "shinycssloaders")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# library(icons)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinycssloaders)
library(shinyFiles)
library(readxl)
library(tidyverse)
library(shinythemes)
library(DT)

# Define icon set from custom SVG files
# iconset <- icon_set("icons/")

source("../../R/general_misc_utils.R")
source("../../R/growth_computation.R")
source("../../R/growth_plots.R")
source("../../R/growth_summaries.R")
source("../../R/fluorescence_computation.R")
source("../../R/fluorescence_plots.R")
source("../../R/fluorescence_summaries.R")

jscode <- "
shinyjs.disableTab = function(name) {
var tab = $('.nav li a[data-value=' + name + ']');
tab.bind('click.tab', function(e) {
e.preventDefault();
return false;
});
tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
var tab = $('.nav li a[data-value=' + name + ']');
tab.unbind('click.tab');
tab.removeClass('disabled');
}
"
css <- "
.nav li a.disabled {
background-color: #65675F !important;
color: #333 !important;
cursor: not-allowed !important;
border-color: #aaa !important;
}"

ui <- fluidPage(theme = shinytheme('sandstone'),
                tagList(
                  # tags$style(type = 'text/css', '.navbar {
                  #          font-size: 200px;
                  #          }',
                  #
                  #            '.navbar-dropdown { background-color: #262626;
                  #          font-family: Arial;
                  #          font-size: 50px;
                  #          color: #FF0000; }',
                  #
                  #            '.navbar-default .navbar-brand {
                  #            ;
                  #          }',
                  #            '.navbar li a {
                  #            font-size: 50px;
                  #          }'
                  #
                  # ),
                  useShinyjs(),
                  shinyjs::extendShinyjs(text = jscode, functions = c("disableTab","enableTab")),
                  shinyjs::inlineCSS(css),
                  navbarPage(
                    'QurvE',
                    id = "navbar",

                    # load input file
                    tabPanel('Data', value = "tabPanel",
                             icon=icon(name = NULL, "verify_fa = FALSE",
                                       img("../icons/icon_Upload.svg")),
                             tabsetPanel(type = "tabs",

                                         tabPanel(title = "Custom",
                                                  sidebarPanel(
                                                    style='border-color: #ADADAD',
                                                    wellPanel(
                                                      h4(strong("Growth data"), style = "line-height: 0.4;font-size: 150%; margin-bottom: 15px;"),
                                                      style='background-color:#F0EBE4; padding: 0.1; border-color: #ADADAD; padding: 1; padding-bottom: 0',
                                                      # select file type
                                                      selectInput(inputId = "format",
                                                                  label = "Select Format",
                                                                  choices = c("Data in columns" = "data_in_columns",
                                                                              "Data in rows" = "data_in_rows")),

                                                      fileInput(inputId = 'growth_file',
                                                                label = 'Choose growth data file',
                                                                accept = c('.xlsx', '.xls', '.csv', 'txt', 'tsv'))
                                                    ),

                                                    conditionalPanel(
                                                      condition = "output.growthfileUploaded && output.data_growth_format == 'xlsx'",
                                                      wellPanel(
                                                        style='background-color:#F0EBE4; padding: 1; border-color: #ADADAD; padding: 1; padding-bottom: 0',
                                                        selectInput(inputId = "custom_growth_sheets",
                                                                    label = "Select Sheet",
                                                                    choices = "Sheet1")
                                                      )), # select sheet: conditional
                                                    conditionalPanel(
                                                      condition = "output.growthfileUploaded && output.data_growth_format == 'csv'",
                                                      wellPanel(
                                                        style='background-color:#F0EBE4; padding: 1; border-color: #ADADAD; padding: 1; padding-bottom: 0',
                                                        selectInput(inputId = "separator",
                                                                    label = "Select separator",
                                                                    choices = c("," = ",",
                                                                                ";" = ";")
                                                        ),

                                                        selectInput(inputId = "decimal_separator",
                                                                    label = "Select Decimal separator",
                                                                    choices = c("." = ".",
                                                                                "," = ",")
                                                        )
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
                                                              accept = c('.xlsx', '.xls', '.csv', '.tsv', '.txt')),

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
                                                      selectInput(inputId = "separator",
                                                                  label = "Select separator",
                                                                  choices = c("," = ",",
                                                                              ";" = ";")
                                                      ),

                                                      selectInput(inputId = "decimal_separator",
                                                                  label = "Select Decimal separator",
                                                                  choices = c("." = ".",
                                                                              "," = ",")
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
                               img(src = 'data_instruction.png',
                                   heigt = '100%',
                                   width = '100%'),
                               h1("Your Data"),
                               withSpinner(
                                 DT::dataTableOutput("growth_data")
                               )
                             ) # main panel

                    ), # Navbar 1

                    navbarMenu('Computation', menuName = "navbarMenu_Computation", icon=icon("gears"),
                               tabPanel("Growth", value = "tabPanel_Growth",
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


                                                                   conditionalPanel(condition = "input.perform_ec50_growth",
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
                                                                   )
                                                                 ),


                                                                 actionButton(inputId = "run_growth",
                                                                              label = "Run computation",
                                                                              icon=icon("gears"),
                                                                              style="padding:5px; font-size:120%")
                                                   ) # sidebarPanel

                                            ), # column
                                            column(8,
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

                                                       conditionalPanel(
                                                         condition = "input.custom_sliding_window_size_growth",
                                                         numericInput(
                                                           inputId = 'custom_sliding_window_size_value_growth',
                                                           label = NULL,
                                                           value = "",
                                                           min = NA,
                                                           max = NA,
                                                         )
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
                                                       width = 4,
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
                                        textOutput("text")
                               ), # Growth Tab Panel

                               "----",

                               tabPanel("Fluorescence", id = "tabPanel_Computation_Fluorescence",
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

                                                   conditionalPanel(
                                                     condition = "input.perform_ec50_fluorescence",
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
                                                     )
                                                   )

                                                   ,


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

                    navbarMenu(title = "Results", menuName = "navbarMenu_Results", icon = icon("magnifying-glass-chart"),
                               tabPanel(title = "Growth", value = "tabPanel_Results_Growth",
                                        tabsetPanel(type = "tabs", id = "tabsetPanel_Results",
                                                    tabPanel(title = "Linear Fit", value = "tabPanel_Results_Growth_Linear",
                                                             DT::dataTableOutput('results_table_growth_linear')
                                                    ),
                                                    tabPanel(title = "Nonparametric Fit", value = "tabPanel_Results_Growth_Spline",
                                                             DT::dataTableOutput('results_table_growth_spline')
                                                    ),
                                                    tabPanel(title = "Parametric Fit", value = "tabPanel_Results_Growth_Model",
                                                             DT::dataTableOutput('results_table_growth_model')
                                                    )
                                        )
                               ),

                               tabPanel(title = "Fluorescence", value = "tabPanel_Results_Fluorescence",
                                        h1('Under construction'))
                    ),
                    # Validate
                    navbarMenu("Validate",
                               tabPanel(title = "Growth Fits", value = "tabPanel_Validate_Growth",
                                        h1("Growth Fits"),
                                        tabsetPanel(type = "tabs",
                                                    tabPanel(title = "Linear Fits", value = "tabPanel_Validate_Growth_linearFits",
                                                             sidebarPanel(width = 5,
                                                                          selectInput(inputId = "sample_validate_growth_linear",
                                                                                      label = "Sample:",
                                                                                      width = "fit-content",
                                                                                      choices = "",
                                                                                      multiple = FALSE,
                                                                                      selectize = FALSE,
                                                                                      size = 5,
                                                                          ),
                                                                          checkboxInput(inputId = 'logy_validate_growth_plot_linear',
                                                                                        label = 'Log-transform y axis',
                                                                                        value = TRUE),
                                                                          checkboxInput(inputId = 'diagnostics_validate_growth_plot_linear',
                                                                                        label = 'Show diagnostics',
                                                                                        value = FALSE)

                                                             ),
                                                             mainPanel(width = 7,
                                                                       plotOutput("validate_growth_plot_linear", width = "100%", height = "600px"),
                                                                       fluidRow(
                                                                         column(6, align = "center", offset = 3,
                                                                                actionButton(inputId = "rerun_growth_linear",
                                                                                             label = "Re-run with modified parameters",
                                                                                             icon=icon("gears"),
                                                                                             style="padding:5px; font-size:120%"),
                                                                                actionButton(inputId = "restore_growth_linear",
                                                                                             label = "Restore fit",
                                                                                             # icon=icon("gears"),
                                                                                             style="padding:5px; font-size:120%")
                                                                         )
                                                                       )
                                                             )

                                                    ),
                                                    tabPanel(title = "Nonparametric fits", value = "tabPanel_Validate_Growth_splineFits",
                                                             sidebarPanel(width = 5,
                                                                          selectInput(inputId = "sample_validate_growth_spline",
                                                                                      label = "Sample:",
                                                                                      width = "fit-content",
                                                                                      choices = "",
                                                                                      multiple = FALSE,
                                                                                      selectize = FALSE,
                                                                                      size = 5,
                                                                          ),
                                                                          checkboxInput(inputId = 'logy_validate_growth_plot_spline',
                                                                                        label = 'Log-transform y axis',
                                                                                        value = TRUE)

                                                             ),
                                                             mainPanel(width = 7,
                                                                       withSpinner(
                                                                         plotOutput("validate_growth_plot_spline",
                                                                                    width = "100%", height = "700px")
                                                                       ),
                                                                       fluidRow(
                                                                         column(6, align = "center", offset = 3,
                                                                                actionButton(inputId = "rerun_growth_spline",
                                                                                             label = "Re-run with modified parameters",
                                                                                             icon=icon("gears"),
                                                                                             style="padding:5px; font-size:120%"),
                                                                                actionButton(inputId = "restore_growth_spline",
                                                                                             label = "Restore fit",
                                                                                             # icon=icon("gears"),
                                                                                             style="padding:5px; font-size:120%")
                                                                         )
                                                                       )
                                                             )

                                                    ),
                                                    tabPanel(title = "Parametric fits", value = "tabPanel_Validate_Growth_modelFits",
                                                             sidebarPanel(width = 5,
                                                                          wellPanel(
                                                                            style='background-color:#F0EBE4; padding: 1; padding-top: 0; padding-bottom: 0',
                                                                            selectInput(inputId = "sample_validate_growth_model",
                                                                                        label = "Sample:",
                                                                                        width = "fit-content",
                                                                                        choices = "",
                                                                                        multiple = FALSE,
                                                                                        selectize = FALSE,
                                                                                        size = 5,
                                                                            )
                                                                          )

                                                             ),
                                                             mainPanel(width = 7,
                                                                       withSpinner(
                                                                         plotOutput("validate_growth_plot_model",
                                                                                    width = "100%", height = "600px")
                                                                       ),
                                                                       fluidRow(
                                                                         column(6, align = "center", offset = 3,
                                                                                actionButton(inputId = "rerun_growth_model",
                                                                                             label = "Re-run with modified parameters",
                                                                                             icon=icon("gears"),
                                                                                             style="padding:5px; font-size:120%"),

                                                                                actionButton(inputId = "restore_growth_model",
                                                                                             label = "Restore fit",
                                                                                             # icon=icon("gears"),
                                                                                             style="padding:5px; font-size:120%")
                                                                         )
                                                                       )
                                                             )

                                                    )
                                        )
                               ),
                               tabPanel(title = "Fluorescence Fits", value = "tabPanel_Validate_Fluorescence",
                                        h1("Fluorescence Fits"),
                                        tabsetPanel(type = "tabs",
                                                    tabPanel(title = "Linear Fits", value = "tabPanel_Validate_Fluorescence_linearFits",
                                                             sidebarPanel(
                                                               selectInput(inputId = "sample_validate_fluorescence_linear",
                                                                           label = "Sample:",
                                                                           choices = "",
                                                                           multiple = TRUE
                                                               )

                                                             )

                                                    ),
                                                    tabPanel(title = "Nonparametric fits",
                                                             sidebarPanel()

                                                    )
                                        )
                               )
                    ),
                    # Visualize
                    navbarMenu("Visualize",
                               tabPanel(title = "Growth Plots", value = "tabPanel_Visalize_Growth",
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
                                                               withSpinner(
                                                                 plotOutput("growth_group_plot")
                                                               )
                                                             )


                                                    ),

                                                    tabPanel(title = "Dose-response analysis", value = "tabPanel_Visualize_Growth_DoseResponse",
                                                             sidebarPanel(
                                                               wellPanel(
                                                                 style='background-color:#F0EBE4; padding: 1; border-color: #ADADAD; padding: 1; padding-bottom: 0',
                                                                 checkboxInput(inputId = 'combine_conditions_into_a_single_plot_dose_response_growth_plot',
                                                                               label = 'Combine conditions into a single plot',
                                                                               value = TRUE)
                                                               ),

                                                               h3('Customize plot appearance'),
                                                               sliderInput(inputId = 'shape_type_dose_response_growth_plot',
                                                                           label = 'Shape type',
                                                                           min = 1,
                                                                           max = 25,
                                                                           value = 15),

                                                               sliderInput(inputId = 'shape_size_dose_response_growth_plot',
                                                                           label = 'Shape size',
                                                                           min = 1,
                                                                           max = 10,
                                                                           value = 2),

                                                               sliderInput(inputId = 'base_size_dose_response_growth_plot',
                                                                           label = 'Base size',
                                                                           min = 1,
                                                                           max = 25,
                                                                           value = 15),

                                                               sliderInput(inputId = 'line_width_dose_response_growth_plot',
                                                                           label = 'Line width',
                                                                           min = 1,
                                                                           max = 10,
                                                                           value = 1),

                                                               checkboxInput(inputId = 'show_ec50_indicator_lines_dose_response_growth_plot',
                                                                             label = 'Show EC50 indicator lines',
                                                                             value = TRUE),

                                                               h3("Customize plot appearance"),

                                                               checkboxInput(inputId = "log_transform_y_axis_dose_response_plot",
                                                                             label = "Log-transform y-axis",
                                                                             value = TRUE),

                                                               strong("x-Range"),
                                                               fluidRow(
                                                                 column(3,
                                                                        textInput(inputId = "x_range_min_dose_response_plot",
                                                                                  label = NULL,
                                                                                  value = "min"
                                                                        )
                                                                 ),

                                                                 column(3,
                                                                        textInput(inputId = "x_range_max_dose_response_plot",
                                                                                  label = NULL,
                                                                                  value = "max"
                                                                        )
                                                                 )
                                                               ),

                                                               strong("y-Range"),
                                                               fluidRow(
                                                                 column(3,
                                                                        textInput(inputId = "y_range_min_dose_response_plot",
                                                                                  label = NULL,
                                                                                  value = "min"
                                                                        )
                                                                 ),

                                                                 column(3,
                                                                        textInput(inputId = "y_range_max_dose_response_plot",
                                                                                  label = NULL,
                                                                                  value = "max"
                                                                        )
                                                                 )
                                                               ),

                                                               strong("y-Range (derivative)"),
                                                               fluidRow(
                                                                 column(3,
                                                                        textInput(inputId = "y_range_min_derivative_dose_response_plot",
                                                                                  label = NULL,
                                                                                  value = "min"
                                                                        )
                                                                 ),

                                                                 column(3,
                                                                        textInput(inputId = "y_range_max_derivative_dose_response_plot",
                                                                                  label = NULL,
                                                                                  value = "max"
                                                                        )
                                                                 )
                                                               ),
                                                               textInput(inputId = "y_axis_title_dose_response_plot",
                                                                         label = "y-axis title",
                                                                         value = "mu.linfit"
                                                               ),

                                                               textInput(inputId = "x_axis_title_dose_response_plot",
                                                                         label = "x-axis title",
                                                                         value = "Concentration"
                                                               )

                                                             ), #

                                                             conditionalPanel(condition = "input.combine_conditions_into_a_single_plot_dose_response_growth_plot",
                                                                              mainPanel(
                                                                                h3('Combined plots'),
                                                                                plotOutput("dose_response_plot_combined")
                                                                              )
                                                             ),

                                                             conditionalPanel(condition = "!input.combine_conditions_into_a_single_plot_dose_response_growth_plot",
                                                                              mainPanel(
                                                                                h3('Individual plots'),
                                                                                selectInput(inputId = 'individual_plots_dose_response_growth_plot',
                                                                                            label = 'Select plot',
                                                                                            choices = ""),
                                                                                plotOutput("dose_response_plot_individual")
                                                                              )
                                                             ),


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

                               tabPanel(title = "Fluorescence Plots",  value = "tabPabel_Visualize_Fluorescence",
                                        h1("Fluorescence Plots"),
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
                                                                         label = "Select sample based on concentration (separate by ;)"
                                                               ),

                                                               textInput(inputId = "exclude_samples_based_on_string_fluorescence_group_plots",
                                                                         label = "Exclude sample based on string (separate by ;)"
                                                               ),

                                                               textInput(inputId = "exclude_samples_based_on_concentration_fluorescence_group_plots",
                                                                         label = "Exclude sample based on concentration (separate by ;)"
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

                                                               strong("x-Range"),
                                                               fluidRow(
                                                                 column(3,
                                                                        textInput(inputId = "x_range_min_fluorescence_group_plot",
                                                                                  label = NULL,
                                                                                  value = "min"
                                                                        )
                                                                 ),

                                                                 column(3,
                                                                        textInput(inputId = "x_range_max_fluorescence_group_plot",
                                                                                  label = NULL,
                                                                                  value = "max"
                                                                        )
                                                                 )
                                                               ),

                                                               strong("y-Range"),
                                                               fluidRow(
                                                                 column(3,
                                                                        textInput(inputId = "y_range_min_fluorescence_group_plot",
                                                                                  label = NULL,
                                                                                  value = "min"
                                                                        )
                                                                 ),

                                                                 column(3,
                                                                        textInput(inputId = "y_range_max_fluorescence_group_plot",
                                                                                  label = NULL,
                                                                                  value = "max"
                                                                        )
                                                                 )
                                                               ),

                                                               strong("y-Range (derivative)"),
                                                               fluidRow(
                                                                 column(3,
                                                                        textInput(inputId = "y_range_min_derivative_fluorescence_group_plot",
                                                                                  label = NULL,
                                                                                  value = "min"
                                                                        )
                                                                 ),

                                                                 column(3,
                                                                        textInput(inputId = "y_range_max_derivative_fluorescence_group_plot",
                                                                                  label = NULL,
                                                                                  value = "max"
                                                                        )
                                                                 )
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

                                                             ), # Side panel growth group plots

                                                             mainPanel(
                                                               plotOutput("fluorescence_group_plot")
                                                             )


                                                    ),

                                                    tabPanel(title = "Dose-response analysis",
                                                             sidebarPanel(
                                                               wellPanel(
                                                                 style='background-color:#F0EBE4; padding: 1; border-color: #ADADAD; padding: 1; padding-bottom: 0',
                                                                 checkboxInput(inputId = 'combine_conditions_into_a_single_plot_dose_response_fluorescence_plot',
                                                                               label = 'Combine conditions into a single plot',
                                                                               value = TRUE)
                                                               ),

                                                               h3('Customize plot appearance'),
                                                               sliderInput(inputId = 'shape_type_dose_response_fluorescence_plot',
                                                                           label = 'Shape type',
                                                                           min = 1,
                                                                           max = 25,
                                                                           value = 15),

                                                               sliderInput(inputId = 'shape_size_dose_response_fluorescence_plot',
                                                                           label = 'Shape size',
                                                                           min = 1,
                                                                           max = 10,
                                                                           value = 2),

                                                               sliderInput(inputId = 'base_size_dose_response_fluorescence_plot',
                                                                           label = 'Base size',
                                                                           min = 1,
                                                                           max = 25,
                                                                           value = 15),

                                                               sliderInput(inputId = 'line_width_dose_response_fluorescence_plot',
                                                                           label = 'Line width',
                                                                           min = 1,
                                                                           max = 25,
                                                                           value = 15),

                                                               checkboxInput(inputId = 'show_ec50_indicator_lines_dose_response_fluorescence_plot',
                                                                             label = 'Show EC50 indicator lines',
                                                                             value = TRUE)
                                                             ),
                                                             mainPanel(
                                                               plotOutput("fluorescence_dose_response_plot")
                                                             )
                                                    ),

                                                    tabPanel(title = "Parameter plots",
                                                             sidebarPanel(
                                                               selectInput(inputId = "parameter_fluorescence_parameter_fluorescence_plot",
                                                                           label = "Parameter",
                                                                           choices = ""
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
                                                                             value = FALSE),

                                                               # Conditional Panel
                                                               conditionalPanel(condition = "input.normalize_to_reference_fluorescence_parameter_plot",
                                                                                # reactive selection
                                                                                selectInput(inputId = 'reference_condition_fluorescence_parameter_plot',
                                                                                            label = 'Reference condition',
                                                                                            choices = ""
                                                                                ),

                                                                                # reactive selection
                                                                                selectInput(inputId = 'reference_concentration_fluorescence_parameter_plot',
                                                                                            label = 'Reference concentration',
                                                                                            choices = ""
                                                                                ),
                                                               ),


                                                               sliderInput(inputId = "shape.size_fluorescence_parameter_plot",
                                                                           label = "Shape size",
                                                                           min = 1,
                                                                           max = 10,
                                                                           value = 2.5),

                                                               sliderInput(inputId = "basesize_fluorescence_parameter_plot",
                                                                           label = "Base size",
                                                                           min = 1,
                                                                           max = 25,
                                                                           value = 12)


                                                             ),

                                                             mainPanel(
                                                               plotOutput("fluorescence_parameter_plot")
                                                             )
                                                             # TEST CONSOL OUTPUT
                                                             #verbatimTextOutput('test')
                                                    )
                                        )
                               ),

                               tabPanel(title = "Growth & Flourescence Plots", value = "tabPabel_Visualize_GrowthandFluorescence",
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

                    tabPanel("Report",
                             sidebarPanel(
                               shinyDirButton("dir",
                                              "Choose destination for saving",
                                              "Choose destination for saving"),

                               verbatimTextOutput("dir", placeholder = TRUE),

                               textInput(inputId = 'report_file_name',
                                         label = 'Choose file name',
                                         value = 'GrowthReport'),

                               selectInput(inputId = 'report_file_type',
                                           label = 'Choose file type',
                                           choices = c('pdf', 'html'))
                             )
                    ),

                    tabPanel("About Us",
                             mainPanel(
                               h1("Authors"),
                               'Nicolas Wirth, Jonathan Funk',
                               h1("Publications"),
                               'featuring publications which use this tool'
                             )
                    )
                  ) #  navbarPage
                ), # tagList
                # show plots
                # TODO: Which plots, which options, ...
                verbatimTextOutput("debug")
)

server <- function(input, output, session){
  output$debug <- renderPrint({
    results$growth$gcFit$gcFittedModels[[selected_vals_validate_growth$sample_validate_growth_model]]$data.in
    input$min.density.model.rerun
  })
  # # Disable navbar menus before running computations
  # disable(selector = "#navbar li a[data-value=Report]")
  # disable(selector = "#navbar li a[data-value=Visualize]")
  # disable(selector = "#navbar li a[data-value=navbarMenu_Results]")


  results <- reactiveValues()

  # Test if growth_file was loaded
  output$growthfileUploaded <- reactive({
    if(is.null(input$growth_file)) return(FALSE)
    else return(TRUE)
  })
  outputOptions(output, 'growthfileUploaded', suspendWhenHidden=FALSE)

  output$growth_data <- DT::renderDT({
    inFile <- input$growth_file

    if(is.null(inFile))
      return(NULL)

    filename <- inFile$datapath
    dec <- input$decimal_separator
    csvsep <- input$separator
    if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "csv") {
      dat <-
        utils::read.csv(
          filename,
          dec = dec,
          sep = csvsep,
          header = T,
          stringsAsFactors = F,
          fill = T,
          na.strings = "",
          quote = "",
          comment.char = "",
          check.names = F
        )
    } else if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "xls" |
               stringr::str_replace(filename, ".{1,}\\.", "") == "xlsx") {
      dat <- data.frame(suppressMessages(readxl::read_excel(filename, col_names = T, sheet = input$custom_growth_sheets, progress = T)))
    } else if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "tsv") {
      dat <-
        utils::read.csv(
          filename,
          dec = dec,
          sep = "\t",
          header = T,
          stringsAsFactors = F,
          fill = T,
          na.strings = "",
          quote = "",
          comment.char = "",
          check.names = F
        )
    } else if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "txt") {
      dat <-
        utils::read.table(
          filename,
          dec = dec,
          sep = "\t",
          header = T,
          stringsAsFactors = F,
          fill = T,
          na.strings = "",
          quote = "",
          comment.char = "",
          check.names = F
        )
    }
    dat[-(1:3),] <- apply(dat[-(1:3),], 2, as.numeric) %>% apply(., 2, round, digits = 2)
    dat
  })

  output$data_growth_format <- reactive({
    if(is.null(input$growth_file)) return(NULL)

    filename <- input$growth_file$datapath

    format <- stringr::str_replace_all(filename, ".{1,}\\.", "")
    format
  })

  outputOptions(output, 'data_growth_format', suspendWhenHidden=FALSE)

  growth_excel_sheets <- reactive({
    filename <- input$growth_file$datapath
    if(is.null(input$growth_file)) return("")
    format <- stringr::str_replace_all(filename, ".{1,}\\.", "")
    if(format != "xlsx" && format != "xls") return("")
    sheets <- readxl::excel_sheets(input$growth_file$datapath)
    sheets
  })

  observeEvent(input$run_growth,{

    inFile <- input$growth_file

    if(is.null(inFile))
      return(NULL)

    # withCallingHandlers({
    #
    #   shinyjs::html(id = "text", html = '<br>', add = TRUE)
    # # Activate menus
    # enable(selector = "#navbar li:nth-child(3)")
    # enable(selector = "#navbar li:nth-child(4)")
    # enable(selector = "#navbar li:nth-child(5)")
    #
    # # Inactivate Fluorescence menus
    # disable(selector = "#navbar li:nth-child(3) li:nth-child(2)")
    # disable(selector = "#navbar li:nth-child(4) li:nth-child(2)")
    # disable(selector = "#navbar li:nth-child(4) li:nth-child(3)")



    # Read data
    grodata <- read_data(inFile$datapath, sheet.density = input$custom_growth_sheets, csvsep = input$separator, dec = input$decimal_separator)

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
    # combine selected models into vector
    models <- c()
    if(input$logistic_growth == TRUE) models <- c(models, "logistic")
    if(input$richards_growth == TRUE) models <- c(models, "richards")
    if(input$gompertz_growth == TRUE) models <- c(models, "gompertz")
    if(input$extended_gompertz_growth == TRUE) models <- c(models, "gompertz.exp")
    if(input$huang_growth == TRUE) models <- c(models, "huang")

    # Run growth workflow
    shiny::withProgress(

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
                                        interactive = F, ### TODO (popups)
                                        nboot.gc = input$number_of_bootstrappings,
                                        smooth.gc = input$smoothing_factor_nonparametric_growth,
                                        model.type = models,
                                        growth.thresh = input$growth_threshold_growth,
                                        dr.parameter = input$response_parameter_growth, ### TODO if else bla
                                        smooth.dr = input$smooth.dr,
                                        log.x.dr = input$log_transform_concentration_growth,
                                        log.y.dr = input$log_transform_response_growth,
                                        nboot.dr = input$number_of_bootstrappings_dr_growth,
                                        report = NULL,
                                        shiny = TRUE

      )
    )
    ## ENABLE DISABLED PLOTS AFTER RUNNING COMPUTATION
    # enable(selector = "#navbar li a[data-value=Report]")
    # enable(selector = "#navbar li a[data-value=Visualize]")
    # enable(selector = "#navbar li a[data-value=navbarMenu_Results]")
    # hide(selector = "#navbar li a[data-value=tabPanel_Results_Fluorescence]")
    # hide(selector = "#navbar li a[data-value=tabPabel_Visualize_Fluorescence]")
    # hide(selector = "#navbar li a[data-value=tabPabel_Visualize_GrowthandFluorescence]")


    # )},
    # message = function(m) {
    #   shinyjs::html(id = "text", html = m$message, add = TRUE)
    # },
    # warning = function(m) {
    #   shinyjs::html(id = "text", html = m$message, add = TRUE)
    # }
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

  # Results Tables
  observe({
    if(!is.null(results$growth)){
      if(!("s" %in% results$growth$control$fit.opt || "a" %in% results$growth$control$fit.opt)){
        hideTab(inputId = "tabsetPanel_Results", target = "tabPanel_Results_Growth_Spline")

      }
      if(!("l" %in% results$growth$control$fit.opt || "a" %in% results$growth$control$fit.opt)){
        hideTab(inputId = "tabsetPanel_Results", target = "tabPanel_Results_Growth_Linear")
      }
      if(!("m" %in% results$growth$control$fit.opt || "a" %in% results$growth$control$fit.opt)){
        hideTab(inputId = "tabsetPanel_Results", target = "tabPanel_Results_Growth_Model")

      }
    }
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
  # Validate
  ## Growth

  #### Hide [Restore Fit] buttons when starting the app
  hide("restore_growth_linear"); hide("restore_growth_spline"); hide("restore_growth_model")
  #### Hide [Restore Fit] buttons whenever a sample is changed
  observeEvent(input$sample_validate_growth_linear, {
    hide("restore_growth_linear")
  })
  observeEvent(input$sample_validate_growth_spline, {
    hide("restore_growth_spline")
  })
  observeEvent(input$sample_validate_growth_model, {
    hide("restore_growth_model")
  })


  #####------- Initialize the Memory to store settings ----------
  selected_vals_validate_growth <- reactiveValues(sample_validate_growth_linear = 1,
                                                  sample_validate_growth_spline = 1,
                                                  sample_validate_growth_model = 1)

  #####------ Whenever any of the inputs are changed, it only modifies the memory----
  observe({
    req(input$sample_validate_growth_linear)
    selected_vals_validate_growth$sample_validate_growth_linear <- input$sample_validate_growth_linear
  })
  observe({
    req(input$sample_validate_growth_spline)
    selected_vals_validate_growth$sample_validate_growth_spline <- input$sample_validate_growth_spline
  })
  observe({
    req(input$sample_validate_growth_model)
    selected_vals_validate_growth$sample_validate_growth_model <- input$sample_validate_growth_model
  })

  #### Linear Fits


  selected_inputs_validate_growth_linear_sample <- reactive({
    results <- results$growth
    if(is.null(results)) return("")
    if("l" %in% results$control$fit.opt || "a" %in% results$control$fit.opt){
      select_samples <- names(results$gcFit$gcFittedLinear)
    } else {
      return("")
    }
    select_samples
  })

  observe({
    updateSelectInput(session,
                      inputId = "sample_validate_growth_linear",
                      choices = selected_inputs_validate_growth_linear_sample(),
                      selected = selected_vals_validate_growth$sample_validate_growth_linear
    )})

  logy_validate_growth_plot_linear <- reactive({
    if(input$logy_validate_growth_plot_linear) return("y")
    else return("")
  })


  output$validate_growth_plot_linear <- renderPlot({
    results <- results$growth

    plot.gcFitLinear(results$gcFit$gcFittedLinear[[selected_vals_validate_growth$sample_validate_growth_linear]],
                     log = logy_validate_growth_plot_linear()
                     # ADD FURTHER INPUT (see Notion)
    )
    if(input$diagnostics_validate_growth_plot_linear){
      plot.gcFitLinear(results$gcFit$gcFittedLinear[[selected_vals_validate_growth$sample_validate_growth_linear]],
                       which = "fit_diagnostics",
                       log = logy_validate_growth_plot_linear()
                       # ADD FURTHER INPUT (see Notion)
      )
    }
  })

  lin.rerun.param <- reactiveValues()

  observeEvent(input$rerun_growth_linear, {
    # display a modal dialog with a header, textinput and action buttons
    showModal(
      modalDialog(
        tags$h2('Please enter adjusted parameters'),
        textInput('t0.lin.rerun', 'Minimum time (t0)', placeholder = paste0("previously: ", results$growth$gcFit$gcFittedLinear[[selected_vals_validate_growth$sample_validate_growth_linear]]$control$t0)),
        textInput('min.density.lin.rerun', 'Minimum density', placeholder = paste0("previously: ", results$growth$gcFit$gcFittedLinear[[selected_vals_validate_growth$sample_validate_growth_linear]]$control$min.density)),
        textAreaInput('quota.rerun', 'Quota', placeholder = HTML(paste0("previously: ", results$growth$gcFit$gcFittedLinear[[selected_vals_validate_growth$sample_validate_growth_linear]]$quota,
                                                                        "\n",
                                                                        "include regression windows with slope = ", expression(µ[max]), " * quota into the final linear fit."))),
        textInput('lin.h.rerun', 'Sliding window size (h)', placeholder = paste0("previously: ",
                                                                                 ifelse(!is.null(results$growth$gcFit$gcFittedLinear[[selected_vals_validate_growth$sample_validate_growth_linear]]$control$lin.h),
                                                                                                 results$growth$gcFit$gcFittedLinear[[selected_vals_validate_growth$sample_validate_growth_linear]]$control$lin.h,
                                                                                                 "NULL"))),
        textInput('lin.R2.rerun', 'R2 threshold', placeholder = paste0("previously: ", results$growth$gcFit$gcFittedLinear[[selected_vals_validate_growth$sample_validate_growth_linear]]$control$lin.R2)),
        textInput('lin.RSD.rerun', 'RSD threshold for slope', placeholder = paste0("previously: ", results$growth$gcFit$gcFittedLinear[[selected_vals_validate_growth$sample_validate_growth_linear]]$control$lin.RSD)),
        footer=tagList(
          actionButton('submit.rerun.linear', 'Submit'),
          modalButton('cancel')
        )
      )
    )
  })

  # Re-run selected linear fit with user-defined parameters upon click on 'submit'
  observeEvent(input$submit.rerun.linear, {
    if(!is.null(results$growth$gcFit)){
      # store previous fit in memory
      selected_vals_validate_growth$restore_growth_linear <- results$growth$gcFit$gcFittedLinear[[selected_vals_validate_growth$sample_validate_growth_linear]]

      # Re-run fit and store in results object
      actwell <- results$growth$gcFit$gcFittedLinear[[selected_vals_validate_growth$sample_validate_growth_linear]]$raw.data
      acttime <- results$growth$gcFit$gcFittedLinear[[selected_vals_validate_growth$sample_validate_growth_linear]]$raw.time
      control <- results$growth$gcFit$gcFittedLinear[[selected_vals_validate_growth$sample_validate_growth_linear]]$control
      control_new <- control
      gcID <- results$growth$gcFit$gcFittedLinear[[selected_vals_validate_growth$sample_validate_growth_linear]]$gcID

      lin.h.new <- dplyr::if_else(!is.na(as.numeric(input$lin.h.rerun)), as.numeric(input$lin.h.rerun), control$lin.h)
      if(!is.na(lin.h.new)) control_new$lin.h <- lin.h.new
      control_new$lin.R2 <- dplyr::if_else(!is.na(as.numeric(input$lin.R2.rerun)), as.numeric(input$lin.R2.rerun), control$lin.R2)
      control_new$lin.RSD <- dplyr::if_else(!is.na(as.numeric(input$lin.RSD.rerun)), as.numeric(input$lin.RSD.rerun), control$lin.RSD)
      control_new$t0 <- ifelse(!is.na(as.numeric(input$t0.lin.rerun)), as.numeric(input$t0.lin.rerun), control$t0)
      min.density.lin.new <- ifelse(!is.na(as.numeric(input$min.density.lin.rerun)), as.numeric(input$min.density.lin.rerun), control$min.density)
      if(is.numeric(min.density.lin.new)){
        if(!is.na(min.density.lin.new) && all(as.vector(actwell) < min.density.lin.new)){
          message(paste0("Start density values need to be greater than 'min.density'.\nThe minimum start value in your dataset is: ",
                         min(as.vector(actwell)),". 'min.density' was not adjusted."), call. = FALSE)
        } else if(!is.na(min.density.lin.new)){
          control_new$min.density <- min.density.lin.new
        }
      }
      quota_new <- ifelse(!is.na(as.numeric(input$quota.rerun)), as.numeric(input$quota.rerun), 0.95)


      try(
        results$growth$gcFit$gcFittedLinear[[selected_vals_validate_growth$sample_validate_growth_linear]] <-
          growth.gcFitLinear(acttime, actwell,
                             gcID = gcID,
                             control = control_new,
                             quota = quota_new)
      )
      # Show [Restore fit] button
      show("restore_growth_linear")
    }

    removeModal()
  })

  # Restore previous linear fit upon click on [Restore Fit]
  observeEvent(input$restore_growth_linear, {
    # store previous fit from memory
    results$growth$gcFit$gcFittedLinear[[selected_vals_validate_growth$sample_validate_growth_linear]] <- selected_vals_validate_growth$restore_growth_linear
    hide("restore_growth_linear")
  })

  #### Spline Fits
  selected_inputs_validate_growth_spline_sample <- reactive({
    results <- results$growth
    if(is.null(results)) return("")
    if("s" %in% results$control$fit.opt || "a" %in% results$control$fit.opt){
      select_samples <- names(results$gcFit$gcFittedSpline)
    } else {
      return("")
    }
    select_samples
  })

  observe({
    updateSelectInput(session,
                      inputId = "sample_validate_growth_spline",
                      choices = selected_inputs_validate_growth_spline_sample(),
                      selected = selected_vals_validate_growth$sample_validate_growth_spline
    )})


  output$validate_growth_plot_spline <- renderPlot({
    results <- results$growth

    plot.gcFitSpline(results$gcFit$gcFittedSpline[[input$sample_validate_growth_spline]],
                     log.y = input$logy_validate_growth_plot_spline, colData = 1
    )
  })

  spline.rerun.param <- reactiveValues()

  observeEvent(input$rerun_growth_spline, {
    # display a modal dialog with a header, textinput and action buttons
    showModal(
      modalDialog(
        tags$h2('Please enter adjusted parameters'),
        textInput('t0.spline.rerun', 'Minimum time (t0)', placeholder = paste0("previously: ", results$growth$gcFit$gcFittedSpline[[input$sample_validate_growth_spline]]$control$t0)),
        textInput('min.density.spline.rerun', 'Minimum density', placeholder = paste0("previously: ", results$growth$gcFit$gcFittedSpline[[input$sample_validate_growth_spline]]$control$min.density)),
        textInput('smooth.gc.rerun', 'Smoothing factor', placeholder = paste0("previously: ", results$growth$gcFit$gcFittedSpline[[input$sample_validate_growth_spline]]$control$smooth.gc)),
        footer=tagList(
          actionButton('submit.rerun.spline', 'Submit'),
          modalButton('cancel')
        )
      )
    )
  })

  # Re-run selected spline fit with user-defined parameters upon click on 'submit'
  observeEvent(input$submit.rerun.spline, {
    if(!is.null(results$growth$gcFit)){
    # store previous fit in memory
    selected_vals_validate_growth$restore_growth_spline <- results$growth$gcFit$gcFittedSplines[[selected_vals_validate_growth$sample_validate_growth_spline]]

    # Re-run fit and store in results object
    actwell <- results$growth$gcFit$gcFittedSplines[[selected_vals_validate_growth$sample_validate_growth_spline]]$data.in
    acttime <- results$growth$gcFit$gcFittedSplines[[selected_vals_validate_growth$sample_validate_growth_spline]]$time.in
    control <- results$growth$gcFit$gcFittedSplines[[selected_vals_validate_growth$sample_validate_growth_spline]]$control
    control_new <- control
    gcID <- results$growth$gcFit$gcFittedSplines[[selected_vals_validate_growth$sample_validate_growth_spline]]$gcID

    control_new$smooth.gc <- dplyr::if_else(!is.na(as.numeric(input$smooth.gc.rerun)), as.numeric(input$smooth.gc.rerun), control$lin.R2)
    control_new$t0 <- ifelse(!is.na(as.numeric(input$t0.spline.rerun)), as.numeric(input$t0.spline.rerun), control$t0)
    min.density.spline.new <- ifelse(!is.na(as.numeric(input$min.density.spline.rerun)), as.numeric(input$min.density.spline.rerun), control$min.density)
    if(is.numeric(min.density.spline.new)){
      if(!is.na(min.density.spline.new) && all(as.vector(actwell) < min.density.spline.new)){
        message(paste0("Start density values need to be greater than 'min.density'.\nThe minimum start value in your dataset is: ",
                       min(as.vector(actwell)),". 'min.density' was not adjusted."), call. = FALSE)
      } else if(!is.na(min.density.spline.new)){
        control_new$min.density <- min.density.spline.new
      }
    }


    try(
      results$growth$gcFit$gcFittedSplines[[selected_vals_validate_growth$sample_validate_growth_spline]] <-
        growth.gcFitSpline(acttime, actwell,
                           gcID = gcID,
                           control = control_new)
    )
    # Show [Restore fit] button
    show("restore_growth_spline")
    }

    removeModal()

  })

  # Restore previous spline fit upon click on [Restore Fit]
  observeEvent(input$restore_growth_spline, {
    # store previous fit from memory
    results$growth$gcFit$gcFittedSpline[[selected_vals_validate_growth$sample_validate_growth_spline]] <- selected_vals_validate_growth$restore_growth_spline
    hide("restore_growth_spline")
  })

  #### Model Fits
  selected_inputs_validate_growth_model_sample <- reactive({
    results <- results$growth
    if(is.null(results)) return("")
    if("m" %in% results$control$fit.opt || "a" %in% results$control$fit.opt){
      select_samples <- names(results$gcFit$gcFittedModel)
    } else {
      return("")
    }
    select_samples
  })

  observe({
    updateSelectInput(session,
                      inputId = "sample_validate_growth_model",
                      choices = selected_inputs_validate_growth_model_sample(),
                      selected = selected_vals_validate_growth$sample_validate_growth_model
    )})

  output$validate_growth_plot_model <- renderPlot({
    results <- results$growth

    plot.gcFitModel(results$gcFit$gcFittedModels[[input$sample_validate_growth_model]],
                    colData=1, colModel=2, colLag = 3,
    )
  })

  model.rerun.param <- reactiveValues()

  observeEvent(input$rerun_growth_model, {
    # display a modal dialog with a header, textinput and action buttons
    showModal(
      modalDialog(
        tags$h2('Please enter adjusted parameters'),
        textInput('t0.model.rerun', 'Minimum time (t0)', placeholder = paste0("previously: ", results$growth$gcFit$gcFittedModels[[input$sample_validate_growth_model]]$control$t0)),
        textInput('min.density.model.rerun', 'Minimum density', placeholder = paste0("previously: ", results$growth$gcFit$gcFittedModels[[input$sample_validate_growth_model]]$control$min.density)),
        wellPanel(
          h4(strong('Models:')),
          style='background-color:#F0EBE4; padding: 1; padding-top: 0; padding-bottom: 0',
          checkboxInput(inputId = 'logistic_growth_rerun',
                        label = 'logistic',
                        value = ("logistic" %in% results$growth$gcFit$gcFittedModels[[input$sample_validate_growth_model]]$control$model.type)),

          checkboxInput(inputId = 'richards_growth_rerun',
                        label = 'Richards',
                        value = ("richards" %in% results$growth$gcFit$gcFittedModels[[input$sample_validate_growth_model]]$control$model.type)),

          checkboxInput(inputId = 'gompertz_growth_rerun',
                        label = 'Gompertz',
                        value = ("gompertz" %in% results$growth$gcFit$gcFittedModels[[input$sample_validate_growth_model]]$control$model.type)),

          checkboxInput(inputId = 'extended_gompertz_growth_rerun',
                        label = 'extended Gompertz',
                        value = ("gompertz.exp" %in% results$growth$gcFit$gcFittedModels[[input$sample_validate_growth_model]]$control$model.type)),

          checkboxInput(inputId = 'huang_growth_rerun',
                        label = 'Huang',
                        value = ("huang" %in% results$growth$gcFit$gcFittedModels[[input$sample_validate_growth_model]]$control$model.type))
        ),
        footer=tagList(
          actionButton('submit.rerun.model', 'Submit'),
          modalButton('cancel')
        )
      )
    )
  })

  # Re-run selected model fit with user-defined parameters upon click on 'submit'
  observeEvent(input$submit.rerun.model, {
    if(!is.null(results$growth$gcFit)){
      # store previous fit in memory
      selected_vals_validate_growth$restore_growth_model <- results$growth$gcFit$gcFittedModels[[selected_vals_validate_growth$sample_validate_growth_model]]

      # Re-run fit and store in results object
      actwell <- results$growth$gcFit$gcFittedModels[[selected_vals_validate_growth$sample_validate_growth_model]]$data.in
      acttime <- results$growth$gcFit$gcFittedModels[[selected_vals_validate_growth$sample_validate_growth_model]]$time.in
      control <- results$growth$gcFit$gcFittedModels[[selected_vals_validate_growth$sample_validate_growth_model]]$control
      control_new <- control
      gcID <- results$growth$gcFit$gcFittedModels[[selected_vals_validate_growth$sample_validate_growth_model]]$gcID

      control_new$t0 <- ifelse(!is.na(as.numeric(input$t0.model.rerun)), as.numeric(input$t0.model.rerun), control$t0)
      min.density.model.new <- ifelse(!is.na(as.numeric(input$min.density.model.rerun)), as.numeric(input$min.density.model.rerun), control$min.density)
      if(is.numeric(min.density.model.new)){
        if(!is.na(min.density.model.new) && all(as.vector(actwell) < min.density.model.new)){
          message(paste0("Start density values need to be greater than 'min.density'.\nThe minimum start value in your dataset is: ",
                         min(as.vector(actwell)),". 'min.density' was not adjusted."), call. = FALSE)
        } else if(!is.na(min.density.model.new)){
          control_new$min.density <- min.density.model.new
        }
      }
      # combine selected models into vector
      models <- c()
      if(input$logistic_growth_rerun == TRUE) models <- c(models, "logistic")
      if(input$richards_growth_rerun == TRUE) models <- c(models, "richards")
      if(input$gompertz_growth_rerun == TRUE) models <- c(models, "gompertz")
      if(input$extended_gompertz_growth_rerun == TRUE) models <- c(models, "gompertz.exp")
      if(input$huang_growth_rerun == TRUE) models <- c(models, "huang")
      control_new$model.type <- models

      try(results$growth$gcFit$gcFittedModels[[selected_vals_validate_growth$sample_validate_growth_model]] <-
        growth.gcFitModel(acttime, actwell,
                           gcID = gcID,
                           control = control_new))
      # Show [Restore fit] button
      show("restore_growth_model")
    }

    removeModal()

  })

  # Restore previous model fit upon click on [Restore Fit]
  observeEvent(input$restore_growth_model, {
    # store previous fit from memory
    results$growth$gcFit$gcFittedModels[[selected_vals_validate_growth$sample_validate_growth_model]] <- selected_vals_validate_growth$restore_growth_model
    hide("restore_growth_model")
  })


  ## Fluorescence
  # Visualize
  ## Growth Plots:
  ### Group Plots
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

  output$dose_response_plot_combined <- renderPlot({
    results <- results$growth$drFit
      plot.drFit(results,
                 combine=TRUE,
                 pch = input$shape_type_dose_response_growth_plot,
                 cex = input$shape_size_dose_response_growth_plot,
                 basesize = input$base_size_dose_response_growth_plot,
                 lwd = input$line_width_dose_response_growth_plot,
                 ec50line = input$show_ec50_indicator_lines_dose_response_growth_plot)
  })

  output$dose_response_plot_individual <- renderPlot({
    results <- results$growth$drFit$drFittedSplines[[input$individual_plots_dose_response_growth_plot]]
    plot.drFitSpline(results,
                     combine=FALSE,
                     pch = input$shape_type_dose_response_growth_plot,
                     cex = input$shape_size_dose_response_growth_plot,
                     basesize = input$base_size_dose_response_growth_plot,
                     lwd = input$line_width_dose_response_growth_plot,
                     ec50line = input$show_ec50_indicator_lines_dose_response_growth_plot)
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

  # conditional selections:
  ## Computations
  # observe({
  #   if(is.null(results$growth)){
  #   shinyjs::disable(selector = "#navbar li a[data-value=navbarMenu_Results]")
  #   shinyjs::disable(selector = "#navbar li a[data-value=Report]")
  #   }
  # })
  # observeEvent(input$run_fluorescence, {
  #   shinyjs::enable(selector = "#navbar li a[data-value=Results]")
  #   shinyjs::enable(selector = "#navbar li a[data-value=Report]")
  # })

  # observe({
  #   if (output$computation_type == "fluorescence") {
  #     shinyjs::enable("tabPanel_Results_Fluorescence")
  #   } else {
  #     shinyjs::disable("tabPanel_Results_Fluorescence")
  #   }
  # })

  ### growth: plot.drFit
  observe({
    if(!is.null(results$growth)){
      if(!input$perform_ec50_growth){
        hideTab(inputId = "tabsetPanel_Visalize_Growth", target = "tabPanel_Visualize_Growth_DoseResponse")
      }
    }
  })
  ### growth: plot.parameter()

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

  select_inputs_individual_plots_dose_response_growth_plot<- reactive({
    if (length(results$growth$drFit)>1) names(results$growth$drFit$drFittedSplines)
    else return("")
  })


  observe({
    updateSelectInput(inputId = "custom_growth_sheets",
                      choices = growth_excel_sheets()
    )})

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

  observe({
    updateSelectInput(inputId = "individual_plots_dose_response_growth_plot",
                      choices = select_inputs_individual_plots_dose_response_growth_plot())
  })


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

  # Save report
  volumes <- getVolumes() # this makes the directory at the base of your computer.

  #shinyDirChoose(input, 'report_folder', roots = volumes())
  shinyDirChoose(
    input,
    'dir',
    roots = c(home = '~'),
    filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
  )

  global <- reactiveValues(datapath = getwd())

  dir <- reactive(input$dir)

  output$dir <- renderText({
    global$datapath
  })

  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$dir
               },
               handlerExpr = {
                 if (!"path" %in% names(dir())) return()
                 home <- normalizePath("~")
                 fname <-paste(input$report_file_name, input$report_file_type, sep = '.')
                 global$datapath <-
                   file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep), fname)
               })

}

shinyApp(ui = ui, server = server)
