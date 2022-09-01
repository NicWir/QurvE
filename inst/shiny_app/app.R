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
# iconset <- icons::icon_set("icons/")

source("../../R/general_misc_utils.R")
source("../../R/growth_computation.R")
source("../../R/growth_plots.R")
source("../../R/growth_summaries.R")
source("../../R/fluorescence_computation.R")
source("../../R/fluorescence_plots.R")
source("../../R/fluorescence_summaries.R")
source("../../R/shiny_app_functions.R")

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
                    #____DATA____####
                    tabPanel('Data',
                             icon = icon("file-lines"),
                             value = "tabPanel",
                             tabsetPanel(type = "tabs", id = "tabs_data",
                                         ##____CUSTOM____####
                                         tabPanel(title = "Custom",
                                                  sidebarPanel(
                                                    style='border-color: #ADADAD',
                                                    selectInput(inputId = "custom_format",
                                                                label = "Select Format",
                                                                choices = c("Data in columns" = "col",
                                                                            "Data in rows" = "row")),
                                                    wellPanel(
                                                      h4(strong("Growth data"), style = "line-height: 0.4;font-size: 150%; margin-bottom: 15px;"),
                                                      style='background-color:#F0EBE4; padding: 0.1; border-color: #ADADAD; padding: 1; padding-bottom: 0',

                                                      fileInput(inputId = 'custom_file_density',
                                                                label = 'Choose growth data file',
                                                                accept = c('.xlsx', '.xls', '.csv', 'txt', 'tsv')
                                                      ),

                                                      conditionalPanel(
                                                        condition = "output.growthfileUploaded && output.custom_density_format == 'xlsx'",
                                                        wellPanel(
                                                          style='background-color:#F0EBE4; padding: 1; border-color: #ADADAD; padding-bottom: 0',
                                                          selectInput(inputId = "custom_growth_sheets",
                                                                      label = "Select Sheet",
                                                                      choices = "Sheet1")
                                                        )
                                                      ), # select sheet: conditional
                                                      conditionalPanel(
                                                        condition = "output.growthfileUploaded && output.custom_density_format == 'csv'",
                                                        wellPanel(
                                                          style='background-color:#F0EBE4; padding: 1; border-color: #ADADAD; padding-bottom: 0',
                                                          selectInput(inputId = "separator_custom_density",
                                                                      label = "Select separator",
                                                                      choices = c("," = ",",
                                                                                  ";" = ";")
                                                          ),

                                                          selectInput(inputId = "decimal_separator_custom_density",
                                                                      label = "Select Decimal separator",
                                                                      choices = c("." = ".",
                                                                                  "," = ",")
                                                          )
                                                        )
                                                      ),
                                                      conditionalPanel(
                                                        condition = "output.growthfileUploaded && (output.custom_density_format == 'tsv' || output.custom_density_format == 'txt')",
                                                        wellPanel(
                                                          style='background-color:#F0EBE4; padding: 1; border-color: #ADADAD; padding-bottom: 0',
                                                          selectInput(inputId = "decimal_separator_custom_density",
                                                                      label = "Select Decimal separator",
                                                                      choices = c("." = ".",
                                                                                  "," = ",")
                                                          )
                                                        )
                                                      )
                                                    ),
                                                    #____Fluorescence 1___________
                                                    wellPanel(
                                                      h4(strong("Fluorescence 1 data"), style = "line-height: 1;font-size: 150%; margin-bottom: 15px;"),
                                                      style='background-color:#F0EBE4; padding: 0.1; border-color: #ADADAD; padding: 1; padding-bottom: 0',

                                                      fileInput(inputId = 'custom_file_fluorescence1',
                                                                label = 'Fluorescence data 1',
                                                                accept = c('.xlsx', '.xls', '.csv')
                                                      ),

                                                      conditionalPanel(
                                                        condition = "output.fluorescence1fileUploaded && output.custom_fluorescence1_format == 'xlsx'",
                                                        wellPanel(
                                                          style='background-color:#F0EBE4; padding: 1; border-color: #ADADAD; padding-bottom: 0',
                                                          selectInput(inputId = "custom_fluorescence1_sheets",
                                                                      label = "Select Sheet",
                                                                      choices = "Sheet1")
                                                        )
                                                      ), # select sheet: conditional
                                                      conditionalPanel(
                                                        condition = "output.fluorescence1fileUploaded && output.custom_fluorescence1_format == 'csv'",
                                                        wellPanel(
                                                          style='background-color:#F0EBE4; padding: 1; border-color: #ADADAD; padding-bottom: 0',
                                                          selectInput(inputId = "separator_custom_fluorescence1",
                                                                      label = "Select separator",
                                                                      choices = c("," = ",",
                                                                                  ";" = ";")
                                                          ),

                                                          selectInput(inputId = "decimal_separator_custom_fluorescence1",
                                                                      label = "Select Decimal separator",
                                                                      choices = c("." = ".",
                                                                                  "," = ",")
                                                          )
                                                        )
                                                      ),
                                                      conditionalPanel(
                                                        condition = "output.fluorescence1fileUploaded && (output.custom_fluorescence1_format == 'tsv' || output.custom_fluorescence1_format == 'txt')",
                                                        wellPanel(
                                                          style='background-color:#F0EBE4; padding: 1; border-color: #ADADAD; padding-bottom: 0',
                                                          selectInput(inputId = "decimal_separator_custom_fluorescence1",
                                                                      label = "Select Decimal separator",
                                                                      choices = c("." = ".",
                                                                                  "," = ",")
                                                          )
                                                        )
                                                      )
                                                    ),
                                                    #_____Fluorescence 2___________
                                                    wellPanel(
                                                      h4(strong("Fluorescence 2 data"), style = "line-height: 1;font-size: 150%; margin-bottom: 15px;"),
                                                      style='background-color:#F0EBE4; padding: 0.1; border-color: #ADADAD; padding: 1; padding-bottom: 0',

                                                      fileInput(inputId = 'custom_file_fluorescence2',
                                                                label = 'Fluorescence data 2',
                                                                accept = c('.xlsx', '.xls', '.csv')
                                                      ),

                                                      conditionalPanel(
                                                        condition = "output.fluorescence2fileUploaded && output.custom_fluorescence2_format == 'xlsx'",
                                                        wellPanel(
                                                          style='background-color:#F0EBE4; padding: 1; border-color: #ADADAD; padding-bottom: 0',
                                                          selectInput(inputId = "custom_fluorescence2_sheets",
                                                                      label = "Select Sheet",
                                                                      choices = "Sheet1")
                                                        )
                                                      ), # select sheet: conditional
                                                      conditionalPanel(
                                                        condition = "output.fluorescence2fileUploaded && output.custom_fluorescence2_format == 'csv'",
                                                        wellPanel(
                                                          style='background-color:#F0EBE4; padding: 1; border-color: #ADADAD; padding-bottom: 0',
                                                          selectInput(inputId = "separator_custom_fluorescence2",
                                                                      label = "Select separator",
                                                                      choices = c("," = ",",
                                                                                  ";" = ";")
                                                          ),

                                                          selectInput(inputId = "decimal_separator_custom_fluorescence2",
                                                                      label = "Select Decimal separator",
                                                                      choices = c("." = ".",
                                                                                  "," = ",")
                                                          )
                                                        )
                                                      ),
                                                      conditionalPanel(
                                                        condition = "output.fluorescence2fileUploaded && (output.custom_fluorescence2_format == 'tsv' || output.custom_fluorescence2_format == 'txt')",
                                                        wellPanel(
                                                          style='background-color:#F0EBE4; padding: 1; border-color: #ADADAD; padding-bottom: 0',
                                                          selectInput(inputId = "decimal_separator_custom_fluorescence2",
                                                                      label = "Select Decimal separator",
                                                                      choices = c("." = ".",
                                                                                  "," = ",")
                                                          )
                                                        )
                                                      )
                                                    ),

                                                    checkboxInput(inputId = 'subtract_blank_custom',
                                                                  label = 'Subtract blank'),

                                                    checkboxInput(inputId = 'calibration',
                                                                  label = 'Calibration (under construction)'),
                                                    conditionalPanel(
                                                      condition = 'output.growthfileUploaded || output.fluorescence1fileUploaded || output.fluorescence1fileUploaded',
                                                      fluidRow(
                                                        column(12,
                                                               div(
                                                                 actionButton(inputId = "read_custom",
                                                                              label = "Read data",
                                                                              icon=icon("file-lines"),
                                                                              style="padding:5px; font-size:120%"),
                                                                 style="float:right")
                                                        )
                                                      )
                                                    )
                                                  ),# sidebar panel
                                         ), # Custom tabPanel

                                         ##____PLATE READER____####

                                         tabPanel(title = "Plate reader",
                                                  sidebarPanel(
                                                    style='border-color: #ADADAD',
                                                    wellPanel(
                                                      div(style = "margin-top: -10px"),
                                                      h3(strong("1. Load data"), style = "line-height: 0.4;font-size: 150%; margin-bottom: 15px;"),
                                                      style='background-color:#F0EBE4; padding: 5px; border-color: #ADADAD;',
                                                      # select file type
                                                      fileInput(inputId = 'parse_file',
                                                                label = 'Choose plate reader export file',
                                                                accept = c('.xlsx', '.xls', '.csv', '.tsv', '.txt')),
                                                      div(style = "margin-top: -10px"),
                                                      conditionalPanel(
                                                        condition = "output.parsefileUploaded && (output.parse_file_format == 'xlsx' | output.parse_file_format == 'xls')",
                                                        selectInput(inputId = "parse_data_sheets",
                                                                    label = "Select sheet with read data",
                                                                    choices = "Sheet1")
                                                      ), # select sheet: conditional
                                                      conditionalPanel(
                                                        condition = "output.parsefileUploaded && output.parse_file_format == 'csv'",
                                                        selectInput(inputId = "separator_parse",
                                                                    label = "Select separator",
                                                                    choices = c("," = ",",
                                                                                ";" = ";")
                                                        ),

                                                        selectInput(inputId = "decimal_separator_parse",
                                                                    label = "Select Decimal separator",
                                                                    choices = c("." = ".",
                                                                                "," = ",")
                                                        )
                                                      ),
                                                      conditionalPanel(
                                                        condition = "output.parsefileUploaded && (output.parse_file_format == 'tsv' || output.parse_file_format == 'txt')",
                                                        selectInput(inputId = "decimal_separator_parse",
                                                                    label = "Select Decimal separator",
                                                                    choices = c("." = ".",
                                                                                "," = ",")
                                                        )
                                                      )
                                                    ),
                                                    div(style = "margin-top: -15px"),
                                                    conditionalPanel(
                                                      condition = "output.parsefileUploaded",
                                                      wellPanel(
                                                        style='background-color:#F0EBE4; padding: 5px; border-color: #ADADAD; padding-bottom: 0',
                                                        div(style = "margin-top: -10px"),
                                                        h3(strong("2. Format"), style = "line-height: 0.4;font-size: 150%; margin-bottom: 15px;"),
                                                        selectInput(inputId = "platereader_software",
                                                                    label = "Platereader software",
                                                                    choices = c("Biotek - Gen5/Gen6" = "Gen5"
                                                                    )
                                                        )
                                                      )
                                                    ),
                                                    div(style = "margin-top: -15px"),
                                                    conditionalPanel(
                                                      condition = "output.parsefileUploaded",
                                                      wellPanel(
                                                        style='background-color:#F0EBE4; padding: 5px; border-color: #ADADAD; padding-bottom: 0',
                                                        div(style = "margin-top: -10px"),
                                                        h3(strong("3. Assign data type"), style = "line-height: 0.4; font-size: 150%; margin-bottom: 15px;"),
                                                        selectInput(inputId = "parsed_reads_density",
                                                                    label = "Density data",
                                                                    choices = ""
                                                        ),

                                                        selectInput(inputId = "parsed_reads_fluorescence1",
                                                                    label = "Fluorescence data 1",
                                                                    choices = ""
                                                        ),

                                                        selectInput(inputId = "parsed_reads_fluorescence2",
                                                                    label = "Fluorescence data 2",
                                                                    choices = ""
                                                        )
                                                      )
                                                    ),
                                                    div(style = "margin-top: -15px"),
                                                    conditionalPanel(
                                                      condition = "output.parsefileUploaded",
                                                      wellPanel(
                                                        style='background-color:#F0EBE4; padding: 5px; border-color: #ADADAD; padding-bottom: 0',
                                                        div(style = "margin-top: -10px"),
                                                        h3(strong("4. Load mapping"), style = "line-height: 0.4; font-size: 150%; margin-bottom: 15px;"),
                                                        checkboxInput(inputId = 'mapping_included_in_parse',
                                                                      label = 'Included in data file (xlsx/xls)',
                                                                      value = FALSE),
                                                        tags$div(title = "Table with four columns: Well | Description | Replicate | Concentration",
                                                                 fileInput(inputId = 'map_file',
                                                                           label = 'Choose mapping file',
                                                                           accept = c('.xlsx', '.xls', '.csv', '.tsv', '.txt'),
                                                                           placeholder = "map file",
                                                                 )
                                                        ),
                                                        conditionalPanel(
                                                          condition = "(input.mapping_included_in_parse && (output.parse_file_format == 'xlsx' | output.parse_file_format == 'xls')) || (!input.mapping_included_in_parse && output.mapfileUploaded && (output.map_file_format == 'xlsx' | output.map_file_format == 'xls'))",
                                                          selectInput(inputId = "map_data_sheets",
                                                                      label = "Select sheet with mapping information",
                                                                      choices = "Sheet1")
                                                        ),
                                                        conditionalPanel(
                                                          condition = "!input.mapping_included_in_parse && output.mapfileUploaded && output.map_file_format == 'csv'",
                                                          selectInput(inputId = "separator_map",
                                                                      label = "Select separator",
                                                                      choices = c("," = ",",
                                                                                  ";" = ";")
                                                          ),

                                                          selectInput(inputId = "decimal_separator_map",
                                                                      label = "Select Decimal separator",
                                                                      choices = c("." = ".",
                                                                                  "," = ",")
                                                          )
                                                        ),
                                                        conditionalPanel(
                                                          condition = "!input.mapping_included_in_parse && output.mapfileUploaded && (output.map_file_format == 'tsv' || output.map_file_format == 'txt')",
                                                          style='background-color:#F0EBE4; padding: 5px; border-color: #ADADAD; padding-bottom: 0',
                                                          selectInput(inputId = "decimal_separator_map",
                                                                      label = "Select Decimal separator",
                                                                      choices = c("." = ".",
                                                                                  "," = ",")
                                                          )
                                                        )
                                                      )
                                                    ),

                                                    checkboxInput(inputId = 'subtract_blank_plate_reader',
                                                                  label = 'subtract blank',
                                                                  value = TRUE),

                                                    checkboxInput(inputId = 'convert_time_values_plate_reader',
                                                                  label = 'Convert time values',
                                                                  value = TRUE),

                                                    checkboxInput(inputId = 'calibration_plate_reader',
                                                                  label = 'Calibration'),
                                                    fluidRow(
                                                      column(12,
                                                             div(
                                                               actionButton(inputId = "parse_data",
                                                                            label = "Parse data",
                                                                            icon=icon("file-lines"),
                                                                            style="padding:5px; font-size:120%"),
                                                               style="float:right")
                                                      )
                                                    )
                                                  ),# sidebar panel
                                         ), # Plate reader tabPanel
                             ), # tabSet Panel

                             ##____DATA - MAIN PANEL____####

                             mainPanel(
                               conditionalPanel(
                                 condition = "input.tabs_data == 'Custom'",
                                 img(src = 'data_instruction.png',
                                     heigt = '100%',
                                     width = '100%')
                               ),

                               conditionalPanel(condition = 'output.growthfileUploaded || output.fluorescence1fileUploaded || output.fluorescence1fileUploaded',
                                                h1("Your Data"),
                                                tabsetPanel(type = "tabs", id = "tabsetPanel_custom_tables",
                                                            tabPanel(title = "Density", value = "tabPanel_custom_tables_density",
                                                                     withSpinner(
                                                                       DT::dataTableOutput("growth_data")
                                                                     )
                                                            ),
                                                            tabPanel(title = "Fluorescence 1", value = "tabPanel_custom_tables_fluorescence1",
                                                                     withSpinner(
                                                                       DT::dataTableOutput("custom_table_fluorescence1")
                                                                     )
                                                            ),
                                                            tabPanel(title = "Fluorescence 2", value = "tabPanel_custom_tables_fluorescence2",
                                                                     withSpinner(
                                                                       DT::dataTableOutput("custom_table_fluorescence2")
                                                                     )
                                                            ),
                                                            tabPanel(title = "Experimental Design", value = "tabPanel_custom_tables_expdesign",
                                                                     DT::dataTableOutput('custom_data_table_expdesign')
                                                            )

                                                )
                               ),
                               conditionalPanel(condition = 'output.parsefileUploaded',
                                                h1("Parsed Data"),
                                                tabsetPanel(type = "tabs", id = "tabsetPanel_parsed_tables",
                                                            tabPanel(title = "Density", value = "tabPanel_parsed_tables_density",
                                                                     DT::dataTableOutput('parsed_data_table_density')
                                                            ),
                                                            tabPanel(title = "Fluorescence 1", value = "tabPanel_parsed_tables_fluorescence1",
                                                                     DT::dataTableOutput('parsed_data_table_fluorescence1')
                                                            ),
                                                            tabPanel(title = "Fluorescence 2", value = "tabPanel_parsed_tables_fluorescence2",
                                                                     DT::dataTableOutput('parsed_data_table_fluorescence2')
                                                            ),
                                                            tabPanel(title = "Experimental Design", value = "tabPanel_parsed_tables_expdesign",
                                                                     DT::dataTableOutput('parsed_data_table_expdesign')
                                                            )
                                                )
                               )
                             ) # main panel
                    ), # Navbar 1

                    #____COMPUTATION____####

                    navbarMenu('Computation', menuName = "navbarMenu_Computation", icon=icon("gears"),

                               ##____Computation_Growth____####

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

                                                                   checkboxInput(inputId = 'biphasic_growth',
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
                                                                                                choices = ""),

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
                                                                   ) # conditionalPanel(condition = "input.perform_ec50_growth"
                                                                 ), #  wellPanel
                                                                 fluidRow(
                                                                   column(12,
                                                                          div(
                                                                            actionButton(inputId = "run_growth",
                                                                                         label = "Run computation",
                                                                                         icon=icon("gears"),
                                                                                         style="padding:5px; font-size:120%"),
                                                                            style="float:right")
                                                                   )
                                                                 )
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
                                                         style='border-color: #ADADAD; background-color:#F0EBE4; padding: 1; padding-top: 0; padding-bottom: 0',
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
                               ##____Computation_Fluorescence____####
                               tabPanel("Fluorescence", value = "tabPanel_Computation_Fluorescence",
                                        fluidRow(
                                          sidebarLayout(
                                            column(4,
                                                   sidebarPanel( width = 12,
                                                                 style = 'border-color: #ADADAD',
                                                                 wellPanel(
                                                                   style = 'background-color:#F0EBE4; padding: 1; border-color: #ADADAD; padding-top: 0; padding-bottom: 0',
                                                                   h2(strong('Fluorescence fit')),
                                                                   h4('Options'),
                                                                   checkboxInput(
                                                                     inputId = 'linear_regression_fluorescence',
                                                                     label = 'linear regression',
                                                                     value = TRUE
                                                                   ),

                                                                   checkboxInput(
                                                                     inputId = 'nonparametric_fit_fluorescence',
                                                                     label = 'nonparametric fit',
                                                                     value = TRUE
                                                                   ),

                                                                   checkboxInput(inputId = 'biphasic_fluorescence',
                                                                                 label = 'Biphasic'),

                                                                   selectInput(
                                                                     inputId = 'data_type_x_fluorescence', # TODO change choices based on presence of density and time
                                                                     label = 'Data type x',
                                                                     choices = ""
                                                                   ),

                                                                   checkboxInput(inputId = 'normalize_fluorescence', # TODO inactivate if no density values are present
                                                                                 label = 'Normalize fluorescence'
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = 'input.data_type_x_fluorescence.includes("density")',
                                                                     numericInput(
                                                                       inputId = 'growth_threshold_in_percent_fluorescence',
                                                                       label = 'growth threshold (in %)',
                                                                       value = 1.5,
                                                                       min = NA,
                                                                       max = NA,
                                                                     )
                                                                   ),
                                                                   conditionalPanel(
                                                                     condition = 'input.data_type_x_fluorescence.includes("density")',
                                                                     numericInput(
                                                                       inputId = 'minimum_density_fluorescence', # TODO inactivate if no density values are present
                                                                       label = 'minimum_density',
                                                                       value = 0,
                                                                       min = NA,
                                                                       max = NA,
                                                                     )
                                                                   ),
                                                                   conditionalPanel(
                                                                     condition = 'input.data_type_x_fluorescence.includes("time")',
                                                                     numericInput(
                                                                       inputId = 't0_fluorescence', # TODO inactivate if no time values are present
                                                                       label = 't0',
                                                                       value = 0,
                                                                       min = NA,
                                                                       max = NA,
                                                                     )
                                                                   ),
                                                                 ), # wellPanel


                                                                 wellPanel(style='background-color:#F0EBE4; padding: 1; border-color: #ADADAD; padding-top: 0; padding-bottom: 0',

                                                                           h2(strong('Dose-response Analysis')),

                                                                           checkboxInput(inputId = 'perform_ec50_fluorescence',
                                                                                         label = 'perform dose-response analysis',
                                                                                         value = FALSE),


                                                                           conditionalPanel(condition = 'input.perform_ec50_fluorescence',

                                                                                            selectInput(inputId = "dr_method_fluorescence",
                                                                                                        label = "Method",
                                                                                                        choices = c("Biosensor response model" = "model",
                                                                                                                    "Response spline fit" = "spline")
                                                                                            ), # TODO tooltip with reference to Meyer et al., 2019

                                                                                            selectInput(inputId = "response_parameter_fluorescence",
                                                                                                        label = "Response Parameter",
                                                                                                        choices = ""),

                                                                                            checkboxInput(inputId = 'log_transform_concentration_fluorescence',
                                                                                                          label = 'log transform concentration'),

                                                                                            checkboxInput(inputId = 'log_transform_response_fluorescence',
                                                                                                          label = 'log transform response'),

                                                                                            numericInput(
                                                                                              inputId = "smoothing_factor_fluorescence", # TODO hide if dr_method_fluorescence == "Biosensor response model"
                                                                                              label = "smoothing factor",
                                                                                              value = NULL,
                                                                                              min = 0,
                                                                                              max = 1,
                                                                                              step = 1
                                                                                            ),

                                                                                            numericInput(
                                                                                              inputId = 'number_of_bootrappings_fluorescence',
                                                                                              label = 'number of bootrappings',
                                                                                              value = 0,
                                                                                              min = NA,
                                                                                              max = NA,
                                                                                            )
                                                                           ) # conditionalPanel(condition = "input.perform_ec50_fluorescence"
                                                                 ), # wellPanel

                                                                 # [Run Computation] button
                                                                 conditionalPanel(
                                                                   condition = 'output.fluorescence_present',
                                                                   fluidRow(
                                                                     column(12,
                                                                            div(
                                                                              actionButton(inputId = "run_fluorescence",
                                                                                           label = "Run computation",
                                                                                           icon=icon("gears"),
                                                                                           style="padding:5px; font-size:120%"),
                                                                              style="float:right")
                                                                     )
                                                                   )
                                                                 )
                                                   ) # sidebarPanel
                                            ), # column

                                            column(8,
                                                   conditionalPanel(
                                                     condition = "input.linear_regression_fluorescence",
                                                     sidebarPanel(
                                                       width = 4,
                                                       style='border-color: #ADADAD; padding-top: 0',
                                                       h3(strong('Linear fit')),

                                                       numericInput(
                                                         inputId = 'R2_threshold_fluorescence',
                                                         label = 'R2 threshold',
                                                         value = 0.95,
                                                         min = NA,
                                                         max = NA,
                                                       ),

                                                       numericInput(
                                                         inputId = 'RSD_threshold_fluorescence',
                                                         label = 'RSD threshold',
                                                         value = 0.1,
                                                         min = NA,
                                                         max = NA,
                                                       ),

                                                       numericInput(
                                                         inputId = 'dY_threshold_fluorescence',
                                                         label = 'dY threshold',
                                                         value = 0.05,
                                                         min = NA,
                                                         max = NA,
                                                       ),
                                                       checkboxInput(inputId = 'log_transform_data_linear_fluorescence',
                                                                     label = 'Log-transform fluorescence data'),

                                                       checkboxInput(inputId = 'log_transform_x_linear_fluorescence',
                                                                     label = 'Log-transform x data'),

                                                       checkboxInput(inputId = 'custom_sliding_window_size_fluorescence',
                                                                     label = 'custom sliding window size',
                                                                     value = FALSE),

                                                       conditionalPanel(
                                                         condition = "input.custom_sliding_window_size_fluorescence",
                                                         numericInput(
                                                           inputId = 'custom_sliding_window_size_value_gfluorescence',
                                                           label = NULL,
                                                           value = "",
                                                           min = NA,
                                                           max = NA,
                                                         )
                                                       )
                                                     )
                                                   ), # conditionalPanel

                                                   conditionalPanel(
                                                     condition = "input.nonparametric_fit_fluorescence",
                                                     sidebarPanel(
                                                       width = 4,
                                                       style='border-color: #ADADAD; padding-top: 0',
                                                       h3(strong('Nonparametric fit')),

                                                       numericInput(
                                                         inputId = 'smoothing_factor_nonparametric_fluorescence',
                                                         label = 'smoothing factor',
                                                         value = 0.75,
                                                         min = NA,
                                                         max = NA,
                                                       ),

                                                       numericInput(
                                                         inputId = 'number_of_bootstrappings_fluorescence',
                                                         label = 'number of bootstrappings',
                                                         value = 0,
                                                         min = NA,
                                                         max = NA,
                                                       ),
                                                       checkboxInput(inputId = 'log_transform_data_nonparametric_fluorescence',
                                                                     label = 'Log-transform fluorescence data'),

                                                       checkboxInput(inputId = 'log_transform_x_nonparametric_fluorescence',
                                                                     label = 'Log-transform x data')
                                                     )
                                                   )  # conditionalPanel
                                            ) # column
                                          ) # sidebarLayout
                                        ) # fluidRow
                               ), # tabPanel("Fluorescence"
                    ), # navbarMenu('Computation'

                    #____RESULTS____####

                    navbarMenu(title = "Results", menuName = "navbarMenu_Results", icon = icon("magnifying-glass-chart"),
                               tabPanel(title = "Growth", value = "tabPanel_Results_Growth",
                                        tabsetPanel(type = "tabs", id = "tabsetPanel_Results_Growth",
                                                    tabPanel(title = "Linear Fit", value = "tabPanel_Results_Growth_Linear",
                                                             conditionalPanel(condition = "input.biphasic_growth",
                                                                              h5("(Values in parentheses indicate parameters for secondary growth phase)")
                                                             ),
                                                             DT::dataTableOutput('results_table_growth_linear'),
                                                             downloadButton('download_table_growth_linear',"Download table")
                                                    ),
                                                    tabPanel(title = "Nonparametric Fit", value = "tabPanel_Results_Growth_Spline",
                                                             conditionalPanel(condition = "input.biphasic_growth",
                                                                              h5("(Values in parentheses indicate parameters for secondary growth phase)")
                                                             ),
                                                             DT::dataTableOutput('results_table_growth_spline'),
                                                             downloadButton('download_table_growth_spline',"Download table")
                                                    ),
                                                    tabPanel(title = "Parametric Fit", value = "tabPanel_Results_Growth_Model",
                                                             DT::dataTableOutput('results_table_growth_model'),
                                                             downloadButton('downloads_table_growth_model',"Download table")
                                                    )
                                        )
                               ),

                               tabPanel(title = "Fluorescence", value = "tabPanel_Results_Fluorescence",
                                        tabsetPanel(type = "tabs", id = "tabsetPanel_Results_Fluorescence",
                                                    tabPanel(title = "Linear Fit", value = "tabPanel_Results_Fluorescence_Linear",
                                                             conditionalPanel(condition = "input.biphasic_fluorescence",
                                                                              h5("(Values in parentheses indicate parameters for secondary phase)")
                                                             ),
                                                             DT::dataTableOutput('results_table_fluorescence1_linear'),
                                                             downloadButton('download_table_fluorescence1_linear',"Download table")
                                                    ),
                                                    tabPanel(title = "Nonparametric Fit", value = "tabPanel_Results_Fluorescence_Spline",
                                                             conditionalPanel(condition = "input.biphasic_fluorescence",
                                                                              h5("(Values in parentheses indicate parameters for secondary phase)")
                                                             ),
                                                             DT::dataTableOutput('results_table_fluorescence1_spline'),
                                                             downloadButton('download_table_fluorescence1_spline',"Download table")
                                                    )
                                        )
                               )
                    ),
                    #____VALIDATE____####
                    navbarMenu("Validate", icon = icon("user-check"),
                               ##____Validate_Growth____####
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
                                                                                      size = 5
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
                               ##____Validate_Fluorescence____####
                               tabPanel(title = "Fluorescence Fits", value = "tabPanel_Validate_Fluorescence",
                                        h1("Fluorescence Fits"),
                                        tabsetPanel(type = "tabs",
                                                    tabPanel(title = "Linear Fits", value = "tabPanel_Validate_Fluorescence_linearFits",
                                                             sidebarPanel(width = 5,
                                                                          selectInput(inputId = "sample_validate_fluorescence_linear",
                                                                                      label = "Sample:",
                                                                                      width = "fit-content",
                                                                                      choices = "",
                                                                                      multiple = FALSE,
                                                                                      selectize = FALSE,
                                                                                      size = 5
                                                                          ),
                                                                          checkboxInput(inputId = 'logy_validate_fluorescence_plot_linear',
                                                                                        label = 'Log-transform y axis',
                                                                                        value = FALSE),
                                                                          checkboxInput(inputId = 'diagnostics_validate_fluorescence_plot_linear',
                                                                                        label = 'Show diagnostics',
                                                                                        value = FALSE)

                                                             ),
                                                             mainPanel(width = 7,
                                                                       plotOutput("validate_fluorescence_plot_linear", width = "100%", height = "600px"),
                                                                       fluidRow(
                                                                         column(6, align = "center", offset = 3,
                                                                                actionButton(inputId = "rerun_fluorescence_linear",
                                                                                             label = "Re-run with modified parameters",
                                                                                             icon=icon("gears"),
                                                                                             style="padding:5px; font-size:120%"),
                                                                                actionButton(inputId = "restore_fluorescence_linear",
                                                                                             label = "Restore fit",
                                                                                             # icon=icon("gears"),
                                                                                             style="padding:5px; font-size:120%")
                                                                         )
                                                                       )
                                                             )

                                                    ),
                                                    tabPanel(title = "Nonparametric fits", value = "tabPanel_Validate_Fluorescence_splineFits",
                                                             sidebarPanel(width = 5,
                                                                          selectInput(inputId = "sample_validate_fluorescence_spline",
                                                                                      label = "Sample:",
                                                                                      width = "fit-content",
                                                                                      choices = "",
                                                                                      multiple = FALSE,
                                                                                      selectize = FALSE,
                                                                                      size = 5,
                                                                          ),
                                                                          checkboxInput(inputId = 'logy_validate_fluorescence_plot_spline',
                                                                                        label = 'Log-transform y axis',
                                                                                        value = FALSE)

                                                             ),
                                                             mainPanel(width = 7,
                                                                       #conditional if diagnostics
                                                                       withSpinner(
                                                                         plotOutput("validate_fluorescence_plot_spline",
                                                                                    width = "100%", height = "700px")
                                                                       ),
                                                                       fluidRow(
                                                                         column(6, align = "center", offset = 3,
                                                                                actionButton(inputId = "rerun_fluorescence_spline",
                                                                                             label = "Re-run with modified parameters",
                                                                                             icon=icon("gears"),
                                                                                             style="padding:5px; font-size:120%"),
                                                                                actionButton(inputId = "restore_fluorescence_spline",
                                                                                             label = "Restore fit",
                                                                                             # icon=icon("gears"),
                                                                                             style="padding:5px; font-size:120%")
                                                                         )
                                                                       )
                                                             )

                                                    )
                                        )
                               )
                    ),
                    #____Visualize____####
                    navbarMenu("Visualize", icon = icon("chart-line"),
                               ## Growth Plots ####
                               tabPanel(title = "Growth Plots", value = "tabPanel_Visalize_Growth",
                                        h1("Growth Plots"),
                                        tabsetPanel(type = "tabs",

                                                    ### Growth Group Plots ####

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
                                                                           value = 1.1),
                                                               sliderInput(inputId = 'base_size_growth_group_plot',
                                                                           label = 'Base font size',
                                                                           min = 10,
                                                                           max = 35,
                                                                           value = 20,
                                                                           step = 0.5)

                                                             ), # Side panel growth group plots

                                                             mainPanel(
                                                               withSpinner(
                                                                 plotOutput("growth_group_plot",
                                                                            width = "100%", height = "1000px"),

                                                               ),
                                                               downloadButton('downloads_growth_group_plot',"Download Plot")
                                                             )


                                                    ),

                                                    ### Growth DR Plots ####

                                                    tabPanel(title = "Dose-response analysis", value = "tabPanel_Visualize_Growth_DoseResponse",
                                                             sidebarPanel(
                                                               wellPanel(
                                                                 style='background-color:#F0EBE4; padding: 1; border-color: #ADADAD; padding-bottom: 0',
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
                                                                           value = 2,
                                                                           step = 0.5),

                                                               sliderInput(inputId = 'base_size_dose_response_growth_plot',
                                                                           label = 'Base font size',
                                                                           min = 10,
                                                                           max = 35,
                                                                           value = 15,
                                                                           step = 0.5),

                                                               sliderInput(inputId = 'line_width_dose_response_growth_plot',
                                                                           label = 'Line width',
                                                                           min = 0.01,
                                                                           max = 5,
                                                                           value = 1),

                                                               checkboxInput(inputId = 'show_ec50_indicator_lines_dose_response_growth_plot',
                                                                             label = 'Show EC50 indicator lines',
                                                                             value = TRUE),

                                                               h3("Customize plot appearance"),

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
                                                                                plotOutput("dose_response_plot_combined",
                                                                                           width = "100%", height = "800px")
                                                                              )
                                                             ),

                                                             conditionalPanel(condition = "!input.combine_conditions_into_a_single_plot_dose_response_growth_plot",
                                                                              mainPanel(
                                                                                h3('Individual plots'),
                                                                                selectInput(inputId = 'individual_plots_dose_response_growth_plot',
                                                                                            label = 'Select plot',
                                                                                            choices = "",
                                                                                            multiple = FALSE,
                                                                                            selectize = FALSE,
                                                                                            size = 3),
                                                                                plotOutput("dose_response_plot_individual",
                                                                                           width = "100%", height = "800px")
                                                                              )
                                                             ),


                                                    ),

                                                    ### Growth Parameter Plots ####

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
                                                                           value = 2.5,
                                                                           step = 0.5),

                                                               sliderInput(inputId = "basesize_growth_parameter_plot",
                                                                           label = "Base font size",
                                                                           min = 10,
                                                                           max = 35,
                                                                           value = 12,
                                                                           step = 0.5),
                                                               sliderInput(inputId = "label.size_growth_parameter_plot",
                                                                           label = "Label font size",
                                                                           min = 5,
                                                                           max = 35,
                                                                           value = 12,
                                                                           step = 0.5)


                                                             ),

                                                             mainPanel(
                                                               plotOutput("growth_parameter_plot",
                                                                          width = "100%", height = "800px")
                                                             )
                                                             # TEST CONSOL OUTPUT
                                                             #verbatimTextOutput('test')
                                                    )
                                        )
                               ),
                               ## Fluorescence Plots ####
                               tabPanel(title = "Fluorescence Plots",  value = "tabPabel_Visualize_Fluorescence",
                                        h1("Fluorescence Plots"),
                                        tabsetPanel(type = "tabs",

                                                    ### Fluorescence Group Plots ####

                                                    tabPanel(title = "Group plots",
                                                             sidebarPanel(

                                                               selectInput(inputId = "data_type_fluorescence_group_plots",
                                                                           label = "Data type",
                                                                           choices = c("Raw fluorescence 1" = "raw1",
                                                                                       "Raw fluorescence 2" = "raw2",
                                                                                       "Spline fits FL1" = "spline1",
                                                                                       "Spline fits FL2" = "spline2",
                                                                                       "Normalized FL1" = "norm.fl1",
                                                                                       "Normalized FL2" = "norm.fl2",
                                                                                       )
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

                                                    ### Fluorescence DR Plots ####

                                                    tabPanel(title = "Dose-response analysis",
                                                             sidebarPanel(
                                                               wellPanel(
                                                                 style='background-color:#F0EBE4; padding: 1; border-color: #ADADAD; padding-bottom: 0',
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
                                                                           value = 2,
                                                                           step = 0.5),

                                                               sliderInput(inputId = 'base_size_dose_response_fluorescence_plot',
                                                                           label = 'Base font size',
                                                                           min = 10,
                                                                           max = 35,
                                                                           value = 15,
                                                                           step = 0.5),

                                                               sliderInput(inputId = 'line_width_dose_response_fluorescence_plot',
                                                                           label = 'Line width',
                                                                           min = 0.1,
                                                                           max = 5,
                                                                           value = 1),

                                                               checkboxInput(inputId = 'show_ec50_indicator_lines_dose_response_fluorescence_plot',
                                                                             label = 'Show EC50 indicator lines',
                                                                             value = TRUE)
                                                             ),
                                                             mainPanel(
                                                               plotOutput("fluorescence_dose_response_plot")
                                                             )
                                                    ),

                                                    ### Fluorescence Parameter Plots ####

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
                                                                           value = 2.5,
                                                                           step = 0.5),

                                                               sliderInput(inputId = "basesize_fluorescence_parameter_plot",
                                                                           label = "Base font size",
                                                                           min = 10,
                                                                           max = 35,
                                                                           value = 12,
                                                                           step = 0.5)


                                                             ),

                                                             mainPanel(
                                                               plotOutput("fluorescence_parameter_plot")
                                                             )
                                                    ) #  tabPanel(title = "Parameter plots"
                                        ) # tabsetPanel(type = "tabs",
                               ), # tabPanel(title = "Fluorescence Plots"

                               ## Growth & Fluorescence Plots ####

                               tabPanel(title = "Growth & Flourescence Plot", value = "tabPabel_Visualize_GrowthandFluorescence",
                                        h1("Growth & Flourescence Plot"),

                                        sidebarPanel(
                                          # add sibar stuff
                                        ),

                                        mainPanel(
                                          plotOutput("growth_and_fluorescence_group_plot")
                                        )

                               ) # tabPanel(title = "Growth & Flourescence Plot")
                    ), # navbarMenu("Visualize"

                    #____REPORT____####


                    tabPanel("Report", icon=icon("file-contract"),
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
                                           choices = c('PDF' = 'pdf', 'HTML' = 'html'))
                             )
                    ),

                    #____ABOUT US____####


                    tabPanel("About Us",
                             mainPanel(
                               h2("Authors"),
                               'Nicolas Wirth, Jonathan Funk',
                               h2("Bug reports"),
                               uiOutput("bug_report"),
                               h2("Publications"),
                               'featuring publications which use this tool'
                             )
                    )
                  ) #  navbarPage
                ), # tagList
                # show plots
                # TODO: Which plots, which options, ...
                verbatimTextOutput("debug")
)

#____SERVER____####

server <- function(input, output, session){
  output$debug <- renderPrint({
    table_fluorescence1_linear()
  })
  # # Disable navbar menus before running computations
  # disable(selector = "#navbar li a[data-value=Report]")
  # disable(selector = "#navbar li a[data-value=Visualize]")
  # disable(selector = "#navbar li a[data-value=navbarMenu_Results]")


  results <- reactiveValues()

  #____Read data____####
  # Test if fluorescence data is contained in custom/parsed object
  output$fluorescence_present <- reactive({
    if(!is.null(results$custom_data)){
      grodata <- results$custom_data
    } else if(!is.null(results$parsed_data)){
      grodata <- results$parsed_data
    } else return(FALSE)

    if(length(grodata$fluorescence1) > 1 || length(grodata$fluorescence2) > 1){
      return(TRUE)
    } else return(FALSE)
  })
  outputOptions(output, 'fluorescence_present', suspendWhenHidden=FALSE)
  ##___Custom____####
  hideTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_density")
  hideTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_fluorescence1")
  hideTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_fluorescence2")
  hideTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_expdesign")

  ### Test if custom_file_density was loaded
  output$growthfileUploaded <- reactive({
    if(is.null(input$custom_file_density)) return(FALSE)
    else return(TRUE)
  })
  outputOptions(output, 'growthfileUploaded', suspendWhenHidden=FALSE)

  ### Test if custom_file_fluorescence1 was loaded
  output$fluorescence1fileUploaded <- reactive({
    if(is.null(input$custom_file_fluorescence1)) return(FALSE)
    else return(TRUE)
  })
  outputOptions(output, 'fluorescence1fileUploaded', suspendWhenHidden=FALSE)

  ### Test if custom_file_fluorescence1 was loaded
  output$fluorescence2fileUploaded <- reactive({
    if(is.null(input$custom_file_fluorescence2)) return(FALSE)
    else return(TRUE)
  })
  outputOptions(output, 'fluorescence2fileUploaded', suspendWhenHidden=FALSE)

  # Read data upon click on [Read data]
  observeEvent(input$read_custom,{
    showModal(modalDialog("Reading data input...", footer=NULL))
    density.file <- input$custom_file_density
    fl1.file <- input$custom_file_fluorescence1
    fl2.file <- input$custom_file_fluorescence2

    if(is.null(density.file) && is.null(fl1.file) && is.null(fl2.file)) return(NULL)

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


    # browser()

    ## Read data
    results$custom_data <- read_data(data.density = density.file$datapath,
                                     data.fluoro1 = fl1.file$datapath,
                                     data.fluoro2 = fl2.file$datapath,
                                     data.format = input$custom_format,
                                     sheet.density = input$custom_growth_sheets,
                                     sheet.fluoro1 = input$custom_fluorescence1_sheets,
                                     sheet.fluoro2 = input$custom_fluorescence2_sheets,
                                     csvsep = input$separator_custom_density,
                                     dec = input$decimal_separator_custom_density,
                                     csvsep.fl1 = input$separator_custom_density,
                                     dec.fl1 = input$decimal_separator_custom_density,
                                     csvsep.fl2 = input$separator_custom_density,
                                     dec.fl2 = input$decimal_separator_custom_density,
                                     subtract.blank = input$subtract_blank_custom)

    showTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_expdesign")

    if("density" %in% names(results$custom_data) && length(results$custom_data$density)>1){
      showTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_density")
      # show [Run Computation] button in Computation-Growth
      show("run_growth")
    }
    if("fluorescence1" %in% names(results$custom_data) && length(results$custom_data$fluorescence1)>1){
      showTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_fluorescence1")
    }
    if("density" %in% names(results$custom_data) && length(results$custom_data$fluorescence2)>1){
      showTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_fluorescence2")
    }

    # Remove eventually pre-loaded parsed data
    results$parse_data <- NULL
    hide("parsed_reads_density")
    hide("parsed_reads_fluorescence1")
    hide("parsed_reads_fluorescence2")
    hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_density")
    hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_fluorescence1")
    hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_fluorescence2")
    hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_expdesign")

    removeModal()
  })

  ### Render custom density table
  output$growth_data <- DT::renderDT({
    inFile <- input$custom_file_density

    if(is.null(inFile))
      return(NULL)

    filename <- inFile$datapath
    dec <- input$decimal_separator_custom_density
    csvsep <- input$separator_custom_density
    if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "csv") {
      dat <-
        utils::read.csv(
          filename,
          dec = dec,
          sep = csvsep,
          header = F,
          stringsAsFactors = F,
          fill = T,
          na.strings = "",
          quote = "",
          comment.char = "",
          check.names = F
        )
    } else if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "xls" |
               stringr::str_replace(filename, ".{1,}\\.", "") == "xlsx") {
      showModal(modalDialog("Reading data file...", footer=NULL))
      dat <- data.frame(suppressMessages(readxl::read_excel(filename, col_names = F, sheet = input$custom_growth_sheets, progress = T)))
      removeModal()
    } else if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "tsv") {
      dat <-
        utils::read.csv(
          filename,
          dec = dec,
          sep = "\t",
          header = F,
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
          header = F,
          stringsAsFactors = F,
          fill = T,
          na.strings = "",
          quote = "",
          comment.char = "",
          check.names = F
        )
    }
    dat[-(1:3),] <- apply(dat[-(1:3),], 2, as.numeric) %>% apply(., 2, round, digits = 2)

    #### Render experimental design table
    output$custom_data_table_expdesign <- DT::renderDT({

      dat.mat <- t(dat)
      label <- unlist(lapply(1:nrow(dat.mat), function(x) paste(dat.mat[x,1], dat.mat[x,2], dat.mat[x,3], sep = " | ")))
      condition <- dat.mat[, 1]
      replicate <- dat.mat[, 2]
      concentration <- dat.mat[, 3]

      expdesign <- data.frame(label, condition, replicate, concentration, check.names = FALSE)

      expdesign[-1, ]
    })

    colnames(dat)[1] <- "Time"
    dat[1,1] <- ""
    datatable(dat,
              options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
              escape = FALSE, rownames = c("Condition", "Replicate", "Concentration", rep("", nrow(dat)-3)))
  })

  ### Render custom fluorescence 1 table
  output$custom_table_fluorescence1 <- DT::renderDT({
    inFile <- input$custom_file_fluorescence1

    if(is.null(inFile))
      return(NULL)

    filename <- inFile$datapath
    dec <- input$decimal_separator_custom_fluorescence1
    csvsep <- input$separator_custom_fluorescence1
    if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "csv") {
      f1 <-
        utils::read.csv(
          filename,
          dec = dec,
          sep = csvsep,
          header = F,
          stringsAsFactors = F,
          fill = T,
          na.strings = "",
          quote = "",
          comment.char = "",
          check.names = F
        )
    } else if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "xls" |
               stringr::str_replace(filename, ".{1,}\\.", "") == "xlsx") {
      showModal(modalDialog("Reading data file...", footer=NULL))
      f1 <- data.frame(suppressMessages(readxl::read_excel(filename, col_names = F, sheet = input$custom_fluorescence1_sheets, progress = T)))
      removeModal()
    } else if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "tsv") {
      f1 <-
        utils::read.csv(
          filename,
          dec = dec,
          sep = "\t",
          header = F,
          stringsAsFactors = F,
          fill = T,
          na.strings = "",
          quote = "",
          comment.char = "",
          check.names = F
        )
    } else if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "txt") {
      f1 <-
        utils::read.table(
          filename,
          dec = dec,
          sep = "\t",
          header = F,
          stringsAsFactors = F,
          fill = T,
          na.strings = "",
          quote = "",
          comment.char = "",
          check.names = F
        )
    }
    f1[-(1:3),] <- apply(f1[-(1:3),], 2, as.numeric) %>% apply(., 2, round, digits = 2)

    #### Render experimental design table
    if(!exists("output$custom_data_table_expdesign")){
      output$custom_data_table_expdesign <- DT::renderDT({

        f1.mat <- t(f1)
        label <- unlist(lapply(1:nrow(f1.mat), function(x) paste(f1.mat[x,1], f1.mat[x,2], f1.mat[x,3], sep = " | ")))
        condition <- f1.mat[, 1]
        replicate <- f1.mat[, 2]
        concentration <- f1.mat[, 3]

        expdesign <- data.frame(label, condition, replicate, concentration, check.names = FALSE)

        expdesign[-1, ]
      })
    }

    colnames(f1)[1] <- "Time"
    f1[1,1] <- ""
    datatable(f1,
              options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
              escape = FALSE, rownames = c("Condition", "Replicate", "Concentration", rep("", nrow(f1)-3)))
  })

  ### Render custom fluorescence 2 table
  output$custom_table_fluorescence2 <- DT::renderDT({
    inFile <- input$custom_file_fluorescence2

    if(is.null(inFile))
      return(NULL)

    filename <- inFile$datapath
    dec <- input$decimal_separator_custom_fluorescence2
    csvsep <- input$separator_custom_fluorescence2
    if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "csv") {
      f2 <-
        utils::read.csv(
          filename,
          dec = dec,
          sep = csvsep,
          header = F,
          stringsAsFactors = F,
          fill = T,
          na.strings = "",
          quote = "",
          comment.char = "",
          check.names = F
        )
    } else if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "xls" |
               stringr::str_replace(filename, ".{1,}\\.", "") == "xlsx") {
      showModal(modalDialog("Reading data file...", footer=NULL))
      f2 <- data.frame(suppressMessages(readxl::read_excel(filename, col_names = F, sheet = input$custom_fluorescence2_sheets, progress = T)))
      removeModal()
    } else if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "tsv") {
      f2 <-
        utils::read.csv(
          filename,
          dec = dec,
          sep = "\t",
          header = F,
          stringsAsFactors = F,
          fill = T,
          na.strings = "",
          quote = "",
          comment.char = "",
          check.names = F
        )
    } else if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "txt") {
      f2 <-
        utils::read.table(
          filename,
          dec = dec,
          sep = "\t",
          header = F,
          stringsAsFactors = F,
          fill = T,
          na.strings = "",
          quote = "",
          comment.char = "",
          check.names = F
        )
    }
    f2[-(1:3),] <- apply(f2[-(1:3),], 2, as.numeric) %>% apply(., 2, round, digits = 2)

    #### Render experimental design table
    if(!exists("output$custom_data_table_expdesign")){
      output$custom_data_table_expdesign <- DT::renderDT({

        f2.mat <- t(f2)
        label <- unlist(lapply(1:nrow(f2.mat), function(x) paste(f2.mat[x,1], f2.mat[x,2], f2.mat[x,3], sep = " | ")))
        condition <- f2.mat[, 1]
        replicate <- f2.mat[, 2]
        concentration <- f2.mat[, 3]

        expdesign <- data.frame(label, condition, replicate, concentration, check.names = FALSE)

        expdesign[-1, ]
      })
    }

    colnames(f2)[1] <- "Time"
    f2[1,1] <- ""
    datatable(f2,
              options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
              escape = FALSE, rownames = c("Condition", "Replicate", "Concentration", rep("", nrow(f2)-3)))
  })



  output$custom_density_format <- reactive({
    if(is.null(input$custom_file_density)) return(NULL)

    filename <- input$custom_file_density$name

    format <- stringr::str_replace_all(filename, ".{1,}\\.", "")
    format
  })
  outputOptions(output, 'custom_density_format', suspendWhenHidden=FALSE)

  output$custom_fluorescence1_format <- reactive({
    if(is.null(input$custom_file_fluorescence1)) return(NULL)

    filename <- input$custom_file_fluorescence1$name

    format <- stringr::str_replace_all(filename, ".{1,}\\.", "")
    format
  })
  outputOptions(output, 'custom_fluorescence1_format', suspendWhenHidden=FALSE)

  output$custom_fluorescence2_format <- reactive({
    if(is.null(input$custom_file_fluorescence2)) return(NULL)

    filename <- input$custom_file_fluorescence2$name

    format <- stringr::str_replace_all(filename, ".{1,}\\.", "")
    format
  })
  outputOptions(output, 'custom_fluorescence2_format', suspendWhenHidden=FALSE)

  growth_excel_sheets <- reactive({
    filename <- input$custom_file_density$datapath
    if(is.null(input$custom_file_density)) return("")
    format <- stringr::str_replace_all(filename, ".{1,}\\.", "")
    if(format != "xlsx" && format != "xls") return("")
    sheets <- readxl::excel_sheets(input$custom_file_density$datapath)
    sheets
  })

  fluorescence1_excel_sheets <- reactive({
    filename <- input$custom_file_fluorescence1$datapath
    if(is.null(input$custom_file_fluorescence1)) return("")
    format <- stringr::str_replace_all(filename, ".{1,}\\.", "")
    if(format != "xlsx" && format != "xls") return("")
    sheets <- readxl::excel_sheets(input$custom_file_fluorescence1$datapath)
    sheets
  })

  fluorescence2_excel_sheets <- reactive({
    filename <- input$custom_file_fluorescence2$datapath
    if(is.null(input$custom_file_fluorescence2)) return("")
    format <- stringr::str_replace_all(filename, ".{1,}\\.", "")
    if(format != "xlsx" && format != "xls") return("")
    sheets <- readxl::excel_sheets(input$custom_file_fluorescence2$datapath)
    sheets
  })

  observe({
    updateSelectInput(inputId = "custom_growth_sheets",
                      choices = growth_excel_sheets()
    )})

  observe({
    updateSelectInput(inputId = "custom_fluorescence1_sheets",
                      choices = fluorescence1_excel_sheets()
    )})

  observe({
    updateSelectInput(inputId = "custom_fluorescence2_sheets",
                      choices = fluorescence2_excel_sheets()
    )})

  ##__Parse data____####
  ### Hide elements to guide user
  hide("parsed_reads_density")
  hide("parsed_reads_fluorescence1")
  hide("parsed_reads_fluorescence2")
  hide("parse_data")
  hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_density")
  hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_fluorescence1")
  hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_fluorescence2")
  hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_expdesign")

  ### Test if parse_file was loaded
  output$parsefileUploaded <- reactive({
    if(is.null(input$parse_file)) return(FALSE)
    else return(TRUE)
  })
  outputOptions(output, 'parsefileUploaded', suspendWhenHidden=FALSE)

  parse_excel_sheets <- reactive({
    filename <- input$parse_file$datapath
    if(is.null(input$parse_file)) return("")
    format <- stringr::str_replace_all(filename, ".{1,}\\.", "")
    if(format != "xlsx" && format != "xls") return("")
    sheets <- readxl::excel_sheets(input$parse_file$datapath)
    sheets
  })

  map_excel_sheets <- reactive({
    filename <- input$map_file$datapath
    if(is.null(input$map_file)) return("")
    format <- stringr::str_replace_all(filename, ".{1,}\\.", "")
    if(format != "xlsx" && format != "xls") return("")
    sheets <- readxl::excel_sheets(input$map_file$datapath)
    sheets
  })

  ### Test if map_file was loaded
  output$mapfileUploaded <- reactive({
    if(is.null(input$map_file)) return(FALSE)
    else return(TRUE)
  })
  outputOptions(output, 'mapfileUploaded', suspendWhenHidden=FALSE)

  ### get file formats for parse_file and map_file
  output$parse_file_format <- reactive({
    if(is.null(input$parse_file)) return(NULL)

    filename <- input$parse_file$name

    format <- stringr::str_replace_all(filename, ".{1,}\\.", "")
    format
  })
  outputOptions(output, 'parse_file_format', suspendWhenHidden=FALSE)

  output$map_file_format <- reactive({
    if(is.null(input$map_file)) return(NULL)

    filename <- input$map_file$name

    format <- stringr::str_replace_all(filename, ".{1,}\\.", "")
    format
  })
  outputOptions(output, 'map_file_format', suspendWhenHidden=FALSE)

  ### Extract reads from data file
  selected_inputs_parsed_reads <- reactive({
    inFile <- input$parse_file

    if(is.null(inFile))
      return(NULL)

    filename <- inFile$datapath

    showModal(modalDialog("Reading data file...", footer=NULL))
    try(reads <- parse_properties_Gen5Gen6(file=filename,
                                           csvsep = input$separator_custom_density,
                                           dec = input$decimal_separator_custom_density,
                                           sheet = ifelse(input$parse_data_sheets == "Sheet1", 1, input$parse_data_sheets) )

    )
    if(exists("reads") && length(reads) > 1){
      show("parsed_reads_density")
      if(length(reads)>1) show("parsed_reads_fluorescence1")
      if(length(reads)>2) show("parsed_reads_fluorescence2")
      show("parse_data")
      removeModal()
      reads <- c(reads, "Ignore")
    } else {
      showModal(modalDialog("No read data could be extracted from the provided data file. Did you choose the correct software?", easyClose = T, footer=NULL))
    }
  })
  observe({
    updateSelectInput(inputId = "parsed_reads_density",
                      choices = selected_inputs_parsed_reads()
    )
  })
  observe({
    updateSelectInput(inputId = "parsed_reads_fluorescence1",
                      choices = selected_inputs_parsed_reads()
    )
  })
  observe({
    updateSelectInput(inputId = "parsed_reads_fluorescence2",
                      choices = selected_inputs_parsed_reads()
    )
  })
  observeEvent(input$mapping_included_in_parse,{
    if(input$mapping_included_in_parse) hide("map_file")
    if(!input$mapping_included_in_parse) show("map_file")
  }, ignoreInit = TRUE)


  #### Parse data and extract read tabs
  observeEvent(input$parse_data,{

    showModal(modalDialog("Parsing data input...", footer=NULL))

    if(input$mapping_included_in_parse){
      try(
        results$parsed_data <- parse_data_shiny(
          data.file = input$parse_file$datapath,
          map.file = input$parse_file$datapath,
          software = input$platereader_software,
          convert.time = input$convert_time_values_plate_reader,
          data.sheet = input$parse_data_sheets,
          map.sheet = input$map_data_sheets,
          csvsep.data =  input$separator_parse,
          dec.data = input$decimal_separator_parse,
          csvsep.map =  input$separator_map,
          dec.map = input$decimal_separator_map,
          subtract.blank = input$subtract_blank_plate_reader,
          density.nm = input$parsed_reads_density,
          fl1.nm = ifelse(
            input$parsed_reads_fluorescence1 == input$parsed_reads_density,
            NA,
            input$parsed_reads_fluorescence1
          ),
          fl2.nm = ifelse(
            input$parsed_reads_fluorescence2 == input$parsed_reads_density |
              input$parsed_reads_fluorescence2 == input$parsed_reads_fluorescence1,
            NA,
            input$parsed_reads_fluorescence2
          )
        )
      )
    } else {
      if(is.null(input$map_file$datapath)){
        showModal(modalDialog("No mapping file was provided. The samples will be identified based on their well position (A1, A2, A3, etc.). Grouping options will not be available if you run any further analysis with QurvE.", easyClose = T, footer=NULL))
      }
      try(
        results$parsed_data <- parse_data_shiny(
          data.file = input$parse_file$datapath,
          map.file = input$map_file$datapath,
          software = input$platereader_software,
          convert.time = input$convert_time_values_plate_reader,
          data.sheet = input$parse_data_sheets,
          map.sheet = input$map_data_sheets,
          csvsep.data =  input$separator_parse,
          dec.data = input$decimal_separator_parse,
          csvsep.map =  input$separator_map,
          dec.map = input$decimal_separator_map,
          subtract.blank = input$subtract_blank_plate_reader,
          density.nm = input$parsed_reads_density,
          fl1.nm = ifelse(
            input$parsed_reads_fluorescence1 == input$parsed_reads_density,
            NA,
            input$parsed_reads_fluorescence1
          ),
          fl2.nm = ifelse(
            input$parsed_reads_fluorescence2 == input$parsed_reads_density |
              input$parsed_reads_fluorescence2 == input$parsed_reads_fluorescence1,
            NA,
            input$parsed_reads_fluorescence1
          )
        )
      )
    }
    showTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_expdesign")

    if("density" %in% names(results$parsed_data) && length(results$parsed_data$density)>1){
      showTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_density")
      # show [Run Computation] button in Computation-Growth
      show("run_growth")
    }
    if("fluorescence1" %in% names(results$parsed_data) && length(results$parsed_data$fluorescence1)>1){
      showTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_fluorescence1")
    }
    if("density" %in% names(results$parsed_data) && length(results$parsed_data$fluorescence2)>1){
      showTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_fluorescence2")
    }
    # Remove eventually pre-loaded custom data
    results$custom_data <- NULL
    hideTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_density")
    hideTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_fluorescence1")
    hideTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_fluorescence2")
    hideTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_expdesign")

    removeModal()
  })
  #### Generate parsed tables to display in [DATA] tab
  output$parsed_data_table_density <- DT::renderDT({

    if(is.null(results$parsed_data) || length(results$parsed_data$density) < 2) return(NULL)

    table_density <- t(results$parsed_data$density)
    table_density[-(1:3), ] <- apply(apply(table_density[-(1:3), ], 2, as.numeric), 2, round, digits = 3)
    rownames(table_density)[-(1:3)] <- ""
    table_density <- cbind(data.frame("Time" = c("","","", round(as.numeric(results$parsed_data$time[1,]), digits = 2))),
                           table_density)

    table_density <- datatable(table_density,
                               options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
                               escape = FALSE, rownames = c("Condition", "Replicate", "Concentration", rep("", nrow(table_density)-3)))

    table_density
  })
  output$parsed_data_table_fluorescence1 <- DT::renderDT({

    if(is.null(results$parsed_data) || length(results$parsed_data$fluorescence1)<2) return(NULL)

    table_fl1 <- t(results$parsed_data$fluorescence1)
    table_fl1[-(1:3), ] <- apply(apply(table_fl1[-(1:3), ], 2, as.numeric), 2, round, digits = 1)
    rownames(table_fl1)[-(1:3)] <- ""
    table_fl1 <- cbind(data.frame("Time" = c("","","", round(as.numeric(results$parsed_data$time[1,]), digits = 2))),
                       table_fl1)

    table_fl1 <- datatable(table_fl1,
                           options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
                           escape = FALSE, rownames = c("Condition", "Replicate", "Concentration", rep("", nrow(table_fl1)-3)))

    table_fl1
  })
  output$parsed_data_table_fluorescence2 <- DT::renderDT({

    if(is.null(results$parsed_data) || length(results$parsed_data$fluorescence2)<2) return(NULL)

    table_fl2 <- t(results$parsed_data$fluorescence2)
    table_fl2[-(1:3), ] <- apply(apply(table_fl2[-(1:3), ], 2, as.numeric), 2, round, digits = 1)
    rownames(table_fl2)[-(1:3)] <- ""
    table_fl2 <- cbind(data.frame("Time" = c("","","", round(as.numeric(results$parsed_data$time[1,]), digits = 2))),
                       table_fl2)

    table_fl2 <- datatable(table_fl2,
                           options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
                           escape = FALSE, rownames = c("Condition", "Replicate", "Concentration", rep("", nrow(table_fl2)-3)))

    table_fl2
  })
  output$parsed_data_table_expdesign <- DT::renderDT({

    if(is.null(results$parsed_data) || length(results$parsed_data$expdesign)<2) return(NULL)

    expdesign <- results$parsed_data$expdesign

    expdesign <- datatable(expdesign,
                           options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
                           escape = FALSE, rownames = T)

    expdesign
  })

  observe({
    updateSelectInput(inputId = "parse_data_sheets",
                      choices = parse_excel_sheets()
    )})

  observe({
    if(input$mapping_included_in_parse){
      updateSelectInput(inputId = "map_data_sheets",
                        choices = parse_excel_sheets()
      )
    } else {
      updateSelectInput(inputId = "map_data_sheets",
                        choices = map_excel_sheets()
      )
    }
  })

  #### Computation ####
  #####____Growth____#####
  hide("run_growth")
  selected_inputs_response_parameter_growth <- reactive({
    select_options <- c()
    if(input$linear_regression_growth) select_options <- c(select_options, 'mu.linfit', 'lambda.linfit', 'dY.linfit',
                                                           'A.linfit')
    if(input$nonparametric_fit_growth) select_options <- c(select_options, 'mu.spline', 'lambda.spline',
                                                           'A.spline', 'dY.spline', 'integral.spline')
    if(input$parametric_fit_growth) select_options <- c(select_options, 'mu.model', 'lambda.model', 'A.model', 'integral.model')
    select_options
  })

  observe({
    updateSelectInput(session,
                      inputId = "response_parameter_growth",
                      choices = selected_inputs_response_parameter_growth()
    )
  })

  observeEvent(input$run_growth,{
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



    ## Read data
    # grodata <- read_data(inFile$datapath, sheet.density = input$custom_growth_sheets, csvsep = input$separator_custom_density, dec = input$decimal_separator_custom_density)
    # Choose data input
    if(!is.null(results$custom_data)){
      grodata <- results$custom_data
    } else if(!is.null(results$parsed_data)){
      grodata <- results$parsed_data
    } else return(NULL)

    if (is.na(input$smoothing_factor_growth) || input$smoothing_factor_growth == "NULL") {
      smooth.dr = NULL
    } else {
      smooth.dr <- input$smoothing_factor_growth
    }

    fit.opt <- c()
    if(input$linear_regression_growth){
      fit.opt <- c(fit.opt,
                   'l')
    }
    if(input$parametric_fit_growth){
      fit.opt <- c(fit.opt,
                   'm')
      # combine selected models into vector
      models <- c()
      if(input$logistic_growth == TRUE) models <- c(models, "logistic")
      if(input$richards_growth == TRUE) models <- c(models, "richards")
      if(input$gompertz_growth == TRUE) models <- c(models, "gompertz")
      if(input$extended_gompertz_growth == TRUE) models <- c(models, "gompertz.exp")
      if(input$huang_growth == TRUE) models <- c(models, "huang")
    } else {
      models <- c("logistic")
    }
    if(input$nonparametric_fit_growth){
      fit.opt <- c(fit.opt,
                   's')
    }

    showModal(modalDialog("Running computations...", footer=NULL))
    # Run growth workflow
    shiny::withProgress(message = "Computations completed",

                        results$growth <- growth.workflow(grodata = grodata,
                                                          ec50 = input$perform_ec50_growth,
                                                          fit.opt = fit.opt,
                                                          t0 = input$t0_growth,
                                                          min.density = input$minimum_density_growth,
                                                          log.x.gc = input$log_transform_time_growth,
                                                          log.y.model = input$log_transform_data_parametric_growth,
                                                          log.y.spline = input$log_transform_data_nonparametric_growth,
                                                          biphasic = input$biphasic_growth,
                                                          lin.h = input$custom_sliding_window_size_value_growth,
                                                          lin.R2 = input$R2_threshold_growth,
                                                          lin.RSD = input$RSD_threshold_growth,
                                                          lin.dY = input$dY_threshold_growth,
                                                          interactive = F,
                                                          nboot.gc = input$number_of_bootstrappings_growth,
                                                          smooth.gc = input$smoothing_factor_nonparametric_growth,
                                                          model.type = models,
                                                          growth.thresh = input$growth_threshold_growth,
                                                          dr.parameter = input$response_parameter_growth,
                                                          smooth.dr = smooth.dr,
                                                          log.x.dr = input$log_transform_concentration_growth,
                                                          log.y.dr = input$log_transform_response_growth,
                                                          nboot.dr = input$number_of_bootstrappings_dr_growth,
                                                          report = NULL,
                                                          shiny = TRUE

                        )
    )
    ## ENABLE DISABLED PANELS AFTER RUNNING COMPUTATION
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

    removeModal()
  })
  #####____Fluorescence____#####
  # Create vector of x_types based on presence of data types
  selected_inputs_fluorescence_x_types <- reactive({
    if(is.null(results$custom_data) && is.null(results$parsed_data)) return(NULL)
    if(!is.null(results$custom_data)) data <- results$custom_data
    if(!is.null(results$parsed_data)) data <- results$parsed_data

    x_types <- c()
    if(length(data$density) > 1) x_types <- c(x_types, 'density')
    if(length(data$time) > 1) x_types <- c(x_types, 'time')

    x_types
  })

  observe({
    updateSelectInput(session,
                      inputId = "data_type_x_fluorescence",
                      choices = selected_inputs_fluorescence_x_types()
    )
  })

  selected_inputs_response_parameter_fluorescence <- reactive({
    select_options <- c()
    if(input$linear_regression_fluorescence) select_options <- c(select_options, 'max_slope.linfit', 'lambda.linfit', 'dY.linfit',
                                                                 'A.linfit')
    if(input$nonparametric_fit_fluorescence) select_options <- c(select_options, 'max_slope.spline', 'lambda.spline',
                                                                 'A.spline', 'dY.spline', 'integral.spline')
  })

  observe({
    updateSelectInput(session,
                      inputId = "response_parameter_fluorescence",
                      choices = selected_inputs_response_parameter_fluorescence()
    )
  })

  observeEvent(input$run_fluorescence,{
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

    # Choose data input

    if(!is.null(results$custom_data)){
      grodata <- results$custom_data
    } else if(!is.null(results$parsed_data)){
      grodata <- results$parsed_data
    } else return(NULL)

    if (is.na(input$smoothing_factor_fluorescence) || input$smoothing_factor_fluorescence == "NULL") {
      smooth.dr = NULL
    } else {
      smooth.dr <- input$smoothing_factor_fluorescence
    }

    fit.opt <- c()
    if(input$linear_regression_fluorescence){
      fit.opt <- c(fit.opt,
                   'l')
    }
    if(input$nonparametric_fit_fluorescence){
      fit.opt <- c(fit.opt,
                   's')
    }
    # browser()
    # removeModal()
    showModal(modalDialog("Running computations...", footer=NULL))
    # Run growth workflow
    shiny::withProgress(message = "Computations completed",

                        results$fluorescence <- fl.workflow(grodata = grodata,
                                                            ec50 = input$perform_ec50_fluorescence,
                                                            fit.opt = fit.opt,
                                                            x_type = input$data_type_x_fluorescence,
                                                            norm_fl = input$normalize_fluorescence,
                                                            t0 = input$t0_fluorescence,
                                                            min.density = input$minimum_density_fluorescence,
                                                            log.x.lin = input$log_transform_x_linear_fluorescence,
                                                            log.x.spline = input$log_transform_x_nonparametric_fluorescence,
                                                            log.y.lin = input$log_transform_data_linear_fluorescence,
                                                            log.y.spline = input$log_transform_data_nonparametric_fluorescence,
                                                            lin.h = input$custom_sliding_window_size_value_fluorescence,
                                                            lin.R2 = input$R2_threshold_fluorescence,
                                                            lin.RSD = input$RSD_threshold_fluorescence,
                                                            lin.dY = input$dY_threshold_fluorescence,
                                                            biphasic = input$biphasic_fluorescence,
                                                            interactive = FALSE,
                                                            dr.parameter = input$response_parameter_fluorescence,
                                                            dr.method = input$dr_method_fluorescence,
                                                            smooth.dr = smooth.dr,
                                                            log.x.dr = input$log_transform_concentration_fluorescence,
                                                            log.y.dr = input$log_transform_response_fluorescence,
                                                            nboot.dr = input$number_of_bootstrappings_dr_fluorescence,
                                                            nboot.fl = input$number_of_bootstrappings_fluorescence,
                                                            smooth.fl = input$smoothing_factor_nonparametric_fluorescence,
                                                            growth.thresh = input$growth_threshold_in_percent_fluorescence,
                                                            suppress.messages = FALSE,
                                                            neg.nan.act = FALSE,
                                                            clean.bootstrap = TRUE,
                                                            report = NULL,
                                                            out.dir = NULL,
                                                            out.nm = NULL,
                                                            export.fig = FALSE,
                                                            shiny = TRUE
                        )
    )
    ## ENABLE DISABLED PANELS AFTER RUNNING COMPUTATION
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
    removeModal()
  })

  ## Results ####
  ### Growth ####
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

  table_growth_linear <- reactive({
    res.table.gc <- results$growth$gcFit$gcTable
    table_linear <- data.frame("Sample|Replicate|Conc." = paste(res.table.gc$TestId, res.table.gc$AddId, res.table.gc$concentration, sep = "|"),
                               "µ<sub>max</sub>" = ifelse(res.table.gc$mu.linfit==0 | is.na(res.table.gc$mu.linfit), "", ifelse(is.na(res.table.gc$mu2.linfit), round(as.numeric(res.table.gc$mu.linfit), 3), paste0("<strong>", round(as.numeric(res.table.gc$mu.linfit), 3), "</strong>", " (", round(as.numeric(res.table.gc$mu2.linfit), 3), ")"))),
                               "t<sub>D</sub>" = ifelse(res.table.gc$mu.linfit==0 | is.na(res.table.gc$mu.linfit), "",  ifelse(is.na(res.table.gc$mu2.linfit), round(log(2)/as.numeric(res.table.gc$mu.linfit), 2), paste0("<strong>", round(log(2)/as.numeric(res.table.gc$mu.linfit), 2), "</strong>", " (", round(log(2)/as.numeric(res.table.gc$mu2.linfit), 2), ")"))),
                               "λ" = round(as.numeric(res.table.gc$lambda.linfit), 2),
                               "ΔY" = round(as.numeric(res.table.gc$dY.linfit), 3),
                               "y<sub>max</sub>" = round(as.numeric(res.table.gc$A.linfit), 3),
                               "t<sub>start</sub><br>(µ<sub>max</sub>)" = ifelse(is.na(res.table.gc$mu2.linfit), round(as.numeric(res.table.gc$tmu.start.linfit), 2), paste0("<strong>", round(as.numeric(res.table.gc$tmu.start.linfit), 2), "</strong>", " (", round(as.numeric(res.table.gc$tmu2.start.linfit), 2), ")")),
                               "t<sub>end</sub><br>(µ<sub>max</sub>)" = ifelse(is.na(res.table.gc$mu2.linfit), round(as.numeric(res.table.gc$tmu.end.linfit), 2), paste0("<strong>", round(as.numeric(res.table.gc$tmu.end.linfit), 2), "</strong>", " (", round(as.numeric(res.table.gc$tmu2.end.linfit), 2), ")")),
                               "R<sup>2</sup><br>(linear fit)" = ifelse(is.na(res.table.gc$mu2.linfit), round(as.numeric(res.table.gc$r2mu.linfit), 3), paste0("<strong>", round(as.numeric(res.table.gc$r2mu.linfit), 3), "</strong>", " (", round(as.numeric(res.table.gc$r2mu2.linfit), 3), ")")),
                               stringsAsFactors = F, check.names = F)
    table_linear

  })

  output$results_table_growth_linear <- DT::renderDT({
    datatable(table_growth_linear(),
              options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
              escape = FALSE)
  })

  table_growth_spline <- reactive({
    res.table.gc <- results$growth$gcFit$gcTable
    table_spline <- data.frame("Sample|Replicate|Conc." = paste(res.table.gc$TestId, res.table.gc$AddId, res.table.gc$concentration, sep = "|"),
                               "µ<sub>max</sub>" = ifelse(res.table.gc$mu.spline==0 | is.na(res.table.gc$mu.spline), "", ifelse(is.na(res.table.gc$mu2.spline), round(as.numeric(res.table.gc$mu.spline), 3), paste0("<strong>", round(as.numeric(res.table.gc$mu.spline), 3), "</strong>", " (", round(as.numeric(res.table.gc$mu2.spline), 3), ")"))),
                               "t<sub>D</sub>" = ifelse(res.table.gc$mu.spline==0 | is.na(res.table.gc$mu.spline), "",  ifelse(is.na(res.table.gc$mu2.spline), round(log(2)/as.numeric(res.table.gc$mu.spline), 2), paste0("<strong>", round(log(2)/as.numeric(res.table.gc$mu.spline), 2), "</strong>", " (", round(log(2)/as.numeric(res.table.gc$mu2.spline), 2), ")"))),
                               "λ" = round(as.numeric(res.table.gc$lambda.spline), 2),
                               "y<sub>max</sub>" = round(as.numeric(res.table.gc$A.spline), 3),
                               "ΔY" = round(as.numeric(res.table.gc$dY.spline), 3),
                               "t<sub>max</sub>" = ifelse(is.na(res.table.gc$mu2.spline), round(as.numeric(res.table.gc$tmax.spline), 2), paste0("<strong>", round(as.numeric(res.table.gc$tmax.spline), 2), "</strong>", " (", round(as.numeric(res.table.gc$tmax2.spline), 2), ")")),
                               "smooth.<br>fac" = res.table.gc$smooth.spline, check.names = F)
    table_spline
  })

  output$results_table_growth_spline <- DT::renderDT({
    datatable(table_growth_spline(),
              options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
              escape = FALSE)
  })

  table_growth_model <- reactive({
    try({
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
      table_model
    })
  })

  output$results_table_growth_model <- DT::renderDT({
    table_model <- table_growth_model()
    datatable(table_model,
              options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
              escape = FALSE)
  })
  ### Fluorescence ####
  observe({
    if(!is.null(results$fluorescence)){
      if(!("s" %in% results$fluorescence$control$fit.opt || "a" %in% results$fluorescence$control$fit.opt)){
        hideTab(inputId = "tabsetPanel_Results", target = "tabPanel_Results_Fluorescence_Spline")
      }
      if(!("l" %in% results$fluorescence$control$fit.opt || "a" %in% results$fluorescence$control$fit.opt)){
        hideTab(inputId = "tabsetPanel_Results", target = "tabPanel_Results_Fluorescence_Linear")
      }
    }
  })
  table_fluorescence1_linear <- reactive({
    res.table.fl <- results$fluorescence$flFit1$flTable

    table_linear <- data.frame("Sample|Replicate|Conc." = paste(res.table.fl$TestId, res.table.fl$AddId, res.table.fl$concentration, sep = "|"),
                               "slope<sub>max</sub>" = ifelse(res.table.fl$max_slope.linfit==0 | is.na(res.table.fl$max_slope.linfit), "", ifelse(is.na(res.table.fl$max_slope2.linfit), round(as.numeric(res.table.fl$max_slope.linfit), 3), paste0("<strong>", round(as.numeric(res.table.fl$max_slope.linfit), 3), "</strong>", " (", round(as.numeric(res.table.fl$max_slope2.linfit), 3), ")"))),
                               "t<sub>D</sub>" = ifelse(res.table.fl$max_slope.linfit==0 | is.na(res.table.fl$max_slope.linfit), "",  ifelse(is.na(res.table.fl$max_slope2.linfit), round(log(2)/as.numeric(res.table.fl$max_slope.linfit), 2), paste0("<strong>", round(log(2)/as.numeric(res.table.fl$max_slope.linfit), 2), "</strong>", " (", round(log(2)/as.numeric(res.table.fl$max_slope2.linfit), 2), ")"))),
                               "λ" = round(as.numeric(res.table.fl$lambda.linfit), 2),
                               "ΔY" = round(as.numeric(res.table.fl$dY.linfit), 3),
                               "y<sub>max</sub>" = round(as.numeric(res.table.fl$A.linfit), 3),
                               "t<sub>start</sub><br>(µ<sub>max</sub>)" = ifelse((is.na(res.table.fl$max_slope2.linfit)), round(as.numeric(res.table.fl$x.mu.start.linfit), 2), paste0("<strong>", round(as.numeric(res.table.fl$x.mu.start.linfit), 2), "</strong>", " (", round(as.numeric(res.table.fl$x.mu2.start.linfit), 2), ")")),
                               "t<sub>end</sub><br>(µ<sub>max</sub>)" = ifelse((is.na(res.table.fl$max_slope2.linfit)), round(as.numeric(res.table.fl$x.mu.end.linfit), 2), paste0("<strong>", round(as.numeric(res.table.fl$x.mu.end.linfit), 2), "</strong>", " (", round(as.numeric(res.table.fl$x.mu2.end.linfit), 2), ")")),
                               "R<sup>2</sup><br>(linear fit)" = ifelse((is.na(res.table.fl$max_slope2.linfit)), round(as.numeric(res.table.fl$r2mu.linfit), 3), paste0("<strong>", round(as.numeric(res.table.fl$r2mu.linfit), 3), "</strong>", " (", round(as.numeric(res.table.fl$r2mu.linfit), 3), ")")),
                               stringsAsFactors = F, check.names = F)

    table_linear

  })

  output$results_table_fluorescence1_linear <- DT::renderDT({
    datatable(table_fluorescence1_linear(),
              options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
              escape = FALSE)
  })

  table_fluorescence1_spline <- reactive({
    res.table.fl <- results$fluorescence$flFit1$flTable
    table_spline <- data.frame("Sample|Replicate|Conc." = paste(res.table.fl$TestId, res.table.fl$AddId, res.table.fl$concentration, sep = "|"),
                               "slope<sub>max</sub>" = ifelse(res.table.fl$max_slope.spline==0 | is.na(res.table.fl$max_slope.spline), "", ifelse(is.na(res.table.fl$max_slope2.spline), round(as.numeric(res.table.fl$max_slope.spline), 3), paste0("<strong>", round(as.numeric(res.table.fl$max_slope.spline), 3), "</strong>", " (", round(as.numeric(res.table.fl$max_slope2.spline), 3), ")"))),
                               "λ" = round(as.numeric(res.table.fl$lambda.spline), 2),
                               "y<sub>max</sub>" = round(as.numeric(res.table.fl$A.spline), 3),
                               "ΔY" = round(as.numeric(res.table.fl$dY.spline), 3),
                               "x<sub>max</sub>" = ifelse(is.na(res.table.fl$max_slope2.spline), round(as.numeric(res.table.fl$x.max.spline), 2), paste0("<strong>", round(as.numeric(res.table.fl$x.max.spline), 2), "</strong>", " (", round(as.numeric(res.table.fl$x.max2.spline), 2), ")")),
                               "smooth.<br>fac" = res.table.fl$smooth.spline, check.names = F)
    table_spline
  })

  output$results_table_fluorescence1_spline <- DT::renderDT({
    datatable(table_fluorescence1_spline(),
              options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
              escape = FALSE)
  })

  # Validate ####
  ## Growth #####

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


  #------- Initialize the Memory to store settings
  selected_vals_validate_growth <- reactiveValues(sample_validate_growth_linear = 1,
                                                  sample_validate_growth_spline = 1,
                                                  sample_validate_growth_model = 1)

  #------ Whenever any of the inputs are changed, it only modifies the memory
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

  #### Linear Fits ####


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
    if(length(results$gcFit$gcFittedLinear[[input$sample_validate_growth_linear]]) > 1){


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
    }
  })

  lin.rerun.param <- reactiveValues()

  observeEvent(input$rerun_growth_linear.growth, {
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
          actionButton('submit.rerun.linear.growth', 'Submit'),
          modalButton('cancel')
        )
      )
    )
  })

  # Re-run selected linear fit with user-defined parameters upon click on 'submit'
  observeEvent(input$submit.rerun.linear.growth, {
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

  #### Spline Fits ####
  selected_inputs_validate_growth_spline_sample <- reactive({
    results <- results$growth
    if(is.null(results)) return("")
    if("s" %in% results$control$fit.opt || "a" %in% results$control$fit.opt){
      select_samples <- names(results$gcFit$gcFittedSplines)
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
    if(length(results$gcFit$gcFittedSplines[[input$sample_validate_growth_spline]]) > 1){
      showModal(modalDialog("Creating plot...", footer=NULL))
      plot.gcFitSpline(results$gcFit$gcFittedSplines[[input$sample_validate_growth_spline]],
                       log.y = input$logy_validate_growth_plot_spline, colData = 1
      )
      removeModal()
    }
  })

  spline.rerun.param <- reactiveValues()

  observeEvent(input$rerun_growth_spline, {
    # display a modal dialog with a header, textinput and action buttons
    showModal(
      modalDialog(
        tags$h2('Please enter adjusted parameters'),
        textInput('t0.spline.rerun', 'Minimum time (t0)', placeholder = paste0("previously: ", results$growth$gcFit$gcFittedSplines[[input$sample_validate_growth_spline]]$control$t0)),
        textInput('min.density.spline.rerun', 'Minimum density', placeholder = paste0("previously: ", results$growth$gcFit$gcFittedSplines[[input$sample_validate_growth_spline]]$control$min.density)),
        textInput('smooth.gc.rerun', 'Smoothing factor', placeholder = paste0("previously: ", results$growth$gcFit$gcFittedSplines[[input$sample_validate_growth_spline]]$control$smooth.gc)),
        footer=tagList(
          actionButton('submit.rerun.spline.growth', 'Submit'),
          modalButton('cancel')
        )
      )
    )
  })

  # Re-run selected spline fit with user-defined parameters upon click on 'submit'
  observeEvent(input$submit.rerun.spline.growth, {
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
    results$growth$gcFit$gcFittedSplines[[selected_vals_validate_growth$sample_validate_growth_spline]] <- selected_vals_validate_growth$restore_growth_spline
    hide("restore_growth_spline")
  })

  #### Model Fits ####
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
    if(length(results$gcFit$gcFittedModels[[input$sample_validate_growth_model]]) > 1){
      showModal(modalDialog("Creating plot...", footer=NULL))
      plot.gcFitModel(results$gcFit$gcFittedModels[[input$sample_validate_growth_model]],
                      colData=1, colModel=2, colLag = 3,
      )
      removeModal()
    }
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


  ## Fluorescence #####

  #### Hide [Restore Fit] buttons when starting the app
  hide("restore_fluorescence_linear"); hide("restore_fluorescence_spline")
  #### Hide [Restore Fit] buttons whenever a sample is changed
  observeEvent(input$sample_validate_fluorescence_linear, {
    hide("restore_fluorescence_linear")
  })
  observeEvent(input$sample_validate_fluorescence_spline, {
    hide("restore_fluorescence_spline")
  })


  #------- Initialize the Memory to store settings
  selected_vals_validate_fluorescence <- reactiveValues(sample_validate_fluorescence_linear = 1,
                                                        sample_validate_fluorescence_spline = 1
  )

  #------ Whenever any of the inputs are changed, it only modifies the memory
  observe({
    req(input$sample_validate_fluorescence_linear)
    selected_vals_validate_fluorescence$sample_validate_fluorescence_linear <- input$sample_validate_fluorescence_linear
  })
  observe({
    req(input$sample_validate_fluorescence_spline)
    selected_vals_validate_fluorescence$sample_validate_fluorescence_spline <- input$sample_validate_fluorescence_spline
  })

  ### Linear Fits ####

  selected_inputs_validate_fluorescence_linear_sample <- reactive({
    results <- results$fluorescence
    if(is.null(results)) return("")
    if("l" %in% results$control$fit.opt || "a" %in% results$control$fit.opt){
      select_samples <- names(results$flFit1$flFittedLinear)
    } else {
      return("")
    }
    select_samples
  })

  observe({
    updateSelectInput(session,
                      inputId = "sample_validate_fluorescence_linear",
                      choices = selected_inputs_validate_fluorescence_linear_sample(),
                      selected = selected_vals_validate_fluorescence$sample_validate_fluorescence_linear
    )})

  logy_validate_fluorescence_plot_linear <- reactive({
    if(input$logy_validate_fluorescence_plot_linear) return("y")
    else return("")
  })

  # Render plot
  output$validate_fluorescence_plot_linear <- renderPlot({
    results <- results$fluorescence
    if(length(results$flFit1$flFittedLinear[[input$sample_validate_fluorescence_linear]]) > 1){


      plot.flFitLinear(results$flFit1$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]],
                       log = logy_validate_fluorescence_plot_linear()
                       # ADD FURTHER INPUT (see Notion)
      )
      if(input$diagnostics_validate_fluorescence_plot_linear){
        plot.flFitLinear(results$flFit1$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]],
                         which = "fit_diagnostics",
                         log = logy_validate_fluorescence_plot_linear()
                         # ADD FURTHER INPUT (see Notion)
        )
      }
    }
  })

  # lin.rerun.param <- reactiveValues()

  observeEvent(input$rerun_fluorescence_linear, {
    # display a modal dialog with a header, textinput and action buttons
    showModal(
      modalDialog(
        tags$h2('Please enter adjusted parameters'),
        textInput('t0.lin.rerun.fluorescence', 'Minimum time (t0)', placeholder = paste0("previously: ", results$fluorescence$flFit1$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]$control$t0)),
        textInput('min.density.lin.rerun.fluorescence', 'Minimum density', placeholder = paste0("previously: ", results$fluorescence$flFit1$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]$control$min.density)),
        textAreaInput('quota.rerun.fluorescence', 'Quota', placeholder = HTML(paste0("previously: ", results$fluorescence$flFit1$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]$quota,
                                                                                     "\n",
                                                                                     "include regression windows with slope = ", expression(µ[max]), " * quota into the final linear fit."))),
        textInput('lin.h.rerun.fluorescence', 'Sliding window size (h)', placeholder = paste0("previously: ",
                                                                                              ifelse(!is.null(results$fluorescence$flFit1$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]$control$lin.h),
                                                                                                     results$fluorescence$flFit1$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]$control$lin.h,
                                                                                                     "NULL"))),
        textInput('lin.R2.rerun.fluorescence', 'R2 threshold', placeholder = paste0("previously: ", results$fluorescence$flFit1$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]$control$lin.R2)),
        textInput('lin.RSD.rerun.fluorescence', 'RSD threshold for slope', placeholder = paste0("previously: ", results$fluorescence$flFit1$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]$control$lin.RSD)),
        footer=tagList(
          actionButton('submit.rerun.linear.fluorescence', 'Submit'),
          modalButton('cancel')
        )
      )
    )
  })

  # Re-run selected linear fit with user-defined parameters upon click on 'submit'
  observeEvent(input$submit.rerun.linear.fluorescence, {
    if(!is.null(results$fluorescence$flFit1)){
      # store previous fit in memory
      selected_vals_validate_fluorescence$restore_fluorescence_linear <- results$fluorescence$flFit1$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]

      # Re-run fit and store in results object
      actwell <- results$fluorescence$flFit1$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]$raw.fl
      acttime <- results$fluorescence$flFit1$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]$raw.x
      control <- results$fluorescence$flFit1$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]$control
      control_new <- control
      ID <- results$fluorescence$flFit1$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]$ID

      lin.h.new <- dplyr::if_else(!is.na(as.numeric(input$lin.h.rerun.fluorescence)), as.numeric(input$lin.h.rerun.fluorescence), control$lin.h)
      if(!is.na(lin.h.new)) control_new$lin.h <- lin.h.new
      control_new$lin.R2 <- dplyr::if_else(!is.na(as.numeric(input$lin.R2.rerun.fluorescence)), as.numeric(input$lin.R2.rerun.fluorescence), control$lin.R2)
      control_new$lin.RSD <- dplyr::if_else(!is.na(as.numeric(input$lin.RSD.rerun.fluorescence)), as.numeric(input$lin.RSD.rerun.fluorescence), control$lin.RSD)
      control_new$t0 <- ifelse(!is.na(as.numeric(input$t0.lin.rerun.fluorescence)), as.numeric(input$t0.lin.rerun.fluorescence), control$t0)
      min.density.lin.new <- ifelse(!is.na(as.numeric(input$min.density.lin.rerun.fluorescence)), as.numeric(input$min.density.lin.rerun.fluorescence), control$min.density)
      if(is.numeric(min.density.lin.new)){
        if(!is.na(min.density.lin.new) && all(as.vector(actwell) < min.density.lin.new)){
          message(paste0("Start density values need to be greater than 'min.density'.\nThe minimum start value in your dataset is: ",
                         min(as.vector(actwell)),". 'min.density' was not adjusted."), call. = FALSE)
        } else if(!is.na(min.density.lin.new)){
          control_new$min.density <- min.density.lin.new
        }
      }
      quota_new <- ifelse(!is.na(as.numeric(input$quota.rerun.fluorescence)), as.numeric(input$quota.rerun.fluorescence), 0.95)

      if(control$x_type == "time"){
        try(
          results$fluorescence$flFit1$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]] <-
            flFitLinear(time = acttime, fl_data = actwell,
                        ID = ID,
                        control = control_new,
                        quota = quota_new)
        )
      } else {
        try(
          results$fluorescence$flFit1$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]] <-
            flFitLinear(density = acttime, fl_data = actwell,
                        ID = ID,
                        control = control_new,
                        quota = quota_new)
        )
      }

      # Show [Restore fit] button
      show("restore_fluorescence_linear")
    }

    removeModal()
  })

  # Restore previous linear fit upon click on [Restore Fit]
  observeEvent(input$restore_fluorescence_linear, {
    # store previous fit from memory
    results$fluorescence$flFit1$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]] <- selected_vals_validate_fluorescence$restore_fluorescence_linear
    hide("restore_fluorescence_linear")
  })

  ### Spline Fits ####
  selected_inputs_validate_fluorescence_spline_sample <- reactive({
    results <- results$fluorescence
    if(is.null(results)) return("")
    if("s" %in% results$control$fit.opt || "a" %in% results$control$fit.opt){
      select_samples <- names(results$flFit1$flFittedSplines)
    } else {
      return("")
    }
    select_samples
  })

  observe({
    updateSelectInput(session,
                      inputId = "sample_validate_fluorescence_spline",
                      choices = selected_inputs_validate_fluorescence_spline_sample(),
                      selected = selected_vals_validate_fluorescence$sample_validate_fluorescence_spline
    )})


  output$validate_fluorescence_plot_spline <- renderPlot({
    results <- results$fluorescence
    if(length(results$flFit1$flFittedSplines[[input$sample_validate_fluorescence_spline]]) > 1){
      showModal(modalDialog("Creating plot...", footer=NULL))
      plot.flFitSpline(results$flFit1$flFittedSplines[[input$sample_validate_fluorescence_spline]],
                       log.y = input$logy_validate_fluorescence_plot_spline, colData = 1
      )
      removeModal()
    }
  })

  spline.rerun.param.fluorescence <- reactiveValues()

  observeEvent(input$rerun_fluorescence_spline, {
    # display a modal dialog with a header, textinput and action buttons
    showModal(
      modalDialog(
        tags$h2('Please enter adjusted parameters'),
        textInput('t0.spline.rerun.fluorescence', 'Minimum time (t0)', placeholder = paste0("previously: ", results$fluorescence$flFit1$flFittedSplines[[input$sample_validate_fluorescence_spline]]$control$t0)),
        textInput('min.density.spline.rerun.fluorescence', 'Minimum density', placeholder = paste0("previously: ", results$fluorescence$flFit1$flFittedSplines[[input$sample_validate_fluorescence_spline]]$control$min.density)),
        textInput('smooth.gc.rerun.fluorescence', 'Smoothing factor', placeholder = paste0("previously: ", results$fluorescence$flFit1$flFittedSplines[[input$sample_validate_fluorescence_spline]]$control$smooth.gc)),
        footer=tagList(
          actionButton('submit.rerun.spline.fluorescence', 'Submit'),
          modalButton('cancel')
        )
      )
    )
  })

  # Re-run selected spline fit with user-defined parameters upon click on 'submit'
  observeEvent(input$submit.rerun.spline.fluorescence, {
    if(!is.null(results$fluorescence$flFit1)){
      # store previous fit in memory
      selected_vals_validate_fluorescence$restore_fluorescence_spline <- results$fluorescence$flFit1$flFittedSplines[[selected_vals_validate_fluorescence$sample_validate_fluorescence_spline]]

      # Re-run fit and store in results object
      actwell <- results$fluorescence$flFit1$flFittedSplines[[selected_vals_validate_fluorescence$sample_validate_fluorescence_spline]]$fl.in
      acttime <- results$fluorescence$flFit1$flFittedSplines[[selected_vals_validate_fluorescence$sample_validate_fluorescence_spline]]$x.in
      control <- results$fluorescence$flFit1$flFittedSplines[[selected_vals_validate_fluorescence$sample_validate_fluorescence_spline]]$control
      control_new <- control
      ID <- results$fluorescence$flFit1$flFittedSplines[[selected_vals_validate_fluorescence$sample_validate_fluorescence_spline]]$ID

      control_new$smooth.gc <- dplyr::if_else(!is.na(as.numeric(input$smooth.gc.rerun.fluorescence)), as.numeric(input$smooth.gc.rerun.fluorescence), control$lin.R2)
      control_new$t0 <- ifelse(!is.na(as.numeric(input$t0.spline.rerun.fluorescence)), as.numeric(input$t0.spline.rerun.fluorescence), control$t0)
      min.density.spline.new <- ifelse(!is.na(as.numeric(input$min.density.spline.rerun.fluorescence)), as.numeric(input$min.density.spline.rerun.fluorescence), control$min.density)
      if(is.numeric(min.density.spline.new)){
        if(!is.na(min.density.spline.new) && all(as.vector(actwell) < min.density.spline.new)){
          message(paste0("Start density values need to be greater than 'min.density'.\nThe minimum start value in your dataset is: ",
                         min(as.vector(actwell)),". 'min.density' was not adjusted."), call. = FALSE)
        } else if(!is.na(min.density.spline.new)){
          control_new$min.density <- min.density.spline.new
        }
      }
      if(control$x_type == "time"){
        try(
          results$fluorescence$flFit1$flFittedSplines[[selected_vals_validate_fluorescence$sample_validate_fluorescence_spline]] <-
            flFitSpline(time = acttime, fl_data = actwell,
                        ID = ID,
                        control = control_new)
        )
      } else {
        try(
          results$fluorescence$flFit1$flFittedSplines[[selected_vals_validate_fluorescence$sample_validate_fluorescence_spline]] <-
            flFitSpline(density = acttime, fl_data = actwell,
                        ID = ID,
                        control = control_new)
        )
      }

      # Show [Restore fit] button
      show("restore_fluorescence_spline")
    }

    removeModal()

  })

  # Restore previous spline fit upon click on [Restore Fit]
  observeEvent(input$restore_fluorescence_spline, {
    # store previous fit from memory
    results$fluorescence$flFit1$flFittedSplines[[selected_vals_validate_fluorescence$sample_validate_fluorescence_spline]] <- selected_vals_validate_fluorescence$restore_fluorescence_spline
    hide("restore_fluorescence_spline")
  })

  # Visualize ####
  ## Growth Plots: #####
  ### Group Plots ####
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
                lwd = input$line_width_growth_group_plot,
                basesize = input$base_size_growth_group_plot,
                shiny = TRUE
    )
  })

  ### DR Plots ####
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

  ### Parameter Plots ####
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
                   basesize = input$basesize_growth_parameter_plot,
                   label.size = input$label.size_growth_parameter_plot
    )
  })

  ## Fluorescence Plots: #####

  #____Plot Downloads____#####

  output$downloads_growth_group_plot <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      out.nm <- gsub(paste0("^.+", paste(.Platform$file.sep)), "", file)
      out.dir <- gsub(paste0("[^", .Platform$file.sep, "]+$"), "", file)
      file.format <- gsub("^.+\\.", "", file)

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
                  lwd = input$line_width_growth_group_plot,
                  basesize = input$base_size_growth_group_plot,
                  shiny = TRUE,
                  export = TRUE,
                  out.nm = out.nm,
                  out.dir = out.dir
      )
    }
  )

  output$downloads_dose_response_plot_combined_growth <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      out.nm <- gsub(paste0("^.+", paste(.Platform$file.sep)), "", file)
      out.dir <- gsub(paste0("[^", .Platform$file.sep, "]+$"), "", file)
      file.format <- gsub("^.+\\.", "", file)
      results <- results$growth$drFit
      plot.drFit(results,
                 combine=TRUE,
                 pch = input$shape_type_dose_response_growth_plot,
                 cex = input$shape_size_dose_response_growth_plot,
                 basesize = input$base_size_dose_response_growth_plot,
                 lwd = input$line_width_dose_response_growth_plot,
                 ec50line = input$show_ec50_indicator_lines_dose_response_growth_plot,
                 export = TRUE,
                 out.nm = out.nm,
                 out.dir = out.dir)
    }
  )

  output$downloads_dose_response_plot_individual_growth <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      out.nm <- gsub(paste0("^.+", paste(.Platform$file.sep)), "", file)
      out.dir <- gsub(paste0("[^", .Platform$file.sep, "]+$"), "", file)
      file.format <- gsub("^.+\\.", "", file)
      results <- results$growth$drFit$drFittedSplines[[input$individual_plots_dose_response_growth_plot]]
      plot.drFitSpline(results,
                       combine=FALSE,
                       pch = input$shape_type_dose_response_growth_plot,
                       cex = input$shape_size_dose_response_growth_plot,
                       basesize = input$base_size_dose_response_growth_plot,
                       lwd = input$line_width_dose_response_growth_plot,
                       ec50line = input$show_ec50_indicator_lines_dose_response_growth_plot,
                       export = TRUE,
                       out.nm = out.nm,
                       out.dir = out.dir)
    }
  )

  output$downloads_growth_parameter_plot <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      out.nm <- gsub(paste0("^.+", paste(.Platform$file.sep)), "", file)
      out.dir <- gsub(paste0("[^", .Platform$file.sep, "]+$"), "", file)
      file.format <- gsub("^.+\\.", "", file)
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
                     basesize = input$basesize_growth_parameter_plot,
                     label.size = input$label.size_growth_parameter_plot,
                     export = TRUE,
                     out.nm = out.nm,
                     out.dir = out.dir
      )
    }
  )

  #____Table Download____####
  output$download_table_growth_linear <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(table_growth_linear(), file)
    }
  )

  output$download_table_growth_spline <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(table_growth_spline(), file)
    }
  )

  output$downloads_table_growth_model <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(table_growth_model(), file)
    }
  )

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

  ### growth: plot.drFit ####
  observe({
    if(!is.null(results$growth)){
      if(!input$perform_ec50_growth){
        hideTab(inputId = "tabsetPanel_Visalize_Growth", target = "tabPanel_Visualize_Growth_DoseResponse")
      }
    }
  })
  ### growth: plot.parameter() ####

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


  ## Fluorescence ####
  selected_inputs_reference_condition_fluorescence_parameter_plot <- reactive({
    results <- results$fluorescence
    results$expdesign$condition
  })

  select_inputs_reference_concentration_fluorescence_parameter_plot <- reactive({
    results <- results$fluorescence
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

  # Report ####
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
  # Bug report message
  github_url <- a("QurvE Github", href="https://github.com/NicWir/QurvE_issues/issues")
  output$bug_report <- renderUI({
    tagList("Please report bugs and user feedback at:", github_url)
  })

}

shinyApp(ui = ui, server = server)
