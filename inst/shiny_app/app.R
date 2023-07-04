options(shiny.maxRequestSize=60*1024^2)

#list of packages required
list.of.packages <- c("shiny", "shinythemes", "shinyFiles", "shinyjs", "shinyBS", "shinycssloaders", "QurvE")
#list of packages required
new_packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#install missing packages
for( i in new_packages ){
  #  require returns TRUE invisibly if it was able to load package
  if( ! require( i , character.only = TRUE ) ){
    #  If package was not able to be loaded then re-install
    install.packages( i , dependencies = TRUE )
    #  Load package after installing
    require( i , character.only = TRUE , quietly = T)
  }
}


library(shiny, quietly = T)
library(QurvE, quietly = T)
library(shinyBS, quietly = T)
library(shinycssloaders, quietly = T)
library(shinyFiles, quietly = T)
library(shinyjs, quietly = T)
library(shinythemes, quietly = T)
library(ggplot2, quietly = T)
library(magrittr)

# Define icon set from custom SVG files
# iconset <- icons::icon_set("icons/")

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

load_data <- function() {
  Sys.sleep(2)
  hide("loading_page")
  show("main_content")
}

widePopover <-
  '<div class="popover popover-lg" role="tooltip"><div class="arrow"></div><h3 class="popover-title"></h3><div class="popover-content"></div></div>'

ui <- fluidPage(theme = shinythemes::shinytheme(theme = "spacelab"),
                tags$head(
                  tags$style(HTML(".popover.popover-lg {width: 600px; max-width: 600px;}"))
                ),
                tags$style(
                  type = 'text/css',
                  '.modal-dialog.gcFitLinear { width: fit-content !important; }'
                ),
                tags$style(
                  type = 'text/css',
                  '.modal-dialog.rdm.data { width: fit-content !important; }'
                ),
                tags$style(
                  type = 'text/css',
                  '.modal-dialog.gcFitSpline { width: fit-content !important; }'
                ),
                tags$style(
                  type = 'text/css',
                  '.modal-dialog.gcFitModel { width: fit-content !important; }'
                ),
                tags$style(
                  type = 'text/css',
                  '.modal-dialog.flFitLinear { width: fit-content !important; }'
                ),
                tags$style(
                  type = 'text/css',
                  '.modal-dialog.flFitSpline { width: fit-content !important; }'
                ),
                tags$style(
                  type = 'text/css',
                  '.modal-dialog.flworkflow { width: fit-content !important; }'
                ),
                tags$style(
                  type = 'text/css',
                  '.modal-dialog.fldr { width: fit-content !important; }'
                ),
                tags$style(
                  type = 'text/css',
                  '.modal-dialog.growthworkflow { width: fit-content !important; }'
                ),
                tags$style(
                  type = 'text/css',
                  '.modal-dialog.growthdr { width: fit-content !important; }'
                ),
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

                  # # Create object input$dimension as c(width, height) with the app window size
                  # tags$head(tags$script('
                  #               var dimension = [0, 0];
                  #               $(document).on("shiny:connected", function(e) {
                  #                   dimension[0] = window.innerWidth;
                  #                   dimension[1] = window.innerHeight;
                  #                   Shiny.onInputChange("dimension", dimension);
                  #               });
                  #               $(window).resize(function(e) {
                  #                   dimension[0] = window.innerWidth;
                  #                   dimension[1] = window.innerHeight;
                  #                   Shiny.onInputChange("dimension", dimension);
                  #               });
                  #           ')),
                  useShinyjs(),
                  shinyjs::extendShinyjs(text = jscode, functions = c("disableTab","enableTab")),
                  shinyjs::inlineCSS(css),
                  div(
                    id = "loading_page",
                    HTML("<br>"),
                    HTML("<br>"),
                    HTML('<center><img src="QurvE_logo.png" width="500" vertical-align="middle"></center>'),
                    HTML("<br>"),
                    HTML("<br>"),
                    h1("Initializing...", align = "center")
                  ),
                  hidden(
                    div(
                      id = "main_content",
                      navbarPage(
                        'QurvE',
                        id = "navbar",

                        # load input file
                        #____DATA____####
                        tabPanel(span("Data", title = "Upload custom formatted data or parse results from plate readers and similar devices."),
                                 icon = icon("file-lines"),
                                 value = "tabPanel",
                                 tabsetPanel(type = "tabs", id = "tabs_data",
                                             ##____CUSTOM____####
                                             tabPanel(value = "Custom", span("Custom", title="Upload manually formatted data. Data from different experiments can be added. In column format, the first three table rows contain (see figure):\n1. sample description\n2. replicate number (optional: followed by a letter to indicate technical replicates)\n3. concentration value (optional, for dose-response analysis)"),
                                                      sidebarPanel(
                                                        style='border-color: #ADADAD',
                                                        # Growth data
                                                        wellPanel(
                                                          h4(strong("Growth data"), style = "line-height: 0.4;font-size: 150%; margin-bottom: 15px;"),
                                                          style='padding: 0.1; border-color: #ADADAD; padding: 1; padding-bottom: 0',

                                                          fileInput(inputId = 'custom_file_growth',
                                                                    label = 'Choose growth data file',
                                                                    accept = c('.xlsx', '.xls', '.csv', '.txt', '.tsv')
                                                          ),

                                                          conditionalPanel(
                                                            condition = "output.growthfileUploaded && output.custom_growth_format == 'xlsx'",
                                                            div(style = "margin-bottom: -20px"),
                                                            selectInput(inputId = "custom_growth_sheets",
                                                                        label = "Select Sheet",
                                                                        choices = "Sheet1")

                                                          ), # select sheet: conditional
                                                          conditionalPanel(
                                                            condition = "output.growthfileUploaded && output.custom_growth_format == 'csv'",

                                                            selectInput(inputId = "separator_custom_growth",
                                                                        label = "Select separator",
                                                                        choices = c("," = ",",
                                                                                    ";" = ";")
                                                            ),

                                                            selectInput(inputId = "decimal_separator_custom_growth",
                                                                        label = "Select Decimal separator",
                                                                        choices = c("." = ".",
                                                                                    "," = ",")
                                                            )
                                                          ),
                                                          conditionalPanel(
                                                            condition = "output.growthfileUploaded && (output.custom_growth_format == 'tsv' || output.custom_growth_format == 'txt')",

                                                            selectInput(inputId = "decimal_separator_custom_growth",
                                                                        label = "Select Decimal separator",
                                                                        choices = c("." = ".",
                                                                                    "," = ",")
                                                            )
                                                          ),
                                                          div(style = "margin-bottom: -15px"),
                                                          tags$div(title="Provide an equation in the form 'y = function(x)' (for example: 'y = x^2 * 0.3 - 0.5') to convert growth values. This can be used to, e.g., convert plate reader absorbance values into OD600.",
                                                                   checkboxInput(inputId = 'calibration_growth_custom',
                                                                                 label = 'Apply calibration')
                                                          ),
                                                          conditionalPanel(
                                                            condition = 'input.calibration_growth_custom',
                                                            div(style = "margin-bottom: -10px"),
                                                            textInput(inputId = "calibration_equation_growth_custom",
                                                                      label = "Type equation in the form 'y = function(x)'",
                                                                      placeholder = 'y = x * 0.5 - 1'
                                                            )
                                                          ),
                                                        ),
                                                        #____Fluorescence___________
                                                        wellPanel(
                                                          h4(strong("Fluorescence data"), style = "line-height: 1;font-size: 150%; margin-bottom: 15px;"),
                                                          style='padding: 0.1; border-color: #ADADAD; padding: 1; padding-bottom: 0',

                                                          fileInput(inputId = 'custom_file_fluorescence',
                                                                    label = 'Choose fluorescence data file',
                                                                    accept = c('.xlsx', '.xls', '.csv', '.txt', '.tsv')
                                                          ),

                                                          conditionalPanel(
                                                            condition = "output.fluorescencefileUploaded && output.custom_fluorescence_format == 'xlsx'",
                                                            div(style = "margin-bottom: -20px"),
                                                            selectInput(inputId = "custom_fluorescence_sheets",
                                                                        label = "Select Sheet",
                                                                        choices = "Sheet1")
                                                          ), # select sheet: conditional
                                                          conditionalPanel(
                                                            condition = "output.fluorescencefileUploaded && output.custom_fluorescence_format == 'csv'",

                                                            selectInput(inputId = "separator_custom_fluorescence",
                                                                        label = "Select separator",
                                                                        choices = c("," = ",",
                                                                                    ";" = ";")
                                                            ),

                                                            selectInput(inputId = "decimal_separator_custom_fluorescence",
                                                                        label = "Select Decimal separator",
                                                                        choices = c("." = ".",
                                                                                    "," = ",")
                                                            )
                                                          ),
                                                          conditionalPanel(
                                                            condition = "output.fluorescencefileUploaded && (output.custom_fluorescence_format == 'tsv' || output.custom_fluorescence_format == 'txt')",

                                                            selectInput(inputId = "decimal_separator_custom_fluorescence",
                                                                        label = "Select Decimal separator",
                                                                        choices = c("." = ".",
                                                                                    "," = ",")
                                                            )
                                                          ),
                                                          div(style = "margin-top: -15px"),
                                                          tags$div(title="Provide an equation in the form 'y = function(x)' (for example: 'y = x^2 * 0.3 - 0.5') to convert fluorescence values. This can be used to, e.g., convert fluorescence intensities into molecule concentrations.",
                                                                   checkboxInput(inputId = 'calibration_fluorescence_custom',
                                                                                 label = 'Apply calibration')
                                                          ),
                                                          conditionalPanel(
                                                            condition = 'input.calibration_fluorescence_custom',
                                                            div(style = "margin-bottom: -10px"),
                                                            textInput(inputId = "calibration_equation_fluorescence_custom",
                                                                      label = "Type equation in the form 'y = function(x)'",
                                                                      placeholder = 'y = x * 0.5 - 1'
                                                            )
                                                          ),
                                                        ), # wellpanel

                                                        conditionalPanel(
                                                          condition = "output.fluorescencefileUploaded",
                                                          checkboxInput(inputId = 'load_fl2_data_custom',
                                                                        label = 'Use second fluorescence to normalize fluorescence.',
                                                                        value = FALSE),
                                                          bsPopover("load_fl2_data_custom", title = "Provide a table file with fluorescence 2 data",
                                                                    content = "Table layout must mimic that of growth data. Fluorescence 2 data is only used to normalize of fluorescence!")
                                                        ),
                                                        # #_____Fluorescence 2___________
                                                        conditionalPanel(
                                                          condition = "input.load_fl2_data_custom",
                                                          wellPanel(
                                                            h4(strong("Fluorescence 2 data"), style = "line-height: 1;font-size: 150%; margin-bottom: 15px;"),
                                                            style='padding: 0.1; border-color: #ADADAD; padding: 1; padding-bottom: 0',


                                                            fileInput(inputId = 'custom_file_fluorescence2',
                                                                      label = 'Choose fluorescence2 data file',
                                                                      accept = c('.xlsx', '.xls', '.csv', '.txt', '.tsv')
                                                            ),


                                                            conditionalPanel(
                                                              condition = "output.fluorescence2fileUploaded && output.custom_fluorescence2_format == 'xlsx'",
                                                              wellPanel(
                                                                style='padding: 1; border-color: #ADADAD; padding-bottom: 0',
                                                                div(style = "margin-bottom: -20px"),
                                                                selectInput(inputId = "custom_fluorescence2_sheets",
                                                                            label = "Select Sheet",
                                                                            choices = "Sheet1")
                                                              )
                                                            ), # select sheet: conditional
                                                            conditionalPanel(
                                                              condition = "output.fluorescence2fileUploaded && output.custom_fluorescence2_format == 'csv'",
                                                              wellPanel(
                                                                style='padding: 1; border-color: #ADADAD; padding-bottom: 0',
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
                                                                style='padding: 1; border-color: #ADADAD; padding-bottom: 0',
                                                                selectInput(inputId = "decimal_separator_custom_fluorescence2",
                                                                            label = "Select Decimal separator",
                                                                            choices = c("." = ".",
                                                                                        "," = ",")
                                                                )
                                                              )
                                                            ),
                                                            div(style = "margin-top: -15px"),
                                                            tags$div(title="Provide an equation in the form 'y = function(x)' (for example: 'y = x^2 * 0.3 - 0.5') to convert fluorescence2 values. This can be used to, e.g., convert fluorescence intensities into molecule concentrations.",
                                                                     checkboxInput(inputId = 'calibration_fluorescence2_custom',
                                                                                   label = 'Apply calibration')
                                                            ),
                                                            conditionalPanel(
                                                              condition = 'input.calibration_fluorescence2_custom',
                                                              div(style = "margin-bottom: -10px"),
                                                              textInput(inputId = "calibration_equation_fluorescence2_custom",
                                                                        label = "Type equation in the form 'y = function(x)'",
                                                                        placeholder = 'y = x * 0.5 - 1'
                                                              )
                                                            ),
                                                          ) # wellPanel
                                                        ),

                                                        selectInput(inputId = 'norm_type_custom',
                                                                    label = 'Select data type for fluorescence normalization',
                                                                    choices = ""),

                                                        tags$div(title="Shall blank values (the mean of samples identified by 'Blank' IDs) be subtracted from values within the same experiment?",
                                                                 checkboxInput(inputId = 'subtract_blank_custom',
                                                                               label = 'Subtract blank',
                                                                               value = TRUE)
                                                        ),

                                                        tags$div(title=HTML(paste("Provide an equation in the form 'y = function(x)' to convert time values. For example, type 'y = x / 60' to convert minutes to hours.\n", "Note: the time unit will affect calculated parameters (e.g., the growth rate in 1/h, 1/min, or 1/s) as well as the time displayed in all plots.")),
                                                                 checkboxInput(inputId = 'convert_time_values_custom',
                                                                               label = 'Convert time values',
                                                                               value = FALSE)
                                                        ),

                                                        conditionalPanel(
                                                          condition = 'input.convert_time_values_custom',
                                                          tags$div(title=HTML(paste("Provide an equation in the form 'y = function(x)' to convert time values. For example, type 'y = x / 60' to convert minutes to hours.\n", "Note: the time unit will affect calculated parameters (e.g., the growth rate in 1/h, 1/min, or 1/s) as well as the time displayed in all plots.")),
                                                                   div(style = "margin-bottom: -10px"),
                                                                   textInput(inputId = "convert_time_equation_custom",
                                                                             label = "Type equation in the form 'y = function(x)'",
                                                                             placeholder = 'y = x / 24')
                                                          )
                                                        ),

                                                        conditionalPanel(
                                                          condition = 'output.growthfileUploaded || output.fluorescencefileUploaded',
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
                                                        ),
                                                        HTML("<br>"),
                                                        HTML("<br>"),
                                                        tags$div(title="Simulate growth curves to generate a random demo dataset.",
                                                                 actionButton(inputId = "random_data_growth",
                                                                              label = "Create random growth dataset",
                                                                              icon=icon("shuffle"),
                                                                              style="padding:4px; font-size:80%; color: white; background-color: #35e51d")
                                                        ),
                                                      ),# sidebar panel
                                             ), # Custom tabPanel

                                             ##____PLATE READER____####

                                             tabPanel(value = "Parse Raw Data", span("Parse Raw Data", title="Upload a results table file generated with the default export function of a plate reader (or similar device) software. Sample information need to be provided in a separate table."),
                                                      sidebarPanel(
                                                        style='border-color: #ADADAD',
                                                        wellPanel(
                                                          div(style = "margin-top: -10px"),
                                                          h3(strong("1. Load data"), style = "line-height: 0.4;font-size: 150%; margin-bottom: 15px;"),
                                                          style='padding: 5px; border-color: #ADADAD;',
                                                          # select file type
                                                          fileInput(inputId = 'parse_file',
                                                                    label = 'Choose raw export file',
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
                                                            style='padding: 5px; border-color: #ADADAD; padding-bottom: 0',
                                                            div(style = "margin-top: -10px"),
                                                            h3(strong("2. Format"), style = "line-height: 0.4;font-size: 150%; margin-bottom: 15px;"),
                                                            selectizeInput(inputId = "platereader_software",
                                                                           label = "Platereader software",
                                                                           choices = c("Biotek - Gen5/Gen6" = "Gen5",
                                                                                       "Biolector" = "Biolector",
                                                                                       "Chi.Bio" = "Chi.Bio",
                                                                                       "Growth Profiler 960" = "GrowthProfiler",
                                                                                       "Tecan i-control" = "Tecan",
                                                                                       "PerkinElmer - Victor Nivo" = "VictorNivo",
                                                                                       "PerkinElmer - Victor X3" = "VictorX3"
                                                                           ),
                                                                           multiple = TRUE,
                                                                           options = list(maxItems = 1)
                                                            )
                                                          )
                                                        ),
                                                        div(style = "margin-top: -15px"),
                                                        conditionalPanel(
                                                          condition = "output.parsefileUploaded",
                                                          wellPanel(
                                                            style='padding: 5px; border-color: #ADADAD; padding-bottom: 0',
                                                            div(style = "margin-top: -10px"),
                                                            h3(strong("3. Assign data type"), style = "line-height: 0.4; font-size: 150%; margin-bottom: 15px;"),
                                                            conditionalPanel(
                                                              condition = "input.platereader_software != 'GrowthProfiler'",

                                                              # Growth Read
                                                              selectInput(inputId = "parsed_reads_growth",
                                                                          label = "Growth data",
                                                                          choices = ""
                                                              ),
                                                              conditionalPanel(
                                                                condition = "input.parsed_reads_growth.length > 0 && input.parsed_reads_growth != 'Ignore'",
                                                                div(style = "margin-top: -15px"),
                                                                tags$div(title="Provide an equation in the form 'y = function(x)' (for example: 'y = x^2 * 0.3 - 0.5') to convert growth values. This can be used to, e.g., convert plate reader absorbance values into OD600.",
                                                                         checkboxInput(inputId = 'calibration_growth_plate_reader',
                                                                                       label = 'Apply calibration'),
                                                                ),
                                                                conditionalPanel(
                                                                  condition = "input.calibration_growth_plate_reader",
                                                                  div(style = "margin-bottom: -10px"),
                                                                  textInput(inputId = "calibration_equation_growth_plate_reader",
                                                                            label = NULL,
                                                                            placeholder = 'y = x * 0.5 - 1',
                                                                            width = "85%")

                                                                )
                                                              ),

                                                              # Fluorescence Read
                                                              selectInput(inputId = "parsed_reads_fluorescence",
                                                                          label = "Fluorescence data",
                                                                          choices = ""
                                                              ),
                                                              conditionalPanel(
                                                                condition = "input.parsed_reads_fluorescence.length > 0 && input.parsed_reads_fluorescence != 'Ignore'",
                                                                div(style = "margin-top: -15px"),
                                                                tags$div(title="Provide an equation in the form 'y = function(x)' (for example: 'y = x^2 * 0.3 - 0.5') to convert fluorescence values. This can be used to, e.g., convert fluorescence intensities into molecule concentrations.",
                                                                         checkboxInput(inputId = 'calibration_fluorescence_plate_reader',
                                                                                       label = 'Apply calibration')
                                                                ),
                                                                conditionalPanel(
                                                                  condition = "input.calibration_fluorescence_plate_reader",
                                                                  div(style = "margin-bottom: -10px"),
                                                                  textInput(inputId = "calibration_equation_fluorescence_plate_reader",
                                                                            label = NULL,
                                                                            placeholder = 'y = x * 0.5 - 1',
                                                                            width = "85%"),
                                                                )
                                                              ),

                                                              # Fluorescence2 Read
                                                              selectInput(inputId = "parsed_reads_fluorescence2",
                                                                          label = "Fluorescence data 2 (used only for normalization)",
                                                                          choices = ""
                                                              ),
                                                              conditionalPanel(
                                                                condition = "input.parsed_reads_fluorescence2.length > 0 && input.parsed_reads_fluorescence2 != 'Ignore'",
                                                                div(style = "margin-top: -15px"),
                                                                tags$div(title="Provide an equation in the form 'y = function(x)' (for example: 'y = x^2 * 0.3 - 0.5') to convert fluorescence values. This can be used to, e.g., convert fluorescence intensities into molecule concentrations.",
                                                                         checkboxInput(inputId = 'calibration_fluorescence2_plate_reader',
                                                                                       label = 'Apply calibration')
                                                                ),
                                                                conditionalPanel(
                                                                  condition = "input.calibration_fluorescence2_plate_reader",
                                                                  div(style = "margin-bottom: -10px"),
                                                                  textInput(inputId = "calibration_equation_fluorescence2_plate_reader",
                                                                            label = NULL,
                                                                            placeholder = 'y = x * 0.5 - 1',
                                                                            width = "85%")

                                                                )
                                                              ),
                                                            ), # conditionalPanel
                                                          ) # wellPanel
                                                        ),
                                                        div(style = "margin-top: -15px"),
                                                        conditionalPanel(
                                                          condition = "output.parsefileUploaded",
                                                          wellPanel(
                                                            style='padding: 5px; border-color: #ADADAD; padding-bottom: 0',
                                                            div(style = "margin-top: -10px"),
                                                            h3(strong("4. Load mapping"), style = "line-height: 0.4; font-size: 150%; margin-bottom: 15px;"),
                                                            conditionalPanel(
                                                              condition = "output.parse_file_format == 'xlsx' | output.parse_file_format == 'xls'",
                                                              tags$div(title="A table with mapping information is stored within the same Excel file that contains experimental data as separate sheet.",
                                                                       checkboxInput(inputId = 'mapping_included_in_parse',
                                                                                     label = 'Included in data file (xlsx/xls)',
                                                                                     value = FALSE)
                                                              )
                                                            ),
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
                                                              style='padding: 5px; border-color: #ADADAD; padding-bottom: 0',
                                                              selectInput(inputId = "decimal_separator_map",
                                                                          label = "Select Decimal separator",
                                                                          choices = c("." = ".",
                                                                                      "," = ",")
                                                              )
                                                            )
                                                          )
                                                        ),

                                                        conditionalPanel(
                                                          condition = "input.parsed_reads_fluorescence.length > 0 && input.parsed_reads_fluorescence != 'Ignore'",
                                                          selectInput(inputId = 'norm_type_parse',
                                                                      label = 'Select Read for fluorescence normalization',
                                                                      choices = "")
                                                        ),

                                                        tags$div(title="Shall blank values (the mean of samples identified by 'Blank' IDs) be subtracted from values within the same experiment?",
                                                                 checkboxInput(inputId = 'subtract_blank_plate_reader',
                                                                               label = 'Subtract blank',
                                                                               value = TRUE)
                                                        ),

                                                        tags$div(title=HTML(paste("Provide an equation in the form 'y = function(x)' to convert time values. For example, type 'y = x / 60' to convert minutes to hours.\n", "Note: the time unit will affect calculated parameters (e.g., the growth rate in 1/h, 1/min, or 1/s) as well as the time displayed in all plots.")),
                                                                 checkboxInput(inputId = 'convert_time_values_plate_reader',
                                                                               label = 'Convert time values',
                                                                               value = TRUE)
                                                        ),
                                                        conditionalPanel(
                                                          condition = 'input.convert_time_values_plate_reader',
                                                          tags$div(title=HTML(paste("Provide an equation in the form 'y = function(x)' to convert time values. For example, type 'y = x / 60' to convert minutes to hours.\n", "Note: the time unit will affect calculated parameters (e.g., the growth rate in 1/h, 1/min, or 1/s) as well as the time displayed in all plots.")),
                                                                   div(style = "margin-bottom: -10px"),
                                                                   textInput(inputId = "convert_time_equation_plate_reader",
                                                                             label = "Type equation in the form 'y = function(x)'",
                                                                             placeholder = 'y = x / 24')
                                                          )
                                                        ),

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
                                   div(
                                     id = "data_instruction",
                                     conditionalPanel(
                                       condition = "input.tabs_data == 'Custom'",
                                       img(src = 'data_instruction.png',
                                           width = '100%')
                                     )
                                   ),
                                   bsPopover(id = "data_instruction", title = "Custom data layout",
                                             content = paste("Please format your data in the format shown in the figure:",
                                                             paste0(
                                                               "<ul>",
                                                               "<li>The first row contains \\'Time\\' and \\'Blank\\', as well as sample identifiers \\(identical for replicates\\).</li>",
                                                               "<li>The second row contains replicate numbers for identical conditions. If technical replicates were used in addition to biological replicates, indicate technical replicates with the same replicate number. Samples with identical IDs, concentrations, and replicate <i>numbers</i> will be combined by their <i>average</i>.</li>",
                                                               "<li>The third row contains \\(optional\\) concentration values to perform a dose-response analysis, if different concentrations of a compound were used in the experiment.</li>",
                                                               "</ul>"
                                                             ),
                                                             "Details:",
                                                             paste0(
                                                               "<ul>",
                                                               "<li>Different experiments with differing time values and experiment-specific blanks are distinguished by an individual \\'Time\\' column to the left of each dataset.</li>",
                                                               "<li>Blank values \\(for each experiment\\) are combined as their average and subtracted from all remaining values if option \\[Subtract blank\\] is selected.</li>",
                                                               "<li>The metadata in the second and third rows are optional to perform the analysis and can be left empty.</li>",
                                                               "</ul>"
                                                             ),
                                                             sep = "<br>"),
                                             trigger = "hover", options = list(container = "body", template = widePopover)
                                   ),

                                   div(
                                     id = "mapping_layout",
                                     conditionalPanel(
                                       condition = "input.tabs_data != 'Custom' && output.parsefileUploaded",
                                       img(src = 'mapping_layout.png',
                                           width = '60%')
                                     )
                                   ),
                                   bsPopover(id = "mapping_layout", title = "Mapping layout",
                                             content = paste("Please format a table providing sample information in the format shown in the figure:",
                                                             paste0(
                                                               "<ul>",
                                                               "<li>The first column contains the <i>well</i> position/name in the plate.</li>",
                                                               "<li>The second column contains the <i>ID</i> \\(i.e., organism, condition, etc.\\) of each sample. The ID needs to be identical for replicates.</li>",
                                                               "<li>The third column row contains replicate numbers for the same conditions. If technical replicates were used in addition to biological replicates, indicate technical replicates with the same replicate number. Samples with identical IDs, concentrations, and replicate <i>numbers</i> will be combined by their <i>average</i>.</li>",
                                                               "<li>The fourth column contains \\(optional\\) concentration values to perform a dose-response analysis, if different concentrations of a compound were used in the experiment.</li>",
                                                               "</ul>"
                                                             ),
                                                             "Details:",
                                                             paste0(
                                                               "The values in \\'Blank\\' samples are combined as their average and subtracted from all remaining values if option \\[Subtract blank\\] is selected. The metadata in the third and fourth columns are optional to perform the analysis and can be left empty."
                                                             ),
                                                             sep = "<br>"),
                                             trigger = "hover", options = list(container = "body", template = widePopover)
                                   ),

                                   div(id = 'Custom_Data_Tables',
                                       h1("Your Data"),
                                       tabsetPanel(type = "tabs", id = "tabsetPanel_custom_tables",
                                                   tabPanel(title = "Growth plot", value = "tabPanel_custom_plots_growth",
                                                            withSpinner(
                                                              plotOutput("custom_raw_growth_plot",
                                                                         width = "100%", height = "1000px"),

                                                            )
                                                   ),
                                                   tabPanel(title = "Fluorescence plot", value = "tabPanel_custom_plots_fluorescence",
                                                            withSpinner(
                                                              plotOutput("custom_raw_fluorescence_plot",
                                                                         width = "100%", height = "1000px"),

                                                            )
                                                   ),
                                                   tabPanel(title = "norm. Fluorescence plot", value = "tabPanel_custom_plots_norm_fluorescence",
                                                            withSpinner(
                                                              plotOutput("custom_raw_norm_fluorescence_plot",
                                                                         width = "100%", height = "1000px"),

                                                            )
                                                   ),
                                                   tabPanel(title = "Growth", value = "tabPanel_custom_tables_growth_processed",
                                                            withSpinner(
                                                              DT::dataTableOutput("growth_data_custom_processed")
                                                            ),
                                                            downloadButton('download_custom_tables_growth_processed',"Download table")
                                                   ),
                                                   # tabPanel(title = "Fluorescence", value = "tabPanel_custom_tables_fluorescence",
                                                   #          withSpinner(
                                                   #            DT::dataTableOutput("custom_table_fluorescence")
                                                   #          )
                                                   # ),
                                                   tabPanel(title = "Fluorescence", value = "tabPanel_custom_tables_fluorescence_processed",
                                                            withSpinner(
                                                              DT::dataTableOutput("custom_table_fluorescence_processed")
                                                            ),
                                                            downloadButton('download_custom_tables_fluorescence_processed',"Download table")
                                                   ),
                                                   tabPanel(title = "Normalized fluorescence", value = "tabPanel_custom_tables_norm_fluorescence_processed",
                                                            withSpinner(
                                                              DT::dataTableOutput("custom_table_norm_fluorescence_processed")
                                                            ),
                                                            downloadButton('download_custom_tables_norm_fluorescence_processed',"Download table")
                                                   ),
                                                   # tabPanel(title = "Fluorescence 2", value = "tabPanel_custom_tables_fluorescence2",
                                                   #          withSpinner(
                                                   #            DT::dataTableOutput("custom_table_fluorescence2")
                                                   #          )
                                                   # ),
                                                   tabPanel(title = "Experimental Design", value = "tabPanel_custom_tables_expdesign",
                                                            DT::dataTableOutput('custom_data_table_expdesign'),
                                                            downloadButton('download_custom_tables_expdesign',"Download table")
                                                   ),

                                       )
                                   ),
                                   div(id = 'Parsed_Data_Tables',
                                       h1("Parsed Data"),
                                       tabsetPanel(type = "tabs", id = "tabsetPanel_parsed_tables",
                                                   tabPanel(title = "Growth plot", value = "tabPanel_parsed_plots_growth",
                                                            withSpinner(
                                                              plotOutput("parsed_raw_growth_plot",
                                                                         width = "100%", height = "1000px"),

                                                            )
                                                   ),
                                                   tabPanel(title = "Fluorescence plot", value = "tabPanel_parsed_plots_fluorescence",
                                                            withSpinner(
                                                              plotOutput("parsed_raw_fluorescence_plot",
                                                                         width = "100%", height = "1000px"),

                                                            )
                                                   ),
                                                   tabPanel(title = "norm. Fluorescence plot", value = "tabPanel_parsed_plots_norm_fluorescence",
                                                            withSpinner(
                                                              plotOutput("parsed_raw_norm_fluorescence_plot",
                                                                         width = "100%", height = "1000px"),

                                                            )
                                                   ),
                                                   tabPanel(title = "Growth", value = "tabPanel_parsed_tables_growth",
                                                            DT::dataTableOutput('parsed_data_table_growth'),
                                                            downloadButton('download_parsed_tables_growth',"Download table")
                                                   ),
                                                   tabPanel(title = "Fluorescence", value = "tabPanel_parsed_tables_fluorescence",
                                                            DT::dataTableOutput('parsed_data_table_fluorescence'),
                                                            downloadButton('download_parsed_tables_fluorescence',"Download table")
                                                   ),
                                                   tabPanel(title = "Normalized fluorescence", value = "tabPanel_parsed_tables_norm_fluorescence",
                                                            withSpinner(
                                                              DT::dataTableOutput("parsed_data_table_norm_fluorescence")
                                                            ),
                                                            downloadButton('download_parsed_data_table_norm_fluorescence',"Download table")
                                                   ),
                                                   # tabPanel(title = "Fluorescence 2", value = "tabPanel_parsed_tables_fluorescence2",
                                                   #          DT::dataTableOutput('parsed_data_table_fluorescence2')
                                                   # ),
                                                   tabPanel(title = "Experimental Design", value = "tabPanel_parsed_tables_expdesign",
                                                            DT::dataTableOutput('parsed_data_table_expdesign'),
                                                            downloadButton('download_parsed_tables_expdesign',"Download table")
                                                   )
                                       )
                                   )
                                 ) # main panel
                        ), # Navbar 1

                        #____COMPUTATION____####

                        navbarMenu(span("Computation", title = "Run a complete data analysis workflow."),
                                   menuName = "navbarMenu_Computation", icon=icon("gears"),

                                   ##____Computation_Growth____####

                                   tabPanel("Growth", value = "tabPanel_Computation_Growth",
                                            fluidRow(
                                              sidebarLayout(
                                                column(4,
                                                       sidebarPanel( width = 12,
                                                                     style='border-color: #ADADAD',
                                                                     wellPanel(
                                                                       style='padding: 1; border-color: #ADADAD; padding-top: 0; padding-bottom: 0',
                                                                       h2(strong('Growth fit')),
                                                                       h4('Global fit options'),
                                                                       tags$div(title="Perform linear regression on (log-transformed) growth data.",
                                                                                checkboxInput(inputId = 'linear_regression_growth',
                                                                                              label = 'Linear regression',
                                                                                              value = TRUE)
                                                                       ),

                                                                       tags$div(title="Fit a selection of growth models to the data.",
                                                                                checkboxInput(inputId = 'parametric_fit_growth',
                                                                                              label = 'Parametric fit',
                                                                                              value = FALSE)
                                                                       ),

                                                                       tags$div(title="Perform a nonparametric fit to the data using the smooth.spline() function.",
                                                                                checkboxInput(inputId = 'nonparametric_fit_growth',
                                                                                              label = 'Non-parametric fit',
                                                                                              value = TRUE)
                                                                       ),

                                                                       tags$div(title="Apply a ln(x+1) transformation to the time data for linear and nonparametric fits.",
                                                                                checkboxInput(inputId = 'log_transform_time_growth',
                                                                                              label = 'Log-transform time')
                                                                       ),

                                                                       tags$div(title="Only for linear and nonparametric fits:\nExtract growth parameters for two different growth phases (as observed with, e.g., diauxic shifts).",
                                                                                checkboxInput(inputId = 'biphasic_growth',
                                                                                              label = 'Biphasic growth')
                                                                       ),

                                                                       QurvE:::numberInput(
                                                                         inputId = 'growth_threshold_growth',
                                                                         label = 'Growth threshold',
                                                                         value = 1.5,
                                                                         min = NA,
                                                                         max = NA,
                                                                         placeholder = 1.5
                                                                       ),
                                                                       bsPopover(id = "growth_threshold_growth", title = HTML("<em>growth.thresh</em>"), content = "A sample will be considered to have no growth if no growth value is greater than [growth threshold] \\* start growth."),

                                                                       QurvE:::numberInput(
                                                                         inputId = 'minimum_growth_growth',
                                                                         label = 'Minimum growth measurement',
                                                                         value = 0,
                                                                         min = NA,
                                                                         max = NA,
                                                                         placeholder = 0
                                                                       ),
                                                                       bsPopover(id = "minimum_growth_growth", title = HTML("<em>min.growth</em>"), content = "Consider only growth values above [Minimum growth] for the fits."),

                                                                       QurvE:::numberInput(
                                                                         inputId = 'maximum_growth_growth',
                                                                         label = 'Maximum growth measurement',
                                                                         value = NULL,
                                                                         min = NA,
                                                                         max = NA
                                                                       ),
                                                                       bsPopover(id = "maximum_growth_growth", title = HTML("<em>max.growth</em>"), content = "Consider only growth values below and including [Maximum growth measurement] for linear and spline fits."),


                                                                       QurvE:::numberInput(
                                                                         inputId = 't0_growth',
                                                                         label = 't0',
                                                                         value = 0,
                                                                         min = NA,
                                                                         max = NA,
                                                                         placeholder = 0
                                                                       ),
                                                                       bsPopover(id = "t0_growth", title = HTML("<em>t0</em>"), content = "Consider only time values above [t0] for the fits."),

                                                                       QurvE:::numberInput(
                                                                         inputId = 'tmax_growth',
                                                                         label = 'tmax',
                                                                         value = NULL,
                                                                         min = NA,
                                                                         max = NA
                                                                       ),
                                                                       bsPopover(id = "tmax_growth", title = HTML("<em>tmax</em>"), content = "Consider only time values below and including [tmax] for linear and spline fits."),

                                                                     ), # Growth fit


                                                                     wellPanel(
                                                                       style='padding: 1; border-color: #ADADAD; padding-top: 0; padding-bottom: 0',
                                                                       h2(strong('Dose-response Analysis')),
                                                                       checkboxInput(inputId = 'perform_ec50_growth',
                                                                                     label = 'Perform EC50 Analysis',
                                                                                     value = FALSE),


                                                                       conditionalPanel(condition = "input.perform_ec50_growth",
                                                                                        selectInput(inputId = "dr_method_growth",
                                                                                                    label = "Method",
                                                                                                    choices = c("Dose-response models" = "model",
                                                                                                                "Response spline fit" = "spline")
                                                                                        ),
                                                                                        bsPopover(id = "dr_method_growth",
                                                                                                  title = HTML("<em>dr.method</em>"),
                                                                                                  placement = "right",
                                                                                                  content = "Fit either various dose-response models (Ritz et al., 2015) to response-vs.-concentration data and select the best model based on the lowest AIC, or apply a nonparametric (spline) fit.",
                                                                                                  trigger = "hover", options = list(container = "body", template = widePopover)
                                                                                                  ),

                                                                                        selectInput(inputId = "response_parameter_growth",
                                                                                                    label = "Response Parameter",
                                                                                                    choices = ""),
                                                                                        bsPopover(id = "response_parameter_growth", title = HTML("<em>dr.parameter</em>"), content = "Choose the response parameter to be used for creating a dose response curve.", placement = "top"),

                                                                                        conditionalPanel(
                                                                                          condition = 'input.dr_method_growth == "spline"',
                                                                                          tags$div(title="Perform a log(x+1) transformation on concentration values.",
                                                                                                   checkboxInput(inputId = 'log_transform_concentration_growth',
                                                                                                                 label = 'Log transform concentration')
                                                                                          ),

                                                                                          tags$div(title="Perform a log(y+1) transformation on response values.",
                                                                                                   checkboxInput(inputId = 'log_transform_response_growth',
                                                                                                                 label = 'Log transform response')
                                                                                          ),

                                                                                          textInput(
                                                                                            inputId = 'smoothing_factor_growth_dr',
                                                                                            label = 'Smoothing factor dose-response splines',
                                                                                            value = "",
                                                                                            placeholder = "NULL (choose automatically)"
                                                                                          ),
                                                                                          bsPopover(id = "smoothing_factor_growth_dr", title = HTML("<em>smooth.dr</em>"), content = "\\'spar\\' argument in the R function smooth.spline() used to create the dose response curve."),

                                                                                          QurvE:::numberInput(
                                                                                            inputId = 'number_of_bootstrappings_dr_growth',
                                                                                            label = 'Number of bootstrappings',
                                                                                            value = 0,
                                                                                            min = NA,
                                                                                            max = NA,
                                                                                            placeholder = 0
                                                                                          ),
                                                                                          bsPopover(id = "number_of_bootstrappings_dr_growth", title = HTML("<em>nboot.dr</em>"), content = "Optional: Define the number of bootstrap samples for EC50 estimation. Bootstrapping resamples the values in a dataset with replacement and performs a spline fit for each bootstrap sample to determine the EC50.")
                                                                                        ), #conditionalPanel(condition = 'input.dr_method_growth == "spline"')
                                                                                        fluidRow(
                                                                                          column(12,
                                                                                                 div(
                                                                                                   actionButton(inputId = "tooltip_growth_dr",
                                                                                                                label = "",
                                                                                                                icon=icon("question"),
                                                                                                                style="padding:2px; font-size:100%"),
                                                                                                   style="float:left")
                                                                                          )
                                                                                        ),
                                                                                        HTML("<br>"),

                                                                       ) # conditionalPanel(condition = "input.perform_ec50_growth"
                                                                     ), #  wellPanel
                                                                     fluidRow(
                                                                       column(12,
                                                                              div(
                                                                                actionButton(inputId = "run_growth",
                                                                                             label = "Run computation",
                                                                                             icon=icon("gears"),
                                                                                             style="padding:5px; font-size:120%"),
                                                                                style="float:right"),
                                                                              div(
                                                                                actionButton(inputId = "tooltip_growth_workflow",
                                                                                             label = "",
                                                                                             icon=icon("question"),
                                                                                             style="padding:2px; font-size:100%"),
                                                                                style="float:left")
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

                                                           tags$div(title="Perform a Ln(y/y0) transformation on growth values.",
                                                                    checkboxInput(inputId = 'log_transform_data_linear_growth',
                                                                                  label = 'Log-transform data',
                                                                                  value = TRUE)
                                                           ),
                                                           QurvE:::numberInput(
                                                             inputId = 'R2_threshold_growth',
                                                             label = 'R2 threshold',
                                                             value = 0.95,
                                                             placeholder = 0.95
                                                           ),
                                                           bsPopover(id = "R2_threshold_growth", title = HTML("<em>lin.R2</em>"), content = "R2 threshold for calculated slopes of linear regression windows to be considered for the maximum growth rate."),

                                                           QurvE:::numberInput(
                                                             inputId = 'RSD_threshold_growth',
                                                             label = 'RSD threshold',
                                                             value = 0.1,
                                                             placeholder = 0.1
                                                           ),
                                                           bsPopover(id = "RSD_threshold_growth", title = HTML("<em>lin.RSD</em>"), content = "Relative standard deviation (RSD) threshold for calculated slopes of linear regression windows to be considered for the maximum growth rate."),

                                                           QurvE:::numberInput(
                                                             inputId = 'dY_threshold_growth',
                                                             label = 'dY threshold',
                                                             value = 0.05,
                                                             placeholder = 0.05
                                                           ),
                                                           bsPopover(id = "dY_threshold_growth", title = HTML("<em>lin.dY</em>"), content = "Threshold for the minimum fraction of growth increase a linear regression window should cover to be considered."),

                                                           checkboxInput(inputId = 'custom_sliding_window_size_growth',
                                                                         label = 'Custom sliding window size',
                                                                         value = FALSE),

                                                           conditionalPanel(
                                                             condition = "input.custom_sliding_window_size_growth",
                                                             numericInput(
                                                               inputId = 'custom_sliding_window_size_value_growth',
                                                               label = NULL,
                                                               value = "NULL",
                                                               min = NA,
                                                               max = NA,
                                                             ),
                                                             bsPopover(id = "custom_sliding_window_size_value_growth", title = HTML("<em>lin.h</em>"), content = "If NULL, the sliding windows size (h) is chosen based on the number of data points within the growth phase (until maximum growth measurement)."),
                                                           ),
                                                           fluidRow(
                                                             column(12,
                                                                    div(
                                                                      actionButton(inputId = "tooltip_growth.gcFitLinear",
                                                                                   label = "",
                                                                                   icon=icon("question"),
                                                                                   style="padding:2px; font-size:100%"),
                                                                      style="float:left")
                                                             )
                                                           ),
                                                         ) # sidebarPanel
                                                       ), # conditionalPanel
                                                       conditionalPanel(
                                                         condition = "input.parametric_fit_growth",
                                                         sidebarPanel(
                                                           style='border-color: #ADADAD; padding-top: 0',
                                                           h3(strong('Parametric fit')),

                                                           tags$div(title="Perform a Ln(y/y0) transformation on growth values.",
                                                                    checkboxInput(inputId = 'log_transform_data_parametric_growth',
                                                                                  label = 'Log-transform data',
                                                                                  value = TRUE)
                                                           ),

                                                           wellPanel(
                                                             h4(strong('Models:')),
                                                             style='border-color: #ADADAD; padding: 1; padding-top: 0; padding-bottom: 0',

                                                             tags$div(title="Reference: Zwietering MH, Jongenburger I, Rombouts FM, van 't Riet K. Modeling of the bacterial growth curve. Appl Environ Microbiol. 1990 Jun;56(6):1875-81. doi: 10.1128/aem.56.6.1875-1881.1990",
                                                                      checkboxInput(inputId = 'logistic_growth',
                                                                                    label = 'logistic',
                                                                                    value = TRUE)
                                                             ),

                                                             tags$div(title="Reference: Zwietering MH, Jongenburger I, Rombouts FM, van 't Riet K. Modeling of the bacterial growth curve. Appl Environ Microbiol. 1990 Jun;56(6):1875-81. doi: 10.1128/aem.56.6.1875-1881.1990",
                                                                      checkboxInput(inputId = 'richards_growth',
                                                                                    label = 'Richards',
                                                                                    value = TRUE)
                                                             ),

                                                             tags$div(title="Reference: Zwietering MH, Jongenburger I, Rombouts FM, van 't Riet K. Modeling of the bacterial growth curve. Appl Environ Microbiol. 1990 Jun;56(6):1875-81. doi: 10.1128/aem.56.6.1875-1881.1990",
                                                                      checkboxInput(inputId = 'gompertz_growth',
                                                                                    label = 'Gompertz',
                                                                                    value = TRUE)
                                                             ),

                                                             tags$div(title="Reference: Kahm, M., Hasenbrink, G., Lichtenberg-Fraté, H., Ludwig, J., & Kschischo, M. (2010). grofit: Fitting Biological Growth Curves with R. Journal of Statistical Software, 33(7), 1–21. https://doi.org/10.18637/jss.v033.i07",
                                                                      checkboxInput(inputId = 'extended_gompertz_growth',
                                                                                    label = 'extended Gompertz',
                                                                                    value = TRUE)
                                                             ),

                                                             tags$div(title="Reference: Huang, Lihan (2011) A new mechanistic growth model for simultaneous determination of lag phase duration and exponential growth rate and a new Belehdradek-type model for evaluating the effect of temperature on growth rate. Food Microbiology 28, 770 – 776. doi: 10.1016/j.fm.2010.05.019",
                                                                      checkboxInput(inputId = 'huang_growth',
                                                                                    label = 'Huang',
                                                                                    value = TRUE)
                                                             ),
                                                             tags$div(title="Reference: Baranyi and Roberts (1994) Mathematics of predictive food microbiology. Food Microbiology 26(2), 199 – 218. doi: 10.1016/0168-1605(94)00121-L",
                                                                      checkboxInput(inputId = 'baranyi_growth',
                                                                                    label = 'Baranyi and Roberts',
                                                                                    value = TRUE)
                                                             )
                                                           ),

                                                           fluidRow(
                                                             column(12,
                                                                    div(
                                                                      actionButton(inputId = "tooltip_growth.gcFitModel",
                                                                                   label = "",
                                                                                   icon=icon("question"),
                                                                                   style="padding:2px; font-size:100%"),
                                                                      style="float:left")
                                                             )
                                                           ),

                                                         )
                                                       ),  # conditionalPanel

                                                       conditionalPanel(
                                                         condition = "input.nonparametric_fit_growth",
                                                         sidebarPanel(
                                                           width = 4,
                                                           style='border-color: #ADADAD; padding-top: 0',
                                                           h3(strong('Nonparametric fit')),
                                                           tags$div(title="Perform a Ln(y/y0) transformation on growth values.",
                                                                    checkboxInput(inputId = 'log_transform_data_nonparametric_growth',
                                                                                  label = 'Log-transform data',
                                                                                  value = TRUE)
                                                           ),

                                                           QurvE:::numberInput(
                                                             inputId = 'smoothing_factor_nonparametric_growth',
                                                             label = 'Smoothing factor',
                                                             value = 0.55,
                                                             min = NA,
                                                             max = NA,
                                                             placeholder = 0.55
                                                           ),
                                                           bsPopover(id = "smoothing_factor_nonparametric_growth", title = HTML("<em>smooth.gc</em>"), content = "\\'spar\\' argument within the R function smooth\\.spline\\(\\)."),


                                                           QurvE:::numberInput(
                                                             inputId = 'number_of_bootstrappings_growth',
                                                             label = 'Number of bootstrappings',
                                                             value = 0,
                                                             min = NA,
                                                             max = NA,
                                                             placeholder = 0
                                                           ),
                                                           bsPopover(id = "number_of_bootstrappings_growth", title = HTML("<em>nboot.gc</em>"), content = "Optional: Define the number of bootstrap samples. Bootstrapping resamples the values in a dataset with replacement and performs a spline fit for each bootstrap sample to yield a statistic distribution of growth parameters."),
                                                           fluidRow(
                                                             column(12,
                                                                    div(
                                                                      actionButton(inputId = "tooltip_growth.gcFitSpline",
                                                                                   label = "",
                                                                                   icon=icon("question"),
                                                                                   style="padding:2px; font-size:100%"),
                                                                      style="float:left")
                                                             )
                                                           ),
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
                                                                       style = 'padding: 1; border-color: #ADADAD; padding-top: 0; padding-bottom: 0',
                                                                       h2(strong('Fluorescence fit')),
                                                                       h4('Options'),
                                                                       tags$div(title="Perform linear regression on log-transformed growth data.",
                                                                                checkboxInput(
                                                                                  inputId = 'linear_regression_fluorescence',
                                                                                  label = 'linear regression',
                                                                                  value = TRUE
                                                                                )
                                                                       ),

                                                                       tags$div(title="Perform a nonparametric fit to the data using the smooth.spline() function.",
                                                                                checkboxInput(
                                                                                  inputId = 'nonparametric_fit_fluorescence',
                                                                                  label = 'nonparametric fit',
                                                                                  value = TRUE
                                                                                )
                                                                       ),

                                                                       tags$div(title="Extract kinetic parameters for two different phases (as observed with, e.g., regulator-promoter systems with varying response in different growth stages).",
                                                                                checkboxInput(inputId = 'biphasic_fluorescence',
                                                                                              label = 'Biphasic')
                                                                       ),

                                                                       selectInput(
                                                                         inputId = 'data_type_x_fluorescence',
                                                                         label = 'Independent variable (x)',
                                                                         choices = ""
                                                                       ),
                                                                       bsPopover(id = "data_type_x_fluorescence", title = HTML("<em>x_type</em>"), content = "Select the data type that is used as the independent variable for all fits."),

                                                                       conditionalPanel(
                                                                         condition = "input.data_type_x_fluorescence == 'time' && output.normalized_fl_present",
                                                                         tags$div(title="Use normalized fluorescence (divided by growth values) for all fits.",
                                                                                  checkboxInput(inputId = 'normalize_fluorescence',
                                                                                                label = 'Use normalized fluorescence'
                                                                                  )
                                                                         )
                                                                       ),

                                                                       conditionalPanel(
                                                                         condition = 'input.data_type_x_fluorescence.includes("growth")',
                                                                         QurvE:::numberInput(
                                                                           inputId = 'growth_threshold_in_percent_fluorescence',
                                                                           label = 'Growth threshold (in %)',
                                                                           value = 1.5,
                                                                           min = NA,
                                                                           max = NA,
                                                                           placeholder = 1.5
                                                                         ),
                                                                         bsPopover(id = "growth_threshold_in_percent_fluorescence", title = HTML("<em>growth.thresh</em>"), content = "A sample will be considered to have no growth if no growth value is greater than [growth threshold] \\* start growth."),
                                                                       ),

                                                                       conditionalPanel(
                                                                         condition = 'input.data_type_x_fluorescence.includes("growth")',
                                                                         QurvE:::numberInput(
                                                                           inputId = 'minimum_growth_fluorescence',
                                                                           label = 'Minimum growth measurement',
                                                                           value = 0,
                                                                           min = NA,
                                                                           max = NA,
                                                                           placeholder = 0
                                                                         ),
                                                                         bsPopover(id = "minimum_growth_fluorescence", title = HTML("<em>min.growth</em>"), content = "Consider only growth values above [Minimum growth measurement] for the fits."),
                                                                       ),

                                                                       conditionalPanel(
                                                                         condition = 'input.data_type_x_fluorescence.includes("growth")',
                                                                         QurvE:::numberInput(
                                                                           inputId = 'maximum_growth_fluorescence',
                                                                           label = 'Maximum growth measurement',
                                                                           value = NULL,
                                                                           min = NA,
                                                                           max = NA
                                                                         ),
                                                                         bsPopover(id = "maximum_growth_fluorescence", title = HTML("<em>max.growth</em>"), content = "Consider only growth values below and including [Maximum growth] for linear and spline fits."),
                                                                       ),

                                                                       conditionalPanel(
                                                                         condition = 'input.data_type_x_fluorescence.includes("time")',
                                                                         QurvE:::numberInput(
                                                                           inputId = 't0_fluorescence',
                                                                           label = 't0',
                                                                           value = 0,
                                                                           min = NA,
                                                                           max = NA,
                                                                           placeholder = 0
                                                                         )
                                                                       ),
                                                                       bsPopover(id = "t0_fluorescence", title = HTML("<em>t0</em>"), content = "Consider only time values above [t0] for the fits."),

                                                                       conditionalPanel(
                                                                         condition = 'input.data_type_x_fluorescence.includes("time")',
                                                                         QurvE:::numberInput(
                                                                           inputId = 'tmax_fluorescence',
                                                                           label = 'tmax',
                                                                           value = NULL,
                                                                           min = NA,
                                                                           max = NA
                                                                         ),
                                                                         bsPopover(id = "tmax_fluorescence", title = HTML("<em>tmax</em>"), content = "Consider only time values below and including [tmax] for linear and spline fits."),
                                                                       ),
                                                                     ), # wellPanel


                                                                     wellPanel(style='padding: 1; border-color: #ADADAD; padding-top: 0; padding-bottom: 0',

                                                                               h2(strong('Dose-response Analysis')),

                                                                               checkboxInput(inputId = 'perform_ec50_fluorescence',
                                                                                             label = 'Perform dose-response analysis',
                                                                                             value = FALSE),


                                                                               conditionalPanel(condition = 'input.perform_ec50_fluorescence',

                                                                                                selectInput(inputId = "dr_method_fluorescence",
                                                                                                            label = "Method",
                                                                                                            choices = c("Biosensor response model" = "model",
                                                                                                                        "Response spline fit" = "spline")
                                                                                                ),
                                                                                                bsPopover(id = "dr_method_fluorescence",
                                                                                                          placement = "right",
                                                                                                          title = HTML("<em>dr.method</em>"),
                                                                                                          content = "Fit either a biosensor response model (Meyer et al., 2019) to response-vs.-concentration data, or apply a nonparametric (spline) fit."
                                                                                                          ),

                                                                                                selectInput(inputId = "response_parameter_fluorescence",
                                                                                                            label = "Response Parameter",
                                                                                                            choices = ""),
                                                                                                bsPopover(id = "response_parameter_fluorescence", title = HTML("<em>dr.parameter</em>"), content = "Choose the response parameter to be used for creating a dose response curve.", placement = "top"),

                                                                                                tags$div(title="Perform a log(x+1) transformation on concentration values.",
                                                                                                         checkboxInput(inputId = 'log_transform_concentration_fluorescence',
                                                                                                                       label = 'log transform concentration')
                                                                                                ),

                                                                                                tags$div(title="Perform a log(x+1) transformation on response values.",
                                                                                                         checkboxInput(inputId = 'log_transform_response_fluorescence',
                                                                                                                       label = 'log transform response')
                                                                                                ),

                                                                                                conditionalPanel(
                                                                                                  condition = 'input.dr_method_fluorescence == "spline"',
                                                                                                  QurvE:::numberInput(
                                                                                                    inputId = 'number_of_bootstrappings_dr_fluorescence',
                                                                                                    label = 'Number of bootstrappings',
                                                                                                    value = 0,
                                                                                                    min = NA,
                                                                                                    max = NA,
                                                                                                    placeholder = 0
                                                                                                  ),
                                                                                                  bsPopover(id = "number_of_bootstrappings_dr_fluorescence", title = HTML("<em>nboot.dr</em>"), content = "Optional: Define the number of bootstrap samples for EC50 estimation. Bootstrapping resamples the values in a dataset with replacement and performs a spline fit for each bootstrap sample to determine the EC50."),
                                                                                                ),

                                                                                                conditionalPanel(
                                                                                                  condition = 'input.dr_method_fluorescence == "spline"',
                                                                                                  textInput(
                                                                                                    inputId = 'smoothing_factor_fluorescence_dr',
                                                                                                    label = 'Smoothing factor dose-response splines',
                                                                                                    value = "",
                                                                                                    placeholder = "NULL (choose automatically)"
                                                                                                  ),
                                                                                                  bsPopover(id = "smoothing_factor_fluorescence_dr", title = HTML("<em>smooth.dr</em>"), content = "\\'spar\\' argument in the R function smooth.spline() used to create the dose response curve."),
                                                                                                ),
                                                                                                fluidRow(
                                                                                                  column(12,
                                                                                                         div(
                                                                                                           actionButton(inputId = "tooltip_fl_dr",
                                                                                                                        label = "",
                                                                                                                        icon=icon("question"),
                                                                                                                        style="padding:2px; font-size:100%"),
                                                                                                           style="float:left")
                                                                                                  )
                                                                                                ),
                                                                                                HTML("<br>"),
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
                                                                                  style="float:right"),
                                                                                div(
                                                                                  actionButton(inputId = "tooltip_fl_workflow",
                                                                                               label = "",
                                                                                               icon=icon("question"),
                                                                                               style="padding:2px; font-size:100%"),
                                                                                  style="float:left")
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

                                                           QurvE:::numberInput(
                                                             inputId = 'R2_threshold_fluorescence',
                                                             label = 'R2 threshold',
                                                             value = 0.95,
                                                             min = NA,
                                                             max = NA,
                                                             placeholder = 0.95
                                                           ),
                                                           bsPopover(id = "R2_threshold_fluorescence", title = HTML("<em>lin.R2</em>"), content = "R2 threshold for calculated slopes of linear regression windows to be considered for the maximum slope."),

                                                           QurvE:::numberInput(
                                                             inputId = 'RSD_threshold_fluorescence',
                                                             label = 'RSD threshold',
                                                             value = 0.1,
                                                             min = NA,
                                                             max = NA,
                                                             placeholder = 0.1
                                                           ),
                                                           bsPopover(id = "RSD_threshold_fluorescence", title = HTML("<em>lin.RSD</em>"), content = "Relative standard deviation (RSD) threshold for calculated slopes of linear regression windows to be considered for the maximum slope."),

                                                           QurvE:::numberInput(
                                                             inputId = 'dY_threshold_fluorescence',
                                                             label = 'dY threshold',
                                                             value = 0.05,
                                                             min = NA,
                                                             max = NA,
                                                             placeholder = 0.05
                                                           ),
                                                           bsPopover(id = "dY_threshold_fluorescence", title = HTML("<em>lin.dY</em>"), content = "Threshold for the minimum fraction of fluorescence increase a linear regression window should cover to be considered."),

                                                           tags$div(title="Perform a Ln(y/y0) transformation on fluorescence values.",
                                                                    checkboxInput(inputId = 'log_transform_data_linear_fluorescence',
                                                                                  label = 'Log-transform fluorescence data')
                                                           ),

                                                           tags$div(title="Perform a Ln(y/y0) transformation on the independent variable",
                                                                    checkboxInput(inputId = 'log_transform_x_linear_fluorescence',
                                                                                  label = 'Log-transform x data')
                                                           ),

                                                           checkboxInput(inputId = 'custom_sliding_window_size_fluorescence',
                                                                         label = 'custom sliding window size',
                                                                         value = FALSE),

                                                           conditionalPanel(
                                                             condition = "input.custom_sliding_window_size_fluorescence",
                                                             numericInput(
                                                               inputId = 'custom_sliding_window_size_value_fluorescence',
                                                               label = NULL,
                                                               value = "NULL",
                                                               min = NA,
                                                               max = NA,
                                                             ),
                                                             bsPopover(id = "custom_sliding_window_size_value_fluorescence", title = HTML("<em>lin.h</em>"), content = "If NULL, the sliding windows size (h) is chosen based on the number of data points within the phase of fluorescence increase (until maximum fluorescence or growth)."),
                                                           ),
                                                           fluidRow(
                                                             column(12,
                                                                    div(
                                                                      actionButton(inputId = "tooltip_flFitLinear",
                                                                                   label = "",
                                                                                   icon=icon("question"),
                                                                                   style="padding:2px; font-size:100%"),
                                                                      style="float:left")
                                                             )
                                                           ),
                                                         )
                                                       ), # conditionalPanel

                                                       conditionalPanel(
                                                         condition = "input.nonparametric_fit_fluorescence",
                                                         sidebarPanel(
                                                           width = 4,
                                                           style='border-color: #ADADAD; padding-top: 0',
                                                           h3(strong('Nonparametric fit')),

                                                           QurvE:::numberInput(
                                                             inputId = 'smoothing_factor_nonparametric_fluorescence',
                                                             label = 'Smoothing factor',
                                                             value = 0.75,
                                                             min = NA,
                                                             max = NA,
                                                             placeholder = 0.75
                                                           ),
                                                           bsPopover(id = "smoothing_factor_nonparametric_fluorescence", title = HTML("<em>smooth.fl</em>"), content = "\\'spar\\' argument within the R function smooth\\.spline\\(\\)."),

                                                           QurvE:::numberInput(
                                                             inputId = 'number_of_bootstrappings_fluorescence',
                                                             label = 'Number of bootstrappings',
                                                             value = 0,
                                                             min = NA,
                                                             max = NA,
                                                             placeholder = 0
                                                           ),
                                                           bsPopover(id = "number_of_bootstrappings_fluorescence", title = HTML("<em>nboot.fl</em>"), content = "Optional: Define the number of bootstrap samples. Bootstrapping resamples the values in a dataset with replacement and performs a spline fit for each bootstrap sample to yield a statistic distribution of growth parameters."),

                                                           tags$div(title="Perform a Ln(y/y0) transformation on fluorescence values.",
                                                                    checkboxInput(inputId = 'log_transform_data_nonparametric_fluorescence',
                                                                                  label = 'Log-transform fluorescence data')
                                                           ),

                                                           tags$div(title="Perform a Ln(y/y0) transformation on the independent variable",
                                                                    checkboxInput(inputId = 'log_transform_x_nonparametric_fluorescence',
                                                                                  label = 'Log-transform x data')
                                                           ),
                                                           fluidRow(
                                                             column(12,
                                                                    div(
                                                                      actionButton(inputId = "tooltip_flFitSpline",
                                                                                   label = "",
                                                                                   icon=icon("question"),
                                                                                   style="padding:2px; font-size:100%"),
                                                                      style="float:left")
                                                             )
                                                           ),
                                                         )
                                                       )  # conditionalPanel
                                                ) # column
                                              ) # sidebarLayout
                                            ) # fluidRow
                                   ), # tabPanel("Fluorescence"
                        ), # navbarMenu('Computation'

                        #____VALIDATE____####
                        navbarMenu(span("Validation", title = "Graphical display for each fit."),
                                   menuName = "navbarMenu_Validate", icon = icon("user-check"),
                                   ##____Validate_Growth____####
                                   tabPanel(title = "Growth Fits", value = "tabPanel_Validate_Growth",
                                            h1("Growth Fits"),
                                            tabsetPanel(type = "tabs", id = "tabsetPanel_Validate_Growth",
                                                        ###___Linear Fits___####
                                                        tabPanel(title = "Linear Fits", value = "tabPanel_Validate_Growth_Linear",
                                                                 sidebarPanel(width = 5,
                                                                              selectizeInput(inputId = "sample_validate_growth_linear",
                                                                                             label = "Sample:",
                                                                                             width = "100%",
                                                                                             choices = "",
                                                                                             multiple = FALSE,
                                                                                             options = list(closeAfterSelect = FALSE)
                                                                              ),
                                                                              checkboxInput(inputId = 'logy_validate_growth_plot_linear',
                                                                                            label = 'Log-transform y axis',
                                                                                            value = TRUE),
                                                                              checkboxInput(inputId = 'diagnostics_validate_growth_plot_linear',
                                                                                            label = 'Show diagnostics',
                                                                                            value = FALSE),

                                                                              h3('Customize plot appearance'),


                                                                              sliderInput(inputId = 'shape_type_validate_growth_plot_linear',
                                                                                          label = 'Shape type',
                                                                                          min = 1,
                                                                                          max = 25,
                                                                                          value = 21),

                                                                              sliderInput(inputId = 'shape_size_validate_growth_plot_linear',
                                                                                          label = 'Shape size',
                                                                                          min = 0.1,
                                                                                          max = 10,
                                                                                          value = 2,
                                                                                          step = 0.1),



                                                                              sliderInput(inputId = 'axis_size_validate_growth_plot_linear',
                                                                                          label = 'Axis title font size',
                                                                                          min = 0.1,
                                                                                          max = 10,
                                                                                          value = 1.9,
                                                                                          step = 0.1),

                                                                              sliderInput(inputId = 'lab_size_validate_growth_plot_linear',
                                                                                          label = 'Axis label font size',
                                                                                          min = 0.1,
                                                                                          max = 10,
                                                                                          value = 1.7,
                                                                                          step = 0.1),

                                                                              sliderInput(inputId = 'line_width_validate_growth_plot_linear',
                                                                                          label = 'Line width',
                                                                                          min = 0.01,
                                                                                          max = 10,
                                                                                          value = 3),


                                                                              strong("x-Range"),
                                                                              fluidRow(
                                                                                column(5,
                                                                                       textInput(inputId = "x_range_min_validate_growth_plot_linear",
                                                                                                 label = NULL,
                                                                                                 value = "", placeholder = "min"
                                                                                       )
                                                                                ),

                                                                                column(5,
                                                                                       textInput(inputId = "x_range_max_validate_growth_plot_linear",
                                                                                                 label = NULL,
                                                                                                 value = "", placeholder = "max"
                                                                                       )
                                                                                )
                                                                              ),

                                                                              strong("y-Range"),
                                                                              fluidRow(
                                                                                column(5,
                                                                                       textInput(inputId = "y_range_min_validate_growth_plot_linear",
                                                                                                 label = NULL,
                                                                                                 value = "", placeholder = "min"
                                                                                       )
                                                                                ),

                                                                                column(5,
                                                                                       textInput(inputId = "y_range_max_validate_growth_plot_linear",
                                                                                                 label = NULL,
                                                                                                 value = "", placeholder = "max"
                                                                                       )
                                                                                )
                                                                              ),

                                                                              textInput(
                                                                                inputId = 'color_validate_growth_plot_linear',
                                                                                label = 'Change color',
                                                                                value = "firebrick3"
                                                                              ),
                                                                              bsPopover(id = "color_validate_growth_plot_linear",
                                                                                        title = HTML("<em>Define the colors used to highlight data points used in linear regression and determined slope</em>"), placement = "top",
                                                                                        content = "Enter color either by name (e.g., red, blue, coral3) or via their hexadecimal code (e.g., #AE4371, #CCFF00FF, #0066FFFF). A full list of colors available by name can be found at http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf"
                                                                              ),

                                                                 ),

                                                                 mainPanel(width = 7,
                                                                           plotOutput("validate_growth_plot_linear", width = "100%", height = "600px"),
                                                                           HTML("<br>"),
                                                                           HTML("<br>"),
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
                                                                           ),

                                                                           HTML("<br>"),
                                                                           h3(strong("Export plot")),

                                                                           fluidRow(
                                                                             column(width = 4,
                                                                                    numericInput(inputId = "width_download_growth_validate_linear",
                                                                                                 label = "Width (in inches)",
                                                                                                 value = 10)
                                                                             ), # column
                                                                             column(width = 4,
                                                                                    numericInput(inputId = "height_download_growth_validate_linear",
                                                                                                 label = "Height (in inches)",
                                                                                                 value = 9)
                                                                             ), # column
                                                                             column(width = 4,
                                                                                    numericInput(inputId = "dpi_download_growth_validate_linear",
                                                                                                 label = "DPI",
                                                                                                 value = 300)
                                                                             ), # column
                                                                             column(width = 4,
                                                                                    downloadButton('download_growth_validate_linear',"Download Plot"),
                                                                                    radioButtons("format_download_growth_validate_linear",
                                                                                                 label = NULL,
                                                                                                 choices = c("PNG" = ".png",
                                                                                                             "PDF" = ".pdf"),
                                                                                                 selected = ".png",
                                                                                                 inline = TRUE)
                                                                             ) # column
                                                                           ) # fluidRow
                                                                 ) #mainPanel

                                                        ), #tabPanel(title = "Linear Fits", value = "tabPanel_Validate_Growth_linearFits",
                                                        ###___Spline Fits___####
                                                        tabPanel(title = "Nonparametric fits", value = "tabPanel_Validate_Growth_Spline",
                                                                 sidebarPanel(width = 5,
                                                                              selectizeInput(inputId = "sample_validate_growth_spline",
                                                                                             label = "Sample:",
                                                                                             width = "100%",
                                                                                             choices = "",
                                                                                             multiple = FALSE,
                                                                                             options = list(closeAfterSelect = FALSE)
                                                                              ),

                                                                              sliderInput(inputId = 'shape_type_validate_growth_plot_spline',
                                                                                          label = 'Shape type',
                                                                                          min = 1,
                                                                                          max = 25,
                                                                                          value = 21),

                                                                              checkboxInput(inputId = 'logy_validate_growth_plot_spline',
                                                                                            label = 'Log-transform y axis',
                                                                                            value = TRUE),

                                                                              checkboxInput(inputId = "plot_derivative_validate_growth_plot_spline",
                                                                                            label = "Plot derivative",
                                                                                            value = TRUE),

                                                                              strong("x-Range"),
                                                                              fluidRow(
                                                                                column(5,
                                                                                       textInput(inputId = "x_range_min_validate_growth_plot_spline",
                                                                                                 label = NULL,
                                                                                                 value = "", placeholder = "min"
                                                                                       )
                                                                                ),

                                                                                column(5,
                                                                                       textInput(inputId = "x_range_max_validate_growth_plot_spline",
                                                                                                 label = NULL,
                                                                                                 value = "", placeholder = "max"
                                                                                       )
                                                                                )
                                                                              ),

                                                                              strong("y-Range"),
                                                                              fluidRow(
                                                                                column(5,
                                                                                       textInput(inputId = "y_range_min_validate_growth_plot_spline",
                                                                                                 label = NULL,
                                                                                                 value = "", placeholder = "min"
                                                                                       )
                                                                                ),

                                                                                column(5,
                                                                                       textInput(inputId = "y_range_max_validate_growth_plot_spline",
                                                                                                 label = NULL,
                                                                                                 value = "", placeholder = "max"
                                                                                       )
                                                                                )
                                                                              ),

                                                                              conditionalPanel(
                                                                                condition = "input.logy_validate_growth_plot_spline",
                                                                                strong("y-Range (derivative)"),
                                                                                fluidRow(
                                                                                  column(5,
                                                                                         textInput(inputId = "y_range_min_derivative_validate_growth_plot_spline",
                                                                                                   label = NULL,
                                                                                                   value = "", placeholder = "min"
                                                                                         )
                                                                                  ),

                                                                                  column(5,
                                                                                         textInput(inputId = "y_range_max_derivative_validate_growth_plot_spline",
                                                                                                   label = NULL,
                                                                                                   value = "", placeholder = "max"
                                                                                         )
                                                                                  )
                                                                                )
                                                                              ),

                                                                              sliderInput(inputId = 'shape_size_validate_growth_plot_spline',
                                                                                          label = 'Shape size',
                                                                                          min = 0.1,
                                                                                          max = 10,
                                                                                          value = 2,
                                                                                          step = 0.1),

                                                                              sliderInput(inputId = "line_width_validate_growth_plot_spline",
                                                                                          label = "Line width",
                                                                                          min = 0.01,
                                                                                          max = 10,
                                                                                          value = 1),

                                                                              sliderInput(inputId = 'base_size_validate_growth_plot_spline',
                                                                                          label = 'Base font size',
                                                                                          min = 10,
                                                                                          max = 35,
                                                                                          value = 23,
                                                                                          step = 0.5),

                                                                              sliderInput(inputId = "nbreaks_validate_growth_plot_spline",
                                                                                          label = "Number of breaks on y-axis",
                                                                                          min = 1,
                                                                                          max = 20,
                                                                                          value = 6),

                                                                              textInput(
                                                                                inputId = 'color_validate_growth_plot_spline',
                                                                                label = 'Change color',
                                                                                value = "dodgerblue3"
                                                                              ),
                                                                              bsPopover(id = "color_validate_growth_plot_spline",
                                                                                        title = HTML("<em>Define the colors used to highlight data points used in linear regression and determined slope</em>"), placement = "top",
                                                                                        content = "Enter color either by name (e.g., red, blue, coral3) or via their hexadecimal code (e.g., #AE4371, #CCFF00FF, #0066FFFF). A full list of colors available by name can be found at http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf"
                                                                              ),

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
                                                                           ),

                                                                           HTML("<br>"),

                                                                           h3(strong("Export plot")),

                                                                           fluidRow(
                                                                             column(width = 4,
                                                                                    numericInput(inputId = "width_download_growth_validate_spline",
                                                                                                 label = "Width (in inches)",
                                                                                                 value = 10)
                                                                             ), # column
                                                                             column(width = 4,
                                                                                    numericInput(inputId = "height_download_growth_validate_spline",
                                                                                                 label = "Height (in inches)",
                                                                                                 value = 9)
                                                                             ), # column
                                                                             column(width = 4,
                                                                                    numericInput(inputId = "dpi_download_growth_validate_spline",
                                                                                                 label = "DPI",
                                                                                                 value = 300)
                                                                             ), # column
                                                                             column(width = 4,
                                                                                    downloadButton('download_growth_validate_spline',"Download Plot"),

                                                                                    radioButtons("format_download_growth_validate_spline",
                                                                                                 label = NULL,
                                                                                                 choices = c("PNG" = ".png",
                                                                                                             "PDF" = ".pdf"),
                                                                                                 selected = ".png",
                                                                                                 inline = TRUE)
                                                                             ) # column
                                                                           ), # fluidRow

                                                                           h3(strong("Export spline values")),
                                                                           fluidRow(
                                                                             column(width = 4,
                                                                                    downloadButton('download_growth_validate_spline_values',"Download value table (x-y)")
                                                                             ),
                                                                             column(width = 4,
                                                                                    downloadButton('download_growth_validate_spline_deriv_values',"Download derivative value table (x-y)")
                                                                             ),
                                                                           )

                                                                 ) # mainPanel

                                                        ), # tabPanel(title = "Nonparametric fits", value = "tabPanel_Validate_Growth_splineFits",
                                                        ###___Model Fits___####
                                                        tabPanel(title = "Parametric fits", value = "tabPanel_Validate_Growth_Model",
                                                                 sidebarPanel(width = 5,
                                                                              wellPanel(
                                                                                style='padding: 1; padding-top: 0; padding-bottom: 0',
                                                                                selectizeInput(inputId = "sample_validate_growth_model",
                                                                                               label = "Sample:",
                                                                                               width = "100%",
                                                                                               choices = "",
                                                                                               multiple = FALSE,
                                                                                               options = list(closeAfterSelect = FALSE)
                                                                                ),
                                                                              ),
                                                                              sliderInput(inputId = 'shape_type_validate_growth_plot_model',
                                                                                          label = 'Shape type',
                                                                                          min = 1,
                                                                                          max = 25,
                                                                                          value = 21),
                                                                              sliderInput(inputId = 'shape_size_validate_growth_plot_model',
                                                                                          label = 'Shape size',
                                                                                          min = 0.1,
                                                                                          max = 10,
                                                                                          value = 2,
                                                                                          step = 0.1),

                                                                              sliderInput(inputId = "line_width_validate_growth_plot_model",
                                                                                          label = "Line width",
                                                                                          min = 0.01,
                                                                                          max = 10,
                                                                                          value = 1),

                                                                              sliderInput(inputId = 'base_size_validate_growth_plot_model',
                                                                                          label = 'Base font size',
                                                                                          min = 10,
                                                                                          max = 35,
                                                                                          value = 23,
                                                                                          step = 0.5),

                                                                              sliderInput(inputId = "nbreaks_validate_growth_plot_model",
                                                                                          label = "Number of breaks on y-axis",
                                                                                          min = 1,
                                                                                          max = 20,
                                                                                          value = 6),

                                                                              sliderInput(inputId = "eqsize_validate_growth_plot_model",
                                                                                          label = "Equation font size",
                                                                                          min = 0.1,
                                                                                          max = 10,
                                                                                          step = 0.1,
                                                                                          value = 1.9),

                                                                              textInput(
                                                                                inputId = 'color_validate_growth_plot_model',
                                                                                label = 'Change color',
                                                                                value = "forestgreen"
                                                                              ),
                                                                              bsPopover(id = "color_validate_growth_plot_model",
                                                                                        title = HTML("<em>Define the colors used to highlight data points used in linear regression and determined slope</em>"), placement = "top",
                                                                                        content = "Enter color either by name (e.g., red, blue, coral3) or via their hexadecimal code (e.g., #AE4371, #CCFF00FF, #0066FFFF). A full list of colors available by name can be found at http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf"
                                                                              ),

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
                                                                           ),

                                                                           HTML("<br>"),

                                                                           h3(strong("Export plot")),

                                                                           fluidRow(
                                                                             column(width = 4,
                                                                                    numericInput(inputId = "width_download_growth_validate_model",
                                                                                                 label = "Width (in inches)",
                                                                                                 value = 10)
                                                                             ), # column
                                                                             column(width = 4,
                                                                                    numericInput(inputId = "height_download_growth_validate_model",
                                                                                                 label = "Height (in inches)",
                                                                                                 value = 9)
                                                                             ), # column
                                                                             column(width = 4,
                                                                                    numericInput(inputId = "dpi_download_growth_validate_model",
                                                                                                 label = "DPI",
                                                                                                 value = 300)
                                                                             ), # column
                                                                             column(width = 4,
                                                                                    downloadButton('download_growth_validate_model',"Download Plot"),

                                                                                    radioButtons("format_download_growth_validate_model",
                                                                                                 label = NULL,
                                                                                                 choices = c("PNG" = ".png",
                                                                                                             "PDF" = ".pdf"),
                                                                                                 selected = ".png",
                                                                                                 inline = TRUE)
                                                                             ) # column
                                                                           ) # fluidRow
                                                                 ) # mainPanel
                                                        ), # tabPanel(title = "Parametric fits", value = "tabPanel_Validate_Growth_modelFits",
                                                        ### Growth Boostrapping Spline Plots ####
                                                        tabPanel(title = "Bootstrapping Spline", value = "tabPanel_Validate_Growth_Spline_bt",
                                                                 sidebarPanel(width = 4,
                                                                              selectizeInput(inputId = "sample_validate_growth_spline_bt",
                                                                                             label = "Sample:",
                                                                                             width = "100%",
                                                                                             choices = "",
                                                                                             multiple = FALSE,
                                                                                             options = list(closeAfterSelect = FALSE)
                                                                              ),

                                                                              checkboxInput(inputId = "plot_derivative_growth_spline_bt",
                                                                                            label = "Plot derivative",
                                                                                            value = TRUE),

                                                                              h3('Customize plot appearance'),


                                                                              sliderInput(inputId = 'shape_type_validate_growth_spline_bt',
                                                                                          label = 'Shape type',
                                                                                          min = 1,
                                                                                          max = 25,
                                                                                          value = 1),

                                                                              sliderInput(inputId = 'shape_size_validate_growth_spline_bt',
                                                                                          label = 'Shape size',
                                                                                          min = 0.1,
                                                                                          max = 10,
                                                                                          value = 2,
                                                                                          step = 0.1),



                                                                              sliderInput(inputId = 'axis_size_validate_growth_spline_bt',
                                                                                          label = 'Axis title font size',
                                                                                          min = 0.1,
                                                                                          max = 10,
                                                                                          value = 1.9,
                                                                                          step = 0.1),

                                                                              sliderInput(inputId = 'lab_size_validate_growth_spline_bt',
                                                                                          label = 'Axis label font size',
                                                                                          min = 0.1,
                                                                                          max = 10,
                                                                                          value = 1.7,
                                                                                          step = 0.1),

                                                                              sliderInput(inputId = 'line_width_validate_growth_spline_bt',
                                                                                          label = 'Line width',
                                                                                          min = 0.01,
                                                                                          max = 10,
                                                                                          value = 0.5),


                                                                              strong("x-Range"),
                                                                              fluidRow(
                                                                                column(5,
                                                                                       textInput(inputId = "x_range_min_validate_growth_spline_bt",
                                                                                                 label = NULL,
                                                                                                 value = "", placeholder = "min"
                                                                                       )
                                                                                ),

                                                                                column(5,
                                                                                       textInput(inputId = "x_range_max_validate_growth_spline_bt",
                                                                                                 label = NULL,
                                                                                                 value = "", placeholder = "max"
                                                                                       )
                                                                                )
                                                                              ),

                                                                              strong("y-Range"),
                                                                              fluidRow(
                                                                                column(5,
                                                                                       textInput(inputId = "y_range_min_validate_growth_spline_bt",
                                                                                                 label = NULL,
                                                                                                 value = "", placeholder = "min"
                                                                                       )
                                                                                ),

                                                                                column(5,
                                                                                       textInput(inputId = "y_range_max_validate_growth_spline_bt",
                                                                                                 label = NULL,
                                                                                                 value = "", placeholder = "max"
                                                                                       )
                                                                                )
                                                                              ),
                                                                              strong("y-Range (derivative)"),
                                                                              fluidRow(
                                                                                column(5,
                                                                                       textInput(inputId = "y_range_min_derivative_validate_growth_spline_bt",
                                                                                                 label = NULL,
                                                                                                 value = "", placeholder = "min"
                                                                                       )
                                                                                ),

                                                                                column(5,
                                                                                       textInput(inputId = "y_range_max_derivative_validate_growth_spline_bt",
                                                                                                 label = NULL,
                                                                                                 value = "", placeholder = "max"
                                                                                       )
                                                                                ),

                                                                                textInput(
                                                                                  inputId = 'color_validate_growth_plot_spline_bt',
                                                                                  label = 'Change color',
                                                                                  value = "dodgerblue3"
                                                                                ),
                                                                                bsPopover(id = "color_validate_growth_plot_spline_bt",
                                                                                          title = HTML("<em>Define the colors used to highlight data points used in linear regression and determined slope</em>"), placement = "top",
                                                                                          content = "Enter color either by name (e.g., red, blue, coral3) or via their hexadecimal code (e.g., #AE4371, #CCFF00FF, #0066FFFF). A full list of colors available by name can be found at http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf"
                                                                                ),
                                                                              ),

                                                                 ), # sidebarPanel

                                                                 mainPanel(width = 8,

                                                                           plotOutput("validate_growth_plot_spline_bt",
                                                                                      width = "100%", height = "1000px"),

                                                                           HTML("<br>"),
                                                                           h3(strong("Export plot")),

                                                                           fluidRow(
                                                                             column(width = 4,
                                                                                    numericInput(inputId = "width_download_growth_validate_spline_bt",
                                                                                                 label = "Width (in inches)",
                                                                                                 value = 10)
                                                                             ), # column
                                                                             column(width = 4,
                                                                                    numericInput(inputId = "height_download_growth_validate_spline_bt",
                                                                                                 label = "Height (in inches)",
                                                                                                 value = 9)
                                                                             ), # column
                                                                             column(width = 4,
                                                                                    numericInput(inputId = "dpi_download_growth_validate_spline_bt",
                                                                                                 label = "DPI",
                                                                                                 value = 300)
                                                                             ), # column
                                                                             column(width = 4,
                                                                                    downloadButton('download_growth_validate_spline_bt',"Download Plot"),
                                                                                    radioButtons("format_download_growth_validate_spline_bt",
                                                                                                 label = NULL,
                                                                                                 choices = c("PNG" = ".png",
                                                                                                             "PDF" = ".pdf"),
                                                                                                 selected = ".png",
                                                                                                 inline = TRUE)
                                                                             ) # column
                                                                           ) # fluidRow
                                                                 ) # mainPanel
                                                        ) # tabPanel(title = "Bootstrapping Spline"
                                            ) # tabsetPanel(type = "tabs",
                                   ), # tabPanel(title = "Growth Fits", value = "tabPanel_Validate_Growth",
                                   ##____Validate_Fluorescence____####
                                   tabPanel(title = "Fluorescence Fits", value = "tabPanel_Validate_Fluorescence",
                                            h1("Fluorescence Fits"),
                                            tabsetPanel(type = "tabs", id = "tabsetPanel_Validate_Fluorescence",
                                                        ###___Linear Fits___####
                                                        tabPanel(title = "Linear Fits", value = "tabPanel_Validate_Fluorescence_Linear",
                                                                 sidebarPanel(width = 5,
                                                                              selectizeInput(inputId = "sample_validate_fluorescence_linear",
                                                                                             label = "Sample:",
                                                                                             width = "100%",
                                                                                             choices = "",
                                                                                             multiple = FALSE,
                                                                                             options = list(closeAfterSelect = FALSE)
                                                                              ),
                                                                              checkboxInput(inputId = 'logy_validate_fluorescence_plot_linear',
                                                                                            label = 'Log-transform y axis',
                                                                                            value = FALSE),
                                                                              checkboxInput(inputId = 'diagnostics_validate_fluorescence_plot_linear',
                                                                                            label = 'Show diagnostics',
                                                                                            value = FALSE),

                                                                              h3('Customize plot appearance'),


                                                                              sliderInput(inputId = 'shape_type_validate_fluorescence_plot_linear',
                                                                                          label = 'Shape type',
                                                                                          min = 1,
                                                                                          max = 25,
                                                                                          value = 21),

                                                                              sliderInput(inputId = 'shape_size_validate_fluorescence_plot_linear',
                                                                                          label = 'Shape size',
                                                                                          min = 0.1,
                                                                                          max = 10,
                                                                                          value = 2,
                                                                                          step = 0.1),



                                                                              sliderInput(inputId = 'axis_size_validate_fluorescence_plot_linear',
                                                                                          label = 'Axis title font size',
                                                                                          min = 0.1,
                                                                                          max = 10,
                                                                                          value = 2,
                                                                                          step = 0.1),

                                                                              sliderInput(inputId = 'lab_size_validate_fluorescence_plot_linear',
                                                                                          label = 'Axis label font size',
                                                                                          min = 0.1,
                                                                                          max = 10,
                                                                                          value = 1.8,
                                                                                          step = 0.1),

                                                                              sliderInput(inputId = 'line_width_validate_fluorescence_plot_linear',
                                                                                          label = 'Line width',
                                                                                          min = 0.01,
                                                                                          max = 10,
                                                                                          value = 3),


                                                                              strong("x-Range"),
                                                                              fluidRow(
                                                                                column(5,
                                                                                       textInput(inputId = "x_range_min_validate_fluorescence_plot_linear",
                                                                                                 label = NULL,
                                                                                                 value = "", placeholder = "min"
                                                                                       )
                                                                                ),

                                                                                column(5,
                                                                                       textInput(inputId = "x_range_max_validate_fluorescence_plot_linear",
                                                                                                 label = NULL,
                                                                                                 value = "", placeholder = "max"
                                                                                       )
                                                                                )
                                                                              ),

                                                                              strong("y-Range"),
                                                                              fluidRow(
                                                                                column(5,
                                                                                       textInput(inputId = "y_range_min_validate_fluorescence_plot_linear",
                                                                                                 label = NULL,
                                                                                                 value = "", placeholder = "min"
                                                                                       )
                                                                                ),

                                                                                column(5,
                                                                                       textInput(inputId = "y_range_max_validate_fluorescence_plot_linear",
                                                                                                 label = NULL,
                                                                                                 value = "", placeholder = "max"
                                                                                       )
                                                                                )
                                                                              ),

                                                                              textInput(
                                                                                inputId = 'color_validate_fluorescence_plot_linear',
                                                                                label = 'Change color',
                                                                                value = "firebrick3"
                                                                              ),
                                                                              bsPopover(id = "color_validate_fluorescence_plot_linear",
                                                                                        title = HTML("<em>Define the colors used to highlight data points used in linear regression and determined slope</em>"), placement = "top",
                                                                                        content = "Enter color either by name (e.g., red, blue, coral3) or via their hexadecimal code (e.g., #AE4371, #CCFF00FF, #0066FFFF). A full list of colors available by name can be found at http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf"
                                                                              ),

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
                                                                           ),
                                                                           HTML("<br>"),
                                                                           h3(strong("Export plot")),

                                                                           fluidRow(
                                                                             column(width = 4,
                                                                                    numericInput(inputId = "width_download_fluorescence_validate_linear",
                                                                                                 label = "Width (in inches)",
                                                                                                 value = 10)
                                                                             ), # column
                                                                             column(width = 4,
                                                                                    numericInput(inputId = "height_download_fluorescence_validate_linear",
                                                                                                 label = "Height (in inches)",
                                                                                                 value = 9)
                                                                             ), # column
                                                                             column(width = 4,
                                                                                    numericInput(inputId = "dpi_download_fluorescence_validate_linear",
                                                                                                 label = "DPI",
                                                                                                 value = 300)
                                                                             ), # column
                                                                             column(width = 4,
                                                                                    downloadButton('download_fluorescence_validate_linear',"Download Plot"),
                                                                                    radioButtons("format_download_fluorescence_validate_linear",
                                                                                                 label = NULL,
                                                                                                 choices = c("PNG" = ".png",
                                                                                                             "PDF" = ".pdf"),
                                                                                                 selected = ".png",
                                                                                                 inline = TRUE)
                                                                             ) # column
                                                                           ) # fluidRow
                                                                 )

                                                        ),
                                                        ###___Spline Fits___####
                                                        tabPanel(title = "Nonparametric fits", value = "tabPanel_Validate_Fluorescence_Spline",
                                                                 sidebarPanel(width = 5,
                                                                              selectizeInput(inputId = "sample_validate_fluorescence_spline",
                                                                                             label = "Sample:",
                                                                                             width = "100%",
                                                                                             choices = "",
                                                                                             multiple = FALSE,
                                                                                             options = list(closeAfterSelect = FALSE)
                                                                              ),
                                                                              checkboxInput(inputId = 'logy_validate_fluorescence_plot_spline',
                                                                                            label = 'Log-transform y axis',
                                                                                            value = FALSE),

                                                                              checkboxInput(inputId = "plot_derivative_validate_fluorescence_plot_spline",
                                                                                            label = "Plot derivative",
                                                                                            value = TRUE),

                                                                              sliderInput(inputId = 'shape_type_validate_fluorescence_plot_spline',
                                                                                          label = 'Shape type',
                                                                                          min = 1,
                                                                                          max = 25,
                                                                                          value = 21),

                                                                              strong("x-Range"),
                                                                              fluidRow(
                                                                                column(5,
                                                                                       textInput(inputId = "x_range_min_validate_fluorescence_plot_spline",
                                                                                                 label = NULL,
                                                                                                 value = "", placeholder = "min"
                                                                                       )
                                                                                ),

                                                                                column(5,
                                                                                       textInput(inputId = "x_range_max_validate_fluorescence_plot_spline",
                                                                                                 label = NULL,
                                                                                                 value = "", placeholder = "max"
                                                                                       )
                                                                                )
                                                                              ),

                                                                              strong("y-Range"),
                                                                              fluidRow(
                                                                                column(5,
                                                                                       textInput(inputId = "y_range_min_validate_fluorescence_plot_spline",
                                                                                                 label = NULL,
                                                                                                 value = "", placeholder = "min"
                                                                                       )
                                                                                ),

                                                                                column(5,
                                                                                       textInput(inputId = "y_range_max_validate_fluorescence_plot_spline",
                                                                                                 label = NULL,
                                                                                                 value = "", placeholder = "max"
                                                                                       )
                                                                                )
                                                                              ),

                                                                              conditionalPanel(
                                                                                condition = "input.logy_validate_growth_plot_spline",
                                                                                strong("y-Range (derivative)"),
                                                                                fluidRow(
                                                                                  column(5,
                                                                                         textInput(inputId = "y_range_min_derivative_validate_fluorescence_plot_spline",
                                                                                                   label = NULL,
                                                                                                   value = "", placeholder = "min"
                                                                                         )
                                                                                  ),

                                                                                  column(5,
                                                                                         textInput(inputId = "y_range_max_derivative_validate_fluorescence_plot_spline",
                                                                                                   label = NULL,
                                                                                                   value = "", placeholder = "max"
                                                                                         )
                                                                                  )
                                                                                )
                                                                              ),

                                                                              sliderInput(inputId = 'shape_size_validate_fluorescence_plot_spline',
                                                                                          label = 'Shape size',
                                                                                          min = 0.1,
                                                                                          max = 10,
                                                                                          value = 2,
                                                                                          step = 0.1),

                                                                              sliderInput(inputId = "line_width_validate_fluorescence_plot_spline",
                                                                                          label = "Line width",
                                                                                          min = 0.01,
                                                                                          max = 10,
                                                                                          value = 1.1),

                                                                              sliderInput(inputId = 'base_size_validate_fluorescence_plot_spline',
                                                                                          label = 'Base font size',
                                                                                          min = 10,
                                                                                          max = 35,
                                                                                          value = 23,
                                                                                          step = 0.5),

                                                                              sliderInput(inputId = "nbreaks__validate_fluorescence_plot_spline",
                                                                                          label = "Number of breaks on y-axis",
                                                                                          min = 1,
                                                                                          max = 20,
                                                                                          value = 6),

                                                                              textInput(
                                                                                inputId = 'color_validate_fluorescence_plot_spline',
                                                                                label = 'Change color',
                                                                                value = "dodgerblue3"
                                                                              ),
                                                                              bsPopover(id = "color_validate_fluorescence_plot_spline",
                                                                                        title = HTML("<em>Define the colors used to highlight data points used in linear regression and determined slope</em>"), placement = "top",
                                                                                        content = "Enter color either by name (e.g., red, blue, coral3) or via their hexadecimal code (e.g., #AE4371, #CCFF00FF, #0066FFFF). A full list of colors available by name can be found at http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf"
                                                                              ),

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
                                                                             ) # column
                                                                           ), # fluidRow

                                                                           HTML("<br>"),

                                                                           h3(strong("Export plot")),

                                                                           fluidRow(
                                                                             column(width = 4,
                                                                                    numericInput(inputId = "width_download_fluorescence_validate_spline",
                                                                                                 label = "Width (in inches)",
                                                                                                 value = 10)
                                                                             ), # column
                                                                             column(width = 4,
                                                                                    numericInput(inputId = "height_download_fluorescence_validate_spline",
                                                                                                 label = "Height (in inches)",
                                                                                                 value = 9)
                                                                             ), # column
                                                                             column(width = 4,
                                                                                    numericInput(inputId = "dpi_download_fluorescence_validate_spline",
                                                                                                 label = "DPI",
                                                                                                 value = 300)
                                                                             ), # column
                                                                             column(width = 4,
                                                                                    downloadButton('download_fluorescence_validate_spline',"Download Plot"),

                                                                                    radioButtons("format_download_fluorescence_validate_spline",
                                                                                                 label = NULL,
                                                                                                 choices = c("PNG" = ".png",
                                                                                                             "PDF" = ".pdf"),
                                                                                                 selected = ".png",
                                                                                                 inline = TRUE)
                                                                             ) # column
                                                                           ), # fluidRow
                                                                           h3(strong("Export spline values")),
                                                                           fluidRow(
                                                                             column(width = 4,
                                                                                    downloadButton('download_fluorescence_validate_spline_values',"Download value table (x-y)")
                                                                             ),
                                                                             column(width = 4,
                                                                                    downloadButton('download_fluorescence_validate_spline_deriv_values',"Download derivative value table (x-y)")
                                                                             ),
                                                                           )
                                                                 ) # mainPanel
                                                        ), # tabPanel(title = "Nonparametric fits", value = "tabPanel_Validate_Fluorescence_splineFits",

                                                        # Bootstrapping spline fit ####

                                                        tabPanel(title = "Bootstrapping Spline", value = "tabPanel_Validate_Fluorescence_Spline_bt",
                                                                 sidebarPanel(width = 4,
                                                                              selectizeInput(inputId = "sample_validate_fluorescence_spline_bt",
                                                                                             label = "Sample:",
                                                                                             width = "100%",
                                                                                             choices = "",
                                                                                             multiple = FALSE,
                                                                                             options = list(closeAfterSelect = FALSE)
                                                                              ),

                                                                              checkboxInput(inputId = "plot_derivative_fluorescence_spline_bt",
                                                                                            label = "Plot derivative",
                                                                                            value = TRUE),

                                                                              h3('Customize plot appearance'),


                                                                              sliderInput(inputId = 'shape_type_validate_fluorescence_spline_bt',
                                                                                          label = 'Shape type',
                                                                                          min = 1,
                                                                                          max = 25,
                                                                                          value = 1),

                                                                              sliderInput(inputId = 'shape_size_validate_fluorescence_spline_bt',
                                                                                          label = 'Shape size',
                                                                                          min = 0.1,
                                                                                          max = 10,
                                                                                          value = 2,
                                                                                          step = 0.1),



                                                                              sliderInput(inputId = 'axis_size_validate_fluorescence_spline_bt',
                                                                                          label = 'Axis title font size',
                                                                                          min = 0.1,
                                                                                          max = 10,
                                                                                          value = 2.8,
                                                                                          step = 0.1),

                                                                              sliderInput(inputId = 'lab_size_validate_fluorescence_spline_bt',
                                                                                          label = 'Axis label font size',
                                                                                          min = 0.1,
                                                                                          max = 10,
                                                                                          value = 2.4,
                                                                                          step = 0.1),

                                                                              sliderInput(inputId = 'line_width_validate_fluorescence_spline_bt',
                                                                                          label = 'Line width',
                                                                                          min = 0.01,
                                                                                          max = 10,
                                                                                          value = 0.5),


                                                                              strong("x-Range"),
                                                                              fluidRow(
                                                                                column(5,
                                                                                       textInput(inputId = "x_range_min_validate_fluorescence_spline_bt",
                                                                                                 label = NULL,
                                                                                                 value = "", placeholder = "min"
                                                                                       )
                                                                                ),

                                                                                column(5,
                                                                                       textInput(inputId = "x_range_max_validate_fluorescence_spline_bt",
                                                                                                 label = NULL,
                                                                                                 value = "", placeholder = "max"
                                                                                       )
                                                                                )
                                                                              ),

                                                                              strong("y-Range"),
                                                                              fluidRow(
                                                                                column(5,
                                                                                       textInput(inputId = "y_range_min_validate_fluorescence_spline_bt",
                                                                                                 label = NULL,
                                                                                                 value = "", placeholder = "min"
                                                                                       )
                                                                                ),

                                                                                column(5,
                                                                                       textInput(inputId = "y_range_max_validate_fluorescence_spline_bt",
                                                                                                 label = NULL,
                                                                                                 value = "", placeholder = "max"
                                                                                       )
                                                                                )
                                                                              ),
                                                                              strong("y-Range (derivative)"),
                                                                              fluidRow(
                                                                                column(5,
                                                                                       textInput(inputId = "y_range_min_derivative_validate_fluorescence_spline_bt",
                                                                                                 label = NULL,
                                                                                                 value = "", placeholder = "min"
                                                                                       )
                                                                                ),

                                                                                column(5,
                                                                                       textInput(inputId = "y_range_max_derivative_validate_fluorescence_spline_bt",
                                                                                                 label = NULL,
                                                                                                 value = "", placeholder = "max"
                                                                                       )
                                                                                )
                                                                              ),

                                                                              textInput(
                                                                                inputId = 'color_validate_fluorescence_spline_bt',
                                                                                label = 'Change color',
                                                                                value = "dodgerblue3"
                                                                              ),
                                                                              bsPopover(id = "color_validate_fluorescence_spline_bt",
                                                                                        title = HTML("<em>Define the colors used to highlight data points used in linear regression and determined slope</em>"), placement = "top",
                                                                                        content = "Enter color either by name (e.g., red, blue, coral3) or via their hexadecimal code (e.g., #AE4371, #CCFF00FF, #0066FFFF). A full list of colors available by name can be found at http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf"
                                                                              ),

                                                                 ), # sidebarPanel

                                                                 mainPanel(width = 8,

                                                                           plotOutput("validate_fluorescence_plot_spline_bt",
                                                                                      width = "100%", height = "1000px"),

                                                                           HTML("<br>"),
                                                                           h3(strong("Export plot")),

                                                                           fluidRow(
                                                                             column(width = 4,
                                                                                    numericInput(inputId = "width_download_fluorescence_validate_spline_bt",
                                                                                                 label = "Width (in inches)",
                                                                                                 value = 10)
                                                                             ), # column
                                                                             column(width = 4,
                                                                                    numericInput(inputId = "height_download_fluorescence_validate_spline_bt",
                                                                                                 label = "Height (in inches)",
                                                                                                 value = 9)
                                                                             ), # column
                                                                             column(width = 4,
                                                                                    numericInput(inputId = "dpi_download_fluorescence_validate_spline_bt",
                                                                                                 label = "DPI",
                                                                                                 value = 300)
                                                                             ), # column
                                                                             column(width = 4,
                                                                                    downloadButton('download_fluorescence_validate_spline_bt',"Download Plot"),
                                                                                    radioButtons("format_download_fluorescence_validate_spline_bt",
                                                                                                 label = NULL,
                                                                                                 choices = c("PNG" = ".png",
                                                                                                             "PDF" = ".pdf"),
                                                                                                 selected = ".png",
                                                                                                 inline = TRUE)
                                                                             ) # column
                                                                           ) # fluidRow
                                                                 ) # mainPanel
                                                        ) # tabPanel(title = "Bootstrapping Spline"
                                            ) # tabsetPanel(type = "tabs",
                                   ) # tabPanel(title = "Fluorescence Fits", value = "tabPanel_Validate_Fluorescence",
                        ), # navbarMenu("Validate", icon = icon("user-check"),
                        #____RESULTS____####

                        navbarMenu(span("Results", title = "Tabular overview of computation results."),
                                   menuName = "navbarMenu_Results", icon = icon("magnifying-glass-chart"),
                                   ##____Results_Growth___####
                                   tabPanel(title = "Growth", value = "tabPanel_Results_Growth",
                                            tabsetPanel(type = "tabs", id = "tabsetPanel_Results_Growth",
                                                        tabPanel(title = "Linear Fit", value = "tabPanel_Results_Growth_Linear",
                                                                 conditionalPanel(condition = "input.biphasic_growth",
                                                                                  h5("(Values in parentheses indicate parameters for secondary growth phase)")
                                                                 ),

                                                                 checkboxInput(inputId = 'grouped_results_growth_linear',
                                                                               label = 'Group averages',
                                                                               value = TRUE),

                                                                 conditionalPanel(
                                                                   condition = "!input.grouped_results_growth_linear",
                                                                   DT::dataTableOutput('results_table_growth_linear')
                                                                 ),
                                                                 conditionalPanel(
                                                                   condition = "input.grouped_results_growth_linear",
                                                                   DT::dataTableOutput('results_table_growth_linear_group')
                                                                 ),
                                                                 conditionalPanel(
                                                                   condition = "!input.grouped_results_growth_linear",
                                                                   downloadButton('download_table_growth_linear',"Download table")
                                                                 ),
                                                                 conditionalPanel(
                                                                   condition = "input.grouped_results_growth_linear",
                                                                   downloadButton('download_table_growth_linear_group',"Download table")
                                                                 ),
                                                        ),
                                                        tabPanel(title = "Nonparametric Fit", value = "tabPanel_Results_Growth_Spline",
                                                                 conditionalPanel(condition = "input.biphasic_growth",
                                                                                  h5("(Values in parentheses indicate parameters for secondary growth phase)")
                                                                 ),
                                                                 checkboxInput(inputId = 'grouped_results_growth_spline',
                                                                               label = 'Group averages',
                                                                               value = TRUE),
                                                                 conditionalPanel(
                                                                   condition = "!input.grouped_results_growth_spline",
                                                                   DT::dataTableOutput('results_table_growth_spline')
                                                                 ),
                                                                 conditionalPanel(
                                                                   condition = "input.grouped_results_growth_spline",
                                                                   DT::dataTableOutput('results_table_growth_spline_group')
                                                                 ),
                                                                 conditionalPanel(
                                                                   condition = "!input.grouped_results_growth_spline",
                                                                   downloadButton('download_table_growth_spline',"Download table")
                                                                 ),
                                                                 conditionalPanel(
                                                                   condition = "input.grouped_results_growth_spline",
                                                                   downloadButton('download_table_growth_spline_group',"Download table")
                                                                 ),
                                                        ),
                                                        tabPanel(title = "Nonparametric Fit (Bootstrapping)", value = "tabPanel_Results_Growth_Spline_bt",
                                                                 DT::dataTableOutput('results_table_growth_spline_bt'),
                                                                 downloadButton('download_table_growth_spline_bt',"Download table")
                                                        ),
                                                        tabPanel(title = "Parametric Fit", value = "tabPanel_Results_Growth_Model",

                                                                 checkboxInput(inputId = 'grouped_results_growth_model',
                                                                               label = 'Group averages',
                                                                               value = TRUE),

                                                                 conditionalPanel(
                                                                   condition = "!input.grouped_results_growth_model",
                                                                   DT::dataTableOutput('results_table_growth_model'),
                                                                   downloadButton('download_table_growth_model',"Download table")
                                                                 ),

                                                                 conditionalPanel(
                                                                   condition = "input.grouped_results_growth_model",
                                                                   DT::dataTableOutput('results_table_growth_model_group'),
                                                                   downloadButton('download_table_growth_model_group',"Download table")
                                                                 ),
                                                        ),
                                                        tabPanel(title = "Dose-response analysis", value = "tabPanel_Results_Growth_DR",
                                                                 DT::dataTableOutput('results_table_growth_dr_spline'),
                                                                 downloadButton('download_table_growth_dr',"Download table")
                                                        )
                                            )
                                   ),
                                   ##____Results_Fluorescence___####
                                   tabPanel(title = "Fluorescence", value = "tabPanel_Results_Fluorescence",
                                            tabsetPanel(type = "tabs", id = "tabsetPanel_Results_Fluorescence",
                                                        tabPanel(title = "Linear Fit", value = "tabPanel_Results_Fluorescence_Linear",
                                                                 conditionalPanel(condition = "input.biphasic_fluorescence",
                                                                                  h5("(Values in parentheses indicate parameters for secondary phase)")
                                                                 ),
                                                                 checkboxInput(inputId = 'grouped_results_fluorescence_linear',
                                                                               label = 'Group averages',
                                                                               value = TRUE),
                                                                 conditionalPanel(
                                                                   condition = "!input.grouped_results_fluorescence_linear",
                                                                   DT::dataTableOutput('results_table_fluorescence_linear')
                                                                 ),
                                                                 conditionalPanel(
                                                                   condition = "input.grouped_results_fluorescence_linear",
                                                                   DT::dataTableOutput('results_table_fluorescence_linear_group')
                                                                 ),
                                                                 conditionalPanel(
                                                                   condition = "!input.grouped_results_fluorescence_linear",
                                                                   downloadButton('download_table_fluorescence_linear',"Download table")
                                                                 ),
                                                                 conditionalPanel(
                                                                   condition = "input.grouped_results_fluorescence_linear",
                                                                   downloadButton('download_table_fluorescence_linear_group',"Download table")
                                                                 ),
                                                        ),
                                                        tabPanel(title = "Nonparametric Fit", value = "tabPanel_Results_Fluorescence_Spline",
                                                                 conditionalPanel(condition = "input.biphasic_fluorescence",
                                                                                  h5("(Values in parentheses indicate parameters for secondary phase)")
                                                                 ),
                                                                 checkboxInput(inputId = 'grouped_results_fluorescence_spline',
                                                                               label = 'Group averages',
                                                                               value = TRUE),
                                                                 conditionalPanel(
                                                                   condition = "!input.grouped_results_fluorescence_spline",
                                                                   DT::dataTableOutput('results_table_fluorescence_spline')
                                                                 ),
                                                                 conditionalPanel(
                                                                   condition = "input.grouped_results_fluorescence_spline",
                                                                   DT::dataTableOutput('results_table_fluorescence_spline_group')
                                                                 ),
                                                                 conditionalPanel(
                                                                   condition = "!input.grouped_results_fluorescence_spline",
                                                                   downloadButton('download_table_fluorescence_spline',"Download table")
                                                                 ),
                                                                 conditionalPanel(
                                                                   condition = "input.grouped_results_fluorescence_spline",
                                                                   downloadButton('download_table_fluorescence_spline_group',"Download table")
                                                                 ),
                                                        ),
                                                        tabPanel(title = "Nonparametric Fit (Bootstrapping)", value = "tabPanel_Results_Fluorescence_Spline_bt",
                                                                 DT::dataTableOutput('results_table_fluorescence_spline_bt'),
                                                                 downloadButton('download_table_fluorescence_spline_bt',"Download table")
                                                        ),
                                                        tabPanel(title = "Dose-response analysis", value = "tabPanel_Results_Fluorescence_DR_Spline",
                                                                 DT::dataTableOutput('results_table_fluorescence_dr_spline'),
                                                                 downloadButton('download_table_fluorescence_dr_spline',"Download table")
                                                        ),
                                                        tabPanel(title = "Dose-response analysis", value = "tabPanel_Results_Fluorescence_DR_Model",
                                                                 DT::dataTableOutput('results_table_fluorescence_dr_model'),
                                                                 downloadButton('download_table_fluorescence_dr_model',"Download table")
                                                        )
                                            )
                                   )
                        ),

                        #____Visualize____####
                        navbarMenu(span("Visualization", title = "Visualize computation results for the entire dataset."),
                                   menuName = "navbarMenu_Visualize", icon = icon("chart-line"),
                                   ## Growth Plots ####
                                   tabPanel(title = "Growth Plots", value = "tabPanel_Visualize_Growth",
                                            h1("Growth Plots"),
                                            tabsetPanel(type = "tabs", id = "tabsetPanel_Visualize_Growth",

                                                        ### Growth Group Plots ####

                                                        tabPanel(title = "Group Plots",
                                                                 sidebarPanel(

                                                                   selectInput(inputId = "data_type_growth_group_plot",
                                                                               label = "Data type",
                                                                               choices = c("Raw growth" = "raw",
                                                                                           "Spline fits" = "spline")
                                                                   ),

                                                                   checkboxInput(inputId = "select_string_visualize_growth_group",
                                                                                 label = "(De-)select samples based on string",
                                                                                 value = FALSE),

                                                                   conditionalPanel(
                                                                     condition = "!input.select_string_visualize_growth_group && !input.plot_group_averages_growth_group_plot",
                                                                     selectizeInput(inputId = "samples_visualize_growth_group",
                                                                                    label = "Samples:",
                                                                                    width = "100%",
                                                                                    choices = "",
                                                                                    multiple = TRUE,
                                                                                    options = list(closeAfterSelect = FALSE,
                                                                                                   plugins= list('remove_button'))
                                                                     )
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "!input.select_string_visualize_growth_group && input.plot_group_averages_growth_group_plot",
                                                                     selectizeInput(inputId = "groups_visualize_growth_group",
                                                                                    label = "Conditions:",
                                                                                    width = "100%",
                                                                                    choices = "",
                                                                                    multiple = TRUE,
                                                                                    options = list(closeAfterSelect = FALSE,
                                                                                                   plugins= list('remove_button'))
                                                                     )
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "input.select_string_visualize_growth_group",
                                                                     textInput(inputId = "select_samples_based_on_string_growth_group_plot",
                                                                               label = "Select sample based on string (separate by ;)"
                                                                     ),

                                                                     textInput(inputId = "exclude_samples_based_on_string_growth_group_plot",
                                                                               label = "Exclude sample based on string (separate by ;)"
                                                                     ),

                                                                   ),
                                                                   conditionalPanel(
                                                                     condition = "input.plot_group_averages_growth_group_plot || input.select_string_visualize_growth_group",
                                                                     textInput(inputId = "select_samples_based_on_concentration_growth_group_plot",
                                                                               label = "Select sample based on concentration (separate by ;)"
                                                                     ),

                                                                     textInput(inputId = "exclude_samples_based_on_concentration_growth_group_plot",
                                                                               label = "Exclude sample based on concentration (separate by ;)"
                                                                     )
                                                                   ),

                                                                   checkboxInput(inputId = "plot_group_averages_growth_group_plot",
                                                                                 label = "Plot group averages",
                                                                                 value = TRUE),

                                                                   conditionalPanel(
                                                                     condition = "input.data_type_growth_group_plot == 'spline'",
                                                                     checkboxInput(inputId = "plot_derivative_growth_group_plot",
                                                                                   label = "Plot derivative",
                                                                                   value = TRUE)
                                                                   ),

                                                                   h3("Customize plot appearance"),

                                                                   checkboxInput(inputId = "log_transform_y_axis_growth_group_plot",
                                                                                 label = "Log-transform y-axis",
                                                                                 value = TRUE),

                                                                   strong("x-Range"),
                                                                   fluidRow(
                                                                     column(5,
                                                                            textInput(inputId = "x_range_min_growth_group_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "min"
                                                                            )
                                                                     ),

                                                                     column(5,
                                                                            textInput(inputId = "x_range_max_growth_group_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "max"
                                                                            )
                                                                     )
                                                                   ),

                                                                   strong("y-Range"),
                                                                   fluidRow(
                                                                     column(5,
                                                                            textInput(inputId = "y_range_min_growth_group_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "min"
                                                                            )
                                                                     ),

                                                                     column(5,
                                                                            textInput(inputId = "y_range_max_growth_group_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "max"
                                                                            )
                                                                     )
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "input.data_type_growth_group_plot == 'spline'",
                                                                     strong("y-Range (derivative)"),
                                                                     fluidRow(
                                                                       column(5,
                                                                              textInput(inputId = "y_range_min_derivative_growth_group_plot",
                                                                                        label = NULL,
                                                                                        value = "", placeholder = "min"
                                                                              )
                                                                       ),

                                                                       column(5,
                                                                              textInput(inputId = "y_range_max_derivative_growth_group_plot",
                                                                                        label = NULL,
                                                                                        value = "", placeholder = "max"
                                                                              )
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
                                                                   conditionalPanel(
                                                                     condition = "input.data_type_growth_group_plot == 'spline'",
                                                                     textInput(inputId = "y_axis_title_derivative_growth_group_plot",
                                                                               label = "y-axis title derivative",
                                                                               value = "Growth rate"
                                                                     )
                                                                   ),

                                                                   sliderInput(inputId = "nbreaks_growth_group_plot",
                                                                               label = "Number of breaks on y-axis",
                                                                               min = 1,
                                                                               max = 20,
                                                                               value = 6),

                                                                   sliderInput(inputId = "line_width_growth_group_plot",
                                                                               label = "Line width",
                                                                               min = 0.01,
                                                                               max = 10,
                                                                               value = 1.1),

                                                                   sliderInput(inputId = 'base_size_growth_group_plot',
                                                                               label = 'Base font size',
                                                                               min = 10,
                                                                               max = 35,
                                                                               value = 23,
                                                                               step = 0.5),

                                                                   selectInput(inputId = "legend_position_group_plot",
                                                                               label = "Legend position",
                                                                               choices = c("Bottom" = "bottom",
                                                                                           "Top" = "top",
                                                                                           "Left" = "left",
                                                                                           "Right" = "right")
                                                                   ),

                                                                   sliderInput(inputId = 'legend_ncol_group_plot',
                                                                               label = 'Number of legend columns',
                                                                               min = 1,
                                                                               max = 10,
                                                                               value = 4,
                                                                               step = 1
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "output.more_than_two_conc",
                                                                     checkboxInput(inputId = "color_groups_group_plot",
                                                                                   label = "Color samples by group",
                                                                                   value = TRUE)
                                                                   ),

                                                                   textInput(
                                                                     inputId = 'custom_colors_group_plot',
                                                                     label = 'Custom colors'
                                                                   ),
                                                                   bsPopover(id = "custom_colors_group_plot",
                                                                             title = HTML("<em>Provide custom colors</em>"), placement = "top",
                                                                             content = "Enter colors either by name (e.g., red, blue, coral3) or via their hexadecimal code (e.g., #AE4371, #CCFF00FF, #0066FFFF). Separate colors with a comma. A full list of colors available by name can be found at http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf"
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "output.more_than_two_conc && input.color_groups_group_plot",
                                                                     selectizeInput(inputId = "color_palettes_group_plot",
                                                                                    label = "Change color palettes",
                                                                                    width = "100%",
                                                                                    choices = names(QurvE:::single_hue_palettes),
                                                                                    selected = names(QurvE:::single_hue_palettes),
                                                                                    multiple = TRUE,
                                                                                    options = list(closeAfterSelect = FALSE,
                                                                                                   plugins= list('remove_button'))
                                                                     ),
                                                                     bsPopover(id = "color_palettes_group_plot",
                                                                               title = HTML("<em>Define the colors used to display sample groups with identical concentrations</em>"), placement = "top",
                                                                               content = "The number of selected color palettes must be at least the number of displayed groups. The order of the chosen palettes corresponds to the oder of conditions in the legend."
                                                                     ),

                                                                   )

                                                                 ), # Side panel growth group plots

                                                                 mainPanel(
                                                                   withSpinner(
                                                                     plotOutput("growth_group_plot",
                                                                                width = "100%", height = "1000px"),

                                                                   ),

                                                                   h3(strong("Export plot")),

                                                                   fluidRow(
                                                                     column(width = 4,
                                                                            numericInput(inputId = "width_download_growth_group_plot",
                                                                                         label = "Width (in inches)",
                                                                                         value = 10)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            numericInput(inputId = "height_download_growth_group_plot",
                                                                                         label = "Height (in inches)",
                                                                                         value = 9)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            numericInput(inputId = "dpi_download_growth_group_plot",
                                                                                         label = "DPI",
                                                                                         value = 300)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            downloadButton('download_growth_group_plot',"Download Plot"),

                                                                            radioButtons("format_download_growth_group_plot",
                                                                                         label = NULL,
                                                                                         choices = c("PNG" = ".png",
                                                                                                     "PDF" = ".pdf"),
                                                                                         selected = ".png",
                                                                                         inline = TRUE)
                                                                     ) # column
                                                                   ) # fluidRow
                                                                 ) #  mainPanel
                                                        ),

                                                        ### Growth Parameter Plots ####
                                                        tabPanel(title = "Parameter Plots",
                                                                 sidebarPanel(
                                                                   selectInput(inputId = "parameter_parameter_growth_plot",
                                                                               label = "Parameter",
                                                                               choices = ""
                                                                   ),

                                                                   checkboxInput(inputId = "select_string_visualize_parameter_growth_plot",
                                                                                 label = "(De-)select samples based on string",
                                                                                 value = FALSE),

                                                                   conditionalPanel(
                                                                     condition = "!input.select_string_visualize_parameter_growth_plot",
                                                                     selectizeInput(inputId = "samples_visualize_parameter_growth_plot",
                                                                                    label = "Conditions:",
                                                                                    width = "100%",
                                                                                    choices = "",
                                                                                    multiple = TRUE,
                                                                                    options = list(closeAfterSelect = FALSE,
                                                                                                   plugins= list('remove_button'))
                                                                     )
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "input.select_string_visualize_parameter_growth_plot",
                                                                     textInput(inputId = "select_sample_based_on_string_growth_parameter_plot",
                                                                               label = "Select sample based on string (separate by ;)"
                                                                     ),

                                                                     textInput(inputId = "exclude_sample_based_on_strings_growth_parameter_plot",
                                                                               label = "Exclude sample based on string (separate by ;)"
                                                                     ),

                                                                   ),

                                                                   textInput(inputId = "select_sample_based_on_concentration_growth_parameter_plot",
                                                                             label = "Select sample based on concentration (separate by ;)"
                                                                   ),

                                                                   textInput(inputId = "exclude_sample_based_on_concentration_growth_parameter_plot",
                                                                             label = "Exclude sample based on concentration (separate by ;)"
                                                                   ),

                                                                   checkboxInput(inputId = 'normalize_to_reference_growth_parameter_plot',
                                                                                 label = 'normalize to reference',
                                                                                 value = FALSE),

                                                                   h3("Customize plot appearance"),

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
                                                                               min = 0.1,
                                                                               max = 10,
                                                                               value = 3,
                                                                               step = 0.1),

                                                                   sliderInput(inputId = "basesize_growth_parameter_plot",
                                                                               label = "Base font size",
                                                                               min = 10,
                                                                               max = 35,
                                                                               value = 23,
                                                                               step = 0.5),
                                                                   sliderInput(inputId = "label.size_growth_parameter_plot",
                                                                               label = "Label font size",
                                                                               min = 5,
                                                                               max = 35,
                                                                               value = 17,
                                                                               step = 0.5),

                                                                   selectInput(inputId = "legend_position_growth_parameter_plot",
                                                                               label = "Legend position",
                                                                               choices = c("Bottom" = "bottom",
                                                                                           "Top" = "top",
                                                                                           "Left" = "left",
                                                                                           "Right" = "right"),
                                                                               selected = "right"
                                                                   ),

                                                                   sliderInput(inputId = 'legend_ncol_growth_parameter_plot',
                                                                               label = 'Number of legend columns',
                                                                               min = 1,
                                                                               max = 10,
                                                                               value = 1,
                                                                               step = 1
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "output.more_than_two_conc",
                                                                     checkboxInput(inputId = "sort_by_conc_growth_parameter_plot",
                                                                                   label = "Sort samples by concentration",
                                                                                   value = FALSE)
                                                                   ),

                                                                   textInput(
                                                                     inputId = 'custom_colors_growth_parameter_plot',
                                                                     label = 'Custom colors'
                                                                   ),
                                                                   bsPopover(id = "custom_colors_growth_parameter_plot",
                                                                             title = HTML("<em>Provide custom colors</em>"), placement = "top",
                                                                             content = "Enter colors either by name (e.g., red, blue, coral3) or via their hexadecimal code (e.g., #AE4371, #CCFF00FF, #0066FFFF). Separate colors with a comma. A full list of colors available by name can be found at http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf"
                                                                   ),


                                                                 ),

                                                                 mainPanel(
                                                                   plotOutput("growth_parameter_plot",
                                                                              width = "100%", height = "800px"),

                                                                   h3(strong("Export plot")),

                                                                   fluidRow(
                                                                     column(width = 4,
                                                                            numericInput(inputId = "width_download_growth_parameter_plot",
                                                                                         label = "Width (in inches)",
                                                                                         value = 7)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            numericInput(inputId = "height_download_growth_parameter_plot",
                                                                                         label = "Height (in inches)",
                                                                                         value = 6)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            numericInput(inputId = "dpi_download_growth_parameter_plot",
                                                                                         label = "DPI",
                                                                                         value = 300)
                                                                     ), # column

                                                                     column(width = 5,
                                                                            downloadButton('download_growth_parameter_plot',"Download Plot"),

                                                                            radioButtons("format_download_growth_parameter_plot",
                                                                                         label = NULL,
                                                                                         choices = c("PNG" = ".png",
                                                                                                     "PDF" = ".pdf"),
                                                                                         selected = ".png",
                                                                                         inline = TRUE)
                                                                     ), # column


                                                                   ) # fluidRow
                                                                 ) #  mainPanel
                                                        ),

                                                        ### Growth Grid Plots ####

                                                        tabPanel(title = "Plot Grid",
                                                                 sidebarPanel(

                                                                   selectInput(inputId = "data_type_growth_grid_plot",
                                                                               label = "Data type",
                                                                               choices = c("Raw growth" = "raw",
                                                                                           "Spline fits" = "spline")
                                                                   ),

                                                                   selectInput(inputId = "parameter_parameter_grid_plot",
                                                                               label = "Parameter",
                                                                               choices = ""
                                                                   ),

                                                                   checkboxInput(inputId = "select_string_visualize_growth_grid",
                                                                                 label = "(De-)select samples based on string",
                                                                                 value = FALSE),

                                                                   conditionalPanel(
                                                                     condition = "!input.select_string_visualize_growth_grid && !input.plot_group_averages_growth_grid_plot",
                                                                     selectizeInput(inputId = "samples_visualize_growth_grid",
                                                                                    label = "Samples:",
                                                                                    width = "100%",
                                                                                    choices = "",
                                                                                    multiple = TRUE,
                                                                                    options = list(closeAfterSelect = FALSE,
                                                                                                   plugins= list('remove_button'))
                                                                     ),
                                                                     checkboxInput(inputId = "order_matters_visualize_growth_grid",
                                                                                   label = "Select order matters",
                                                                                   value = FALSE
                                                                                   ),
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "!input.select_string_visualize_growth_grid && input.plot_group_averages_growth_grid_plot",
                                                                     selectizeInput(inputId = "groups_visualize_growth_grid",
                                                                                    label = "Conditions:",
                                                                                    width = "100%",
                                                                                    choices = "",
                                                                                    multiple = TRUE,
                                                                                    options = list(closeAfterSelect = FALSE,
                                                                                                   plugins= list('remove_button'))
                                                                     )
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "input.select_string_visualize_growth_grid",
                                                                     textInput(inputId = "select_samples_based_on_string_growth_grid_plot",
                                                                               label = "Select sample based on string (separate by ;)"
                                                                     ),

                                                                     textInput(inputId = "exclude_samples_based_on_string_growth_grid_plot",
                                                                               label = "Exclude sample based on string (separate by ;)"
                                                                     ),

                                                                   ),
                                                                   conditionalPanel(
                                                                     condition = "input.plot_group_averages_growth_grid_plot || input.select_string_visualize_growth_grid",
                                                                     textInput(inputId = "select_samples_based_on_concentration_growth_grid_plot",
                                                                               label = "Select sample based on concentration (separate by ;)"
                                                                     ),

                                                                     textInput(inputId = "exclude_samples_based_on_concentration_growth_grid_plot",
                                                                               label = "Exclude sample based on concentration (separate by ;)"
                                                                     )
                                                                   ),

                                                                   checkboxInput(inputId = "plot_group_averages_growth_grid_plot",
                                                                                 label = "Plot group averages",
                                                                                 value = TRUE),

                                                                   conditionalPanel(
                                                                     condition = "output.more_than_two_conc",
                                                                     checkboxInput(inputId = "sort_by_conc_growth_grid_plot",
                                                                                   label = "Sort by concentration",
                                                                                   value = TRUE)
                                                                   ),

                                                                   h3("Customize plot appearance"),

                                                                   checkboxInput(inputId = "log_transform_y_axis_growth_grid_plot",
                                                                                 label = "Log-transform y-axis",
                                                                                 value = TRUE),

                                                                   strong("x-Range"),
                                                                   fluidRow(
                                                                     column(5,
                                                                            textInput(inputId = "x_range_min_growth_grid_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "min"
                                                                            )
                                                                     ),

                                                                     column(5,
                                                                            textInput(inputId = "x_range_max_growth_grid_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "max"
                                                                            )
                                                                     )
                                                                   ),

                                                                   strong("y-Range"),
                                                                   fluidRow(
                                                                     column(5,
                                                                            textInput(inputId = "y_range_min_growth_grid_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "min"
                                                                            )
                                                                     ),

                                                                     column(5,
                                                                            textInput(inputId = "y_range_max_growth_grid_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "max"
                                                                            )
                                                                     )
                                                                   ),

                                                                   strong("Color scale limits"),
                                                                   fluidRow(
                                                                     column(5,
                                                                            textInput(inputId = "legend_lim_min_growth_grid_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "min"
                                                                            )
                                                                     ),

                                                                     column(5,
                                                                            textInput(inputId = "legend_lim_max_growth_grid_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "max"
                                                                            )
                                                                     )
                                                                   ),


                                                                   textInput(inputId = "y_axis_title_growth_grid_plot",
                                                                             label = "y-axis title",
                                                                             value = "Growth [y(t)]"
                                                                   ),

                                                                   textInput(inputId = "x_axis_title_growth_grid_plot",
                                                                             label = "x-axis title",
                                                                             value = "Time"
                                                                   ),

                                                                   sliderInput(inputId = "nbreaks_growth_grid_plot",
                                                                               label = "Number of breaks on y-axis",
                                                                               min = 1,
                                                                               max = 20,
                                                                               value = 6),

                                                                   sliderInput(inputId = "line_width_growth_grid_plot",
                                                                               label = "Line width",
                                                                               min = 0.01,
                                                                               max = 10,
                                                                               value = 1.1),

                                                                   sliderInput(inputId = 'base_size_growth_grid_plot',
                                                                               label = 'Base font size',
                                                                               min = 10,
                                                                               max = 35,
                                                                               value = 23,
                                                                               step = 0.5),


                                                                   conditionalPanel(
                                                                     condition = "!input.sort_by_conc_growth_grid_plot",
                                                                     sliderInput(inputId = "nrows_growth_grid_plot",
                                                                                 label = "Number of rows in grid",
                                                                                 min = 1,
                                                                                 max = 20,
                                                                                 value = 2)
                                                                   ),


                                                                   selectInput(inputId = "color_palettes_grid_plot",
                                                                                  label = "Change color palette",
                                                                                  width = "100%",
                                                                                  choices = names(QurvE:::single_hue_palettes),
                                                                                  selected = names(QurvE:::single_hue_palettes)[1],
                                                                                  multiple = FALSE
                                                                   ),
                                                                   bsPopover(id = "color_palettes_grid_plot",
                                                                             title = HTML("<em>Define the colors used to visualize the value of the chosen parameter</em>"), placement = "top",
                                                                             content = ""
                                                                   ),

                                                                   checkboxInput(inputId = "invert_color_palette_grid_plot",
                                                                                 label = "Invert color palette",
                                                                                 value = FALSE)

                                                                 ), # Side panel growth group plots

                                                                 mainPanel(
                                                                   withSpinner(
                                                                     plotOutput("growth_grid_plot",
                                                                                width = "100%", height = "1000px"),

                                                                   ),

                                                                   h3(strong("Export plot")),

                                                                   fluidRow(
                                                                     column(width = 4,
                                                                            numericInput(inputId = "width_download_growth_grid_plot",
                                                                                         label = "Width (in inches)",
                                                                                         value = 10)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            numericInput(inputId = "height_download_growth_grid_plot",
                                                                                         label = "Height (in inches)",
                                                                                         value = 9)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            numericInput(inputId = "dpi_download_growth_grid_plot",
                                                                                         label = "DPI",
                                                                                         value = 300)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            downloadButton('download_growth_grid_plot',"Download Plot"),

                                                                            radioButtons("format_download_growth_grid_plot",
                                                                                         label = NULL,
                                                                                         choices = c("PNG" = ".png",
                                                                                                     "PDF" = ".pdf"),
                                                                                         selected = ".png",
                                                                                         inline = TRUE)
                                                                     ) # column
                                                                   ) # fluidRow
                                                                 ) #  mainPanel
                                                        ),

                                                        ### Growth DR Plots Spline ####

                                                        tabPanel(title = "Dose-Response Analysis", value = "tabPanel_Visualize_Growth_DoseResponse_Spline",
                                                                 sidebarPanel(
                                                                   conditionalPanel(
                                                                     condition = "output.more_than_one_drfit_spline",
                                                                     wellPanel(
                                                                       style='padding: 1; border-color: #ADADAD; padding-bottom: 0',
                                                                       checkboxInput(inputId = 'combine_conditions_into_a_single_plot_dose_response_growth_plot',
                                                                                     label = 'Combine conditions into a single plot',
                                                                                     value = FALSE)
                                                                     )
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "input.combine_conditions_into_a_single_plot_dose_response_growth_plot",
                                                                     textInput(inputId = "select_samples_based_on_string_dose_response_growth_plot",
                                                                               label = "Select sample based on string (separate by ;)"
                                                                     )
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "input.combine_conditions_into_a_single_plot_dose_response_growth_plot",
                                                                     textInput(inputId = "exclude_samples_based_on_string_dose_response_growth_plot",
                                                                               label = "Exclude sample based on string (separate by ;)"
                                                                     )
                                                                   ),

                                                                   h3('Customize plot appearance'),

                                                                   checkboxInput(inputId = "log_transform_y_axis_dose_response_growth_plot",
                                                                                 label = "Log-transform y-axis",
                                                                                 value = FALSE),

                                                                   checkboxInput(inputId = "log_transform_x_axis_dose_response_growth_plot",
                                                                                 label = "Log-transform x-axis",
                                                                                 value = FALSE),

                                                                   sliderInput(inputId = 'shape_type_dose_response_growth_plot',
                                                                               label = 'Shape type',
                                                                               min = 1,
                                                                               max = 25,
                                                                               value = 15),

                                                                   sliderInput(inputId = 'shape_size_dose_response_growth_plot',
                                                                               label = 'Shape size',
                                                                               min = 0.1,
                                                                               max = 10,
                                                                               value = 2,
                                                                               step = 0.1),

                                                                   conditionalPanel(
                                                                     condition = "input.combine_conditions_into_a_single_plot_dose_response_growth_plot",
                                                                     sliderInput(inputId = 'base_size_dose_response_growth_plot',
                                                                                 label = 'Base size',
                                                                                 min = 10,
                                                                                 max = 35,
                                                                                 value = 15,
                                                                                 step = 0.5)
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "!input.combine_conditions_into_a_single_plot_dose_response_growth_plot",
                                                                     sliderInput(inputId = 'axis_size_dose_response_growth_plot',
                                                                                 label = 'Axis title font size',
                                                                                 min = 0.1,
                                                                                 max = 10,
                                                                                 value = 1.3,
                                                                                 step = 0.1)
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "!input.combine_conditions_into_a_single_plot_dose_response_growth_plot",
                                                                     sliderInput(inputId = 'lab_size_dose_response_growth_plot',
                                                                                 label = 'Axis label font size',
                                                                                 min = 0.1,
                                                                                 max = 10,
                                                                                 value = 1.3,
                                                                                 step = 0.1)
                                                                   ),

                                                                   sliderInput(inputId = 'line_width_dose_response_growth_plot',
                                                                               label = 'Line width',
                                                                               min = 0.01,
                                                                               max = 10,
                                                                               value = 1),

                                                                   checkboxInput(inputId = 'show_ec50_indicator_lines_dose_response_growth_plot',
                                                                                 label = 'Show EC50 indicator lines',
                                                                                 value = TRUE),

                                                                   strong("x-Range"),
                                                                   fluidRow(
                                                                     column(5,
                                                                            textInput(inputId = "x_range_min_dose_response_growth_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "min"
                                                                            )
                                                                     ),

                                                                     column(5,
                                                                            textInput(inputId = "x_range_max_dose_response_growth_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "max"
                                                                            )
                                                                     )
                                                                   ),

                                                                   strong("y-Range"),
                                                                   fluidRow(
                                                                     column(5,
                                                                            textInput(inputId = "y_range_min_dose_response_growth_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "min"
                                                                            )
                                                                     ),

                                                                     column(5,
                                                                            textInput(inputId = "y_range_max_dose_response_growth_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "max"
                                                                            )
                                                                     )
                                                                   ),

                                                                   textInput(inputId = "y_axis_title_dose_response_growth_plot",
                                                                             label = "y-axis title",
                                                                             value = ""
                                                                   ),

                                                                   textInput(inputId = "x_axis_title_dose_response_growth_plot",
                                                                             label = "x-axis title",
                                                                             value = ""
                                                                   )

                                                                 ), # sidebarPanel

                                                                 conditionalPanel(condition = "input.combine_conditions_into_a_single_plot_dose_response_growth_plot",
                                                                                  mainPanel(
                                                                                    h3('Combined plots'),
                                                                                    plotOutput("dose_response_growth_plot_combined",
                                                                                               width = "100%", height = "800px"),

                                                                                    fluidRow(
                                                                                      column(6, align = "center", offset = 3,
                                                                                             actionButton(inputId = "rerun_dr_growth",
                                                                                                          label = "Re-run dose-response analysis with modified parameters",
                                                                                                          icon=icon("gears"),
                                                                                                          style="padding:5px; font-size:120%"),
                                                                                             actionButton(inputId = "restore_dr_growth",
                                                                                                          label = "Restore dose-response analysis",
                                                                                                          # icon=icon("gears"),
                                                                                                          style="padding:5px; font-size:120%")
                                                                                      )
                                                                                    ),

                                                                                    h3(strong("Export plot")),

                                                                                    fluidRow(
                                                                                      column(width = 4,
                                                                                             numericInput(inputId = "width_download_dose_response_growth_plot_combined",
                                                                                                          label = "Width (in inches)",
                                                                                                          value = 7)
                                                                                      ), # column
                                                                                      column(width = 4,
                                                                                             numericInput(inputId = "height_download_dose_response_growth_plot_combined",
                                                                                                          label = "Height (in inches)",
                                                                                                          value = 6)
                                                                                      ), # column
                                                                                      column(width = 4,
                                                                                             numericInput(inputId = "dpi_download_dose_response_growth_plot_combined",
                                                                                                          label = "DPI",
                                                                                                          value = 300)
                                                                                      ), # column
                                                                                      column(width = 4,
                                                                                             downloadButton('download_dose_response_growth_plot_combined',"Download Plot"),

                                                                                             radioButtons("format_download_dose_response_growth_plot_combined",
                                                                                                          label = NULL,
                                                                                                          choices = c("PNG" = ".png",
                                                                                                                      "PDF" = ".pdf"),
                                                                                                          selected = ".png",
                                                                                                          inline = TRUE)
                                                                                      ), # column


                                                                                    ) # fluidRow
                                                                                  ) # mainPanel
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
                                                                                    plotOutput("dose_response_growth_plot_individual",
                                                                                               width = "100%", height = "800px"),

                                                                                    fluidRow(
                                                                                      column(6, align = "center", offset = 3,
                                                                                             actionButton(inputId = "rerun_dr_growth2",
                                                                                                          label = "Re-run dose-response analysis with modified parameters",
                                                                                                          icon=icon("gears"),
                                                                                                          style="padding:5px; font-size:120%"),
                                                                                             actionButton(inputId = "restore_dr_growth2",
                                                                                                          label = "Restore dose-response analysis",
                                                                                                          # icon=icon("gears"),
                                                                                                          style="padding:5px; font-size:120%")
                                                                                      )
                                                                                    ),

                                                                                    h3(strong("Export plot")),

                                                                                    fluidRow(
                                                                                      column(width = 4,
                                                                                             numericInput(inputId = "width_download_dose_response_growth_plot_individual",
                                                                                                          label = "Width (in inches)",
                                                                                                          value = 7)
                                                                                      ), # column
                                                                                      column(width = 4,
                                                                                             numericInput(inputId = "height_download_dose_response_growth_plot_individual",
                                                                                                          label = "Height (in inches)",
                                                                                                          value = 6)
                                                                                      ), # column
                                                                                      column(width = 4,
                                                                                             numericInput(inputId = "dpi_download_dose_response_growth_plot_individual",
                                                                                                          label = "DPI",
                                                                                                          value = 300)
                                                                                      ), # column
                                                                                      column(width = 4,
                                                                                             downloadButton('download_dose_response_growth_plot_individual',"Download Plot"),

                                                                                             radioButtons("format_download_dose_response_growth_plot_individual",
                                                                                                          label = NULL,
                                                                                                          choices = c("PNG" = ".png",
                                                                                                                      "PDF" = ".pdf"),
                                                                                                          selected = ".png",
                                                                                                          inline = TRUE)
                                                                                      ), # column
                                                                                    ) # fluidRow
                                                                                  ) #  mainPanel

                                                                 ),


                                                        ), # tabPanel(title = "Dose-response analysis"

                                                        ### Growth DR Plots Model ####

                                                        tabPanel(title = "Dose-Response Analysis", value = "tabPanel_Visualize_Growth_DoseResponse_Model",
                                                                 sidebarPanel(

                                                                   h3('Customize plot appearance'),

                                                                   checkboxInput(inputId = "log_transform_x_axis_dose_response_growth_plot_model",
                                                                                 label = "Log-transform x-axis",
                                                                                 value = TRUE),

                                                                   sliderInput(inputId = 'shape_type_dose_response_growth_plot_model',
                                                                               label = 'Shape type',
                                                                               min = 1,
                                                                               max = 25,
                                                                               value = 15),

                                                                   sliderInput(inputId = 'shape_size_dose_response_growth_plot_model',
                                                                               label = 'Shape size',
                                                                               min = 0.1,
                                                                               max = 10,
                                                                               value = 2,
                                                                               step = 0.1),

                                                                   sliderInput(inputId = 'axis_size_dose_response_growth_plot_model',
                                                                               label = 'Axis title font size',
                                                                               min = 0.1,
                                                                               max = 10,
                                                                               value = 1.3,
                                                                               step = 0.1),

                                                                   sliderInput(inputId = 'lab_size_dose_response_growth_plot_model',
                                                                               label = 'Axis label font size',
                                                                               min = 0.1,
                                                                               max = 10,
                                                                               value = 1.3,
                                                                               step = 0.1),

                                                                   sliderInput(inputId = 'line_width_dose_response_growth_plot_model',
                                                                               label = 'Line width',
                                                                               min = 0.01,
                                                                               max = 10,
                                                                               value = 1),

                                                                   conditionalPanel(
                                                                     condition = "input.log_transform_x_axis_dose_response_growth_plot_model",
                                                                     sliderInput(inputId = "nbreaks_x_growth_dose_response_plot_model",
                                                                                 label = "Number of breaks on x-axis",
                                                                                 min = 1,
                                                                                 max = 20,
                                                                                 value = 6)
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "input.log_transform_y_axis_dose_response_growth_plot_model",
                                                                     sliderInput(inputId = "nbreaks_y_growth_dose_response_plot_model",
                                                                                 label = "Number of breaks on y-axis",
                                                                                 min = 1,
                                                                                 max = 20,
                                                                                 value = 6)
                                                                   ),

                                                                   checkboxInput(inputId = 'show_ec50_indicator_lines_dose_response_growth_plot_model',
                                                                                 label = 'Show EC50 indicator lines',
                                                                                 value = TRUE),

                                                                   conditionalPanel(
                                                                     condition = "input.log_transform_x_axis_dose_response_growth_plot_model",
                                                                     checkboxInput(inputId = 'show_break_dose_response_growth_plot_model',
                                                                                   label = 'Show x axis break',
                                                                                   value = TRUE)
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "input.show_break_dose_response_growth_plot_model && input.log_transform_x_axis_dose_response_growth_plot_model",
                                                                     QurvE:::numberInput(inputId = 'bp_dose_response_growth_plot_model',
                                                                                 label = 'Break point position',
                                                                                 value = ""),
                                                                   ),

                                                                   strong("x-Range"),
                                                                   fluidRow(
                                                                     column(5,
                                                                            textInput(inputId = "x_range_min_dose_response_growth_plot_model",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "min"
                                                                            )
                                                                     ),

                                                                     column(5,
                                                                            textInput(inputId = "x_range_max_dose_response_growth_plot_model",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "max"
                                                                            )
                                                                     )
                                                                   ),

                                                                   strong("y-Range"),
                                                                   fluidRow(
                                                                     column(5,
                                                                            textInput(inputId = "y_range_min_dose_response_growth_plot_model",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "min"
                                                                            )
                                                                     ),

                                                                     column(5,
                                                                            textInput(inputId = "y_range_max_dose_response_growth_plot_model",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "max"
                                                                            )
                                                                     )
                                                                   ),

                                                                   textInput(inputId = "y_axis_title_dose_response_growth_plot_model",
                                                                             label = "y-axis title",
                                                                             value = ""
                                                                   ),

                                                                   textInput(inputId = "x_axis_title_dose_response_growth_plot_model",
                                                                             label = "x-axis title",
                                                                             value = ""
                                                                   )

                                                                 ), # sidebarPanel

                                                                 mainPanel(

                                                                   selectInput(inputId = 'individual_plots_dose_response_growth_plot_model',
                                                                               label = 'Select plot',
                                                                               choices = "",
                                                                               multiple = FALSE,
                                                                               selectize = FALSE,
                                                                               size = 3),
                                                                   plotOutput("dose_response_growth_plot_model",
                                                                              width = "100%", height = "800px"),

                                                                   fluidRow(
                                                                     column(6, align = "center", offset = 3,
                                                                            actionButton(inputId = "rerun_dr_growth3",
                                                                                         label = "Re-run dose-response analysis with modified parameters",
                                                                                         icon=icon("gears"),
                                                                                         style="padding:5px; font-size:120%"),
                                                                            actionButton(inputId = "restore_dr_growth3",
                                                                                         label = "Restore dose-response analysis",
                                                                                         # icon=icon("gears"),
                                                                                         style="padding:5px; font-size:120%")
                                                                     )
                                                                   ),

                                                                   h3(strong("Export plot")),

                                                                   fluidRow(
                                                                     column(width = 4,
                                                                            numericInput(inputId = "width_download_dose_response_growth_plot_model",
                                                                                         label = "Width (in inches)",
                                                                                         value = 7)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            numericInput(inputId = "height_download_dose_response_growth_plot_model",
                                                                                         label = "Height (in inches)",
                                                                                         value = 6)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            numericInput(inputId = "dpi_download_dose_response_growth_plot_model",
                                                                                         label = "DPI",
                                                                                         value = 300)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            downloadButton('download_dose_response_growth_plot_model',"Download Plot"),

                                                                            radioButtons("format_download_dose_response_growth_plot_model",
                                                                                         label = NULL,
                                                                                         choices = c("PNG" = ".png",
                                                                                                     "PDF" = ".pdf"),
                                                                                         selected = ".png",
                                                                                         inline = TRUE)
                                                                     ), # column
                                                                   ) # fluidRow
                                                                 ), # mainPanel
                                                        ), # tabPanel(title = "Dose-response analysis"

                                                        ### Growth DR Plots Bootstrap ####

                                                        tabPanel(title = "Dose-Response Analysis (Bootstrap)", value = "tabPanel_Visualize_Growth_DoseResponse_Spline_bt",
                                                                 sidebarPanel(

                                                                   h3('Customize plot appearance'),


                                                                   sliderInput(inputId = 'shape_type_dose_response_growth_plot_bt',
                                                                               label = 'Shape type',
                                                                               min = 1,
                                                                               max = 25,
                                                                               value = 15),

                                                                   sliderInput(inputId = 'shape_size_dose_response_growth_plot_bt',
                                                                               label = 'Shape size',
                                                                               min = 0.1,
                                                                               max = 10,
                                                                               value = 2,
                                                                               step = 0.1),

                                                                   sliderInput(inputId = 'axis_size_dose_response_growth_plot_bt',
                                                                               label = 'Axis title font size',
                                                                               min = 0.1,
                                                                               max = 10,
                                                                               value = 1.3,
                                                                               step = 0.1),


                                                                   sliderInput(inputId = 'lab_size_dose_response_growth_plot_bt',
                                                                               label = 'Axis label font size',
                                                                               min = 0.1,
                                                                               max = 10,
                                                                               value = 1.3,
                                                                               step = 0.1),

                                                                   sliderInput(inputId = 'line_width_dose_response_growth_plot_bt',
                                                                               label = 'Line width',
                                                                               min = 0.01,
                                                                               max = 10,
                                                                               value = 1),

                                                                 ), # sidebarPanel

                                                                 mainPanel(
                                                                   h3('Individual plots'),
                                                                   selectInput(inputId = 'individual_plots_dose_response_growth_plot_bt',
                                                                               label = 'Select plot',
                                                                               choices = "",
                                                                               multiple = FALSE,
                                                                               selectize = FALSE,
                                                                               size = 3),
                                                                   plotOutput("dose_response_growth_plot_individual_bt",
                                                                              width = "100%", height = "800px"),

                                                                   h3(strong("Export plot")),

                                                                   fluidRow(
                                                                     column(width = 4,
                                                                            numericInput(inputId = "width_download_dose_response_growth_plot_individual_bt",
                                                                                         label = "Width (in inches)",
                                                                                         value = 7)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            numericInput(inputId = "height_download_dose_response_growth_plot_individual_bt",
                                                                                         label = "Height (in inches)",
                                                                                         value = 6)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            numericInput(inputId = "dpi_download_dose_response_growth_plot_individual_bt",
                                                                                         label = "DPI",
                                                                                         value = 300)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            downloadButton('download_dose_response_growth_plot_individual_bt',"Download Plot"),

                                                                            radioButtons("format_download_dose_response_growth_plot_individual_bt",
                                                                                         label = NULL,
                                                                                         choices = c("PNG" = ".png",
                                                                                                     "PDF" = ".pdf"),
                                                                                         selected = ".png",
                                                                                         inline = TRUE)
                                                                     ), # column
                                                                   ) # fluidRow
                                                                 ) #  mainPanel
                                                        ), # tabPanel(title = "Dose-response analysis (Bootstrap)"

                                                        ### Growth DR Parameters ####

                                                        tabPanel(title = "DR Parameter Plots",value = "tabPanel_Visualize_Growth_DoseResponseParameters",
                                                                 sidebarPanel(
                                                                   selectInput(inputId = "parameter_dr_parameter_growth_plot",
                                                                               label = "Parameter",
                                                                               choices = ""
                                                                   ),

                                                                   textInput(inputId = "select_sample_based_on_string_growth_dr_parameter_plot",
                                                                             label = "Select sample based on string (separated by ;)"
                                                                   ),

                                                                   textInput(inputId = "exclude_sample_based_on_strings_growth_dr_parameter_plot",
                                                                             label = "Exclude sample based on strings (separated by ;)"
                                                                   ),

                                                                   checkboxInput(inputId = 'normalize_to_reference_growth_dr_parameter_plot',
                                                                                 label = 'normalize to reference',
                                                                                 value = FALSE),

                                                                   h3("Customize plot appearance"),

                                                                   # Conditional Panel
                                                                   conditionalPanel(condition = "input.normalize_to_reference_growth_dr_parameter_plot",
                                                                                    # reactive selection
                                                                                    selectInput(inputId = 'reference_condition_growth_dr_parameter_plot',
                                                                                                label = 'Reference condition',
                                                                                                choices = ""
                                                                                    )
                                                                   ),


                                                                   sliderInput(inputId = "basesize_growth_dr_parameter_plot",
                                                                               label = "Base font size",
                                                                               min = 10,
                                                                               max = 35,
                                                                               value = 23,
                                                                               step = 0.5),

                                                                   sliderInput(inputId = "label.size_growth_dr_parameter_plot",
                                                                               label = "Label font size",
                                                                               min = 5,
                                                                               max = 35,
                                                                               value = 20,
                                                                               step = 0.5)


                                                                 ),

                                                                 mainPanel(
                                                                   plotOutput("growth_dr_parameter_plot",
                                                                              width = "100%", height = "800px"),

                                                                   h3(strong("Export plot")),

                                                                   fluidRow(
                                                                     column(width = 4,
                                                                            numericInput(inputId = "width_download_growth_dr_parameter_plot",
                                                                                         label = "Width (in inches)",
                                                                                         value = 7)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            numericInput(inputId = "height_download_growth_dr_parameter_plot",
                                                                                         label = "Height (in inches)",
                                                                                         value = 6)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            numericInput(inputId = "dpi_download_growth_dr_parameter_plot",
                                                                                         label = "DPI",
                                                                                         value = 300)
                                                                     ), # column

                                                                     column(width = 5,
                                                                            downloadButton('download_growth_dr_parameter_plot',"Download Plot"),

                                                                            radioButtons("format_download_growth_dr_parameter_plot",
                                                                                         label = NULL,
                                                                                         choices = c("PNG" = ".png",
                                                                                                     "PDF" = ".pdf"),
                                                                                         selected = ".png",
                                                                                         inline = TRUE)
                                                                     ), # column


                                                                   ) # fluidRow
                                                                 ) #  mainPanel
                                                        ), # tabPanel Growth_Parameter_Plots
                                            )
                                   ),

                                   ## Fluorescence Plots ####
                                   tabPanel(title = "Fluorescence Plots",  value = "tabPanel_Visualize_Fluorescence",
                                            h1("Fluorescence Plots"),
                                            tabsetPanel(type = "tabs",id = "tabsetPanel_Visualize_Fluorescence",

                                                        ### Fluorescence Group Plots ####

                                                        tabPanel(title = "Group plots",
                                                                 sidebarPanel(

                                                                   selectInput(inputId = "data_type_fluorescence_group_plot",
                                                                               label = "Data type",
                                                                               choices = c("Raw fluorescence" = "raw",
                                                                                           "Spline fits FL" = "spline",
                                                                                           "Normalized FL" = "norm.fl"
                                                                               )
                                                                   ),

                                                                   checkboxInput(inputId = "select_string_visualize_fluorescence_group",
                                                                                 label = "(De-)select samples based on string",
                                                                                 value = FALSE),

                                                                   conditionalPanel(
                                                                     condition = "!input.select_string_visualize_fluorescence_group && !input.plot_group_averages_fluorescence_group_plot",
                                                                     selectizeInput(inputId = "samples_visualize_fluorescence_group",
                                                                                    label = "Samples:",
                                                                                    width = "100%",
                                                                                    choices = "",
                                                                                    multiple = TRUE,
                                                                                    options = list(closeAfterSelect = FALSE,
                                                                                                   plugins= list('remove_button'))
                                                                     )
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "!input.select_string_visualize_fluorescence_group && input.plot_group_averages_fluorescence_group_plot",
                                                                     selectizeInput(inputId = "groups_visualize_fluorescence_group",
                                                                                    label = "Conditions:",
                                                                                    width = "100%",
                                                                                    choices = "",
                                                                                    multiple = TRUE,
                                                                                    options = list(closeAfterSelect = FALSE,
                                                                                                   plugins= list('remove_button'))
                                                                     )
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "input.select_string_visualize_fluorescence_group",
                                                                     textInput(inputId = "select_samples_based_on_string_fluorescence_group_plot",
                                                                               label = "Select sample based on string (separate by ;)"
                                                                     ),

                                                                     textInput(inputId = "exclude_samples_based_on_string_fluorescence_group_plot",
                                                                               label = "Exclude sample based on string (separate by ;)"
                                                                     ),

                                                                   ),
                                                                   conditionalPanel(
                                                                     condition = "input.plot_group_averages_fluorescence_group_plot || input.select_string_visualize_fluorescence_group",
                                                                     textInput(inputId = "select_samples_based_on_concentration_fluorescence_group_plot",
                                                                               label = "Select sample based on concentration (separate by ;)"
                                                                     ),

                                                                     textInput(inputId = "exclude_samples_based_on_concentration_fluorescence_group_plot",
                                                                               label = "Exclude sample based on concentration (separate by ;)"
                                                                     )
                                                                   ),

                                                                   checkboxInput(inputId = "plot_group_averages_fluorescence_group_plot",
                                                                                 label = "Plot group averages",
                                                                                 value = TRUE),

                                                                   conditionalPanel(
                                                                     condition = "input.data_type_fluorescence_group_plot == 'spline' ",
                                                                     checkboxInput(inputId = "plot_derivative_fluorescence_group_plot",
                                                                                   label = "Plot derivative",
                                                                                   value = TRUE)
                                                                   ),

                                                                   h3("Customize plot appearance"),

                                                                   checkboxInput(inputId = "log_transform_y_axis_fluorescence_group_plot",
                                                                                 label = "Log-transform y-axis",
                                                                                 value = FALSE),

                                                                   strong("x-Range"),
                                                                   fluidRow(
                                                                     column(5,
                                                                            textInput(inputId = "x_range_min_fluorescence_group_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "min"
                                                                            )
                                                                     ),

                                                                     column(5,
                                                                            textInput(inputId = "x_range_max_fluorescence_group_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "max"
                                                                            )
                                                                     )
                                                                   ),

                                                                   strong("y-Range"),
                                                                   fluidRow(
                                                                     column(5,
                                                                            textInput(inputId = "y_range_min_fluorescence_group_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "min"
                                                                            )
                                                                     ),

                                                                     column(5,
                                                                            textInput(inputId = "y_range_max_fluorescence_group_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "max"
                                                                            )
                                                                     )
                                                                   ),
                                                                   conditionalPanel(
                                                                     condition = "input.data_type_fluorescence_group_plot == 'spline'",
                                                                     strong("y-Range (derivative)"),
                                                                     fluidRow(
                                                                       column(5,
                                                                              textInput(inputId = "y_range_min_derivative_fluorescence_group_plot",
                                                                                        label = NULL,
                                                                                        value = "min", placeholder = "min"
                                                                              )
                                                                       ),

                                                                       column(5,
                                                                              textInput(inputId = "y_range_max_derivative_fluorescence_group_plot",
                                                                                        label = NULL,
                                                                                        value = "", placeholder = "max"
                                                                              )
                                                                       )
                                                                     )
                                                                   ),

                                                                   textInput(inputId = "y_axis_title_fluorescence_group_plot",
                                                                             label = "y-axis title",
                                                                             value = ""
                                                                   ),

                                                                   textInput(inputId = "x_axis_title_fluorescence_group_plot",
                                                                             label = "x-axis title",
                                                                             value = ""
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "input.data_type_fluorescence_group_plot == 'spline' ",
                                                                     textInput(inputId = "y_axis_title_derivative_fluorescence_group_plot",
                                                                               label = "y-axis title derivative",
                                                                               value = ""
                                                                     )
                                                                   ),

                                                                   sliderInput(inputId = "nbreaks_fluorescence_group_plot",
                                                                               label = "Number of breaks on y-axis",
                                                                               min = 1,
                                                                               max = 20,
                                                                               value = 6),

                                                                   sliderInput(inputId = "line_width_fluorescence_group_plot",
                                                                               label = "Line width",
                                                                               min = 0.01,
                                                                               max = 10,
                                                                               value = 1.1),

                                                                   sliderInput(inputId = 'base_size_fluorescence_group_plot',
                                                                               label = 'Base font size',
                                                                               min = 10,
                                                                               max = 35,
                                                                               value = 23,
                                                                               step = 0.5),

                                                                   sliderInput(inputId = 'legend_ncol_fluorescence_group_plot',
                                                                               label = 'Number of legend columns',
                                                                               min = 1,
                                                                               max = 10,
                                                                               value = 4,
                                                                               step = 1),

                                                                   selectInput(inputId = "legend_position_fluorescence_group_plot",
                                                                               label = "Legend position",
                                                                               choices = c("Bottom" = "bottom",
                                                                                           "Top" = "top",
                                                                                           "Left" = "left",
                                                                                           "Right" = "right")
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "output.more_than_two_conc",
                                                                     checkboxInput(inputId = "color_groups_fluorescence_group_plot",
                                                                                   label = "Color samples by group",
                                                                                   value = TRUE)
                                                                   ),

                                                                   textInput(
                                                                     inputId = 'custom_colors_fluorescence_group_plot',
                                                                     label = 'Custom colors'
                                                                   ),
                                                                   bsPopover(id = "custom_colors_fluorescence_group_plot",
                                                                             title = HTML("<em>Provide custom colors</em>"), placement = "top",
                                                                             content = "Enter colors either by name (e.g., red, blue, coral3) or via their hexadecimal code (e.g., #AE4371, #CCFF00FF, #0066FFFF). Separate colors with a comma. A full list of colors available by name can be found at http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf"
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "output.more_than_two_conc && input.color_groups_fluorescence_group_plot",
                                                                     selectizeInput(inputId = "color_palettes_fluorescence_group_plot",
                                                                                    label = "Change color palettes",
                                                                                    width = "100%",
                                                                                    choices = names(QurvE:::single_hue_palettes),
                                                                                    selected = names(QurvE:::single_hue_palettes),
                                                                                    multiple = TRUE,
                                                                                    options = list(closeAfterSelect = FALSE,
                                                                                                   plugins= list('remove_button'))
                                                                     ),
                                                                     bsPopover(id = "color_palettes_fluorescence_group_plot",
                                                                               title = HTML("<em>Define the colors used to display sample groups with identical concentrations</em>"), placement = "top",
                                                                               content = "The number of selected color palettes must be at least the number of displayed groups. The order of the chosen palettes corresponds to the oder of conditions in the legend."
                                                                     ),
                                                                   ),

                                                                 ), # Side panel growth group plots

                                                                 mainPanel(
                                                                   withSpinner(
                                                                   plotOutput("fluorescence_group_plot",
                                                                              width = "100%", height = "1000px")
                                                                   ),

                                                                   h3(strong("Export plot")),

                                                                   fluidRow(
                                                                     column(width = 4,
                                                                            numericInput(inputId = "width_download_fluorescence_group_plot",
                                                                                         label = "Width (in inches)",
                                                                                         value = 7)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            numericInput(inputId = "height_download_fluorescence_group_plot",
                                                                                         label = "Height (in inches)",
                                                                                         value = 6)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            numericInput(inputId = "dpi_download_fluorescence_group_plot",
                                                                                         label = "DPI",
                                                                                         value = 300)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            downloadButton('download_fluorescence_group_plot',"Download Plot"),

                                                                            radioButtons("format_download_fluorescence_group_plot",
                                                                                         label = NULL,
                                                                                         choices = c("PNG" = ".png",
                                                                                                     "PDF" = ".pdf"),
                                                                                         selected = ".png",
                                                                                         inline = TRUE)
                                                                     ), # column


                                                                   ) # fluidRow
                                                                 ) # mainPanel


                                                        ), # tabPanel Group_Plots

                                                        ### Dual Plots ####

                                                        tabPanel(title = "Growth & Flourescence Plot", value = "tabPabel_Visualize_Dual",
                                                                 h1("Growth & Flourescence Plot"),

                                                                 sidebarPanel(

                                                                   selectInput(inputId = "fluorescence_type_dual_plot",
                                                                               label = "Fluorescence type",
                                                                               choices = c("Fluorescence" = "fl",
                                                                                           "Normalized fluorescence" = "norm.fl"
                                                                               )
                                                                   ),

                                                                   checkboxInput(inputId = "select_string_visualize_dual_plot",
                                                                                 label = "(De-)select samples based on string",
                                                                                 value = FALSE),

                                                                   conditionalPanel(
                                                                     condition = "!input.select_string_visualize_dual_plot",
                                                                     selectizeInput(inputId = "samples_visualize_dual_plot",
                                                                                    label = "Samples:",
                                                                                    width = "100%",
                                                                                    choices = "",
                                                                                    multiple = TRUE,
                                                                                    options = list(closeAfterSelect = FALSE,
                                                                                                   plugins= list('remove_button'))
                                                                     )
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "input.select_string_visualize_dual_plot",
                                                                     textInput(inputId = "select_samples_based_on_string_dual_plot",
                                                                               label = "Select sample based on string (separate by ;)"
                                                                     ),

                                                                     textInput(inputId = "exclude_samples_based_on_string_dual_plot",
                                                                               label = "Exclude sample based on string (separate by ;)"
                                                                     ),

                                                                   ),
                                                                   conditionalPanel(
                                                                     condition = "input.plot_group_averages_dual_plot || input.select_string_visualize_dual_plot",
                                                                     textInput(inputId = "select_samples_based_on_concentration_dual_plot",
                                                                               label = "Select sample based on concentration (separate by ;)"
                                                                     ),

                                                                     textInput(inputId = "exclude_samples_based_on_concentration_dual_plot",
                                                                               label = "Exclude sample based on concentration (separate by ;)"
                                                                     )
                                                                   ),

                                                                   checkboxInput(inputId = "plot_group_averages_dual_plot",
                                                                                 label = "Plot group averages",
                                                                                 value = TRUE),

                                                                   h3("Customize plot appearance"),

                                                                   checkboxInput(inputId = "log_transform_y_axis_growth_dual_plot",
                                                                                 label = "Log-transform y-axis (growth)",
                                                                                 value = FALSE),

                                                                   checkboxInput(inputId = "log_transform_y_axis_fluorescence_dual_plot",
                                                                                 label = "Log-transform y-axis (Fluorescence)",
                                                                                 value = FALSE),

                                                                   strong("x-Range"),
                                                                   fluidRow(
                                                                     column(5,
                                                                            textInput(inputId = "x_range_min_dual_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "min"
                                                                            )
                                                                     ),

                                                                     column(5,
                                                                            textInput(inputId = "x_range_max_dual_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "max"
                                                                            )
                                                                     )
                                                                   ),

                                                                   strong("y-Range (growth)"),
                                                                   fluidRow(
                                                                     column(5,
                                                                            textInput(inputId = "y_range_min_growth_dual_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "min"
                                                                            )
                                                                     ),

                                                                     column(5,
                                                                            textInput(inputId = "y_range_max_growth_dual_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "max"
                                                                            )
                                                                     )
                                                                   ),

                                                                   strong("y-Range (Fluorescence"),
                                                                   fluidRow(
                                                                     column(5,
                                                                            textInput(inputId = "y_range_min_fluorescence_dual_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "min"
                                                                            )
                                                                     ),

                                                                     column(5,
                                                                            textInput(inputId = "y_range_max_fluorescence_dual_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "max"
                                                                            )
                                                                     )
                                                                   ),

                                                                   textInput(inputId = "y_axis_title_growth_dual_plot",
                                                                             label = "y-axis title (growth)",
                                                                             value = ""
                                                                   ),

                                                                   textInput(inputId = "y_axis_title_fluorescence_dual_plot",
                                                                             label = "y-axis title (Fluorescence)",
                                                                             value = ""
                                                                   ),

                                                                   textInput(inputId = "x_axis_title_dual_plot",
                                                                             label = "x-axis title",
                                                                             value = ""
                                                                   ),

                                                                   sliderInput(inputId = "nbreaks_dual_plot",
                                                                               label = "Number of breaks on y-axis",
                                                                               min = 1,
                                                                               max = 20,
                                                                               value = 6),

                                                                   sliderInput(inputId = "line_width_dual_plot",
                                                                               label = "Line width",
                                                                               min = 0.01,
                                                                               max = 10,
                                                                               value = 1.1),

                                                                   sliderInput(inputId = 'base_size_dual_plot',
                                                                               label = 'Base font size',
                                                                               min = 10,
                                                                               max = 35,
                                                                               value = 23,
                                                                               step = 0.5),

                                                                   selectInput(inputId = "legend_position_dual_plot",
                                                                               label = "Legend position",
                                                                               choices = c("Bottom" = "bottom",
                                                                                           "Top" = "top",
                                                                                           "Left" = "left",
                                                                                           "Right" = "right")
                                                                   ),

                                                                   sliderInput(inputId = 'legend_ncol_dual_plot',
                                                                               label = 'Number of legend columns',
                                                                               min = 1,
                                                                               max = 10,
                                                                               value = 4,
                                                                               step = 1
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "output.more_than_two_conc",
                                                                     checkboxInput(inputId = "color_groups_dual_plot",
                                                                                   label = "Color samples by group",
                                                                                   value = TRUE)
                                                                   ),

                                                                   textInput(
                                                                     inputId = 'custom_colors_dual_plot',
                                                                     label = 'Custom colors'
                                                                   ),
                                                                   bsPopover(id = "custom_colors_dual_plot",
                                                                             title = HTML("<em>Provide custom colors</em>"), placement = "top",
                                                                             content = "Enter colors either by name (e.g., red, blue, coral3) or via their hexadecimal code (e.g., #AE4371, #CCFF00FF, #0066FFFF). Separate colors with a comma. A full list of colors available by name can be found at http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf"
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "output.more_than_two_conc && input.color_groups_dual_plot",
                                                                     selectizeInput(inputId = "color_palettes_dual_plot",
                                                                                    label = "Change color palettes",
                                                                                    width = "100%",
                                                                                    choices = names(QurvE:::single_hue_palettes),
                                                                                    selected = names(QurvE:::single_hue_palettes),
                                                                                    multiple = TRUE,
                                                                                    options = list(closeAfterSelect = FALSE,
                                                                                                   plugins= list('remove_button'))
                                                                     ),
                                                                     bsPopover(id = "color_palettes_dual_plot",
                                                                               title = HTML("<em>Define the colors used to display sample groups with identical concentrations</em>"), placement = "top",
                                                                               content = "The number of selected color palettes must be at least the number of displayed groups. The order of the chosen palettes corresponds to the oder of conditions in the legend."
                                                                     ),
                                                                   ),
                                                                 ),

                                                                 mainPanel(
                                                                   withSpinner(
                                                                   plotOutput("dual_plot",
                                                                              width = "100%", height = "1000px")
                                                                   ),

                                                                   h3(strong("Export plot")),

                                                                   fluidRow(
                                                                     column(width = 4,
                                                                            numericInput(inputId = "width_download_dual_plot",
                                                                                         label = "Width (in inches)",
                                                                                         value = 7)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            numericInput(inputId = "height_download_dual_plot",
                                                                                         label = "Height (in inches)",
                                                                                         value = 6)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            numericInput(inputId = "dpi_download_dual_plot",
                                                                                         label = "DPI",
                                                                                         value = 300)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            downloadButton('download_dual_plot',"Download Plot"),

                                                                            radioButtons("format_download_dual_plot",
                                                                                         label = NULL,
                                                                                         choices = c("PNG" = ".png",
                                                                                                     "PDF" = ".pdf"),
                                                                                         selected = ".png",
                                                                                         inline = TRUE)
                                                                     ), # column
                                                                   ) # fluidRow
                                                                 ) # mainPanel
                                                        ), # tabPanel(title = "Growth & Flourescence Plot")

                                                        ### Fluorescence Parameter Plots ####

                                                        tabPanel(title = "Parameter plots",
                                                                 sidebarPanel(
                                                                   selectInput(inputId = "parameter_fluorescence_parameter_fluorescence_plot",
                                                                               label = "Parameter",
                                                                               choices = ""
                                                                   ),
                                                                   checkboxInput(inputId = "select_string_visualize_parameter_fluorescence_plot",
                                                                                 label = "(De-)select samples based on string",
                                                                                 value = FALSE),

                                                                   conditionalPanel(
                                                                     condition = "!input.select_string_visualize_parameter_fluorescence_plot",
                                                                     selectizeInput(inputId = "samples_visualize_parameter_fluorescence_plot",
                                                                                    label = "Conditions:",
                                                                                    width = "100%",
                                                                                    choices = "",
                                                                                    multiple = TRUE,
                                                                                    options = list(closeAfterSelect = FALSE,
                                                                                                   plugins= list('remove_button'))
                                                                     )
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "input.select_string_visualize_parameter_fluorescence_plot",
                                                                     textInput(inputId = "select_sample_based_on_string_fluorescence_parameter_plot",
                                                                               label = "Select sample based on string (separate by ;)"
                                                                     ),

                                                                     textInput(inputId = "exclude_sample_based_on_strings_fluorescence_parameter_plot",
                                                                               label = "Exclude sample based on string (separate by ;)"
                                                                     ),

                                                                   ),

                                                                   textInput(inputId = "select_sample_based_on_concentration_fluorescence_parameter_plot",
                                                                             label = "Select sample based on concentration (separate by ;)"
                                                                   ),

                                                                   textInput(inputId = "exclude_sample_based_on_concentration_fluorescence_parameter_plot",
                                                                             label = "Exclude sample based on concentration (separate by ;)"
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

                                                                   h3("Customize plot appearance"),

                                                                   sliderInput(inputId = "shape.size_fluorescence_parameter_plot",
                                                                               label = "Shape size",
                                                                               min = 0.1,
                                                                               max = 10,
                                                                               value = 3,
                                                                               step = 0.1),

                                                                   sliderInput(inputId = "basesize_fluorescence_parameter_plot",
                                                                               label = "Base font size",
                                                                               min = 10,
                                                                               max = 35,
                                                                               value = 23,
                                                                               step = 0.5),

                                                                   sliderInput(inputId = "label.size_fluorescence_parameter_plot",
                                                                               label = "Label font size",
                                                                               min = 5,
                                                                               max = 35,
                                                                               value = 17,
                                                                               step = 0.5),

                                                                   selectInput(inputId = "legend_position_fluorescence_parameter_plot",
                                                                               label = "Legend position",
                                                                               choices = c("Bottom" = "bottom",
                                                                                           "Top" = "top",
                                                                                           "Left" = "left",
                                                                                           "Right" = "right"),
                                                                               selected = "right"
                                                                   ),

                                                                   sliderInput(inputId = 'legend_ncol_fluorescence_parameter_plot',
                                                                               label = 'Number of legend columns',
                                                                               min = 1,
                                                                               max = 10,
                                                                               value = 1,
                                                                               step = 1
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "output.more_than_two_conc",
                                                                     checkboxInput(inputId = "sort_by_conc_fluorescence_parameter_plot",
                                                                                   label = "Sort samples by concentration",
                                                                                   value = FALSE)
                                                                   ),

                                                                   textInput(
                                                                     inputId = 'custom_colors_fluorescence_parameter_plot',
                                                                     label = 'Custom colors'
                                                                   ),
                                                                   bsPopover(id = "custom_colors_fluorescence_parameter_plot",
                                                                             title = HTML("<em>Provide custom colors</em>"), placement = "top",
                                                                             content = "Enter colors either by name (e.g., red, blue, coral3) or via their hexadecimal code (e.g., #AE4371, #CCFF00FF, #0066FFFF). Separate colors with a comma. A full list of colors available by name can be found at http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf"
                                                                   ),


                                                                 ),

                                                                 mainPanel(
                                                                   plotOutput("fluorescence_parameter_plot",
                                                                              width = "100%", height = "800px"),

                                                                   h3(strong("Export plot")),

                                                                   fluidRow(
                                                                     column(width = 4,
                                                                            numericInput(inputId = "width_download_fluorescence_parameter_plot",
                                                                                         label = "Width (in inches)",
                                                                                         value = 7)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            numericInput(inputId = "height_download_fluorescence_parameter_plot",
                                                                                         label = "Height (in inches)",
                                                                                         value = 6)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            numericInput(inputId = "dpi_download_fluorescence_parameter_plot",
                                                                                         label = "DPI",
                                                                                         value = 300)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            downloadButton('download_fluorescence_parameter_plot',"Download Plot"),

                                                                            radioButtons("format_download_fluorescence_parameter_plot",
                                                                                         label = NULL,
                                                                                         choices = c("PNG" = ".png",
                                                                                                     "PDF" = ".pdf"),
                                                                                         selected = ".png",
                                                                                         inline = TRUE)
                                                                     ), # column
                                                                   ) # fluidRow
                                                                 ) # mainPanel
                                                        ), #  tabPanel(title = "Parameter plots"
                                                        ### Fluorescence Grid Plots ####

                                                        tabPanel(title = "Plot Grid",
                                                                 sidebarPanel(

                                                                   selectInput(inputId = "data_type_fluorescence_grid_plot",
                                                                               label = "Data type",
                                                                               choices = c("Raw growth" = "raw",
                                                                                           "Spline fits" = "spline")
                                                                   ),

                                                                   selectInput(inputId = "parameter_parameter_grid_plot_fluorescence",
                                                                               label = "Parameter",
                                                                               choices = ""
                                                                   ),

                                                                   checkboxInput(inputId = "select_string_visualize_fluorescence_grid",
                                                                                 label = "(De-)select samples based on string",
                                                                                 value = FALSE),

                                                                   conditionalPanel(
                                                                     condition = "!input.select_string_visualize_fluorescence_grid && !input.plot_group_averages_fluorescence_grid_plot",
                                                                     selectizeInput(inputId = "samples_visualize_fluorescence_grid",
                                                                                    label = "Samples:",
                                                                                    width = "100%",
                                                                                    choices = "",
                                                                                    multiple = TRUE,
                                                                                    options = list(closeAfterSelect = FALSE,
                                                                                                   plugins= list('remove_button'))
                                                                     ),
                                                                     checkboxInput(inputId = "order_matters_visualize_fluorescence_grid",
                                                                                   label = "Select order matters",
                                                                                   value = FALSE
                                                                     ),
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "!input.select_string_visualize_fluorescence_grid && input.plot_group_averages_fluorescence_grid_plot",
                                                                     selectizeInput(inputId = "groups_visualize_fluorescence_grid",
                                                                                    label = "Conditions:",
                                                                                    width = "100%",
                                                                                    choices = "",
                                                                                    multiple = TRUE,
                                                                                    options = list(closeAfterSelect = FALSE,
                                                                                                   plugins= list('remove_button'))
                                                                     )
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "input.select_string_visualize_fluorescence_grid",
                                                                     textInput(inputId = "select_samples_based_on_string_fluorescence_grid_plot",
                                                                               label = "Select sample based on string (separate by ;)"
                                                                     ),

                                                                     textInput(inputId = "exclude_samples_based_on_string_fluorescence_grid_plot",
                                                                               label = "Exclude sample based on string (separate by ;)"
                                                                     ),

                                                                   ),
                                                                   conditionalPanel(
                                                                     condition = "input.plot_group_averages_fluorescence_grid_plot || input.select_string_visualize_fluorescence_grid",
                                                                     textInput(inputId = "select_samples_based_on_concentration_fluorescence_grid_plot",
                                                                               label = "Select sample based on concentration (separate by ;)"
                                                                     ),

                                                                     textInput(inputId = "exclude_samples_based_on_concentration_fluorescence_grid_plot",
                                                                               label = "Exclude sample based on concentration (separate by ;)"
                                                                     )
                                                                   ),

                                                                   checkboxInput(inputId = "plot_group_averages_fluorescence_grid_plot",
                                                                                 label = "Plot group averages",
                                                                                 value = TRUE),

                                                                   conditionalPanel(
                                                                     condition = "output.more_than_two_conc",
                                                                     checkboxInput(inputId = "sort_by_conc_fluorescence_grid_plot",
                                                                                   label = "Sort by concentration",
                                                                                   value = TRUE)
                                                                   ),

                                                                   h3("Customize plot appearance"),

                                                                   checkboxInput(inputId = "log_transform_y_axis_fluorescence_grid_plot",
                                                                                 label = "Log-transform y-axis",
                                                                                 value = TRUE),

                                                                   strong("x-Range"),
                                                                   fluidRow(
                                                                     column(5,
                                                                            textInput(inputId = "x_range_min_fluorescence_grid_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "min"
                                                                            )
                                                                     ),

                                                                     column(5,
                                                                            textInput(inputId = "x_range_max_fluorescence_grid_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "max"
                                                                            )
                                                                     )
                                                                   ),

                                                                   strong("y-Range"),
                                                                   fluidRow(
                                                                     column(5,
                                                                            textInput(inputId = "y_range_min_fluorescence_grid_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "min"
                                                                            )
                                                                     ),

                                                                     column(5,
                                                                            textInput(inputId = "y_range_max_fluorescence_grid_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "max"
                                                                            )
                                                                     )
                                                                   ),

                                                                   strong("Color scale limits"),
                                                                   fluidRow(
                                                                     column(5,
                                                                            textInput(inputId = "legend_lim_min_fluorescence_grid_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "min"
                                                                            )
                                                                     ),

                                                                     column(5,
                                                                            textInput(inputId = "legend_lim_max_fluorescence_grid_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "max"
                                                                            )
                                                                     )
                                                                   ),


                                                                   textInput(inputId = "y_axis_title_fluorescence_grid_plot",
                                                                             label = "y-axis title",
                                                                             value = ""
                                                                   ),

                                                                   textInput(inputId = "x_axis_title_fluorescence_grid_plot",
                                                                             label = "x-axis title",
                                                                             value = ""
                                                                   ),

                                                                   sliderInput(inputId = "nbreaks_fluorescence_grid_plot",
                                                                               label = "Number of breaks on y-axis",
                                                                               min = 1,
                                                                               max = 20,
                                                                               value = 6),

                                                                   sliderInput(inputId = "line_width_fluorescence_grid_plot",
                                                                               label = "Line width",
                                                                               min = 0.01,
                                                                               max = 10,
                                                                               value = 1.1),

                                                                   sliderInput(inputId = 'base_size_fluorescence_grid_plot',
                                                                               label = 'Base font size',
                                                                               min = 10,
                                                                               max = 35,
                                                                               value = 23,
                                                                               step = 0.5),


                                                                   conditionalPanel(
                                                                     condition = "!input.sort_by_conc_fluorescence_grid_plot",
                                                                     sliderInput(inputId = "nrows_fluorescence_grid_plot",
                                                                                 label = "Number of rows in grid",
                                                                                 min = 1,
                                                                                 max = 20,
                                                                                 value = 2)
                                                                   ),


                                                                   selectInput(inputId = "color_palettes_grid_plot_fluorescence",
                                                                               label = "Change color palette",
                                                                               width = "100%",
                                                                               choices = names(QurvE:::single_hue_palettes),
                                                                               selected = names(QurvE:::single_hue_palettes)[1],
                                                                               multiple = FALSE
                                                                   ),
                                                                   bsPopover(id = "color_palettes_grid_plot_fluorescence",
                                                                             title = HTML("<em>Define the colors used to visualize the value of the chosen parameter</em>"), placement = "top",
                                                                             content = ""
                                                                   ),

                                                                   checkboxInput(inputId = "invert_color_palette_grid_plot_fluorescence",
                                                                                 label = "Invert color palette",
                                                                                 value = FALSE)

                                                                 ), # Side panel fluorescence group plots

                                                                 mainPanel(
                                                                   withSpinner(
                                                                     plotOutput("fluorescence_grid_plot",
                                                                                width = "100%", height = "1000px"),

                                                                   ),

                                                                   h3(strong("Export plot")),

                                                                   fluidRow(
                                                                     column(width = 4,
                                                                            numericInput(inputId = "width_download_fluorescence_grid_plot",
                                                                                         label = "Width (in inches)",
                                                                                         value = 10)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            numericInput(inputId = "height_download_fluorescence_grid_plot",
                                                                                         label = "Height (in inches)",
                                                                                         value = 9)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            numericInput(inputId = "dpi_download_fluorescence_grid_plot",
                                                                                         label = "DPI",
                                                                                         value = 300)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            downloadButton('download_fluorescence_grid_plot',"Download Plot"),

                                                                            radioButtons("format_download_fluorescence_grid_plot",
                                                                                         label = NULL,
                                                                                         choices = c("PNG" = ".png",
                                                                                                     "PDF" = ".pdf"),
                                                                                         selected = ".png",
                                                                                         inline = TRUE)
                                                                     ) # column
                                                                   ) # fluidRow
                                                                 ) #  mainPanel
                                                        ),
                                                        ### Fluorescence DR Plots Spline ####

                                                        tabPanel(title = "Dose-response analysis", value = "tabPanel_Visualize_Fluorescence_DoseResponse_spline",
                                                                 sidebarPanel(
                                                                   wellPanel(
                                                                     style='padding: 1; border-color: #ADADAD; padding-bottom: 0',
                                                                     checkboxInput(inputId = 'combine_conditions_into_a_single_plot_dose_response_fluorescence_plot',
                                                                                   label = 'Combine conditions into a single plot',
                                                                                   value = TRUE)
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "input.combine_conditions_into_a_single_plot_dose_response_fluorescence_plot",
                                                                     textInput(inputId = "select_samples_based_on_string_dose_response_fluorescence_plot",
                                                                               label = "Select sample based on string (separate by ;)"
                                                                     )
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "input.combine_conditions_into_a_single_plot_dose_response_fluorescence_plot",
                                                                     textInput(inputId = "exclude_samples_based_on_string_dose_response_fluorescence_plot",
                                                                               label = "Exclude sample based on string (separate by ;)"
                                                                     )
                                                                   ),

                                                                   h3('Customize plot appearance'),

                                                                   checkboxInput(inputId = "log_transform_y_axis_dose_response_fluorescence_plot",
                                                                                 label = "Log-transform y-axis",
                                                                                 value = FALSE),

                                                                   checkboxInput(inputId = "log_transform_x_axis_dose_response_fluorescence_plot",
                                                                                 label = "Log-transform x-axis",
                                                                                 value = FALSE),

                                                                   sliderInput(inputId = 'shape_type_dose_response_fluorescence_plot',
                                                                               label = 'Shape type',
                                                                               min = 1,
                                                                               max = 25,
                                                                               value = 15),

                                                                   sliderInput(inputId = 'shape_size_dose_response_fluorescence_plot',
                                                                               label = 'Shape size',
                                                                               min = 0.1,
                                                                               max = 10,
                                                                               value = 2,
                                                                               step = 0.1),

                                                                   conditionalPanel(
                                                                     condition = "input.combine_conditions_into_a_single_plot_dose_response_fluorescence_plot",
                                                                     sliderInput(inputId = 'base_size_dose_response_fluorescence_plot',
                                                                                 label = 'Base font size',
                                                                                 min = 10,
                                                                                 max = 35,
                                                                                 value = 15,
                                                                                 step = 0.5)
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "!input.combine_conditions_into_a_single_plot_dose_response_fluorescence_plot",
                                                                     sliderInput(inputId = 'axis_size_dose_response_fluorescence_plot',
                                                                                 label = 'Axis title font size',
                                                                                 min = 0.1,
                                                                                 max = 10,
                                                                                 value = 1.5,
                                                                                 step = 0.1)
                                                                   ),

                                                                   conditionalPanel(
                                                                     condition = "!input.combine_conditions_into_a_single_plot_dose_response_fluorescence_plot",
                                                                     sliderInput(inputId = 'lab_size_dose_response_fluorescence_plot',
                                                                                 label = 'Axis label font size',
                                                                                 min = 0.1,
                                                                                 max = 10,
                                                                                 value = 1.3,
                                                                                 step = 0.1)
                                                                   ),

                                                                   sliderInput(inputId = 'line_width_dose_response_fluorescence_plot',
                                                                               label = 'Line width',
                                                                               min = 0.01,
                                                                               max = 10,
                                                                               value = 1),

                                                                   checkboxInput(inputId = 'show_ec50_indicator_lines_dose_response_fluorescence_plot',
                                                                                 label = 'Show EC50 indicator lines',
                                                                                 value = TRUE),

                                                                   strong("x-Range"),
                                                                   fluidRow(
                                                                     column(5,
                                                                            textInput(inputId = "x_range_min_dose_response_fluorescence_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "min"
                                                                            )
                                                                     ),

                                                                     column(5,
                                                                            textInput(inputId = "x_range_max_dose_response_fluorescence_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "max"
                                                                            )
                                                                     )
                                                                   ),

                                                                   strong("y-Range"),
                                                                   fluidRow(
                                                                     column(5,
                                                                            textInput(inputId = "y_range_min_dose_response_fluorescence_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "min"
                                                                            )
                                                                     ),

                                                                     column(5,
                                                                            textInput(inputId = "y_range_max_dose_response_fluorescence_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "max"
                                                                            )
                                                                     )
                                                                   ),

                                                                   textInput(inputId = "y_axis_title_dose_response_fluorescence_plot",
                                                                             label = "y-axis title",
                                                                             value = ""
                                                                   ),

                                                                   textInput(inputId = "x_axis_title_dose_response_fluorescence_plot",
                                                                             label = "x-axis title",
                                                                             value = ""
                                                                   )

                                                                 ), # sidebarPanel

                                                                 conditionalPanel(condition = "input.combine_conditions_into_a_single_plot_dose_response_fluorescence_plot",
                                                                                  mainPanel(
                                                                                    h3('Combined plots'),
                                                                                    plotOutput("dose_response_plot_fluorescence_combined",
                                                                                               width = "100%", height = "800px"),

                                                                                    fluidRow(
                                                                                      column(6, align = "center", offset = 3,
                                                                                             actionButton(inputId = "rerun_dr_fluorescence",
                                                                                                          label = "Re-run dose-response analysis with modified parameters",
                                                                                                          icon=icon("gears"),
                                                                                                          style="padding:5px; font-size:120%"),
                                                                                             actionButton(inputId = "restore_dr_fluorescence",
                                                                                                          label = "Restore dose-response analysis",
                                                                                                          # icon=icon("gears"),
                                                                                                          style="padding:5px; font-size:120%")
                                                                                      )
                                                                                    ),

                                                                                    h3(strong("Export plot")),

                                                                                    fluidRow(
                                                                                      column(width = 4,
                                                                                             numericInput(inputId = "width_download_dose_response_plot_fluorescence_combined",
                                                                                                          label = "Width (in inches)",
                                                                                                          value = 7)
                                                                                      ), # column
                                                                                      column(width = 4,
                                                                                             numericInput(inputId = "height_download_dose_response_plot_fluorescence_combined",
                                                                                                          label = "Height (in inches)",
                                                                                                          value = 6)
                                                                                      ), # column
                                                                                      column(width = 4,
                                                                                             numericInput(inputId = "dpi_download_dose_response_plot_fluorescence_combined",
                                                                                                          label = "DPI",
                                                                                                          value = 300)
                                                                                      ), # column
                                                                                      column(width = 4,
                                                                                             downloadButton('download_dose_response_plot_fluorescence_combined',"Download Plot"),

                                                                                             radioButtons("format_download_dose_response_plot_fluorescence_combined",
                                                                                                          label = NULL,
                                                                                                          choices = c("PNG" = ".png",
                                                                                                                      "PDF" = ".pdf"),
                                                                                                          selected = ".png",
                                                                                                          inline = TRUE)
                                                                                      ), # column


                                                                                    ) # fluidRow
                                                                                  ) # mainPanel
                                                                 ),

                                                                 conditionalPanel(condition = "!input.combine_conditions_into_a_single_plot_dose_response_fluorescence_plot",
                                                                                  mainPanel(
                                                                                    h3('Individual plots'),
                                                                                    selectInput(inputId = 'individual_plots_dose_response_fluorescence_plot',
                                                                                                label = 'Select plot',
                                                                                                choices = "",
                                                                                                multiple = FALSE,
                                                                                                selectize = FALSE,
                                                                                                size = 3),

                                                                                    plotOutput("dose_response_fluorescence_plot_individual",
                                                                                               width = "100%", height = "800px"),

                                                                                    fluidRow(
                                                                                      column(6, align = "center", offset = 3,
                                                                                             actionButton(inputId = "rerun_dr_fluorescence2",
                                                                                                          label = "Re-run dose-response analysis with modified parameters",
                                                                                                          icon=icon("gears"),
                                                                                                          style="padding:5px; font-size:120%"),
                                                                                             actionButton(inputId = "restore_dr_fluorescence2",
                                                                                                          label = "Restore dose-response analysis",
                                                                                                          # icon=icon("gears"),
                                                                                                          style="padding:5px; font-size:120%")
                                                                                      )
                                                                                    ),

                                                                                    h3(strong("Export plot")),

                                                                                    fluidRow(
                                                                                      column(width = 4,
                                                                                             numericInput(inputId = "width_download_dose_response_fluorescence_plot_individual",
                                                                                                          label = "Width (in inches)",
                                                                                                          value = 7)
                                                                                      ), # column
                                                                                      column(width = 4,
                                                                                             numericInput(inputId = "height_download_dose_response_fluorescence_plot_individual",
                                                                                                          label = "Height (in inches)",
                                                                                                          value = 6)
                                                                                      ), # column
                                                                                      column(width = 4,
                                                                                             numericInput(inputId = "dpi_download_dose_response_fluorescence_plot_individual",
                                                                                                          label = "DPI",
                                                                                                          value = 300)
                                                                                      ), # column
                                                                                      column(width = 4,
                                                                                             downloadButton('download_dose_response_fluorescence_plot_individual',"Download Plot"),

                                                                                             radioButtons("format_download_dose_response_fluorescence_plot_individual",
                                                                                                          label = NULL,
                                                                                                          choices = c("PNG" = ".png",
                                                                                                                      "PDF" = ".pdf"),
                                                                                                          selected = ".png",
                                                                                                          inline = TRUE)
                                                                                      ), # column
                                                                                    ) # fluidRow
                                                                                  ) # mainPanel

                                                                 ), # conditionalPanel
                                                        ), #  tabPanel(title = "Dose-response analysis", value = "tabPanel_Visualize_Fluorescence_DoseResponse_spline",

                                                        ### Fluorescence DR Plots Model ####
                                                        tabPanel(title = "Dose-response analysis", value = "tabPanel_Visualize_Fluorescence_DoseResponse_model",
                                                                 sidebarPanel(

                                                                   checkboxInput(inputId = "log_transform_y_axis_dose_response_model_fluorescence_plot",
                                                                                 label = "Log-transform y-axis",
                                                                                 value = TRUE),

                                                                   checkboxInput(inputId = "log_transform_x_axis_dose_response_model_fluorescence_plot",
                                                                                 label = "Log-transform x-axis",
                                                                                 value = TRUE),

                                                                   h3('Customize plot appearance'),
                                                                   sliderInput(inputId = 'shape_type_dose_response_model_fluorescence_plot',
                                                                               label = 'Shape type',
                                                                               min = 1,
                                                                               max = 25,
                                                                               value = 15),

                                                                   sliderInput(inputId = 'shape_size_dose_response_model_fluorescence_plot',
                                                                               label = 'Shape size',
                                                                               min = 0.1,
                                                                               max = 10,
                                                                               value = 2,
                                                                               step = 0.1),

                                                                   sliderInput(inputId = 'axis_size_dose_response_model_fluorescence_plot',
                                                                               label = 'Axis title font size',
                                                                               min = 0.1,
                                                                               max = 10,
                                                                               value = 1.5,
                                                                               step = 0.1),

                                                                   sliderInput(inputId = 'lab_size_dose_response_model_fluorescence_plot',
                                                                               label = 'Axis label font size',
                                                                               min = 0.1,
                                                                               max = 10,
                                                                               value = 1.3,
                                                                               step = 0.1),

                                                                   sliderInput(inputId = 'line_width_dose_response_model_fluorescence_plot',
                                                                               label = 'Line width',
                                                                               min = 0.01,
                                                                               max = 10,
                                                                               value = 1),

                                                                   checkboxInput(inputId = 'show_ec50_indicator_lines_dose_response_model_fluorescence_plot',
                                                                                 label = 'Show EC50 indicator lines',
                                                                                 value = TRUE),

                                                                   strong("x-Range"),
                                                                   fluidRow(
                                                                     column(5,
                                                                            textInput(inputId = "x_range_min_dose_response_model_fluorescence_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "min"
                                                                            )
                                                                     ),

                                                                     column(5,
                                                                            textInput(inputId = "x_range_max_dose_response_model_fluorescence_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "max"
                                                                            )
                                                                     )
                                                                   ),

                                                                   strong("y-Range"),
                                                                   fluidRow(
                                                                     column(5,
                                                                            textInput(inputId = "y_range_min_dose_response_model_fluorescence_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "min"
                                                                            )
                                                                     ),

                                                                     column(5,
                                                                            textInput(inputId = "y_range_max_dose_response_model_fluorescence_plot",
                                                                                      label = NULL,
                                                                                      value = "", placeholder = "max"
                                                                            )
                                                                     )
                                                                   ),
                                                                 ), # sidebarPanel

                                                                 mainPanel(
                                                                   selectInput(inputId = 'individual_plots_dose_response_model_fluorescence_plot',
                                                                               label = 'Select plot',
                                                                               choices = "",
                                                                               multiple = FALSE,
                                                                               selectize = FALSE,
                                                                               size = 3),
                                                                   plotOutput("dose_response_model_fluorescence_plot_individual",
                                                                              width = "100%", height = "800px"),

                                                                   fluidRow(
                                                                     column(6, align = "center", offset = 3,
                                                                            actionButton(inputId = "rerun_dr_fluorescence3",
                                                                                         label = "Re-run dose-response analysis with modified parameters",
                                                                                         icon=icon("gears"),
                                                                                         style="padding:5px; font-size:120%"),
                                                                            actionButton(inputId = "restore_dr_fluorescence3",
                                                                                         label = "Restore dose-response analysis",
                                                                                         # icon=icon("gears"),
                                                                                         style="padding:5px; font-size:120%")
                                                                     )
                                                                   ),

                                                                   h3(strong("Export plot")),

                                                                   fluidRow(
                                                                     column(width = 4,
                                                                            numericInput(inputId = "width_download_dose_response_model_fluorescence_plot_individual",
                                                                                         label = "Width (in inches)",
                                                                                         value = 7)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            numericInput(inputId = "height_download_dose_response_model_fluorescence_plot_individual",
                                                                                         label = "Height (in inches)",
                                                                                         value = 6)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            numericInput(inputId = "dpi_download_dose_response_model_fluorescence_plot_individual",
                                                                                         label = "DPI",
                                                                                         value = 300)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            downloadButton('download_dose_response_model_fluorescence_plot_individual',"Download Plot"),

                                                                            radioButtons("format_download_dose_response_model_fluorescence_plot_individual",
                                                                                         label = NULL,
                                                                                         choices = c("PNG" = ".png",
                                                                                                     "PDF" = ".pdf"),
                                                                                         selected = ".png",
                                                                                         inline = TRUE)
                                                                     ), # column


                                                                   ) # fluidRow
                                                                 ) # mainPanel
                                                        ), # tabPanel(title = "Dose-response analysis",

                                                        ### Fluorescence DR Plots Bootstrap ####

                                                        tabPanel(title = "Dose-Response Analysis (Bootstrap)", value = "tabPanel_Visualize_Fluorescence_DoseResponse_bt",
                                                                 sidebarPanel(

                                                                   h3('Customize plot appearance'),


                                                                   sliderInput(inputId = 'shape_type_dose_response_fluorescence_plot_bt',
                                                                               label = 'Shape type',
                                                                               min = 1,
                                                                               max = 25,
                                                                               value = 15),

                                                                   sliderInput(inputId = 'shape_size_dose_response_fluorescence_plot_bt',
                                                                               label = 'Shape size',
                                                                               min = 0.1,
                                                                               max = 10,
                                                                               value = 2,
                                                                               step = 0.1),

                                                                   sliderInput(inputId = 'axis_size_dose_response_fluorescence_plot_bt',
                                                                               label = 'Axis title font size',
                                                                               min = 0.1,
                                                                               max = 10,
                                                                               value = 1.3,
                                                                               step = 0.1),


                                                                   sliderInput(inputId = 'lab_size_dose_response_fluorescence_plot_bt',
                                                                               label = 'Axis label font size',
                                                                               min = 0.1,
                                                                               max = 10,
                                                                               value = 1.3,
                                                                               step = 0.1),

                                                                   sliderInput(inputId = 'line_width_dose_response_fluorescence_plot_bt',
                                                                               label = 'Line width',
                                                                               min = 0.01,
                                                                               max = 10,
                                                                               value = 1),

                                                                 ), # sidebarPanel

                                                                 mainPanel(
                                                                   h3('Individual plots'),
                                                                   selectInput(inputId = 'individual_plots_dose_response_fluorescence_plot_bt',
                                                                               label = 'Select plot',
                                                                               choices = "",
                                                                               multiple = FALSE,
                                                                               selectize = FALSE,
                                                                               size = 3),
                                                                   plotOutput("dose_response_fluorescence_plot_individual_bt",
                                                                              width = "100%", height = "800px"),

                                                                   h3(strong("Export plot")),

                                                                   fluidRow(
                                                                     column(width = 4,
                                                                            numericInput(inputId = "width_download_dose_response_fluorescence_plot_individual_bt",
                                                                                         label = "Width (in inches)",
                                                                                         value = 7)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            numericInput(inputId = "height_download_dose_response_fluorescence_plot_individual_bt",
                                                                                         label = "Height (in inches)",
                                                                                         value = 6)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            numericInput(inputId = "dpi_download_dose_response_fluorescence_plot_individual_bt",
                                                                                         label = "DPI",
                                                                                         value = 300)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            downloadButton('download_dose_response_fluorescence_plot_individual_bt',"Download Plot"),

                                                                            radioButtons("format_download_dose_response_fluorescence_plot_individual_bt",
                                                                                         label = NULL,
                                                                                         choices = c("PNG" = ".png",
                                                                                                     "PDF" = ".pdf"),
                                                                                         selected = ".png",
                                                                                         inline = TRUE)
                                                                     ), # column
                                                                   ) # fluidRow
                                                                 ) #  mainPanel
                                                        ), # tabPanel(title = "Dose-response analysis (Bootstrap)"



                                                        ### Fluorescence DR Parameters ####

                                                        tabPanel(title = "DR Parameter Plots",value = "tabPanel_Visualize_Fluorescence_DoseResponseParameters",
                                                                 sidebarPanel(
                                                                   selectInput(inputId = "parameter_dr_parameter_fluorescence_plot",
                                                                               label = "Parameter",
                                                                               choices = ""
                                                                   ),

                                                                   textInput(inputId = "select_sample_based_on_string_fluorescence_dr_parameter_plot",
                                                                             label = "Select sample based on string (separated by ;)"
                                                                   ),

                                                                   textInput(inputId = "exclude_sample_based_on_strings_fluorescence_dr_parameter_plot",
                                                                             label = "Exclude sample based on strings (separated by ;)"
                                                                   ),

                                                                   checkboxInput(inputId = 'normalize_to_reference_fluorescence_dr_parameter_plot',
                                                                                 label = 'normalize to reference',
                                                                                 value = FALSE),

                                                                   h3("Customize plot appearance"),

                                                                   # Conditional Panel
                                                                   conditionalPanel(condition = "input.normalize_to_reference_fluorescence_dr_parameter_plot",
                                                                                    # reactive selection
                                                                                    selectInput(inputId = 'reference_condition_fluorescence_dr_parameter_plot',
                                                                                                label = 'Reference condition',
                                                                                                choices = ""
                                                                                    )
                                                                   ),


                                                                   sliderInput(inputId = "basesize_fluorescence_dr_parameter_plot",
                                                                               label = "Base font size",
                                                                               min = 10,
                                                                               max = 35,
                                                                               value = 23,
                                                                               step = 0.5),

                                                                   sliderInput(inputId = "label.size_fluorescence_dr_parameter_plot",
                                                                               label = "Label font size",
                                                                               min = 5,
                                                                               max = 35,
                                                                               value = 20,
                                                                               step = 0.5)


                                                                 ),

                                                                 mainPanel(

                                                                   plotOutput("fluorescence_dr_parameter_plot",
                                                                              width = "100%", height = "800px"),


                                                                   h3(strong("Export plot")),

                                                                   fluidRow(
                                                                     column(width = 4,
                                                                            numericInput(inputId = "width_download_fluorescence_dr_parameter_plot",
                                                                                         label = "Width (in inches)",
                                                                                         value = 7)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            numericInput(inputId = "height_download_fluorescence_dr_parameter_plot",
                                                                                         label = "Height (in inches)",
                                                                                         value = 6)
                                                                     ), # column
                                                                     column(width = 4,
                                                                            numericInput(inputId = "dpi_download_fluorescence_dr_parameter_plot",
                                                                                         label = "DPI",
                                                                                         value = 300)
                                                                     ), # column

                                                                     column(width = 5,
                                                                            downloadButton('download_fluorescence_dr_parameter_plot',"Download Plot"),

                                                                            radioButtons("format_download_fluorescence_dr_parameter_plot",
                                                                                         label = NULL,
                                                                                         choices = c("PNG" = ".png",
                                                                                                     "PDF" = ".pdf"),
                                                                                         selected = ".png",
                                                                                         inline = TRUE)
                                                                     ), # column


                                                                   ) # fluidRow
                                                                 ) #  mainPanel
                                                        ), # tabPanel Fluorescence_Parameter_Plots
                                            ) # tabsetPanel(type = "tabs",
                                   ), # tabPanel(title = "Fluorescence Plots"


                        ), # navbarMenu("Visualize"

                        #____REPORT____####


                        tabPanel(span("Report", title = "Generate a PDF or HTML report summarizing the results."),
                                 value = "tabPanel_Report", icon=icon("file-contract"),
                                 tabsetPanel(type = "tabs", id = "tabsetPanel_Report",
                                             ##____Growth report___####
                                             tabPanel(title = "Growth", value = "tabPanel_report_growth",
                                                      sidebarPanel(width = 6,

                                                                   selectInput(inputId = 'report_filetype_growth',
                                                                               label = 'Choose file type',
                                                                               choices = c('PDF' = 'pdf', 'HTML' = 'html')),

                                                                   conditionalPanel(condition = "input.report_filetype_growth == 'pdf'",
                                                                                    fluidRow(
                                                                                      column(12,
                                                                                             div(
                                                                                               downloadButton(outputId = 'download_report_growth_pdf',
                                                                                                              label = "Render Report",
                                                                                                              icon = icon("file-pdf"),
                                                                                                              style="padding:5px; font-size:120%"),
                                                                                               style="float:right")
                                                                                      )
                                                                                    )
                                                                   ),
                                                                   conditionalPanel(condition = "input.report_filetype_growth == 'html'",
                                                                                    fluidRow(
                                                                                      column(12,
                                                                                             div(
                                                                                               downloadButton(outputId = 'download_report_growth_html',
                                                                                                              label = "Render Report",
                                                                                                              icon = icon("file-code"),
                                                                                                              style="padding:5px; font-size:120%"),
                                                                                               style="float:right")
                                                                                      )
                                                                                    )
                                                                   ),
                                                      ) # sidebarPanel
                                             ), # tabPanel(title = "Growth", value = "tabs_export_data_growth",
                                             tabPanel(title = "Fluorescence", value = "tabPanel_report_fluorescence",
                                                      sidebarPanel(width = 6,
                                                                   selectInput(inputId = 'report_filetype_fluorescence',
                                                                               label = 'Choose file type',
                                                                               choices = c('PDF' = 'pdf', 'HTML' = 'html')),

                                                                   conditionalPanel(condition = "input.report_filetype_fluorescence == 'pdf'",
                                                                                    fluidRow(
                                                                                      column(12,
                                                                                             div(
                                                                                               downloadButton(outputId = 'download_report_fluorescence_pdf',
                                                                                                              label = "Render Report",
                                                                                                              icon = icon("file-pdf"),
                                                                                                              style="padding:5px; font-size:120%"),
                                                                                               style="float:right")
                                                                                      )
                                                                                    )
                                                                   ),
                                                                   conditionalPanel(condition = "input.report_filetype_fluorescence == 'html'",
                                                                                    fluidRow(
                                                                                      column(12,
                                                                                             div(
                                                                                               downloadButton(outputId = 'download_report_fluorescence_html',
                                                                                                              label = "Render Report",
                                                                                                              icon = icon("file-code"),
                                                                                                              style="padding:5px; font-size:120%"),
                                                                                               style="float:right")
                                                                                      )
                                                                                    )
                                                                   ),
                                                      ) # sidebarPanel
                                             ) # tabPanel(title = "fluorescence", value = "tabs_export_data_fluorescence",
                                 ), # tabsetPanel(type = "tabs", id = "tabs_report",
                                 checkboxInput(inputId = "report_issues",
                                               label = "Creating reports does not work",
                                               value = FALSE),
                                 conditionalPanel(condition = "input.report_issues",
                                                  actionButton(inputId = "run_report_fix",
                                                               label = "Run Report Troubleshooting"),
                                                  bsPopover(id = "run_report_fix", title = HTML("<em>Run Report Troubleshooting</em>"), content = HTML("This process will take several minutes!<br><br>Reinstalls TinyTeX, updates tlmgr and ensures that Pandoc is recognized correctly.")),
                                 )
                        ), # tabPanel("Report",  value = "tabPanel_Report", icon=icon("file-contract"),
                        #___Export RData___####
                        tabPanel(span("Data Export", title = "Export all computation results as RData file."),
                                 icon = icon("download"),
                                 value = "tabPanel_Export_RData",
                                 tabsetPanel(type = "tabs", id = "tabsetPanel_Export_Data",
                                             ##____Growth results export____####
                                             tabPanel(title = "Growth", value = "tabPanel_export_data_growth",
                                                      sidebarPanel(width = 3,
                                                                   fluidRow(
                                                                     column(12,
                                                                            div(
                                                                              downloadButton(outputId = 'export_RData_growth',
                                                                                             label = "Export RData file",
                                                                                             icon = icon("file-export"),
                                                                                             style="padding:5px; font-size:120%"),
                                                                              style="float:right")
                                                                     )
                                                                   )
                                                      )

                                             ),
                                             ## Fluorescence results export____####
                                             tabPanel(title = "Fluorescence", value = "tabPanel_export_data_fluorescence",
                                                      sidebarPanel(width = 3,
                                                                   fluidRow(
                                                                     column(12,
                                                                            div(
                                                                              downloadButton(outputId = 'export_RData_fluorescence',
                                                                                             label = "Export RData file",
                                                                                             icon = icon("file-export"),
                                                                                             style="padding:5px; font-size:120%"),
                                                                              style="float:right")
                                                                     )
                                                                   )
                                                      )

                                             ),
                                 )
                        ),
                        #___Import RData___####
                        tabPanel(span("Data Import", title = "Import and RData file with results from a previous QurvE analysis."),
                                 icon = icon("upload"),
                                 value = "tabPanel_Import_RData",
                                 tabsetPanel(type = "tabs", id = "tabsetPanel_Import_Data",
                                             ##____Growth results export____####
                                             tabPanel(title = "Growth", value = "tabPanel_import_data_growth",
                                                      sidebarPanel(width = 3,
                                                                   fluidRow(
                                                                     column(12,
                                                                            fileInput(inputId = 'import_RData_growth',
                                                                                      label = 'Choose growth RData file',
                                                                                      accept = c('.rdata')
                                                                            ),
                                                                            conditionalPanel(
                                                                              condition = 'output.RData_growth_uploaded',
                                                                              div(
                                                                                actionButton(inputId = "read_RData_growth",
                                                                                             label = "Read data",
                                                                                             icon=icon("upload"),
                                                                                             style="padding:5px; font-size:120%"),
                                                                                style="float:right")
                                                                            ),

                                                                     )
                                                                   )
                                                      )

                                             ),
                                             ## Fluorescence results export____####
                                             tabPanel(title = "Fluorescence", value = "tabPanel_import_data_fluorescence",
                                                      sidebarPanel(width = 3,
                                                                   fluidRow(
                                                                     column(12,
                                                                            fileInput(inputId = 'import_RData_fluorescence',
                                                                                      label = 'Choose fluorescence RData file',
                                                                                      accept = c('.rdata')
                                                                            ),
                                                                            conditionalPanel(
                                                                              condition = 'output.RData_fluorescence_uploaded',
                                                                              div(
                                                                                actionButton(inputId = "read_RData_fluorescence",
                                                                                             label = "Read data",
                                                                                             icon=icon("upload"),
                                                                                             style="padding:5px; font-size:120%"),
                                                                                style="float:right")
                                                                            ),
                                                                     )
                                                                   )
                                                      )

                                             ),
                                 )
                        ),
                        #____ABOUT US____####


                        tabPanel("About Us",
                                 mainPanel(
                                   h2("Creators"),
                                   'Nicolas Wirth', tags$a(icon("twitter"), href="https://twitter.com/The_NiWi"), br(),
                                   'Jonathan Funk', tags$a(icon("twitter"), href="https://twitter.com/JonathanFunk12"),
                                   h2("Bug reports"),
                                   uiOutput("bug_report"),
                                   h2("Cite QurvE"),
                                   "Wirth, N.T., Funk, J., Donati, S. et al. QurvE: user-friendly software for the analysis of biological growth and fluorescence data. Nat Protoc (2023). https://doi.org/10.1038/s41596-023-00850-7"
                                   # h2("Publications"),
                                   # ''
                                 )
                        ),
                      ) #  navbarPage
                    ) # div(
                  ) # hidden(
                ) # tagList(
)

#____SERVER____####

server <- function(input, output, session){

  # Disable navbar menus before running computations
  shinyjs::disable(selector = "#navbar li a[data-value=tabPanel_Export_RData]")
  shinyjs::disable(selector = "#navbar li a[data-value=tabPanel_Report]")
  shinyjs::disable(selector = "#navbar li a[data-value=navbarMenu_Visualize]")
  shinyjs::disable(selector = "#navbar li a[data-value=navbarMenu_Computation]")
  shinyjs::disable(selector = "#navbar li a[data-value=navbarMenu_Results]")
  shinyjs::disable(selector = "#navbar li a[data-value=navbarMenu_Validate]")
  # Disable menus based on the type of computations run
  observe({
    if(is.null(results$growth)){
      shinyjs::disable(selector = "#navbar li a[data-value=tabPanel_Results_Growth]")
      shinyjs::disable(selector = "#navbar li a[data-value=tabPanel_Validate_Growth]")
      shinyjs::disable(selector = "#navbar li a[data-value=tabPanel_Visualize_Growth]")
    } else {
      shinyjs::enable(selector = "#navbar li a[data-value=tabPanel_Results_Growth]")
      shinyjs::enable(selector = "#navbar li a[data-value=tabPanel_Validate_Growth]")
      shinyjs::enable(selector = "#navbar li a[data-value=tabPanel_Visualize_Growth]")
    }

    if(is.null(results$fluorescence)){
      shinyjs::disable(selector = "#navbar li a[data-value=tabPanel_Results_Fluorescence]")
      shinyjs::disable(selector = "#navbar li a[data-value=tabPanel_Validate_Fluorescence]")
      shinyjs::disable(selector = "#navbar li a[data-value=tabPanel_Visualize_Fluorescence]")
      shinyjs::disable(selector = "#navbar li a[data-value=tabPanel_Visualize_GrowthandFluorescence]")
    } else {
      shinyjs::enable(selector = "#navbar li a[data-value=tabPanel_Results_Fluorescence]")
      shinyjs::enable(selector = "#navbar li a[data-value=tabPanel_Validate_Fluorescence]")
      shinyjs::enable(selector = "#navbar li a[data-value=tabPanel_Visualize_Fluorescence]")
      shinyjs::enable(selector = "#navbar li a[data-value=tabPanel_Visualize_GrowthandFluorescence]")
    }

    if(!is.null(results$custom_data)){
      grodata <- results$custom_data
    } else if(!is.null(results$parsed_data)){
      grodata <- results$parsed_data
    } else {
      grodata <- NULL
    }
    if(!is.null(grodata)){
      if(length(grodata$fluorescence) < 2){
        shinyjs::disable(selector = "#navbar li a[data-value=tabPanel_Computation_Fluorescence]")
      } else {
        shinyjs::enable(selector = "#navbar li a[data-value=tabPanel_Computation_Fluorescence]")
      }
      if(length(grodata$growth) < 2){
        shinyjs::disable(selector = "#navbar li a[data-value=tabPanel_Computation_Growth]")
      } else {
        shinyjs::enable(selector = "#navbar li a[data-value=tabPanel_Computation_Growth]")
      }
    }
  })


  # sidePanel_width <- reactive({
  #   if(as.numeric(input$dimension[1]) < 1000) return(TRUE)
  #   else return(6)
  # })

  results <- reactiveValues()

  ### Function help modals ####

  output$gcFitLinear_tooltip <- renderText({
    temp <- tools::Rd2HTML("../shiny_manuals/growth.gcFitLinear.Rd", out = paste0(tempfile("docs_gcFitLinear"), ".txt"))
    content <- readLines(temp)
    file.remove(temp)
    content
  })

  observeEvent(input$tooltip_growth.gcFitLinear,{
    showModal(QurvE:::help_modal(size = "l", idcss = "gcFitLinear",
                         htmlOutput("gcFitLinear_tooltip"), easyClose = T )
    )
  })

  observeEvent(input$tooltip_growth.gcFitLinear_validate,{
    showModal(QurvE:::help_modal(size = "l", idcss = "gcFitLinear",
                         htmlOutput("gcFitLinear_tooltip"), easyClose = T )
    )
  })

  output$rdm.data_tooltip <- renderText({
    temp <- tools::Rd2HTML("../shiny_manuals/rdm.data.Rd", out = paste0(tempfile("docs_rdm.data"), ".txt"))
    content <- readLines(temp)
    file.remove(temp)
    content
  })

  observeEvent(input$tooltip_rdm.data,{
    showModal(QurvE:::help_modal(size = "l", idcss = "rdm.data",
                                 htmlOutput("rdm.data_tooltip"), easyClose = T )
    )
  })

  output$gcFitSpline_tooltip <- renderText({
    temp <- tools::Rd2HTML("../shiny_manuals/growth.gcFitSpline.Rd", out = paste0(tempfile("docs_gcFitSpline"), ".txt"))
    content <- readLines(temp)
    file.remove(temp)
    content
  })

  observeEvent(input$tooltip_growth.gcFitSpline,{
    showModal(QurvE:::help_modal(size = "l", idcss = "gcFitSpline",
                         htmlOutput("gcFitSpline_tooltip"), easyClose = T )
    )
  })

  observeEvent(input$tooltip_growth.gcFitSpline_validate,{
    showModal(QurvE:::help_modal(size = "l", idcss = "gcFitSpline",
                         htmlOutput("gcFitSpline_tooltip"), easyClose = T )
    )
  })

  output$gcFitModel_tooltip <- renderText({
    temp <- tools::Rd2HTML("../shiny_manuals/growth.gcFitModel.Rd", out = paste0(tempfile("docs_gcFitModel"), ".txt"))
    content <- readLines(temp)
    file.remove(temp)
    content
  })

  observeEvent(input$tooltip_growth.gcFitModel,{
    showModal(QurvE:::help_modal(size = "l", idcss = "gcFitModel",
                         htmlOutput("gcFitModel_tooltip"), easyClose = T )
    )
  })

  observeEvent(input$tooltip_growth.gcFitModel_validate,{
    showModal(QurvE:::help_modal(size = "l", idcss = "gcFitModel",
                         htmlOutput("gcFitModel_tooltip"), easyClose = T )
    )
  })

  output$tooltip_growth_workflow <- renderText({
    temp <- tools::Rd2HTML("../shiny_manuals/growth.workflow.Rd", out = paste0(tempfile("docs_growth_workflow"), ".txt"))
    content <- readLines(temp)
    file.remove(temp)
    content
  })


  observeEvent(input$tooltip_growth_workflow,{
    showModal(QurvE:::help_modal(size = "l", idcss = "growthworkflow",
                         htmlOutput("tooltip_growth_workflow"), easyClose = T )
    )
  })

  output$tooltip_growth_dr <- renderText({
    temp <- tools::Rd2HTML("../shiny_manuals/growth.drFit.Rd", out = paste0(tempfile("docs_growth_drFit"), ".txt"))
    content <- readLines(temp)
    file.remove(temp)
    content
  })

  observeEvent(input$tooltip_growth_dr,{
    showModal(QurvE:::help_modal(size = "l", idcss = "growthdr",
                                 htmlOutput("tooltip_growth_dr"), easyClose = T )
    )
  })

  output$flFitLinear_tooltip <- renderText({
    temp <- tools::Rd2HTML("../shiny_manuals/flFitLinear.Rd", out = paste0(tempfile("docs_flFitLinear"), ".txt"))
    content <- readLines(temp)
    file.remove(temp)
    content
  })

  observeEvent(input$tooltip_flFitLinear,{
    showModal(QurvE:::help_modal(size = "l", idcss = "flFitLinear",
                         htmlOutput("flFitLinear_tooltip"), easyClose = T )
    )
  })

  observeEvent(input$tooltip_flFitLinear_validate,{
    showModal(QurvE:::help_modal(size = "l", idcss = "flFitLinear",
                         htmlOutput("flFitLinear_tooltip"), easyClose = T )
    )
  })

  output$flFitSpline_tooltip <- renderText({
    temp <- tools::Rd2HTML("../shiny_manuals/flFitSpline.Rd", out = paste0(tempfile("docs_flFitSpline"), ".txt"))
    content <- readLines(temp)
    file.remove(temp)
    content
  })

  observeEvent(input$tooltip_flFitSpline,{
    showModal(QurvE:::help_modal(size = "l", idcss = "flFitSpline",
                         htmlOutput("flFitSpline_tooltip"), easyClose = T )
    )
  })

  observeEvent(input$tooltip_flFitSpline_validate,{
    showModal(QurvE:::help_modal(size = "l", idcss = "flFitSpline",
                         htmlOutput("flFitSpline_tooltip"), easyClose = T )
    )
  })

  output$tooltip_fl_workflow <- renderText({
    temp <- tools::Rd2HTML("../shiny_manuals/fl.workflow.Rd", out = paste0(tempfile("docs_fl_workflow"), ".txt"))
    content <- readLines(temp)
    file.remove(temp)
    content
  })

  output$tooltip_fl_dr <- renderText({
    temp <- tools::Rd2HTML("../shiny_manuals/fl.drFit.Rd", out = paste0(tempfile("docs_fl_dr"), ".txt"))
    content <- readLines(temp)
    file.remove(temp)
    content
  })

  observeEvent(input$tooltip_fl_workflow,{
    showModal(QurvE:::help_modal(size = "l", idcss = "flworkflow",
                         htmlOutput("tooltip_fl_workflow"), easyClose = T )
    )
  })

  observeEvent(input$tooltip_fl_dr,{
    showModal(QurvE:::help_modal(size = "l", idcss = "fldr",
                                 htmlOutput("tooltip_fl_dr"), easyClose = T )
    )
  })

  #____Read data____####
  # Test if fluorescence data is contained in custom/parsed object
  output$fluorescence_present <- reactive({
    if(!is.null(results$custom_data)){
      grodata <- results$custom_data
    } else if(!is.null(results$parsed_data)){
      grodata <- results$parsed_data
    } else return(FALSE)

    if(length(grodata$fluorescence) > 1 || length(grodata$fluorescence2) > 1){
      return(TRUE)
    } else return(FALSE)
  })
  outputOptions(output, 'fluorescence_present', suspendWhenHidden=FALSE)

  # Test if more than one two concentrations are being tested
  output$more_than_two_conc <-  reactive({
    if(!is.null(results$custom_data)){
      grodata <- results$custom_data
    } else if(!is.null(results$parsed_data)){
      grodata <- results$parsed_data
    } else if(!is.null(results$growth)){
      grodata <- results$growth
    } else if(!is.null(results$fluorescence)){
      grodata <- results$fluorescence
    }
    else return(FALSE)

    if(length(unique(grodata$expdesign$concentration)) > 2 )
      return(TRUE)
    else
      return(FALSE)
  })
  outputOptions(output, 'more_than_two_conc', suspendWhenHidden=FALSE)
    ##___Custom____####

  hide("Custom_Data_Tables")
  hide("norm_type_custom")
  ### Test if custom_file_growth was loaded
  output$growthfileUploaded <- reactive({
    if(is.null(input$custom_file_growth)) return(FALSE)
    else return(TRUE)
  })
  outputOptions(output, 'growthfileUploaded', suspendWhenHidden=FALSE)

  ### Test if custom_file_fluorescence was loaded
  output$fluorescencefileUploaded <- reactive({
    if(is.null(input$custom_file_fluorescence)) return(FALSE)
    else return(TRUE)
  })
  outputOptions(output, 'fluorescencefileUploaded', suspendWhenHidden=FALSE)

  ## Test if custom_file_fluorescence was loaded
  output$fluorescence2fileUploaded <- reactive({
    if(is.null(input$custom_file_fluorescence2)) return(FALSE)
    else return(TRUE)
  })
  outputOptions(output, 'fluorescence2fileUploaded', suspendWhenHidden=FALSE)

  observe({
    if(!is.null(input$custom_file_fluorescence) && (!is.null(input$custom_file_growth) | !is.null(input$custom_file_fluorescence2))){
      show("norm_type_custom")
    } else {
      hide("norm_type_custom")
      return(NULL)
    }
    norm.types <- c()
    if(!is.null(input$custom_file_growth)){
      norm.types <- c(norm.types, "growth")
    }
    if(!is.null(input$custom_file_fluorescence2)){
      norm.types <- c(norm.types, "fluorescence 2")
    }
    if(length(norm.types) > 0){
      show("norm_type_custom")
    } else {
      hide("norm_type_custom")
    }
    updateSelectInput(inputId = "norm_type_custom",
                      choices = norm.types
    )
  })

  # Read data upon click on [Read data]
  observeEvent(input$read_custom,{
    showModal(modalDialog("Reading data input...", footer=NULL))
    growth.file <- input$custom_file_growth
    fl.file <- input$custom_file_fluorescence
    fl2.file <- input$custom_file_fluorescence2

    if(is.null(growth.file) && is.null(fl.file) ) return(NULL)

    if(input$convert_time_equation_custom == "" || is.na(input$convert_time_equation_custom)) convert.time <- NULL
    else convert.time <- input$convert_time_equation_custom

    fl.normtype <- ifelse(input$load_fl2_data_custom, "fl2", "growth")
    ## Read data
    try(
      results$custom_data <- read_data(data.growth = growth.file$datapath,
                                       data.fl = fl.file$datapath,
                                       data.fl2 = fl2.file$datapath,
                                       data.format = "col",
                                       sheet.growth = input$custom_growth_sheets,
                                       sheet.fl = input$custom_fluorescence_sheets,
                                       sheet.fl2 = input$custom_fluorescence2_sheets,
                                       csvsep = input$separator_custom_growth,
                                       dec = input$decimal_separator_custom_growth,
                                       csvsep.fl = input$separator_custom_fluorescence,
                                       dec.fl = input$decimal_separator_custom_fluorescence,
                                       csvsep.fl2 = input$separator_custom_fluorescence2,
                                       dec.fl2 = input$decimal_separator_custom_fluorescence2,
                                       subtract.blank = input$subtract_blank_custom,
                                       convert.time = convert.time,
                                       calib.growth = ifelse(input$calibration_growth_custom, input$calibration_equation_growth_custom, ""),
                                       calib.fl = ifelse(input$calibration_fluorescence_custom, input$calibration_equation_fluorescence_custom, ""),
                                       calib.fl2 = ifelse(input$calibration_fluorescence2_custom, input$calibration_equation_fluorescence2_custom, ""),
                                       fl.normtype = fl.normtype
      )
    )

    if(length(results$custom_data)<2){
      showModal(modalDialog("Data could not be extracted from the provided file. Did you correctly format your custom data?", easyClose = T, footer=NULL))
    } else {
      hide("data_instruction")
      show("Custom_Data_Tables")
      hide("Parsed_Data_Tables")
      if("growth" %in% names(results$custom_data) && length(results$custom_data$growth)>1){
        # show [Run Computation] button in Computation-Growth
        show("run_growth")
      }
      # if("growth" %in% names(results$custom_data) && length(results$custom_data$fluorescence2)>1){
      #   showTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_fluorescence2")
      # }

      # Remove eventually pre-loaded parsed data
      results$parsed_data <- NULL
      hide("parsed_reads_growth")
      hide("parsed_reads_fluorescence")
      hide("parsed_reads_fluorescence2")
      hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_growth")
      hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_plots_growth")

      hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_fluorescence")
      hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_plots_fluorescence")

      hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_norm_fluorescence")
      hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_plots_norm_fluorescence")
      # hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_fluorescence2")
      hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_expdesign")

      shinyjs::enable(selector = "#navbar li a[data-value=navbarMenu_Computation]")

      removeModal()
    }
  })

  ### Render custom growth table
  # growth_data_custom <- reactive({
  #   inFile <- input$custom_file_growth
  #
  #   if(is.null(inFile))
  #     return(NULL)
  #
  #   filename <- inFile$datapath
  #   dec <- input$decimal_separator_custom_growth
  #   csvsep <- input$separator_custom_growth
  #   if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "csv") {
  #     dat <-
  #       utils::read.csv(
  #         filename,
  #         dec = dec,
  #         sep = csvsep,
  #         header = FALSE,
  #         stringsAsFactors = FALSE,
  #         fill = T,
  #         na.strings = "",
  #         quote = "",
  #         comment.char = "",
  #         check.names = F
  #       )
  #   } else if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "xls" |
  #              stringr::str_replace(filename, ".{1,}\\.", "") == "xlsx") {
  #     showModal(modalDialog("Reading data file...", footer=NULL))
  #     try(
  #       dat <- data.frame(suppressMessages(readxl::read_excel(filename, col_names = FALSE, sheet = input$custom_growth_sheets, progress = T)))
  #       )
  #     removeModal()
  #   } else if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "tsv") {
  #     dat <-
  #       utils::read.csv(
  #         filename,
  #         dec = dec,
  #         sep = "\t",
  #         header = FALSE,
  #         stringsAsFactors = FALSE,
  #         fill = T,
  #         na.strings = "",
  #         quote = "",
  #         comment.char = "",
  #         check.names = F
  #       )
  #   } else if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "txt") {
  #     dat <-
  #       utils::read.table(
  #         filename,
  #         dec = dec,
  #         sep = "\t",
  #         header = FALSE,
  #         stringsAsFactors = FALSE,
  #         fill = T,
  #         na.strings = "",
  #         quote = "",
  #         comment.char = "",
  #         check.names = F
  #       )
  #   }
  #   if(exists("dat")){
  #     dat[-(1:3),] <- apply(dat[-(1:3),], 2, as.numeric) %>% apply(., 2, round, digits = 2)
  #
  #     colnames(dat)[1] <- "Time"
  #     dat[1,1] <- ""
  #     return(dat)
  #   } else return(NULL)
  # })

  # output$growth_data_rendered <- DT::renderDT({
  #   dat <- growth_data_custom()
  #   DT::datatable(dat,
  #             options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
  #             escape = FALSE, rownames = c("Condition", "Replicate", "Concentration", rep("", nrow(dat)-3)))
  # })

  ### Render experimental design table
  custom_data_table_expdesign <- reactive({
    if(is.null(growth_data_custom_processed()) && is.null(custom_table_fluorescence_processed())) return(NULL)
    else if(!is.null(growth_data_custom_processed())){
      dat <- growth_data_custom_processed()
    }
    else if(!is.null(custom_table_fluorescence_processed())){
      dat <- custom_table_fluorescence_processed()
    }

    dat.mat <- t(dat)
    label <- unlist(lapply(1:nrow(dat.mat), function(x) paste(dat.mat[x,1], dat.mat[x,2], dat.mat[x,3], sep = " | ")))
    condition <- dat.mat[, 1]
    replicate <- dat.mat[, 2]
    concentration <- dat.mat[, 3]

    expdesign <- data.frame(label, condition, replicate, concentration, check.names = FALSE)

    expdesign[-1, ]
  })

  output$custom_data_table_expdesign <- DT::renderDT({
    expdesign <- custom_data_table_expdesign()
    DT::datatable(expdesign,
              options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
              escape = FALSE)
  })


  observe({
    # if(exists("growth_data_custom") && !is.null(growth_data_custom()) && is.null(growth_data_custom_processed()) ){
    #   showTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_growth")
    # } else {
    #   hideTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_growth")
    # }
    # if(exists("custom_table_fluorescence") && !is.null(custom_table_fluorescence()) && is.null(custom_table_fluorescence_processed()) ){
    #   showTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_fluorescence", select = TRUE)
    # } else {
    #   hideTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_fluorescence")
    # }
    if(exists("custom_data_table_expdesign") && !is.null(custom_data_table_expdesign())){
      showTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_expdesign")
    } else {
      hideTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_expdesign")
    }
    if(exists("growth_data_custom_processed") && !is.null(growth_data_custom_processed())){
      showTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_growth_processed", select = TRUE)
      showTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_plots_growth", select = FALSE)
    } else {
      hideTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_growth_processed")
      hideTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_plots_growth")
    }
    if(exists("custom_table_fluorescence_processed") && !is.null(custom_table_fluorescence_processed()) ){
      showTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_fluorescence_processed", select = TRUE)
      showTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_plots_fluorescence", select = FALSE)
    } else {
      hideTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_fluorescence_processed")
      hideTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_plots_fluorescence")
    }
    if(exists("custom_table_norm_fluorescence_processed") && !is.null(custom_table_norm_fluorescence_processed()) ){
      showTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_norm_fluorescence_processed", select = TRUE)
      showTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_plots_norm_fluorescence", select = FALSE)
    } else {
      hideTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_norm_fluorescence_processed")
      hideTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_plots_norm_fluorescence")
    }
    if(exists("custom_table_fluorescence_processed") && !is.null(custom_table_fluorescence_processed()) &&
       exists("growth_data_custom_processed") && !is.null(growth_data_custom_processed()) &&
       all(results$custom_data$growth == results$custom_data$fluorescence, na.rm = T)){
      showModal(modalDialog("growth and Fluorescence data are identical. Did you assign the correct files and/or sheets?", easyClose = T))
    }
  })


  ### Render custom fluorescence table
  custom_table_fluorescence <- reactive({
    inFile <- input$custom_file_fluorescence

    if(is.null(inFile))
      return(NULL)

    filename <- inFile$datapath
    dec <- input$decimal_separator_custom_fluorescence
    csvsep <- input$separator_custom_fluorescence
    if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "csv") {
      f1 <-
        utils::read.csv(
          filename,
          dec = dec,
          sep = csvsep,
          header = FALSE,
          stringsAsFactors = FALSE,
          fill = T,
          na.strings = "",
          quote = "",
          comment.char = "",
          check.names = F
        )
    } else if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "xls" |
               stringr::str_replace(filename, ".{1,}\\.", "") == "xlsx") {
      showModal(
        modalDialog(
          HTML("<strong>Reading data file...</strong><br><br><i>Note: Reading .xls/.xlsx files can take up to several minutes. Consider saving as .csv/.tsv/.txt before running QurvE.</i>"),
          footer=NULL))
      try(
        f1 <- data.frame(suppressMessages(readxl::read_excel(filename, col_names = FALSE, sheet = input$custom_fluorescence_sheets, progress = T)))
      )
      removeModal()
    } else if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "tsv") {
      f1 <-
        utils::read.csv(
          filename,
          dec = dec,
          sep = "\t",
          header = FALSE,
          stringsAsFactors = FALSE,
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
          header = FALSE,
          stringsAsFactors = FALSE,
          fill = T,
          na.strings = "",
          quote = "",
          comment.char = "",
          check.names = F
        )
    }
    if(exists("f1")){
    f1[-(1:3),] <- apply(apply(f1[-(1:3),], 2, as.numeric), 2, round, digits = 2)
    colnames(f1)[1] <- "Time"
    f1[1,1] <- ""
    return(f1)
    } else return(NULL)
  })

  output$custom_table_fluorescence <- DT::renderDT({
    f1 <- custom_table_fluorescence()
    DT::datatable(f1,
              options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
              escape = FALSE, rownames = c("Condition", "Replicate", "Concentration", rep("", nrow(f1)-3)))
  })

  # Render processed growth table
  growth_data_custom_processed <- reactive({

    if(is.null(results$custom_data) || length(results$custom_data$growth) < 2) return(NULL)

    table_growth <- t(results$custom_data$growth)
    table_growth[-(1:3), ] <- apply(apply(table_growth[-(1:3), ], 2, as.numeric), 2, round, digits = 3)
    rownames(table_growth)[-(1:3)] <- ""
    table_growth <- cbind(data.frame("Time" = c("","","", round(as.numeric(results$custom_data$time[1,]), digits = 2))),
                           table_growth)

    table_growth
  })

  output$growth_data_custom_processed <- DT::renderDT({
    table_growth <- growth_data_custom_processed()
    DT::datatable(table_growth,
              options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
              escape = FALSE, rownames = c("Condition", "Replicate", "Concentration", rep("", nrow(table_growth)-3)))
  })

  # render processed fluorescence table
  custom_table_fluorescence_processed <- reactive({

    if(is.null(results$custom_data) || length(results$custom_data$fluorescence)<2) return(NULL)

    table_fl <- t(results$custom_data$fluorescence)
    table_fl[-(1:3), ] <- apply(apply(table_fl[-(1:3), ], 2, as.numeric), 2, round, digits = 1)
    rownames(table_fl)[-(1:3)] <- ""
    table_fl <- cbind(data.frame("Time" = c("","","", round(as.numeric(results$custom_data$time[1,]), digits = 2))),
                       table_fl)

    table_fl
  })

  output$custom_table_fluorescence_processed <- DT::renderDT({
    table_fl <- custom_table_fluorescence_processed()
    DT::datatable(table_fl,
                           options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
                           escape = FALSE, rownames = c("Condition", "Replicate", "Concentration", rep("", nrow(table_fl)-3)))
  })

  # render processed normalized fluorescence table
  custom_table_norm_fluorescence_processed <- reactive({

    if(is.null(results$custom_data) || length(results$custom_data$norm.fluorescence)<2) return(NULL)

    table_fl <- t(results$custom_data$norm.fluorescence)
    table_fl[-(1:3), ] <- apply(apply(table_fl[-(1:3), ], 2, as.numeric), 2, round, digits = 1)
    rownames(table_fl)[-(1:3)] <- ""
    table_fl <- cbind(data.frame("Time" = c("","","", round(as.numeric(results$custom_data$time[1,]), digits = 2))),
                      table_fl)

    table_fl
  })

  output$custom_table_norm_fluorescence_processed <- DT::renderDT({
    table_fl <- custom_table_norm_fluorescence_processed()
    DT::datatable(table_fl,
                  options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
                  escape = FALSE, rownames = c("Condition", "Replicate", "Concentration", rep("", nrow(table_fl)-3)))
  })


  custom_raw_growth_plot <- reactive({
    if(is.null(results$custom_data) || length(results$custom_data$growth) < 2) return(NULL)

    try(
      suppressWarnings(
        plot.grodata(x = results$custom_data,
                     data.type = "growth",
                    IDs = NULL
        )
      )
    )
  })

  output$custom_raw_growth_plot <- renderPlot({
    custom_raw_growth_plot()
  })

  custom_raw_fluorescence_plot <- reactive({
    if(is.null(results$custom_data) || length(results$custom_data$fluorescence) < 2) return(NULL)

    try(
      suppressWarnings(
        plot.grodata(x = results$custom_data,
                     data.type = "fl",
                     IDs = NULL
        )
      )
    )
  })

  output$custom_raw_fluorescence_plot <- renderPlot({
    custom_raw_fluorescence_plot()
  })

  custom_raw_norm_fluorescence_plot <- reactive({
    if(is.null(results$custom_data) || length(results$custom_data$norm.fluorescence) < 2) return(NULL)

    try(
      suppressWarnings(
        plot.grodata(x = results$custom_data,
                     data.type = "norm.fl",
                     IDs = NULL
        )
      )
    )
  })

  output$custom_raw_norm_fluorescence_plot <- renderPlot({
    custom_raw_norm_fluorescence_plot()
  })



  output$custom_growth_format <- reactive({
    if(is.null(input$custom_file_growth)) return(NULL)

    filename <- input$custom_file_growth$name

    format <- stringr::str_replace_all(filename, ".{1,}\\.", "")
    format
  })
  outputOptions(output, 'custom_growth_format', suspendWhenHidden=FALSE)

  output$custom_fluorescence_format <- reactive({
    if(is.null(input$custom_file_fluorescence)) return(NULL)

    filename <- input$custom_file_fluorescence$name

    format <- stringr::str_replace_all(filename, ".{1,}\\.", "")
    format
  })
  outputOptions(output, 'custom_fluorescence_format', suspendWhenHidden=FALSE)

  output$custom_fluorescence2_format <- reactive({
    if(is.null(input$custom_file_fluorescence2)) return(NULL)

    filename <- input$custom_file_fluorescence2$name

    format <- stringr::str_replace_all(filename, ".{1,}\\.", "")
    format
  })
  outputOptions(output, 'custom_fluorescence2_format', suspendWhenHidden=FALSE)

  growth_excel_sheets <- reactive({
    filename <- input$custom_file_growth$datapath
    if(is.null(input$custom_file_growth)) return("")
    format <- stringr::str_replace_all(filename, ".{1,}\\.", "")
    if(format != "xlsx" && format != "xls") return("")
    sheets <- readxl::excel_sheets(input$custom_file_growth$datapath)
    sheets
  })

  fluorescence_excel_sheets <- reactive({
    filename <- input$custom_file_fluorescence$datapath
    if(is.null(input$custom_file_fluorescence)) return("")
    format <- stringr::str_replace_all(filename, ".{1,}\\.", "")
    if(format != "xlsx" && format != "xls") return("")
    sheets <- readxl::excel_sheets(input$custom_file_fluorescence$datapath)
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
    updateSelectInput(inputId = "custom_fluorescence_sheets",
                      choices = fluorescence_excel_sheets()
    )})

  observe({
    updateSelectInput(inputId = "custom_fluorescence2_sheets",
                      choices = fluorescence2_excel_sheets()
    )})

  observeEvent(input$random_data_growth, {
    # display a modal dialog with a header, textinput and action buttons
    showModal(
      modalDialog(
        tags$h2('Please enter parameters to simulate growth curves'),
        textInput('d.random.growth', 'Number of samples', placeholder = "35"),
        textInput('y0.random.growth', 'Start growth', placeholder = "0.05"),
        textInput('tmax.random.growth', 'Maximum time value', placeholder = "24"),
        textInput('mu.random.growth', 'Maximum growth rate', placeholder = "0.6"),
        textInput('lambda.random.growth', 'Minimum lag time', placeholder = "5"),
        textInput('A.random.growth', 'Maximum growth', placeholder = "3"),

        footer=tagList(
          fluidRow(
            column(12,
                   div(
                     actionButton('submit.random.data.growth', 'Submit'),
                     style="float:right"),
                   div(
                     modalButton('cancel'),
                     style="float:right"),
                   div(
                     actionButton(inputId = "tooltip_rdm.data",
                                  label = "",
                                  icon=icon("question"),
                                  style="padding:2px; font-size:100%"),
                     style="float:left")

            )
          )
        ) # footer
      ) # modalDialog
    ) # showModal
  })

  # Generate random dataset
  observeEvent(input$submit.random.data.growth, {
    d <- ifelse(input$d.random.growth == "", 35, as.numeric(input$d.random.growth))
    y0 <- ifelse(input$y0.random.growth == "", 0.05, as.numeric(input$y0.random.growth))
    tmax <- ifelse(input$tmax.random.growth == "", 24, as.numeric(input$tmax.random.growth))
    mu <- ifelse(input$mu.random.growth == "", 0.6, as.numeric(input$mu.random.growth))
    lambda <- ifelse(input$lambda.random.growth == "", 5, as.numeric(input$lambda.random.growth))
    A <- ifelse(input$A.random.growth == "", 3, as.numeric(input$A.random.growth))

    results$custom_data <- rdm.data(d, y0 = y0, tmax = tmax, mu = mu, lambda = lambda, A = A, label = "Test")

    hide("data_instruction")
    show("Custom_Data_Tables")
    hide("Parsed_Data_Tables")
    if("growth" %in% names(results$custom_data) && length(results$custom_data$growth)>1){
      # show [Run Computation] button in Computation-Growth
      show("run_growth")
    }
    # if("growth" %in% names(results$custom_data) && length(results$custom_data$fluorescence2)>1){
    #   showTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_fluorescence2")
    # }

    # Remove eventually pre-loaded parsed data
    results$parsed_data <- NULL
    hide("parsed_reads_growth")
    hide("parsed_reads_fluorescence")
    hide("parsed_reads_fluorescence2")
    hide("norm_type_parse")
    hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_growth")
    hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_plots_growth")

    hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_fluorescence")
    hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_plots_fluorescence")

    hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_norm_fluorescence")
    hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_plots_norm_fluorescence")

    hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_expdesign")


    shinyjs::enable(selector = "#navbar li a[data-value=navbarMenu_Computation]")

    results$df_random_data <- data.frame("time" = c("time", "","", results$custom_data$time[1,]),
                     "data" = t(results$custom_data$data))
    colnames(results$df_random_data ) <- results$df_random_data [1,]
    write.csv(results$df_random_data[-1,], file = paste0(tempdir(), "/random_data.csv"))
    results$random_data_growth <- paste0(tempdir(), "/random_data.csv")
    removeModal()
  })

  observe({
    growth.file <- results$random_data_growth

    if(is.null(growth.file)) return(NULL)
    ## Read data
    try(
      results$custom_data <- read_data(results$df_random_data,
                                       # data.fluoro2 = fl2.file$datapath,
                                       data.format = "col"

      )
    )

    if(length(results$custom_data)<2){
      showModal(modalDialog("Data could not be extracted from the provided file. Did you correctly format your custom data?", easyClose = T, footer=NULL))
    } else {
      hide("data_instruction")
      show("Custom_Data_Tables")
      hide("Parsed_Data_Tables")
      if("growth" %in% names(results$custom_data) && length(results$custom_data$growth)>1){
        # show [Run Computation] button in Computation-Growth
        show("run_growth")
      }
      # if("growth" %in% names(results$custom_data) && length(results$custom_data$fluorescence2)>1){
      #   showTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_fluorescence2")
      # }

      # Remove eventually pre-loaded parsed data
      results$parsed_data <- NULL
      hide("parsed_reads_growth")
      hide("parsed_reads_fluorescence")
      hide("parsed_reads_norm_fluorescence")
      hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_growth")
      hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_plots_growth")

      hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_fluorescence")
      hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_plots_fluorescence")

      hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_norm_fluorescence")
      hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_plots_norm_fluorescence")
      # hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_fluorescence2")
      hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_expdesign")

      shinyjs::enable(selector = "#navbar li a[data-value=navbarMenu_Computation]")
      hide("random_data_growth")
      removeModal()
    }
  })

    ##__Parse data____####
  ### Hide elements to guide user
  hide("Parsed_Data_Tables")
  hide("parsed_reads_growth")
  hide("parsed_reads_fluorescence")
  hide("parsed_reads_fluorescence2")
  hide("parse_data")
  hide("norm_type_parse")
  hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_growth")
  hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_plots_growth")

  hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_fluorescence")
  hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_plots_fluorescence")
  # hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_fluorescence2")
  hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_expdesign")

  observe({
    if(exists("parsed_data_table_expdesign") && !is.null(parsed_data_table_expdesign())){
      showTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_expdesign")
    } else {
      hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_expdesign")
    }
    if(exists("parsed_data_table_growth") && !is.null(parsed_data_table_growth())){
      showTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_growth", select = TRUE)
      showTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_plots_growth", select = FALSE)
    } else {
      hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_growth")
      hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_plots_growth")
    }
    if(exists("parsed_data_table_fluorescence") && !is.null(parsed_data_table_fluorescence()) ){
      showTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_fluorescence", select = TRUE)
      showTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_plots_fluorescence", select = FALSE)
    } else {
      hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_fluorescence")
      hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_plots_fluorescence")
    }
    if(exists("parsed_data_table_norm_fluorescence") && !is.null(parsed_data_table_norm_fluorescence()) ){
      showTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_norm_fluorescence", select = TRUE)
      showTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_plots_norm_fluorescence", select = FALSE)
    } else {
      hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_norm_fluorescence")
      hideTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_plots_norm_fluorescence")
    }
  })

  # Update placeholder for time conversion based on selected software
  observe({
    if("Gen5" %in% input$platereader_software){
      updateTextInput(session, "convert_time_equation_plate_reader", value = "y = x * 24")
    }
    if("Chi.Bio" %in% input$platereader_software){
      updateTextInput(session, "convert_time_equation_plate_reader", value = "y = x / 3600")
    }
    if("GrowthProfiler" %in% input$platereader_software){
      updateTextInput(session, "convert_time_equation_plate_reader", value = "y = x / 60")
    }
    if("Tecan" %in% input$platereader_software){
      updateTextInput(session, "convert_time_equation_plate_reader", value = "y = x / 3600")
    }
    if("Biolector" %in% input$platereader_software){
      updateTextInput(session, "convert_time_equation_plate_reader", value = NULL, placeholder = "")
    }
    if("VictorNivo" %in% input$platereader_software){
      updateTextInput(session, "convert_time_equation_plate_reader", value = "y = x / 3600")
    }
    if("VictorX3" %in% input$platereader_software){
      updateTextInput(session, "convert_time_equation_plate_reader", value = "y = x / 60")
    }
  })

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
    if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "xls" |
        stringr::str_replace(filename, ".{1,}\\.", "") == "xlsx"){
      showModal(modalDialog(HTML("<strong>Reading data file...</strong><br><br><i>Note: Reading .xls/.xlsx files can take up to several minutes. Consider saving as .csv/.tsv/.txt before running QurvE.</i>"), footer=NULL))

    }else {
    showModal(modalDialog("Reading data file...", footer=NULL))
    }
    if("Gen5" %in% input$platereader_software){
      try(reads <- QurvE:::parse_properties_Gen5Gen6(file=filename,
                                                     csvsep = input$separator_parse,
                                                     dec = input$decimal_separator_parse,
                                                     sheet = ifelse(input$parse_data_sheets == "Sheet1", 1, input$parse_data_sheets) ),
          silent = FALSE

      )
    }
    if("Chi.Bio" %in% input$platereader_software){
      try(reads <- QurvE:::parse_properties_chibio(file=filename,
                                                     csvsep = input$separator_parse,
                                                     dec = input$decimal_separator_parse,
                                                     sheet = ifelse(input$parse_data_sheets == "Sheet1", 1, input$parse_data_sheets) ),
          silent = FALSE

      )
    }
    if("Tecan" %in% input$platereader_software){
      try(reads <- QurvE:::parse_properties_tecan(file=filename,
                                                   csvsep = input$separator_parse,
                                                   dec = input$decimal_separator_parse,
                                                   sheet = ifelse(input$parse_data_sheets == "Sheet1", 1, input$parse_data_sheets) ),
          silent = FALSE

      )
    }
    if("Biolector" %in% input$platereader_software){
      try(reads <- QurvE:::parse_properties_biolector(file=filename,
                                                  csvsep = input$separator_parse,
                                                  dec = input$decimal_separator_parse,
                                                  sheet = ifelse(input$parse_data_sheets == "Sheet1", 1, input$parse_data_sheets) ),
          silent = FALSE

      )
    }
    if("VictorNivo" %in% input$platereader_software){
      try(reads <- QurvE:::parse_properties_victornivo(file=filename,
                                                      csvsep = input$separator_parse,
                                                      dec = input$decimal_separator_parse,
                                                      sheet = ifelse(input$parse_data_sheets == "Sheet1", 1, input$parse_data_sheets) ),
          silent = FALSE

      )
    }
    if("VictorX3" %in% input$platereader_software){
      try(reads <- QurvE:::parse_properties_victorx3(file=filename,
                                                       csvsep = input$separator_parse,
                                                       dec = input$decimal_separator_parse,
                                                       sheet = ifelse(input$parse_data_sheets == "Sheet1", 1, input$parse_data_sheets) ),
          silent = FALSE

      )
    }
    if(exists("reads") && length(reads) > 0){
      show("parsed_reads_growth")
      show("parsed_reads_fluorescence")
      if(length(reads)>1){
        show("parsed_reads_fluorescence2")
      } else {
        hide("parsed_reads_fluorescence2")
      }

      show("parse_data")
      removeModal()
      reads <- c(reads, "Ignore")
    } else if(!("GrowthProfiler" %in% input$platereader_software) &&
              !is.null(input$platereader_software)){
                if(stringr::str_replace_all(filename, ".{1,}\\.", "") == "xlsx")
                  showModal(modalDialog("No read data could be extracted from the provided data file. Did you choose the correct software and sheet?", easyClose = T, footer=NULL))
                if(stringr::str_replace_all(filename, ".{1,}\\.", "") == "csv")
                  showModal(modalDialog("No read data could be extracted from the provided data file. Did you choose the correct software and separator?", easyClose = T, footer=NULL))
                else
                  showModal(modalDialog("No read data could be extracted from the provided data file. Did you choose the correct software?", easyClose = T, footer=NULL))
    } else {
      show("parse_data")
      removeModal()
    }
  })

  observe({
    reads <- selected_inputs_parsed_reads()
    if(length(reads) < 2)
      return(NULL)
    if(length(reads) > 1 && input$parsed_reads_fluorescence != "Ignore"){
      show("parsed_reads_fluorescence2")
      show("norm_type_parse")
    } else {
      hide("parsed_reads_fluorescence2")
      hide("norm_type_parse")
    }
    norm.types <- c()
    if(input$parsed_reads_growth != "Ignore"){
      norm.types <- c(norm.types, "growth")
    }
    if(input$parsed_reads_fluorescence2 != "Ignore"){
      norm.types <- c(norm.types, "fluorescence 2")
    }
    if(length(norm.types) > 0){
      show("norm_type_parse")
    } else {
      hide("norm_type_parse")
    }
    updateSelectInput(inputId = "norm_type_parse",
                      choices = norm.types
    )
  })

  observe({
    updateSelectInput(inputId = "parsed_reads_growth",
                      choices = selected_inputs_parsed_reads()
    )
  })
  observe({
    updateSelectInput(inputId = "parsed_reads_fluorescence",
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
    if(input$norm_type_parse == "fluorescence 2"){
      fl.normtype <- "fl2"
    } else {
      fl.normtype <- "growth"
    }
    showModal(modalDialog("Parsing data input...", footer=NULL))
    if(input$convert_time_equation_plate_reader == "" || is.na(input$convert_time_equation_plate_reader)) convert.time <- NULL
    else convert.time <- input$convert_time_equation_plate_reader
    if(input$mapping_included_in_parse){
      try(
        results$parsed_data <- QurvE:::parse_data_shiny(
          data.file = input$parse_file$datapath,
          map.file = input$parse_file$datapath,
          software = input$platereader_software,
          convert.time = if(input$convert_time_values_plate_reader){
            convert.time
          } else {
            NULL
          },
          sheet.data = input$parse_data_sheets,
          sheet.map = input$map_data_sheets,
          csvsep.data =  input$separator_parse,
          dec.data = input$decimal_separator_parse,
          csvsep.map =  input$separator_map,
          dec.map = input$decimal_separator_map,
          subtract.blank = input$subtract_blank_plate_reader,
          growth.nm = input$parsed_reads_growth,
          fl.nm = ifelse(
            input$parsed_reads_fluorescence == input$parsed_reads_growth,
            NA,
            input$parsed_reads_fluorescence
          ),
          calib.growth = ifelse(input$calibration_growth_plate_reader, input$calibration_equation_growth_plate_reader, ""),
          calib.fl = ifelse(input$calibration_fluorescence_plate_reader, input$calibration_equation_fluorescence_plate_reader, ""),
          calib.fl2 = ifelse(input$calibration_fluorescence2_plate_reader, input$calibration_equation_fluorescence2_plate_reader, ""),
          fl2.nm = ifelse(
            input$parsed_reads_fluorescence2 == input$parsed_reads_growth |
              input$parsed_reads_fluorescence2 == input$parsed_reads_fluorescence,
            NA,
            input$parsed_reads_fluorescence2
          ),
          fl.normtype = fl.normtype
        )
      )
    } else {
      if(is.null(input$map_file$datapath)){
        showModal(
          modalDialog(
            HTML(
              "<strong>Parsing data input...</strong><br><br>No mapping file was provided. The samples will be identified based on their well position (A1, A2, A3, etc.). Grouping options will not be available if you run any further analysis with QurvE."
            ),
            easyClose = FALSE,
            footer=NULL
          )
        )
      }
      try(
        results$parsed_data <- QurvE:::parse_data_shiny(
          data.file = input$parse_file$datapath,
          map.file = input$map_file$datapath,
          software = input$platereader_software,
          convert.time = if(input$convert_time_values_plate_reader){
            convert.time
          } else {
            NULL
          },
          sheet.data = input$parse_data_sheets,
          sheet.map = input$map_data_sheets,
          csvsep.data =  input$separator_parse,
          dec.data = input$decimal_separator_parse,
          csvsep.map =  input$separator_map,
          dec.map = input$decimal_separator_map,
          subtract.blank = input$subtract_blank_plate_reader,
          growth.nm = input$parsed_reads_growth,
          fl.nm = ifelse(
            input$parsed_reads_fluorescence == input$parsed_reads_growth,
            NA,
            input$parsed_reads_fluorescence
          ),
          calib.growth = ifelse(input$calibration_growth_plate_reader, input$calibration_equation_growth_plate_reader, ""),
          calib.fl = ifelse(input$calibration_fluorescence_plate_reader, input$calibration_equation_fluorescence_plate_reader, ""),
          calib.fl2 = ifelse(input$calibration_fluorescence2_plate_reader, input$calibration_equation_fluorescence2_plate_reader, ""),
          fl2.nm = ifelse(
            input$parsed_reads_fluorescence2 == input$parsed_reads_growth |
              input$parsed_reads_fluorescence2 == input$parsed_reads_fluorescence,
            NA,
            input$parsed_reads_fluorescence
          ),
          fl.normtype = fl.normtype
        )
      )
    }
    showTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_expdesign")
    hide("mapping_layout")
    hide("Custom_Data_Tables")
    show("Parsed_Data_Tables")
    if("growth" %in% names(results$parsed_data) && length(results$parsed_data$growth)>1){
      showTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_growth")
      showTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_plots_growth")

      # show [Run Computation] button in Computation-Growth
      show("run_growth")
      shinyjs::enable(selector = "#navbar li a[data-value=navbarMenu_Computation]")
    }
    if("fluorescence" %in% names(results$parsed_data) && length(results$parsed_data$fluorescence)>1){
      showTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_fluorescence")
      showTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_plots_fluorescence")
      shinyjs::enable(selector = "#navbar li a[data-value=navbarMenu_Computation]")
    }
    # if("growth" %in% names(results$parsed_data) && length(results$parsed_data$fluorescence2)>1){
    #   showTab(inputId = "tabsetPanel_parsed_tables", target = "tabPanel_parsed_tables_fluorescence2")
    # }
    # Remove eventually pre-loaded custom data
    results$custom_data <- NULL
    hideTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_growth_processed")
    hideTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_plots_growth")

    hideTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_fluorescence_processed")
    hideTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_plots_fluorescence")

    hideTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_norm_fluorescence_processed")
    hideTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_plots_norm_fluorescence")


    # hideTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_fluorescence2")
    hideTab(inputId = "tabsetPanel_custom_tables", target = "tabPanel_custom_tables_expdesign")


    removeModal()
  })
  #### Generate parsed tables to display in [DATA] tab
  parsed_data_table_growth <- reactive({
    if(is.null(results$parsed_data) || length(results$parsed_data$growth) < 2) return(NULL)

    table_growth <- t(results$parsed_data$growth)
    if(dim(table_growth)[2]<2){
      table_growth[-(1:3), ] <- round(as.numeric(table_growth[-(1:3), ]), digits = 3)
    }else{
      table_growth[-(1:3), ] <- apply(apply(table_growth[-(1:3), ], 2, as.numeric), 2, round, digits = 3)
    }
    rownames(table_growth)[-(1:3)] <- ""
    table_growth <- cbind(data.frame("Time" = c("","","", round(as.numeric(results$parsed_data$time[1,]), digits = 2))),
                           table_growth)
    table_growth
  })

  output$parsed_data_table_growth <- DT::renderDT({
    DT::datatable(parsed_data_table_growth(),
                               options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
                               escape = FALSE, rownames = c("Condition", "Replicate", "Concentration", rep("", nrow(parsed_data_table_growth())-3)))

  })

  parsed_data_table_fluorescence <- reactive({
    if(is.null(results$parsed_data) || length(results$parsed_data$fluorescence)<2) return(NULL)

    table_fl <- t(results$parsed_data$fluorescence)
    table_fl[-(1:3), ] <- apply(apply(table_fl[-(1:3), ], 2, as.numeric), 2, round, digits = 1)
    rownames(table_fl)[-(1:3)] <- ""
    table_fl <- cbind(data.frame("Time" = c("","","", round(as.numeric(results$parsed_data$time[1,]), digits = 2))),
                      table_fl)
    table_fl
  })

  output$parsed_data_table_fluorescence <- DT::renderDT({
    DT::datatable(parsed_data_table_fluorescence(),
                           options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
                           escape = FALSE, rownames = c("Condition", "Replicate", "Concentration", rep("", nrow(parsed_data_table_fluorescence())-3)))

  })

  # render processed normalized fluorescence table
  parsed_data_table_norm_fluorescence <- reactive({
    if(is.null(results$parsed_data) || length(results$parsed_data$norm.fluorescence)<2) return(NULL)

    table_fl <- t(results$parsed_data$norm.fluorescence)
    table_fl[-(1:3), ] <- apply(apply(table_fl[-(1:3), ], 2, as.numeric), 2, round, digits = 1)
    rownames(table_fl)[-(1:3)] <- ""
    table_fl <- cbind(data.frame("Time" = c("","","", round(as.numeric(results$parsed_data$time[1,]), digits = 2))),
                      table_fl)
    table_fl
  })

  output$parsed_data_table_norm_fluorescence <- DT::renderDT({
    DT::datatable(parsed_data_table_norm_fluorescence(),
                  options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
                  escape = FALSE, rownames = c("Condition", "Replicate", "Concentration", rep("", nrow(parsed_data_table_norm_fluorescence())-3)))

  })
  # output$parsed_data_table_fluorescence2 <- DT::renderDT({
  #
  #   if(is.null(results$parsed_data) || length(results$parsed_data$fluorescence2)<2) return(NULL)
  #
  #   table_fl2 <- t(results$parsed_data$fluorescence2)
  #   table_fl2[-(1:3), ] <- apply(apply(table_fl2[-(1:3), ], 2, as.numeric), 2, round, digits = 1)
  #   rownames(table_fl2)[-(1:3)] <- ""
  #   table_fl2 <- cbind(data.frame("Time" = c("","","", round(as.numeric(results$parsed_data$time[1,]), digits = 2))),
  #                      table_fl2)
  #
  #   table_fl2 <- DT::datatable(table_fl2,
  #                          options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
  #                          escape = FALSE, rownames = c("Condition", "Replicate", "Concentration", rep("", nrow(table_fl2)-3)))
  #
  #   table_fl2
  # })
  parsed_data_table_expdesign <- reactive({
    if(is.null(results$parsed_data) || length(results$parsed_data$expdesign)<2) return(NULL)
    expdesign <- results$parsed_data$expdesign
    expdesign
  })

  output$parsed_data_table_expdesign <- DT::renderDT({
    expdesign <- parsed_data_table_expdesign()
    DT::datatable(expdesign,
                           options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
                           escape = FALSE, rownames = T)
  })

  parsed_raw_growth_plot <- reactive({
    if(is.null(results$parsed_data) || length(results$parsed_data$growth) < 2) return(NULL)

    try(
      suppressWarnings(
        plot.grodata(x = results$parsed_data,
                     data.type = "growth",
                     IDs = NULL
        )
      )
    )
  })

  output$parsed_raw_growth_plot <- renderPlot({
    parsed_raw_growth_plot()
  })

  parsed_raw_fluorescence_plot <- reactive({
    if(is.null(results$parsed_data) || length(results$parsed_data$fluorescence) < 2) return(NULL)

    try(
      suppressWarnings(
        plot.grodata(x = results$parsed_data,
                     data.type = "fl",
                     IDs = NULL
        )
      )
    )
  })

  output$parsed_raw_fluorescence_plot <- renderPlot({
    parsed_raw_fluorescence_plot()
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

  parsed_raw_norm_fluorescence_plot <- reactive({
    if(is.null(results$parsed_data) || length(results$parsed_data$norm.fluorescence) < 2) return(NULL)
    try(
      suppressWarnings(
        plot.grodata(x = results$parsed_data,
                     data.type = "norm.fl",
                     IDs = NULL
        )
      )
    )
  })

  output$parsed_raw_norm_fluorescence_plot <- renderPlot({
    parsed_raw_norm_fluorescence_plot()
  })

    ##__Table_download____####
  output$download_custom_tables_growth_processed <- downloadHandler(
    filename = function() {
      paste("custom_growth_data", ".csv", sep="")
    },
    content = function(file) {
      table <- growth_data_custom_processed()
      colnames(table) <- gsub("<sub>", "_", gsub("</sub>|<sup>|</sup>", "", gsub("<br>", " ", colnames(table))))
      QurvE:::write.csv.utf8.BOM(table, file)
    }
  )

  output$download_custom_tables_fluorescence_processed <- downloadHandler(
    filename = function() {
      paste("custom_fluorescence_data", ".csv", sep="")
    },
    content = function(file) {
      table <- custom_table_fluorescence_processed()
      colnames(table) <- gsub("<sub>", "_", gsub("</sub>|<sup>|</sup>", "", gsub("<br>", " ", colnames(table))))
      QurvE:::write.csv.utf8.BOM(table, file)
    }
  )

  output$download_custom_tables_norm_fluorescence_processed <- downloadHandler(
    filename = function() {
      paste("custom_norm_fluorescence_data", ".csv", sep="")
    },
    content = function(file) {
      table <- custom_table_norm_fluorescence_processed()
      colnames(table) <- gsub("<sub>", "_", gsub("</sub>|<sup>|</sup>", "", gsub("<br>", " ", colnames(table))))
      QurvE:::write.csv.utf8.BOM(table, file)
    }
  )

  output$download_custom_tables_expdesign <- downloadHandler(
    filename = function() {
      paste("custom_expdesign", ".csv", sep="")
    },
    content = function(file) {
      table <- custom_data_table_expdesign()
      colnames(table) <- gsub("<sub>", "_", gsub("</sub>|<sup>|</sup>", "", gsub("<br>", " ", colnames(table))))
      QurvE:::write.csv.utf8.BOM(table, file)
    }
  )

  output$download_parsed_tables_growth <- downloadHandler(
    filename = function() {
      paste("parsed_growth_data", ".csv", sep="")
    },
    content = function(file) {
      table <- parsed_data_table_growth()
      colnames(table) <- gsub("<sub>", "_", gsub("</sub>|<sup>|</sup>", "", gsub("<br>", " ", colnames(table))))
      QurvE:::write.csv.utf8.BOM(table, file)
    }
  )

  output$download_parsed_tables_fluorescence <- downloadHandler(
    filename = function() {
      paste("parsed_fluorescence_data", ".csv", sep="")
    },
    content = function(file) {
      table <- parsed_data_table_fluorescence()
      colnames(table) <- gsub("<sub>", "_", gsub("</sub>|<sup>|</sup>", "", gsub("<br>", " ", colnames(table))))
      QurvE:::write.csv.utf8.BOM(table, file)
    }
  )

  output$download_parsed_data_table_norm_fluorescence <- downloadHandler(
    filename = function() {
      paste("parsed_norm_fluorescence_data", ".csv", sep="")
    },
    content = function(file) {
      table <- parsed_data_table_norm_fluorescence()
      colnames(table) <- gsub("<sub>", "_", gsub("</sub>|<sup>|</sup>", "", gsub("<br>", " ", colnames(table))))
      QurvE:::write.csv.utf8.BOM(table, file)
    }
  )

  output$download_parsed_tables_expdesign <- downloadHandler(
    filename = function() {
      paste("parsed_expdesign", ".csv", sep="")
    },
    content = function(file) {
      if(is.null(results$parsed_data) || length(results$parsed_data$expdesign)<2) return(NULL)
      table <- results$parsed_data$expdesign
      colnames(table) <- gsub("<sub>", "_", gsub("</sub>|<sup>|</sup>", "", gsub("<br>", " ", colnames(table))))
      QurvE:::write.csv.utf8.BOM(table, file)
    }
  )
  # Computation ####
    ##____Growth____#####
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
    ## Read data
    # grodata <- read_data(inFile$datapath, sheet.growth = input$custom_growth_sheets, csvsep = input$separator_custom_growth, dec = input$decimal_separator_custom_growth)
    # Choose data input
    if(!is.null(results$custom_data)){
      grodata <- results$custom_data
    } else if(!is.null(results$parsed_data)){
      grodata <- results$parsed_data
    } else return(NULL)

    if (is.null(input$number_of_bootstrappings_growth) || is.na(input$number_of_bootstrappings_growth) || input$number_of_bootstrappings_growth == "NULL" || input$number_of_bootstrappings_growth == "") {
      nboot.gc <- 0.55
    } else {
      nboot.gc <- as.numeric(input$number_of_bootstrappings_growth)
    }

    if (is.null(input$smoothing_factor_nonparametric_growth) || is.na(input$smoothing_factor_nonparametric_growth) || input$smoothing_factor_nonparametric_growth == "NULL" || input$smoothing_factor_nonparametric_growth == "") {
      smooth.gc <- 0.55
    } else {
      smooth.gc <- as.numeric(input$smoothing_factor_nonparametric_growth)
    }

    if (is.null(input$smoothing_factor_growth_dr) || is.na(input$smoothing_factor_growth_dr) || input$smoothing_factor_growth_dr == "NULL" || input$smoothing_factor_growth_dr == "") {
      smooth.dr = NULL
    } else {
      smooth.dr <- as.numeric(input$smoothing_factor_growth_dr)
    }

    if (is.null(input$number_of_bootstrappings_dr_growth) || is.na(input$number_of_bootstrappings_dr_growth) || input$number_of_bootstrappings_dr_growth == "NULL" || input$number_of_bootstrappings_dr_growth == "") {
      nboot.dr <- 0
    } else {
      nboot.dr <- as.numeric(input$number_of_bootstrappings_dr_growth)
    }

    if (is.null(input$R2_threshold_growth) || is.na(input$R2_threshold_growth) || input$R2_threshold_growth == "NULL" || input$R2_threshold_growth == "") {
      lin.R2 <- 0.95
    } else {
      lin.R2 <- as.numeric(input$R2_threshold_growth)
    }

    if (is.null(input$RSD_threshold_growth) || is.na(input$RSD_threshold_growth) || input$RSD_threshold_growth == "NULL" || input$RSD_threshold_growth == "") {
      lin.RSD <- 0.1
    } else {
      lin.RSD <- as.numeric(input$RSD_threshold_growth)
    }

    if (is.null(input$dY_threshold_growth) || is.na(input$dY_threshold_growth) || input$dY_threshold_growth == "NULL" || input$dY_threshold_growth == "") {
      lin.dY <- 0.05
    } else {
      lin.dY <- as.numeric(input$dY_threshold_growth)
    }

    if (is.null(input$minimum_growth_growth) || is.na(input$minimum_growth_growth) || input$minimum_growth_growth == "NULL" || input$minimum_growth_growth == "") {
      min.growth <- 0
    } else {
      min.growth <- as.numeric(input$minimum_growth_growth)
    }

    if (is.null(input$maximum_growth_growth) || is.na(input$maximum_growth_growth) || input$maximum_growth_growth == "NULL" || input$maximum_growth_growth == "") {
      max.growth <- NA
    } else {
      max.growth <- as.numeric(input$maximum_growth_growth)
    }

    if (is.null(input$t0_growth) || is.na(input$t0_growth) || input$t0_growth == "NULL" || input$t0_growth == "") {
      t0 <- 0
    } else {
      t0 <- as.numeric(input$t0_growth)
    }

    if (is.null(input$tmax_growth) || is.na(input$tmax_growth) || input$tmax_growth == "NULL" || input$tmax_growth == "") {
      tmax <- NA
    } else {
      tmax <- as.numeric(input$tmax_growth)
    }

    if (is.null(input$growth_threshold_growth) || is.na(input$growth_threshold_growth) || input$growth_threshold_growth == "NULL" || input$growth_threshold_growth == "") {
      growth.thresh <- 1.5
    } else {
      growth.thresh <- as.numeric(input$growth_threshold_growth)
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
      if(input$baranyi_growth == TRUE) models <- c(models, "baranyi")
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
                        try(
                          results$growth <-
                            suppressWarnings(
                              QurvE::growth.workflow(grodata = grodata,
                                              ec50 = input$perform_ec50_growth,
                                              fit.opt = fit.opt,
                                              t0 = t0,
                                              tmax = tmax,
                                              min.growth = min.growth,
                                              max.growth = max.growth,
                                              log.x.gc = input$log_transform_time_growth,
                                              log.y.lin = input$log_transform_data_linear_growth,
                                              log.y.model = input$log_transform_data_parametric_growth,
                                              log.y.spline = input$log_transform_data_nonparametric_growth,
                                              biphasic = input$biphasic_growth,
                                              lin.h = input$custom_sliding_window_size_value_growth,
                                              lin.R2 = lin.R2,
                                              lin.RSD = lin.RSD,
                                              lin.dY = lin.dY,
                                              interactive = FALSE,
                                              nboot.gc = nboot.gc,
                                              smooth.gc = smooth.gc,
                                              model.type = models,
                                              growth.thresh = growth.thresh,
                                              dr.method = input$dr_method_growth,
                                              dr.parameter = input$response_parameter_growth,
                                              smooth.dr = smooth.dr,
                                              log.x.dr = input$log_transform_concentration_growth,
                                              log.y.dr = input$log_transform_response_growth,
                                              nboot.dr = nboot.dr,
                                              suppress.messages = T,
                                              report = NULL,
                                              shiny = TRUE

                              )
                            )
                        )
    )

    if(!is.null("results$growth")){
      # ENABLE DISABLED PANELS AFTER RUNNING COMPUTATION
      shinyjs::enable(selector = "#navbar li a[data-value=tabPanel_Export_RData]")
      shinyjs::enable(selector = "#navbar li a[data-value=tabPanel_Report]")
      shinyjs::enable(selector = "#navbar li a[data-value=navbarMenu_Visualize]")
      shinyjs::enable(selector = "#navbar li a[data-value=navbarMenu_Results]")
      shinyjs::enable(selector = "#navbar li a[data-value=navbarMenu_Validate]")
      removeModal()
    }
    if(exists("results$growth") && !is.null("results$growth"))
      showModal(modalDialog(geterrmessage()) )

  })
    ##____Fluorescence____#####

  # Create vector of x_types based on presence of data types
  output$normalized_fl_present <- reactive({
    if(!is.null(results$custom_data)){
      grodata <- results$custom_data
    } else if(!is.null(results$parsed_data)){
      grodata <- results$parsed_data
      if(input$parsed_reads_growth == input$parsed_reads_fluorescence)
        showModal(modalDialog("Identical values for growth and fluorescence data. Did you choose the correct data identifiers?"))

    } else return(FALSE)

    if(length(grodata$norm.fluorescence) > 1){
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  outputOptions(output, 'normalized_fl_present', suspendWhenHidden=FALSE)


  selected_inputs_fluorescence_x_types <- reactive({
    if(is.null(results$custom_data) && is.null(results$parsed_data)) return(NULL)
    if(!is.null(results$custom_data)) data <- results$custom_data
    if(!is.null(results$parsed_data)) data <- results$parsed_data

    x_types <- c()
    if(length(data$growth) > 1) x_types <- c(x_types, 'growth')
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
    select_options
  })

  observe({
    updateSelectInput(session,
                      inputId = "response_parameter_fluorescence",
                      choices = selected_inputs_response_parameter_fluorescence()
    )
  })

  observeEvent(input$run_fluorescence,{
    # Choose data input
    if(!is.null(results$custom_data)){
      grodata <- results$custom_data
    } else if(!is.null(results$parsed_data)){
      grodata <- results$parsed_data
    } else return(NULL)

    if (is.null(input$smoothing_factor_nonparametric_fluorescence) || is.na(input$smoothing_factor_nonparametric_fluorescence) || input$smoothing_factor_nonparametric_fluorescence == "NULL" || input$smoothing_factor_nonparametric_fluorescence == "") {
      smooth.fl = 0.75
    } else {
      smooth.fl <- as.numeric(input$smoothing_factor_nonparametric_fluorescence)
    }

    if (is.null(input$smoothing_factor_fluorescence_dr) || is.na(input$smoothing_factor_fluorescence_dr) || input$smoothing_factor_fluorescence_dr == "NULL" || input$smoothing_factor_fluorescence_dr == "") {
      smooth.dr = NULL
    } else {
      smooth.dr <- as.numeric(input$smoothing_factor_fluorescence_dr)
    }

    if (is.null(input$number_of_bootstrappings_fluorescence) || is.na(input$number_of_bootstrappings_fluorescence) || input$number_of_bootstrappings_fluorescence == "NULL" || input$number_of_bootstrappings_fluorescence == "") {
      nboot.fl <- 0
    } else {
      nboot.fl <- as.numeric(input$number_of_bootstrappings_fluorescence)
    }

    if (is.null(input$number_of_bootstrappings_dr_fluorescence) || is.na(input$number_of_bootstrappings_dr_fluorescence) || input$number_of_bootstrappings_dr_fluorescence == "NULL" || input$number_of_bootstrappings_dr_fluorescence == "") {
      nboot.dr <- 0
    } else {
      nboot.dr <- as.numeric(input$number_of_bootstrappings_dr_fluorescence)
    }

    if (is.null(input$R2_threshold_fluorescence) || is.na(input$R2_threshold_fluorescence) || input$R2_threshold_fluorescence == "NULL" || input$R2_threshold_fluorescence == "") {
      lin.R2 <- 0.95
    } else {
      lin.R2 <- as.numeric(input$R2_threshold_fluorescence)
    }

    if (is.null(input$RSD_threshold_fluorescence) || is.na(input$RSD_threshold_fluorescence) || input$RSD_threshold_fluorescence == "NULL" || input$RSD_threshold_fluorescence == "") {
      lin.RSD <- 0.1
    } else {
      lin.RSD <- as.numeric(input$RSD_threshold_fluorescence)
    }

    if (is.null(input$dY_threshold_fluorescence) || is.na(input$dY_threshold_fluorescence) || input$dY_threshold_fluorescence == "NULL" || input$dY_threshold_fluorescence == "") {
      lin.dY <- 0.05
    } else {
      lin.dY <- as.numeric(input$dY_threshold_fluorescence)
    }

    if (is.null(input$growth_threshold_in_percent_fluorescence) || is.na(input$growth_threshold_in_percent_fluorescence) || input$growth_threshold_in_percent_fluorescence == "NULL" || input$growth_threshold_in_percent_fluorescence == "") {
      growth.thresh <- 1.5
    } else {
      growth.thresh <- as.numeric(input$growth_threshold_in_percent_fluorescence)
    }

    if (is.null(input$t0_fluorescence) || is.na(input$t0_fluorescence) || input$t0_fluorescence == "NULL" || input$t0_fluorescence == "") {
      t0 <- 0
    } else {
      t0 <- as.numeric(input$t0_fluorescence)
    }

    if (is.null(input$minimum_growth_fluorescence) || is.na(input$minimum_growth_fluorescence) || input$minimum_growth_fluorescence == "NULL" || input$minimum_growth_fluorescence == "") {
      min.growth <- 0
    } else {
      min.growth <- as.numeric(input$minimum_growth_fluorescence)
    }

    if (is.null(input$maximum_growth_fluorescence) || is.na(input$maximum_growth_fluorescence) || input$maximum_growth_fluorescence == "NULL" || input$maximum_growth_fluorescence == "") {
      max.growth <- NA
    } else {
      max.growth <- as.numeric(input$maximum_growth_fluorescence)
    }


    if (is.null(input$tmax_fluorescence) || is.na(input$tmax_fluorescence) || input$tmax_fluorescence == "NULL" || input$tmax_fluorescence == "") {
      tmax <- NA
    } else {
      tmax <- as.numeric(input$tmax_fluorescence)
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


    # removeModal()
    showModal(modalDialog("Running computations...", footer=NULL))
    # Run fluorescence workflow
    try(
      shiny::withProgress(message = "Computations completed",
                          results$fluorescence <-
                            suppressWarnings(
                             QurvE::fl.workflow(grodata = grodata,
                                          ec50 = input$perform_ec50_fluorescence,
                                          fit.opt = fit.opt,
                                          x_type = input$data_type_x_fluorescence,
                                          norm_fl = input$normalize_fluorescence,
                                          t0 = t0,
                                          min.growth = min.growth,
                                          log.x.lin = input$log_transform_x_linear_fluorescence,
                                          log.x.spline = input$log_transform_x_nonparametric_fluorescence,
                                          log.y.lin = input$log_transform_data_linear_fluorescence,
                                          log.y.spline = input$log_transform_data_nonparametric_fluorescence,
                                          lin.h = as.numeric(input$custom_sliding_window_size_value_fluorescence),
                                          lin.R2 = lin.R2,
                                          lin.RSD = lin.RSD,
                                          lin.dY = lin.dY,
                                          biphasic = input$biphasic_fluorescence,
                                          interactive = FALSE,
                                          dr.parameter = input$response_parameter_fluorescence,
                                          dr.method = input$dr_method_fluorescence,
                                          smooth.dr = smooth.dr,
                                          log.x.dr = input$log_transform_concentration_fluorescence,
                                          log.y.dr = input$log_transform_response_fluorescence,
                                          nboot.dr = nboot.dr,
                                          nboot.fl = nboot.fl,
                                          smooth.fl = smooth.fl,
                                          growth.thresh = growth.thresh,
                                          suppress.messages = T,
                                          neg.nan.act = FALSE,
                                          clean.bootstrap = TRUE,
                                          report = NULL,
                                          out.dir = NULL,
                                          out.nm = NULL,
                                          export.fig = FALSE,
                                          shiny = TRUE,
                                          tmax = tmax,
                                          max.growth = max.growth
                              )
                            )
      )
    )

    if(!is.null("results$fluorescence")){
      ## ENABLE DISABLED PANELS AFTER RUNNING COMPUTATION
      shinyjs::enable(selector = "#navbar li a[data-value=tabPanel_Export_RData]")
      shinyjs::enable(selector = "#navbar li a[data-value=tabPanel_Report]")
      shinyjs::enable(selector = "#navbar li a[data-value=navbarMenu_Visualize]")
      shinyjs::enable(selector = "#navbar li a[data-value=navbarMenu_Results]")
      shinyjs::enable(selector = "#navbar li a[data-value=navbarMenu_Validate]")
      removeModal()
    }
    if(exists("results$fluorescence") && !is.null("results$fluorescence"))
      showModal(modalDialog(geterrmessage()) )
  })

  # Results ####
    ## Growth ####
  observe({
    if(!is.null(results$growth)){
      if(!("s" %in% results$growth$control$fit.opt || "a" %in% results$growth$control$fit.opt)){
        hideTab(inputId = "tabsetPanel_Results_Growth", target = "tabPanel_Results_Growth_Spline")
        hideTab(inputId = "tabsetPanel_Results_Growth", target = "tabPanel_Results_Growth_Spline_bt")
      } else{
        showTab(inputId = "tabsetPanel_Results_Growth", target = "tabPanel_Results_Growth_Spline")
      }
      if(!("l" %in% results$growth$control$fit.opt || "a" %in% results$growth$control$fit.opt)){
        hideTab(inputId = "tabsetPanel_Results_Growth", target = "tabPanel_Results_Growth_Linear")
      } else {
        showTab(inputId = "tabsetPanel_Results_Growth", target = "tabPanel_Results_Growth_Linear")
      }
      if(!("m" %in% results$growth$control$fit.opt || "a" %in% results$growth$control$fit.opt)){
        hideTab(inputId = "tabsetPanel_Results_Growth", target = "tabPanel_Results_Growth_Model")
      } else {
        showTab(inputId = "tabsetPanel_Results_Growth", target = "tabPanel_Results_Growth_Model")
      }
      if(!("s" %in% results$growth$control$fit.opt || "a" %in% results$growth$control$fit.opt) || results$growth$control$nboot.gc <= 1){
        hideTab(inputId = "tabsetPanel_Results_Growth", target = "tabPanel_Results_Growth_Spline_bt")
      } else {
        showTab(inputId = "tabsetPanel_Results_Growth", target = "tabPanel_Results_Growth_Spline_bt")
      }
      if(length(results$growth$drFit) >1 && length(results$growth$drFit$drTable) > 1){
        showTab(inputId = "tabsetPanel_Results_Growth", target = "tabPanel_Results_Growth_DR")
      } else {
        hideTab(inputId = "tabsetPanel_Results_Growth", target = "tabPanel_Results_Growth_DR")
      }
    }
  })

    ### Linear ####
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
                               stringsAsFactors = FALSE, check.names = F)
    table_linear

  })

  output$results_table_growth_linear <- DT::renderDT({
    DT::datatable(table_growth_linear(),
              options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
              escape = FALSE)
  })

  table_growth_linear_group <- reactive({
    gcTable <- results$growth$gcFit$gcTable
    table <- QurvE:::table_group_growth_linear(gcTable)
    colnames(table) <- c("Sample|Conc.", "\u03bc<sub>max</sub>", "t<sub>D</sub>", "\u03bb",
                         "\u2206Y", "y<sub>max</sub>", "t<sub>start</sub><br>(\u03bc<sub>max</sub>)",
                         "t<sub>end</sub><br>(\u03bc<sub>max</sub>)")
    table
  })

  output$results_table_growth_linear_group <- DT::renderDT({
    DT::datatable(table_growth_linear_group(),
              options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
              escape = FALSE)
  })

    ### Spline ####

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
    DT::datatable(table_growth_spline(),
              options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
              escape = FALSE)
  })

  table_growth_spline_group <- reactive({
    gcTable <- results$growth$gcFit$gcTable
    table <- QurvE:::table_group_growth_spline(gcTable)
    colnames(table) <- c("Sample|Conc.", "\u03bc<sub>max</sub>", "t<sub>D</sub>", "\u03bb",
                         "\u2206Y", "y<sub>max</sub>", "t(\u03bc<sub>max</sub>)")
    table
  })

  output$results_table_growth_spline_group <- DT::renderDT({
    DT::datatable(table_growth_spline_group(),
              options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
              escape = FALSE)
  })

    ### Spline BT ####

  table_growth_spline_bt <- reactive({
    res.table.gc <- results$growth$gcFit$gcTable
    table_spline <- data.frame("Sample|Replicate|Conc." = paste(res.table.gc$TestId, res.table.gc$AddId, res.table.gc$concentration, sep = "|"),
                               "µ<sub>max</sub>" = ifelse(res.table.gc$mu.bt==0 | is.na(res.table.gc$mu.bt),
                                                                    "",
                                                                    paste0(round(as.numeric(res.table.gc$mu.bt), 3) ,
                                                                           " \u00B1 ",
                                                                           round(as.numeric(res.table.gc$stdmu.bt), 3)
                                                                          )
                                                                    ),
                               "t<sub>D</sub>" = ifelse(res.table.gc$mu.bt==0 | is.na(res.table.gc$mu.bt),
                                                                  "",
                                                                  paste0(round(log(2)/as.numeric(res.table.gc$mu.bt), 3) ,
                                                                         " \u00B1 ",
                                                                         round(log(2)/as.numeric(res.table.gc$stdmu.bt), 3)
                                                                        )
                                                                  ),
                               "λ" = ifelse(res.table.gc$lambda.bt==0 | is.na(res.table.gc$lambda.bt),
                                            "",
                                            paste0(round(as.numeric(res.table.gc$lambda.bt), 3) ,
                                                   " \u00B1 ",
                                                   round(as.numeric(res.table.gc$stdlambda.bt ), 3)
                                            )
                               ),
                               "y<sub>max</sub>" = ifelse(res.table.gc$lambda.bt==0 | is.na(res.table.gc$lambda.bt),
                                                          "",
                                                          paste0(round(as.numeric(res.table.gc$lambda.bt), 3) ,
                                                                 " \u00B1 ",
                                                                 round(as.numeric(res.table.gc$stdlambda.bt ), 3)
                                                          )
                               ),
                               "ΔY" = ifelse(res.table.gc$dY.bt==0 | is.na(res.table.gc$dY.bt),
                                             "",
                                             paste0(round(as.numeric(res.table.gc$dY.bt), 3) ,
                                                    " \u00B1 ",
                                                    round(as.numeric(res.table.gc$stddY.bt ), 3)
                                             )
                               ),
                               "smooth.<br>fac" = res.table.gc$smooth.spline, check.names = F)
    table_spline
  })

  output$results_table_growth_spline_bt <- DT::renderDT({
    DT::datatable(table_growth_spline_bt(),
              options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
              escape = FALSE)
  })

    ### Model ####

  table_growth_model <- reactive({
    try({
      res.table.gc <- results$growth$gcFit$gcTable
      table_model <- data.frame("Sample|Replicate|Conc." = paste(res.table.gc$TestId, res.table.gc$AddId, res.table.gc$concentration, sep = "|"),
                                "Model" = res.table.gc$used.model,
                                "µ<sub>max</sub>" = ifelse(res.table.gc$mu.model==0 | is.na(res.table.gc$mu.model), "", paste(round(as.numeric(res.table.gc$mu.model), 3),"\u00B1", round(as.numeric(res.table.gc$stdmu.model),3))),
                                "t<sub>D</sub>" = paste(ifelse(res.table.gc$mu.model==0 | is.na(res.table.gc$mu.model), "", paste(round(log(2)/as.numeric(res.table.gc$mu.model), 2), "\u00B1", round(sqrt(((-log(2)*as.numeric(res.table.gc$stdmu.model))/(as.numeric(res.table.gc$mu.model))^2)^2), 2)))),
                                "λ" = ifelse(res.table.gc$lambda.model==0 | is.na(res.table.gc$lambda.model), "", paste(round(as.numeric(res.table.gc$lambda.model), 2), "\u00B1", round(as.numeric(res.table.gc$stdlambda.model),3))),
                                "A" = ifelse(res.table.gc$A.model==0 | is.na(res.table.gc$A.model), "", paste(round(as.numeric(res.table.gc$A.model), 3), "\u00B1", round(as.numeric(res.table.gc$stdA.model),3))),
                                stringsAsFactors = FALSE, check.names = F)
      if(!is.null(res.table.gc)){
        if ( "richards" %in% res.table.gc$used.model  ){
          table_model <- suppressWarnings(cbind(table_model, data.frame("ν" = round(as.numeric(res.table.gc$parameter_nu.model), 3), stringsAsFactors = FALSE, check.names = F)))
        }
        if ( "gompertz" %in% res.table.gc$used.model ){
          table_model <- suppressWarnings(cbind(table_model, data.frame("α" = round(as.numeric(res.table.gc$parameter_alpha.model), 3), stringsAsFactors = FALSE, check.names = F)))
        }
        if ( "gompertz.exp" %in% res.table.gc$used.model ){
          table_model <- suppressWarnings(cbind(table_model, data.frame("t<sub>shift</sub>" = round(as.numeric(res.table.gc$parameter_t_shift.model), 3), stringsAsFactors = FALSE, check.names = F)))
        }
        if ("baranyi" %in% res.table.gc$used.model ||  "huang" %in% res.table.gc$used.model ){
          table_model <- suppressWarnings(cbind(table_model, data.frame("y0" = round(as.numeric(res.table.gc$parameter_y0.model), 3), stringsAsFactors = FALSE, check.names = F)))
        }
      }
      table_model
    })
  })

  output$results_table_growth_model <- DT::renderDT({
    table_model <- table_growth_model()
    DT::datatable(table_model,
              options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
              escape = FALSE)
  })

  table_growth_model_group <- reactive({
    gcTable <- results$growth$gcFit$gcTable
    table <- QurvE:::table_group_growth_model(gcTable)
    colnames(table) <- c("Sample|Conc.", "\u03bc<sub>max</sub>", "t<sub>D</sub>", "\u03bb",
                         "y<sub>max</sub>", "\u2206Y")
    table
  })

  output$results_table_growth_model_group <- DT::renderDT({
    DT::datatable(table_growth_model_group(),
                  options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
                  escape = FALSE)
  })

  ### DR Spline ####

  table_growth_dr_spline <- reactive({
    if(results$growth$drFit$control$dr.method == "spline"){
      try({
        res.table.dr <- results$growth$drFit$drTable
        table_dr <- data.frame("Test" = res.table.dr$Test,
                               "EC50" = round(as.numeric(res.table.dr[["EC50"]]), 3),
                               "Response(EC50)" = round(as.numeric(res.table.dr[["yEC50"]]), 3),
                               "Test parameter" = res.table.dr[["test"]],
                               stringsAsFactors = FALSE, check.names = F)
        if(!is.null(res.table.dr)){
          if(results$growth$control$log.x.dr){
            table_dr <- suppressWarnings(cbind(table_dr,
                                               data.frame("EC50 (original)" = round(as.numeric(res.table.dr[["EC50.orig"]]), 3),
                                                          stringsAsFactors = FALSE, check.names = F)))
          }
          if(results$growth$control$log.y.dr){
            table_dr <- suppressWarnings(cbind(table_dr,
                                               data.frame("Response(EC50) (original)" = round(as.numeric(res.table.dr[["yEC50.orig"]]), 3),
                                                          stringsAsFactors = FALSE, check.names = F)))
          }
          if ( results$growth$control$nboot.dr > 2 ){
            table_dr <- suppressWarnings(cbind(table_dr,
                                               data.frame("Mean EC50 (bootstrap)" = paste(round(as.numeric(res.table.dr[["drboot.meanEC50"]]), 3), round(as.numeric(res.table.dr[["drboot.sdEC50"]]), 3), sep = " \u00B1 "),
                                                          "Response(EC50) (bootstrap)" = paste(round(as.numeric(res.table.dr[["drboot.meanEC50y"]]), 3), round(as.numeric(res.table.dr[["drboot.sdEC50y"]]), 3), sep = " \u00B1 "),
                                                          stringsAsFactors = FALSE, check.names = F)))
          }
        }
        table_dr
      })
    }
    else{
      models <- invisible(drc:::getMeanFunctions(display = F))
      model.names <- c(unlist(lapply(1:length(models), function(x) models[[x]][1])), "weibull2x", "gammadr", "multi2")
      model.descr <- c(unlist(lapply(1:length(models), function(x) models[[x]][2])), "four-parameter Weibull model", "Gamma dose-response model", "Multistage dose-response model with quadratic terms")
      try({
        res.table.dr <- results$growth$drFit$drTable
        table_dr <- data.frame("Test" = res.table.dr$Test,
                               "EC50 \u00B1 SE" = paste0(round(as.numeric(res.table.dr[["EC50.Estimate"]]), 3), " \u00B1 ", round(as.numeric(res.table.dr[["EC50.Std..Error"]]), 3)),
                               "Response(EC50)" = round(as.numeric(res.table.dr[["yEC50"]]), 3),
                               "Test parameter" = res.table.dr[["test"]],
                               "Model" = paste0(res.table.dr[["model"]], ": ", model.descr[match(res.table.dr[["model"]], model.names)]),
                               stringsAsFactors = FALSE, check.names = F)

        table_dr
      })
    }
  })

  output$results_table_growth_dr_spline <- DT::renderDT({
    table_dr <- table_growth_dr_spline()
    DT::datatable(table_dr,
              options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "All")) ),
              escape = FALSE)
  })

      ###____Table Download____####
  output$download_table_growth_linear <- downloadHandler(
    filename = function() {
      paste("growth_results_linear_fits", ".csv", sep="")
    },
    content = function(file) {
      table <- table_growth_linear()
      colnames(table) <- gsub("<sub>", "_", gsub("</sub>|<sup>|</sup>", "", gsub("<br>", " ", colnames(table))))
      QurvE:::write.csv.utf8.BOM(table, file)
    }
  )

  output$download_table_growth_linear_group <- downloadHandler(
    filename = function() {
      paste("growth_results_linear_fits_grouped", ".csv", sep="")
    },
    content = function(file) {
      table <- table_growth_linear_group()
      colnames(table) <- gsub("<sub>", "_", gsub("</sub>|<sup>|</sup>", "", gsub("<br>", " ", colnames(table))))
      QurvE:::write.csv.utf8.BOM(table, file)
    }
  )

  output$download_table_growth_spline <- downloadHandler(
    filename = function() {
      paste("growth_results_spline_fits", ".csv", sep="")
    },
    content = function(file) {
      table <- table_growth_spline()
      colnames(table) <- gsub("<sub>", "_", gsub("</sub>|<sup>|</sup>", "", gsub("<br>", " ", colnames(table))))
      QurvE:::write.csv.utf8.BOM(table, file)
    }
  )

  output$download_table_growth_spline_group <- downloadHandler(
    filename = function() {
      paste("growth_results_spline_fits_grouped", ".csv", sep="")
    },
    content = function(file) {
      table <- table_growth_spline_group()
      colnames(table) <- gsub("<sub>", "_", gsub("</sub>|<sup>|</sup>", "", gsub("<br>", " ", colnames(table))))
      QurvE:::write.csv.utf8.BOM(table, file)
    }
  )

  output$download_table_growth_spline_bt <- downloadHandler(
    filename = function() {
      paste("growth_results_spline_bootstrappings", ".csv", sep="")
    },
    content = function(file) {
      table <- table_growth_spline_bt()
      colnames(table) <- gsub("<sub>", "_", gsub("</sub>|<sup>|</sup>", "", gsub("<br>", " ", colnames(table))))
      QurvE:::write.csv.utf8.BOM(table, file)
    }
  )

  output$download_table_growth_model <- downloadHandler(
    filename = function() {
      paste("growth_results_model_fits", ".csv", sep="")
    },
    content = function(file) {
      table <- table_growth_model()
      colnames(table) <- gsub("<sub>", "_", gsub("</sub>|<sup>|</sup>", "", gsub("<br>", " ", colnames(table))))
      QurvE:::write.csv.utf8.BOM(table, file)
    }
  )

  output$download_table_growth_model_group <- downloadHandler(
    filename = function() {
      paste("growth_results_model_fits_grouped", ".csv", sep="")
    },
    content = function(file) {
      table <- table_growth_model_group()
      colnames(table) <- gsub("<sub>", "_", gsub("</sub>|<sup>|</sup>", "", gsub("<br>", " ", colnames(table))))
      QurvE:::write.csv.utf8.BOM(table, file)
    }
  )

  output$download_table_growth_dr <- downloadHandler(
    filename = function() {
      paste("growth_results_dose-response_analysis", ".csv", sep="")
    },
    content = function(file) {
      table <- table_growth_dr_spline()
      colnames(table) <- gsub("<sub>", "_", gsub("</sub>|<sup>|</sup>", "", gsub("<br>", " ", colnames(table))))
      QurvE:::write.csv.utf8.BOM(table, file)
    }
  )
    ## Fluorescence ####
  observe({
    if(!is.null(results$fluorescence)){
      if(!("s" %in% results$fluorescence$control$fit.opt || "a" %in% results$fluorescence$control$fit.opt)){
        hideTab(inputId = "tabsetPanel_Results_Fluorescence", target = "tabPanel_Results_Fluorescence_Spline")
        hideTab(inputId = "tabsetPanel_Results_Fluorescence", target = "tabPanel_Results_Growth_Fluorescence_bt")
      } else {
        showTab(inputId = "tabsetPanel_Results_Fluorescence", target = "tabPanel_Results_Fluorescence_Spline")
      }
      if(!("l" %in% results$fluorescence$control$fit.opt || "a" %in% results$fluorescence$control$fit.opt)){
        hideTab(inputId = "tabsetPanel_Results_Fluorescence", target = "tabPanel_Results_Fluorescence_Linear")
      } else {
        showTab(inputId = "tabsetPanel_Results_Fluorescence", target = "tabPanel_Results_Fluorescence_Linear")
      }
      if(!("s" %in% results$fluorescence$control$fit.opt || "a" %in% results$fluorescence$control$fit.opt) || results$fluorescence$control$nboot.fl <=1){
        hideTab(inputId = "tabsetPanel_Results_Fluorescence", target = "tabPanel_Results_Fluorescence_Spline_bt")
      } else {
        showTab(inputId = "tabsetPanel_Results_Fluorescence", target = "tabPanel_Results_Fluorescence_Spline_bt")
      }
      if(length(results$fluorescence$drFit) >1 && length(results$fluorescence$drFit$drTable) > 1 && results$fluorescence$drFit$control$dr.method == "spline"){
        showTab(inputId = "tabsetPanel_Results_Fluorescence", target = "tabPanel_Results_Fluorescence_DR_Spline")
      } else {
        hideTab(inputId = "tabsetPanel_Results_Fluorescence", target = "tabPanel_Results_Fluorescence_DR_Spline")
      }
      if(length(results$fluorescence$drFit) >1 && length(results$fluorescence$drFit$drTable) > 1 && results$fluorescence$drFit$control$dr.method == "model"){
        showTab(inputId = "tabsetPanel_Results_Fluorescence", target = "tabPanel_Results_Fluorescence_DR_Model")
      } else {
        hideTab(inputId = "tabsetPanel_Results_Fluorescence", target = "tabPanel_Results_Fluorescence_DR_Model")
      }
    }
  })
  table_fluorescence_linear <- reactive({
    res.table.fl <- results$fluorescence$flFit$flTable

    table_linear <- data.frame("Sample|Replicate|Conc." = paste(res.table.fl$TestId, res.table.fl$AddId, res.table.fl$concentration, sep = "|"),
                               "slope<sub>max</sub>" = ifelse(res.table.fl$max_slope.linfit==0 | is.na(res.table.fl$max_slope.linfit), "", ifelse(is.na(res.table.fl$max_slope2.linfit), round(as.numeric(res.table.fl$max_slope.linfit), 3), paste0("<strong>", round(as.numeric(res.table.fl$max_slope.linfit), 3), "</strong>", " (", round(as.numeric(res.table.fl$max_slope2.linfit), 3), ")"))),
                               "t<sub>D</sub>" = ifelse(res.table.fl$max_slope.linfit==0 | is.na(res.table.fl$max_slope.linfit), "",  ifelse(is.na(res.table.fl$max_slope2.linfit), round(log(2)/as.numeric(res.table.fl$max_slope.linfit), 2), paste0("<strong>", round(log(2)/as.numeric(res.table.fl$max_slope.linfit), 2), "</strong>", " (", round(log(2)/as.numeric(res.table.fl$max_slope2.linfit), 2), ")"))),
                               "λ" = round(as.numeric(res.table.fl$lambda.linfit), 2),
                               "ΔY" = round(as.numeric(res.table.fl$dY.linfit), 3),
                               "y<sub>max</sub>" = round(as.numeric(res.table.fl$A.linfit), 3),
                               "x<sub>start</sub><br>(slope<sub>max</sub>)" = ifelse((is.na(res.table.fl$max_slope2.linfit)), round(as.numeric(res.table.fl$x.mu.start.linfit), 2), paste0("<strong>", round(as.numeric(res.table.fl$x.mu.start.linfit), 2), "</strong>", " (", round(as.numeric(res.table.fl$x.mu2.start.linfit), 2), ")")),
                               "x<sub>end</sub><br>(slope<sub>max</sub>)" = ifelse((is.na(res.table.fl$max_slope2.linfit)), round(as.numeric(res.table.fl$x.mu.end.linfit), 2), paste0("<strong>", round(as.numeric(res.table.fl$x.mu.end.linfit), 2), "</strong>", " (", round(as.numeric(res.table.fl$x.mu2.end.linfit), 2), ")")),
                               "R<sup>2</sup><br>(linear fit)" = ifelse((is.na(res.table.fl$max_slope2.linfit)), round(as.numeric(res.table.fl$r2mu.linfit), 3), paste0("<strong>", round(as.numeric(res.table.fl$r2mu.linfit), 3), "</strong>", " (", round(as.numeric(res.table.fl$r2mu.linfit), 3), ")")),
                               stringsAsFactors = FALSE, check.names = F)

    table_linear

  })

  output$results_table_fluorescence_linear <- DT::renderDT({
    DT::datatable(table_fluorescence_linear(),
              options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
              escape = FALSE)
  })

  table_fluorescence_linear_group <- reactive({
    flTable <- results$fluorescence$flFit$flTable
    table <-  QurvE:::table_group_fluorescence_linear(flTable)
    colnames(table) <- c("Sample|Conc.", "slope<sub>max</sub>", "\u03bb",
                         "\u0394Y", "y<sub>max</sub>", "x<sub>start</sub><br>(\u03bc<sub>max</sub>)",
                         "x<sub>end</sub><br>(\u03bc<sub>max</sub>)")
    table
  })

  output$results_table_fluorescence_linear_group <- DT::renderDT({
    DT::datatable(table_fluorescence_linear_group(),
              options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
              escape = FALSE)
  })

  table_fluorescence_spline <- reactive({
    res.table.fl <- results$fluorescence$flFit$flTable
    table_spline <- data.frame("Sample|Replicate|Conc." = paste(res.table.fl$TestId, res.table.fl$AddId, res.table.fl$concentration, sep = "|"),
                               "slope<sub>max</sub>" = ifelse(res.table.fl$max_slope.spline==0 | is.na(res.table.fl$max_slope.spline), "", ifelse(is.na(res.table.fl$max_slope2.spline), round(as.numeric(res.table.fl$max_slope.spline), 3), paste0("<strong>", round(as.numeric(res.table.fl$max_slope.spline), 3), "</strong>", " (", round(as.numeric(res.table.fl$max_slope2.spline), 3), ")"))),
                               "λ" = round(as.numeric(res.table.fl$lambda.spline), 2),
                               "y<sub>max</sub>" = round(as.numeric(res.table.fl$A.spline), 3),
                               "ΔY" = round(as.numeric(res.table.fl$dY.spline), 3),
                               "x<sub>max</sub>" = ifelse(is.na(res.table.fl$max_slope2.spline), round(as.numeric(res.table.fl$x.max.spline), 2), paste0("<strong>", round(as.numeric(res.table.fl$x.max.spline), 2), "</strong>", " (", round(as.numeric(res.table.fl$x.max2.spline), 2), ")")),
                               "smooth.<br>fac" = res.table.fl$smooth.spline, check.names = F)
    table_spline
  })

  output$results_table_fluorescence_spline <- DT::renderDT({
    DT::datatable(table_fluorescence_spline(),
              options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
              escape = FALSE)
  })

  table_fluorescence_spline_group <- reactive({
    flTable <- results$fluorescence$flFit$flTable
    QurvE:::table_group_fluorescence_spline(flTable)
    colnames(table) <- c("Sample|Conc.", "slope<sub>max</sub>", "\u03bb",
                         "\u0394Y", "y<sub>max</sub>", "x(slope<sub>max</sub>)")
    table
  })

  output$results_table_fluorescence_spline_group <- DT::renderDT({
    DT::datatable(table_fluorescence_spline_group(),
              options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
              escape = FALSE)
  })

  table_fluorescence_spline_bt <- reactive({
    res.table.fl <- results$fluorescence$flFit$flTable
    table_spline <- data.frame("Sample|Replicate|Conc." = paste(res.table.fl$TestId, res.table.fl$AddId, res.table.fl$concentration, sep = "|"),
                               "µ<sub>max</sub>" = ifelse(res.table.fl$max_slope.bt==0 | is.na(res.table.fl$max_slope.bt),
                                                          "",
                                                          paste0(round(as.numeric(res.table.fl$max_slope.bt), 3) ,
                                                                 " \u00B1 ",
                                                                 round(as.numeric(res.table.fl$stdmax_slope.bt), 3)
                                                          )
                               ),
                               "t<sub>D</sub>" = ifelse(res.table.fl$max_slope.bt==0 | is.na(res.table.fl$max_slope.bt),
                                                        "",
                                                        paste0(round(log(2)/as.numeric(res.table.fl$max_slope.bt), 3) ,
                                                               " \u00B1 ",
                                                               round(log(2)/as.numeric(res.table.fl$stdmax_slope.bt), 3)
                                                        )
                               ),
                               "λ" = ifelse(res.table.fl$lambda.bt==0 | is.na(res.table.fl$lambda.bt),
                                            "",
                                            paste0(round(as.numeric(res.table.fl$lambda.bt), 3) ,
                                                   " \u00B1 ",
                                                   round(as.numeric(res.table.fl$stdlambda.bt ), 3)
                                            )
                               ),
                               "y<sub>max</sub>" = ifelse(res.table.fl$lambda.bt==0 | is.na(res.table.fl$lambda.bt),
                                                          "",
                                                          paste0(round(as.numeric(res.table.fl$lambda.bt), 3) ,
                                                                 " \u00B1 ",
                                                                 round(as.numeric(res.table.fl$stdlambda.bt ), 3)
                                                          )
                               ),
                               "ΔY" = ifelse(res.table.fl$dY.bt==0 | is.na(res.table.fl$dY.bt),
                                             "",
                                             paste0(round(as.numeric(res.table.fl$dY.bt), 3) ,
                                                    " \u00B1 ",
                                                    round(as.numeric(res.table.fl$stddY.bt ), 3)
                                             )
                               ),
                               "smooth.<br>fac" = res.table.fl$smooth.spline, check.names = F)
    table_spline
  })

  output$results_table_fluorescence_spline_bt <- DT::renderDT({
    DT::datatable(table_fluorescence_spline_bt(),
              options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "50", "All")) ),
              escape = FALSE)
  })

  table_fluorescence_dr_spline <- reactive({
    try({
      res.table.dr <- results$fluorescence$drFit$drTable
      table_dr <- data.frame("Test" = res.table.dr$Test,
                             "EC50" = round(as.numeric(res.table.dr[["EC50"]]), 3),
                             "Response(EC50)" = round(as.numeric(res.table.dr[["yEC50"]]), 3),
                             "Test parameter" = res.table.dr[["test"]],
                             stringsAsFactors = FALSE, check.names = F)
      if(!is.null(res.table.dr)){
        if(results$fluorescence$control$log.x.dr){
          table_dr <- suppressWarnings(cbind(table_dr,
                                             data.frame("EC50 (original)" = round(as.numeric(res.table.dr[["EC50.orig"]]), 3),
                                                        stringsAsFactors = FALSE, check.names = F)))
        }
        if(results$fluorescence$control$log.y.dr){
          table_dr <- suppressWarnings(cbind(table_dr,
                                             data.frame("Response(EC50) (original)" = round(as.numeric(res.table.dr[["yEC50.orig"]]), 3),
                                                        stringsAsFactors = FALSE, check.names = F)))
        }
        if ( results$fluorescence$control$nboot.dr > 2 ){
          table_dr <- suppressWarnings(cbind(table_dr,
                                             data.frame("Mean EC50 (bootstrap)" = paste(round(as.numeric(res.table.dr[["drboot.meanEC50"]]), 3), round(as.numeric(res.table.dr[["drboot.sdEC50"]]), 3), sep = " \u00B1 "),
                                                        "Response(EC50) (bootstrap)" = paste(round(as.numeric(res.table.dr[["drboot.meanEC50y"]]), 3), round(as.numeric(res.table.dr[["drboot.sdEC50y"]]), 3), sep = " \u00B1 "),
                                                        stringsAsFactors = FALSE, check.names = F)))
        }
      }
      table_dr
    })
  })

  output$results_table_fluorescence_dr_spline <- DT::renderDT({
    table_dr <- table_fluorescence_dr_spline()
    DT::datatable(table_dr,
              options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "All")) ),
              escape = FALSE)
  })

  table_fluorescence_dr_model <- reactive({
    try({
      res.table.dr <- results$fluorescence$drFit$drTable
      table_dr <- data.frame("Test" = res.table.dr$Test,
                             "Leakiness (min. FL)" = round(as.numeric(res.table.dr[["y.min"]]), 3),
                             "Sensitivity (K, EC50)" = round(as.numeric(res.table.dr[["K"]]), 3),
                             "Fold change (fc)" = round(as.numeric(res.table.dr[["fc"]]), 3),
                             "Cooperativity (n)" = round(as.numeric(res.table.dr[["n"]]), 3),
                             "Response(EC50)" = round(as.numeric(res.table.dr[["yEC50"]]), 3),
                             "Maximum FL" = round(as.numeric(res.table.dr[["y.max"]]), 3),

                             stringsAsFactors = FALSE, check.names = F)
      if(!is.null(res.table.dr)){
        if(results$fluorescence$control$log.x.dr){
          table_dr <- suppressWarnings(cbind(table_dr,
                                             data.frame("K (original)" = round(as.numeric(res.table.dr[["K.orig"]]), 3),
                                                        stringsAsFactors = FALSE, check.names = F)))
        }
        if(results$fluorescence$control$log.y.dr){
          table_dr <- suppressWarnings(cbind(table_dr,
                                             data.frame("Response(EC50) (original)" = round(as.numeric(res.table.dr[["yEC50.orig"]]), 3),
                                                        stringsAsFactors = FALSE, check.names = F)))
        }
      }
      table_dr
    })
  })

  output$results_table_fluorescence_dr_model <- DT::renderDT({
    table_dr_model <- table_fluorescence_dr_model()
    DT::datatable(table_dr_model,
              options = list(pageLength = 25, info = FALSE, lengthMenu = list(c(15, 25, 50, -1), c("15","25", "All")) ),
              escape = FALSE)
  })

      ###____Table Download____####
  output$download_table_fluorescence_linear <- downloadHandler(
    filename = function() {
      paste("fluorescence_results_linear_fits", ".csv", sep="")
    },
    content = function(file) {
      table <- table_fluorescence_linear()
      colnames(table) <- gsub("<sub>", "_", gsub("</sub>|<sup>|</sup>", "", gsub("<br>", " ", colnames(table))))
      QurvE:::write.csv.utf8.BOM(table, file)
    }
  )

  output$download_table_fluorescence_linear_group <- downloadHandler(
    filename = function() {
      paste("fluorescence_results_linear_fits_grouped", ".csv", sep="")
    },
    content = function(file) {
      table <- table_fluorescence_linear_group()
      colnames(table) <- gsub("<sub>", "_", gsub("</sub>|<sup>|</sup>", "", gsub("<br>", " ", colnames(table))))
      QurvE:::write.csv.utf8.BOM(table, file)
    }
  )

  output$download_table_fluorescence_spline <- downloadHandler(
    filename = function() {
      paste("fluorescence_results_spline_fits", ".csv", sep="")
    },
    content = function(file) {
      table <- table_fluorescence_spline()
      colnames(table) <- gsub("<sub>", "_", gsub("</sub>|<sup>|</sup>", "", gsub("<br>", " ", colnames(table))))
      QurvE:::write.csv.utf8.BOM(table, file)
    }
  )

  output$download_table_fluorescence_spline_group <- downloadHandler(
    filename = function() {
      paste("fluorescence_results_spline_fits_grouped", ".csv", sep="")
    },
    content = function(file) {
      table <- table_fluorescence_spline_group()
      colnames(table) <- gsub("<sub>", "_", gsub("</sub>|<sup>|</sup>", "", gsub("<br>", " ", colnames(table))))
      QurvE:::write.csv.utf8.BOM(table, file)
    }
  )

  output$download_table_fluorescence_spline_bt <- downloadHandler(
    filename = function() {
      paste("fluorescence_results_spline_bootstrappings", ".csv", sep="")
    },
    content = function(file) {
      table <- table_fluorescence_spline_bt()
      colnames(table) <- gsub("<sub>", "_", gsub("</sub>|<sup>|</sup>", "", gsub("<br>", " ", colnames(table))))
      QurvE:::write.csv.utf8.BOM(table, file)
    }
  )

  output$download_table_fluorescence_dr_spline <- downloadHandler(
    filename = function() {
      paste("fluorescence_results_dose-response_analysis_spline", ".csv", sep="")
    },
    content = function(file) {
      table <- table_fluorescence_dr_spline()
      colnames(table) <- gsub("<sub>", "_", gsub("</sub>|<sup>|</sup>", "", gsub("<br>", " ", colnames(table))))
      QurvE:::write.csv.utf8.BOM(table, file)
    }
  )

  output$download_table_fluorescence_dr_model <- downloadHandler(
    filename = function() {
      paste("fluorescence_results_dose-response_analysis_model", ".csv", sep="")
    },
    content = function(file) {
      table <- table_fluorescence_dr_model()
      colnames(table) <- gsub("<sub>", "_", gsub("</sub>|<sup>|</sup>", "", gsub("<br>", " ", colnames(table))))
      QurvE:::write.csv.utf8.BOM(table, file)
    }
  )

  # Validate ####
    ## Growth #####
  observe({
    if(!is.null(results$growth)){
      if(!("s" %in% results$growth$control$fit.opt || "a" %in% results$growth$control$fit.opt)){
        hideTab(inputId = "tabsetPanel_Validate_Growth", target = "tabPanel_Validate_Growth_Spline")
      } else {
        showTab(inputId = "tabsetPanel_Validate_Growth", target = "tabPanel_Validate_Growth_Spline")
      }
      if(!("l" %in% results$growth$control$fit.opt || "a" %in% results$growth$control$fit.opt)){
        hideTab(inputId = "tabsetPanel_Validate_Growth", target = "tabPanel_Validate_Growth_Linear")
      } else {
        showTab(inputId = "tabsetPanel_Validate_Growth", target = "tabPanel_Validate_Growth_Linear")
      }
      if(!("m" %in% results$growth$control$fit.opt || "a" %in% results$growth$control$fit.opt)){
        hideTab(inputId = "tabsetPanel_Validate_Growth", target = "tabPanel_Validate_Growth_Model")
      } else {
        showTab(inputId = "tabsetPanel_Validate_Growth", target = "tabPanel_Validate_Growth_Model")
      }
      if(!("s" %in% results$growth$control$fit.opt || "a" %in% results$growth$control$fit.opt) || results$growth$control$nboot.gc <=1){
        hideTab(inputId = "tabsetPanel_Validate_Growth", target = "tabPanel_Validate_Growth_Spline_bt")
      } else {
        showTab(inputId = "tabsetPanel_Validate_Growth", target = "tabPanel_Validate_Growth_Spline_bt")
      }
    }
  })

  #### Hide [Restore Fit] buttons when starting the app
  hide("restore_growth_linear"); hide("restore_growth_spline"); hide("restore_growth_model")
  hide("restore_dr_growth"); hide("restore_dr_growth2"); hide("restore_dr_growth3");
  hide("restore_dr_fluorescence"); hide("restore_dr_fluorescence2"); hide("restore_dr_fluorescence3")
  # hide("restore_dr_spline_individual_fluorescence")

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
                                                  sample_validate_growth_model = 1,
                                                  sample_validate_growth_spline_bt = 1)

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
  observe({
    req(input$sample_validate_growth_spline_bt)
    selected_vals_validate_growth$sample_validate_growth_spline_bt <- input$sample_validate_growth_spline_bt
  })

      ### Linear Fits ####


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

  validate_growth_plot_linear <- reactive({
    results <- results$growth
    # Define x- and y-axis limits
    if(any(input$y_range_min_validate_growth_plot_linear == "",
           input$y_range_max_validate_growth_plot_linear == "")){
      y.lim <- NULL
    } else {
      y.lim <- c(as.numeric(input$y_range_min_validate_growth_plot_linear),
                as.numeric(input$y_range_max_validate_growth_plot_linear))
    }

    if(any(input$y_range_min_derivative_validate_growth_plot_linear == "",
           input$y_range_max_derivative_validate_growth_plot_linear == "")){
      ylim.deriv <- NULL
    } else {
      ylim.deriv <- c(as.numeric(input$y_range_min_derivative_validate_growth_plot_linear),
                      as.numeric(input$y_range_max_derivative_validate_growth_plot_linear))
    }

    if(any(input$x_range_min_validate_growth_plot_linear == "",
           input$x_range_max_validate_growth_plot_linear == "")){
      x.lim <- NULL
    } else {
      x.lim <- c(as.numeric(input$x_range_min_validate_growth_plot_linear),
                 as.numeric(input$x_range_max_validate_growth_plot_linear))
    }
    if (length(results$gcFit$gcFittedLinear[[ifelse(
      selected_vals_validate_growth$sample_validate_growth_linear == "1" ||
      is.null(
        selected_vals_validate_growth$sample_validate_growth_linear
      ) ||
      selected_vals_validate_growth$sample_validate_growth_linear == "" ,
      1,
      selected_vals_validate_growth$sample_validate_growth_linear
    )]]) > 1) {
      if(input$diagnostics_validate_growth_plot_linear){
        try(
          suppressWarnings(
            plot.gcFitLinear(results$gcFit$gcFittedLinear[[ifelse(
              selected_vals_validate_growth$sample_validate_growth_linear == "1" ||
                is.null(
                  selected_vals_validate_growth$sample_validate_growth_linear
                ),
              1,
              selected_vals_validate_growth$sample_validate_growth_linear
            )]],
            which = "fit_diagnostics",
            pch = input$shape_type_validate_growth_plot_linear,
            log = logy_validate_growth_plot_linear(),
            cex.point = input$shape_size_validate_growth_plot_linear,
            cex.lab = input$axis_size_validate_growth_plot_linear,
            cex.axis = input$lab_size_validate_growth_plot_linear,
            lwd = input$line_width_validate_growth_plot_linear,
            color = input$color_validate_growth_plot_linear
            )
          )
        )
      } else {
        try(
          suppressWarnings(
            plot.gcFitLinear(
              results$gcFit$gcFittedLinear[[ifelse(
                selected_vals_validate_growth$sample_validate_growth_linear == "1" ||
                  is.null(
                    selected_vals_validate_growth$sample_validate_growth_linear
                  ),
                1,
                selected_vals_validate_growth$sample_validate_growth_linear
              )]],
              pch = input$shape_type_validate_growth_plot_linear,

              log = logy_validate_growth_plot_linear(),
              cex.point = input$shape_size_validate_growth_plot_linear,
              cex.lab = input$axis_size_validate_growth_plot_linear,
              cex.axis = input$lab_size_validate_growth_plot_linear,
              lwd = input$line_width_validate_growth_plot_linear,
              y.lim = y.lim,
              x.lim = x.lim,
              color = input$color_validate_growth_plot_linear
            )
          )
        )
      }
    }
  }

  )

  output$validate_growth_plot_linear <- renderPlot({
    validate_growth_plot_linear()
  })

  lin.rerun.param <- reactiveValues()

  observeEvent(input$rerun_growth_linear, {
    # display a modal dialog with a header, textinput and action buttons
    showModal(
      modalDialog(
        tags$h2('Please enter adjusted parameters'),
        checkboxInput('log.y.lin.rerun', 'log-transform growth values', value = results$growth$gcFit$gcFittedLinear[[selected_vals_validate_growth$sample_validate_growth_linear]]$control$log.y.lin),
        textInput('t0.lin.rerun', 'Minimum time (t0)', placeholder = paste0("previously: ", results$growth$gcFit$gcFittedLinear[[selected_vals_validate_growth$sample_validate_growth_linear]]$control$t0)),
        textInput('tmax.lin.rerun', 'Maximum time (tmax)', placeholder = paste0("previously: ", results$growth$gcFit$gcFittedLinear[[selected_vals_validate_growth$sample_validate_growth_linear]]$control$tmax)),
        textInput('min.growth.lin.rerun', 'Minimum growth measurement', placeholder = paste0("previously: ", results$growth$gcFit$gcFittedLinear[[selected_vals_validate_growth$sample_validate_growth_linear]]$control$min.growth)),
        textInput('max.growth.lin.rerun', 'Maximum growth measurement', placeholder = paste0("previously: ", results$growth$gcFit$gcFittedLinear[[selected_vals_validate_growth$sample_validate_growth_linear]]$control$max.growth)),
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
          fluidRow(
            column(12,
                   div(
                     actionButton('submit.rerun.linear.growth', 'Submit'),
                     style="float:right"),
                   div(
                     modalButton('cancel'),
                     style="float:right"),
                   div(
                     actionButton(inputId = "tooltip_growth.gcFitLinear_validate",
                                  label = "",
                                  icon=icon("question"),
                                  style="padding:2px; font-size:100%"),
                     style="float:left")

            )
          )
        )
      )
    )
  })

  # Re-run selected linear fit with user-defined parameters upon click on 'submit'
  observeEvent(input$submit.rerun.linear.growth, {
    if(!is.null(results$growth$gcFit)){

      showModal(modalDialog("Fitting sample data...", footer = NULL))

      # store previous fit in memory
      selected_vals_validate_growth$restore_growth_linear <- results$growth$gcFit$gcFittedLinear[[selected_vals_validate_growth$sample_validate_growth_linear]]

      # Re-run fit and store in results object
      actwell <- results$growth$gcFit$gcFittedLinear[[selected_vals_validate_growth$sample_validate_growth_linear]]$raw.data
      acttime <- results$growth$gcFit$gcFittedLinear[[selected_vals_validate_growth$sample_validate_growth_linear]]$raw.time
      control <- results$growth$gcFit$gcFittedLinear[[selected_vals_validate_growth$sample_validate_growth_linear]]$control
      control_new <- control
      gcID <- results$growth$gcFit$gcFittedLinear[[selected_vals_validate_growth$sample_validate_growth_linear]]$gcID
      lin.h.new <- ifelse(!is.na(as.numeric(input$lin.h.rerun)), as.numeric(input$lin.h.rerun), as.numeric(control$lin.h))
      if(!is.na(lin.h.new)) control_new$lin.h <- lin.h.new
      control_new$lin.R2 <- ifelse(!is.na(as.numeric(input$lin.R2.rerun)), as.numeric(input$lin.R2.rerun), control$lin.R2)
      control_new$lin.RSD <- ifelse(!is.na(as.numeric(input$lin.RSD.rerun)), as.numeric(input$lin.RSD.rerun), control$lin.RSD)
      control_new$t0 <- ifelse(!is.na(as.numeric(input$t0.lin.rerun)), as.numeric(input$t0.lin.rerun), control$t0)
      control_new$tmax <- ifelse(!is.na(as.numeric(input$tmax.lin.rerun)), as.numeric(input$tmax.lin.rerun), control$tmax)
      control_new$log.y.lin <- input$log.y.lin.rerun
      min.growth.lin.new <- ifelse(!is.na(as.numeric(input$min.growth.lin.rerun)), as.numeric(input$min.growth.lin.rerun), control$min.growth)
      if(is.numeric(min.growth.lin.new)){
        if(!is.na(min.growth.lin.new) && all(as.vector(actwell) < min.growth.lin.new)){
          message(paste0("Start growth values need to be greater than 'min.growth'.\nThe minimum start value in your dataset is: ",
                         min(as.vector(actwell)),". 'min.growth' was not adjusted."), call. = FALSE)
        } else if(!is.na(min.growth.lin.new)){
          control_new$min.growth <- min.growth.lin.new
        }
      }
      control_new$max.growth <- ifelse(!is.na(as.numeric(input$max.growth.lin.rerun)), as.numeric(input$max.growth.lin.rerun), control$max.growth)

      quota_new <- ifelse(!is.na(as.numeric(input$quota.rerun)), as.numeric(input$quota.rerun), 0.95)
      try(
        results$growth$gcFit$gcFittedLinear[[selected_vals_validate_growth$sample_validate_growth_linear]] <-
          growth.gcFitLinear(acttime, actwell,
                             gcID = gcID,
                             control = control_new,
                             quota = quota_new)
      )
      # Update gcTable with new results
      res.table.gc <- results$growth$gcFit$gcTable
      fit.summary <- QurvE:::summary.gcFitLinear(results$growth$gcFit$gcFittedLinear[[selected_vals_validate_growth$sample_validate_growth_linear]])

      sample.ndx <- ifelse(is.numeric(selected_vals_validate_growth$sample_validate_growth_linear),
                           selected_vals_validate_growth$sample_validate_growth_linear,
                           match(selected_vals_validate_growth$sample_validate_growth_linear, names(results$growth$gcFit$gcFittedLinear)))
      results$growth$gcFit$gcTable[sample.ndx, colnames(res.table.gc) %in% colnames(fit.summary)] <- fit.summary

      # Re-run drFit if respective fit type (linear/spline/model) was chosen as dr.parameter
      dr.parameter.fit.method <- gsub(".+\\.", "", results$growth$control$dr.parameter)

      if(length(results$growth$drFit) > 1 && dr.parameter.fit.method == "linfit"){
        # Store previous drFit in memory
        selected_vals_validate_growth$restore_growth_drFit <- results$growth$drFit

        # Re-run drFit
        if (is.null(input$smoothing_factor_growth_dr) || is.na(input$smoothing_factor_growth_dr) || input$smoothing_factor_growth_dr == "NULL" || input$smoothing_factor_growth_dr == "") {
          smooth.dr = NULL
        } else {
          smooth.dr <- as.numeric(input$smoothing_factor_growth_dr)
        }
        if (is.null(input$number_of_bootstrappings_dr_growth) || is.na(input$number_of_bootstrappings_dr_growth) || input$number_of_bootstrappings_dr_growth == "NULL" || input$number_of_bootstrappings_dr_growth == "") {
          nboot.dr <- 0
        } else {
          nboot.dr <- as.numeric(input$number_of_bootstrappings_dr_growth)
        }
        showModal(modalDialog("Re-calculating dose-response analysis...", footer = NULL))

        try(
          results$growth$drFit <-
            suppressWarnings(
              growth.drFit(gcTable = results$growth$gcFit$gcTable,
                           control = growth.control(dr.method = input$dr_method_growth,
                                                    dr.parameter = results$growth$control$dr.parameter,
                                                    smooth.dr = smooth.dr,
                                                    log.x.dr = input$log_transform_concentration_growth,
                                                    log.y.dr = input$log_transform_response_growth,
                                                    nboot.dr = nboot.dr
                           )
              )
            )
        )
      }

      # Show [Restore fit] button
      show("restore_growth_linear")
    }

    removeModal()
  })

  # Restore previous linear fit upon click on [Restore Fit]
  observeEvent(input$restore_growth_linear, {
    # restore previous fit from memory
    results$growth$gcFit$gcFittedLinear[[selected_vals_validate_growth$sample_validate_growth_linear]] <- selected_vals_validate_growth$restore_growth_linear
    dr.parameter.fit.method <- gsub(".+\\.", "", results$growth$control$dr.parameter)
    if(length(results$growth$drFit) > 1 && dr.parameter.fit.method == "linfit"){
      results$growth$drFit <- selected_vals_validate_growth$restore_growth_drFit
    }
    hide("restore_growth_linear")
  })

  output$download_growth_validate_linear <- downloadHandler(
    filename = function() {
      paste("linear_fit_",  gsub(" \\| ", "_", selected_vals_validate_growth$sample_validate_growth_linear), input$format_download_growth_validate_linear, sep="")
    },
    content = function(file) {
      if(input$format_download_growth_validate_linear == ".pdf"){
        if (requireNamespace("Cairo", quietly = TRUE)) {
          Cairo::CairoPDF(file = file,
                          width = input$width_download_growth_validate_linear,
                          height = input$height_download_growth_validate_linear)
        } else {
          showModal(modalDialog("Package 'Cairo' must be installed to preserve special characters in the exported PDF image", easyClose = T))
          grDevices::pdf(file = file,
                         width = input$width_download_growth_validate_linear,
                         height = input$height_download_growth_validate_linear)
        }

      } else {
        grDevices::png(file = file,
            width = input$width_download_growth_validate_linear,
            height = input$height_download_growth_validate_linear,
            units = "in",
            res = input$dpi_download_growth_validate_linear)
      }
      if(input$logy_validate_growth_plot_linear) log <- "y"
      else  log <- ""
      results <- results$growth
      # Define x- and y-axis limits
      if(any(input$y_range_min_validate_growth_plot_linear == "",
             input$y_range_max_validate_growth_plot_linear == "")){
        y.lim <- NULL
      } else {
        y.lim <- c(as.numeric(input$y_range_min_validate_growth_plot_linear),
                  as.numeric(input$y_range_max_validate_growth_plot_linear))
      }

      if(any(input$y_range_min_derivative_validate_growth_plot_linear == "",
             input$y_range_max_derivative_validate_growth_plot_linear == "")){
        ylim.deriv <- NULL
      } else {
        ylim.deriv <- c(as.numeric(input$y_range_min_derivative_validate_growth_plot_linear),
                        as.numeric(input$y_range_max_derivative_validate_growth_plot_linear))
      }

      if(any(input$x_range_min_validate_growth_plot_linear == "",
             input$x_range_max_validate_growth_plot_linear == "")){
        x.lim <- NULL
      } else {
        x.lim <- c(as.numeric(input$x_range_min_validate_growth_plot_linear),
                  as.numeric(input$x_range_max_validate_growth_plot_linear))
      }


      if (length(results$gcFit$gcFittedLinear[[ifelse(
        selected_vals_validate_growth$sample_validate_growth_linear == "1" ||
        is.null(
          selected_vals_validate_growth$sample_validate_growth_linear
        ),
        1,
        selected_vals_validate_growth$sample_validate_growth_linear
      )]]) > 1) {
        suppressWarnings(
          plot.gcFitLinear(results$gcFit$gcFittedLinear[[ifelse(selected_vals_validate_growth$sample_validate_growth_linear == "1" || is.null(selected_vals_validate_growth$sample_validate_growth_linear), 1, selected_vals_validate_growth$sample_validate_growth_linear)]],
                           pch = input$shape_type_validate_growth_plot_linear,
                           log = log,
                           cex.point = input$shape_size_validate_growth_plot_linear,
                           cex.lab = input$axis_size_validate_growth_plot_linear,
                           cex.axis = input$lab_size_validate_growth_plot_linear,
                           lwd = input$line_width_validate_growth_plot_linear,
                           y.lim = y.lim,
                           x.lim = x.lim
          )
        )
        if(input$diagnostics_validate_growth_plot_linear){
          suppressWarnings(
            plot.gcFitLinear(results$gcFit$gcFittedLinear[[ifelse(
              selected_vals_validate_growth$sample_validate_growth_linear == "1" ||
                is.null(
                  selected_vals_validate_growth$sample_validate_growth_linear
                ),
              1,
              selected_vals_validate_growth$sample_validate_growth_linear
            )]],
            which = "fit_diagnostics",
            pch = input$shape_type_validate_growth_plot_linear,
            log = log,
            cex.point = input$shape_size_validate_growth_plot_linear,
            cex.lab = input$axis_size_validate_growth_plot_linear,
            cex.axis = input$lab_size_validate_growth_plot_linear,
            lwd = input$line_width_validate_growth_plot_linear
            )
          )
        }
      }
      dev.off()
    },
    contentType = ifelse(input$format_download_growth_validate_linear == ".pdf", "image/pdf", "image/png")

  )

      ### Spline Fits ####
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
    )
  })


  output$validate_growth_plot_spline <- renderPlot({
    results <- results$growth
    if(length(results$gcFit$gcFittedSplines[[ifelse(
      selected_vals_validate_growth$sample_validate_growth_spline == "1" ||
      selected_vals_validate_growth$sample_validate_growth_spline == "" ||
      is.null(selected_vals_validate_growth$sample_validate_growth_spline),
      1,
      selected_vals_validate_growth$sample_validate_growth_spline
    )]]) > 1) {
      showModal(modalDialog("Creating plot...", footer = NULL))
      try(
        suppressWarnings(
          plot.gcFitSpline(
            results$gcFit$gcFittedSplines[[ifelse(
              selected_vals_validate_growth$sample_validate_growth_spline == "1" ||
                is.null(
                  selected_vals_validate_growth$sample_validate_growth_spline
                ),
              1,
              selected_vals_validate_growth$sample_validate_growth_spline
            )]],
            log.y = input$logy_validate_growth_plot_spline,
            x.lim = c(
              input$x_range_min_validate_growth_plot_spline,
              input$x_range_max_validate_growth_plot_spline
            ),
            y.lim = c(
              input$y_range_min_validate_growth_plot_spline,
              input$y_range_max_validate_growth_plot_spline
            ),
            y.lim.deriv = c(
              input$y_range_min_derivative_validate_growth_plot_spline,
              input$y_range_max_derivative_validate_growth_plot_spline
            ),
            lwd = input$line_width_validate_growth_plot_spline,
            cex.point = input$shape_size_validate_growth_plot_spline,
            basesize = input$base_size_validate_growth_plot_spline,
            n.ybreaks = input$nbreaks_validate_growth_plot_spline,
            pch = input$shape_type_validate_growth_plot_spline,
            deriv = input$plot_derivative_validate_growth_plot_spline,
            colSpline = input$color_validate_growth_plot_spline
          )
        )
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
        textInput('t0.spline.rerun', 'Minimum time (t0)', placeholder = paste0("previously: ", results$growth$gcFit$gcFittedSplines[[selected_vals_validate_growth$sample_validate_growth_spline]]$control$t0)),
        textInput('tmax.spline.rerun', 'Maximum time (tmax)', placeholder = paste0("previously: ", results$growth$gcFit$gcFittedSplines[[selected_vals_validate_growth$sample_validate_growth_spline]]$control$tmax)),
        textInput('min.growth.spline.rerun', 'Minimum growth measurement', placeholder = paste0("previously: ", results$growth$gcFit$gcFittedSplines[[selected_vals_validate_growth$sample_validate_growth_spline]]$control$min.growth)),
        textInput('max.growth.spline.rerun', 'Maximum growth measurement', placeholder = paste0("previously: ", results$growth$gcFit$gcFittedSplines[[selected_vals_validate_growth$sample_validate_growth_spline]]$control$max.growth)),
        textInput('smooth.gc.rerun', 'Smoothing factor', placeholder = paste0("previously: ", results$growth$gcFit$gcFittedSplines[[selected_vals_validate_growth$sample_validate_growth_spline]]$control$smooth.gc)),
        footer=tagList(
          fluidRow(
            column(12,
                   div(
                     actionButton('submit.rerun.spline.growth', 'Submit'),
                     style="float:right"),
                   div(
                     modalButton('cancel'),
                     style="float:right"),
                   div(
                     actionButton(inputId = "tooltip_growth.gcFitSpline_validate",
                                  label = "",
                                  icon=icon("question"),
                                  style="padding:2px; font-size:100%"),
                     style="float:left")

            )
          )
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

      control_new$smooth.gc <- ifelse(!is.na(as.numeric(input$smooth.gc.rerun)), as.numeric(input$smooth.gc.rerun), control$smooth.gc)
      control_new$t0 <- ifelse(!is.na(as.numeric(input$t0.spline.rerun)), as.numeric(input$t0.spline.rerun), control$t0)
      control_new$tmax <- ifelse(!is.na(as.numeric(input$tmax.spline.rerun)), as.numeric(input$tmax.spline.rerun), control$tmax)
      min.growth.spline.new <- ifelse(!is.na(as.numeric(input$min.growth.spline.rerun)), as.numeric(input$min.growth.spline.rerun), control$min.growth)
      if(is.numeric(min.growth.spline.new)){
        if(!is.na(min.growth.spline.new) && all(as.vector(actwell) < min.growth.spline.new)){
          message(paste0("Start growth values need to be greater than 'min.growth'.\nThe minimum start value in your dataset is: ",
                         min(as.vector(actwell)),". 'min.growth' was not adjusted."), call. = FALSE)
        } else if(!is.na(min.growth.spline.new)){
          control_new$min.growth <- min.growth.spline.new
        }
      }
      control_new$max.growth <- ifelse(!is.na(as.numeric(input$max.growth.spline.rerun)), as.numeric(input$max.growth.spline.rerun), control$max.growth)

      showModal(modalDialog("Fitting sample data...", footer = NULL))

      try(
        results$growth$gcFit$gcFittedSplines[[selected_vals_validate_growth$sample_validate_growth_spline]] <-
          growth.gcFitSpline(acttime, actwell,
                             gcID = gcID,
                             control = control_new)
      )
      # Update gcTable with new results
      res.table.gc <- results$growth$gcFit$gcTable
      fit.summary <- QurvE:::summary.gcFitSpline(results$growth$gcFit$gcFittedSplines[[selected_vals_validate_growth$sample_validate_growth_spline]])

      sample.ndx <- ifelse(is.numeric(selected_vals_validate_growth$sample_validate_growth_spline),
                           selected_vals_validate_growth$sample_validate_growth_spline,
                           match(selected_vals_validate_growth$sample_validate_growth_spline, names(results$growth$gcFit$gcFittedSplines)))
      results$growth$gcFit$gcTable[sample.ndx, colnames(res.table.gc) %in% colnames(fit.summary)] <- fit.summary

      # Re-run drFit if respective fit type (linear/spline/model) was chosen as dr.parameter
      dr.parameter.fit.method <- gsub(".+\\.", "", results$growth$control$dr.parameter)

      if(length(results$growth$drFit) > 1 && dr.parameter.fit.method == "spline"){
        # Store previous drFit in memory
        selected_vals_validate_growth$restore_growth_drFit <- results$growth$drFit

        # Re-run drFit
        if (is.null(input$smoothing_factor_growth_dr) || is.na(input$smoothing_factor_growth_dr) || input$smoothing_factor_growth_dr == "NULL" || input$smoothing_factor_growth_dr == "") {
          smooth.dr = NULL
        } else {
          smooth.dr <- as.numeric(input$smoothing_factor_growth_dr)
        }
        if (is.null(input$number_of_bootstrappings_dr_growth) || is.na(input$number_of_bootstrappings_dr_growth) || input$number_of_bootstrappings_dr_growth == "NULL" || input$number_of_bootstrappings_dr_growth == "") {
          nboot.dr <- 0
        } else {
          nboot.dr <- as.numeric(input$number_of_bootstrappings_dr_growth)
        }
        showModal(modalDialog("Re-calculating dose-response analysis...", footer = NULL))

        try(
          results$growth$drFit <-
            suppressWarnings(
              growth.drFit(gcTable = results$growth$gcFit$gcTable,
                           control = growth.control(dr.method = input$dr_method_growth,
                                                    dr.parameter = results$growth$control$dr.parameter,
                                                    smooth.dr = smooth.dr,
                                                    log.x.dr = input$log_transform_concentration_growth,
                                                    log.y.dr = input$log_transform_response_growth,
                                                    nboot.dr = nboot.dr
                           )
              )
            )
        )
      }
      # Show [Restore fit] button
      show("restore_growth_spline")
    }

    removeModal()

  })

  # Restore previous spline fit upon click on [Restore Fit]
  observeEvent(input$restore_growth_spline, {
    # restore previous fit from memory
    results$growth$gcFit$gcFittedSplines[[selected_vals_validate_growth$sample_validate_growth_spline]] <- selected_vals_validate_growth$restore_growth_spline
    hide("restore_growth_spline")
    dr.parameter.fit.method <- gsub(".+\\.", "", results$growth$control$dr.parameter)
    if(length(results$growth$drFit) > 1 && dr.parameter.fit.method == "spline"){
      results$growth$drFit <- selected_vals_validate_growth$restore_growth_drFit
    }
  })

  output$download_growth_validate_spline <- downloadHandler(
    filename = function() {
      paste("spline_fit_",  gsub(" \\| ", "_", selected_vals_validate_growth$sample_validate_growth_spline), input$format_download_growth_validate_spline, sep="")
    },
    content = function(file) {
      if(input$format_download_growth_validate_spline == ".pdf"){
        if (requireNamespace("Cairo", quietly = TRUE)) {
          ggsave(filename = file, width = input$width_download_growth_validate_spline,
                 height = input$height_download_growth_validate_spline,
                 dpi = input$dpi_download_growth_validate_spline,
                 device = cairo_pdf)
        } else {
          showModal(modalDialog("Package 'Cairo' must be installed to preserve special characters in the exported PDF image", easyClose = T))
          ggsave(filename = file, width = input$width_download_growth_validate_spline,
                 height = input$height_download_growth_validate_spline,
                 dpi = input$dpi_download_growth_validate_spline,
                 device = pdf)
        }

      } else {
        ggsave(filename = file, width = input$width_download_growth_validate_spline,
               height = input$height_download_growth_validate_spline,
               dpi = input$dpi_download_growth_validate_spline,
               device = png)
      }

    },
    contentType = ifelse(input$format_download_growth_validate_spline == ".pdf", "image/pdf", "image/png")

  )

  output$download_growth_validate_spline_values <- downloadHandler(
    filename = function() {
      paste("values_spline_fit_",  gsub(" \\| ", "_", selected_vals_validate_growth$sample_validate_growth_spline), ".csv", sep="")
    },
    content = function(file) {
      results <- results$growth
      if(length(results$gcFit$gcFittedSplines[[ifelse(
        selected_vals_validate_growth$sample_validate_growth_spline == "1" ||
        selected_vals_validate_growth$sample_validate_growth_spline == "" ||
        is.null(selected_vals_validate_growth$sample_validate_growth_spline),
        1,
        selected_vals_validate_growth$sample_validate_growth_spline
      )]]) > 1) {
        try(p <-
              suppressWarnings(
                plot.gcFitSpline(
                  results$gcFit$gcFittedSplines[[ifelse(
                    selected_vals_validate_growth$sample_validate_growth_spline == "1" ||
                      is.null(
                        selected_vals_validate_growth$sample_validate_growth_spline
                      ),
                    1,
                    selected_vals_validate_growth$sample_validate_growth_spline
                  )]],
                  export = FALSE,
                  plot = FALSE,
                  log.y = FALSE,
                  deriv = F
                )
              ))
      }
      table <- data.frame("x" = as.numeric(p$data$fit.time), "y" = as.numeric(p$data$fit.data))
      QurvE:::write.csv.utf8.BOM(table, file)
    },
    contentType = ".csv"
  )

  output$download_growth_validate_spline_deriv_values <- downloadHandler(
    filename = function() {
      paste("values_spline_fit_derivative_",  gsub(" \\| ", "_", selected_vals_validate_growth$sample_validate_growth_spline), ".csv", sep="")
    },
    content = function(file) {
      results <- results$growth
      if (length(results$gcFit$gcFittedSplines[[ifelse(
        selected_vals_validate_growth$sample_validate_growth_spline == "1" ||
        selected_vals_validate_growth$sample_validate_growth_spline == "" ||
        is.null(
          selected_vals_validate_growth$sample_validate_growth_spline
        ),
        1,
        selected_vals_validate_growth$sample_validate_growth_spline
      )]]) > 1) {
        table <-  as.data.frame(results$gcFit$gcFittedSplines[[ifelse(
          selected_vals_validate_growth$sample_validate_growth_spline == "1" ||
            is.null(
              selected_vals_validate_growth$sample_validate_growth_spline
            ),
          1,
          selected_vals_validate_growth$sample_validate_growth_spline
        )]]$spline.deriv1)
      }
      QurvE:::write.csv.utf8.BOM(table, file)
    },
    contentType = ".csv"
  )

      ### Model Fits ####
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
    if(length(results$gcFit$gcFittedModels[[ifelse(selected_vals_validate_growth$sample_validate_growth_model == "1"||
                                                   selected_vals_validate_growth$sample_validate_growth_model == "" ||
                                                   is.null(selected_vals_validate_growth$sample_validate_growth_model), 1, selected_vals_validate_growth$sample_validate_growth_model)]]) > 1){
      showModal(modalDialog("Creating plot...", footer=NULL))
      try(
        suppressWarnings(
          plot.gcFitModel(results$gcFit$gcFittedModels[[ifelse(selected_vals_validate_growth$sample_validate_growth_model == "1" || is.null(selected_vals_validate_growth$sample_validate_growth_model), 1, selected_vals_validate_growth$sample_validate_growth_model)]],
                          colModel = input$color_validate_growth_plot_model,
                          pch = input$shape_type_validate_growth_plot_model,
                          basesize = input$base_size_validate_growth_plot_model,
                          cex.point = input$shape_size_validate_growth_plot_model,
                          lwd = input$line_width_validate_growth_plot_model,
                          n.ybreaks = input$nbreaks_validate_growth_plot_model,
                          eq.size = input$eqsize_validate_growth_plot_model,
                          export = FALSE
          )
        )
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
        textInput('t0.model.rerun', 'Minimum time (t0)', placeholder = paste0("previously: ", results$growth$gcFit$gcFittedModels[[selected_vals_validate_growth$sample_validate_growth_model]]$control$t0)),
        textInput('min.growth.model.rerun', 'Minimum growth measurement', placeholder = paste0("previously: ", results$growth$gcFit$gcFittedModels[[selected_vals_validate_growth$sample_validate_growth_model]]$control$min.growth)),
        wellPanel(
          h4(strong('Models:')),
          style='padding: 1; padding-top: 0; padding-bottom: 0',
          checkboxInput(inputId = 'logistic_growth_rerun',
                        label = 'logistic',
                        value = ("logistic" %in% results$growth$gcFit$gcFittedModels[[selected_vals_validate_growth$sample_validate_growth_model]]$control$model.type)),

          checkboxInput(inputId = 'richards_growth_rerun',
                        label = 'Richards',
                        value = ("richards" %in% results$growth$gcFit$gcFittedModels[[selected_vals_validate_growth$sample_validate_growth_model]]$control$model.type)),

          checkboxInput(inputId = 'gompertz_growth_rerun',
                        label = 'Gompertz',
                        value = ("gompertz" %in% results$growth$gcFit$gcFittedModels[[selected_vals_validate_growth$sample_validate_growth_model]]$control$model.type)),

          checkboxInput(inputId = 'extended_gompertz_growth_rerun',
                        label = 'extended Gompertz',
                        value = ("gompertz.exp" %in% results$growth$gcFit$gcFittedModels[[selected_vals_validate_growth$sample_validate_growth_model]]$control$model.type)),

          checkboxInput(inputId = 'huang_growth_rerun',
                        label = 'Huang',
                        value = ("huang" %in% results$growth$gcFit$gcFittedModels[[selected_vals_validate_growth$sample_validate_growth_model]]$control$model.type)),

          checkboxInput(inputId = 'baranyi_growth_rerun',
                        label = 'Baranyi and Roberts',
                        value = ("baranyi" %in% results$growth$gcFit$gcFittedModels[[selected_vals_validate_growth$sample_validate_growth_model]]$control$model.type))
        ),
        footer=tagList(
          fluidRow(
            column(12,
                   div(
                     actionButton('submit.rerun.model', 'Submit'),
                     style="float:right"),
                   div(
                     modalButton('cancel'),
                     style="float:right"),
                   div(
                     actionButton(inputId = "tooltip_growth.gcFitModel_validate",
                                  label = "",
                                  icon=icon("question"),
                                  style="padding:2px; font-size:100%"),
                     style="float:left")

            )
          )
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
      min.growth.model.new <- ifelse(!is.na(as.numeric(input$min.growth.model.rerun)), as.numeric(input$min.growth.model.rerun), control$min.growth)
      if(is.numeric(min.growth.model.new)){
        if(!is.na(min.growth.model.new) && all(as.vector(actwell) < min.growth.model.new)){
          message(paste0("Start growth values need to be greater than 'min.growth'.\nThe minimum start value in your dataset is: ",
                         min(as.vector(actwell)),". 'min.growth' was not adjusted."), call. = FALSE)
        } else if(!is.na(min.growth.model.new)){
          control_new$min.growth <- min.growth.model.new
        }
      }
      # combine selected models into vector
      models <- c()
      if(input$logistic_growth_rerun == TRUE) models <- c(models, "logistic")
      if(input$richards_growth_rerun == TRUE) models <- c(models, "richards")
      if(input$gompertz_growth_rerun == TRUE) models <- c(models, "gompertz")
      if(input$extended_gompertz_growth_rerun == TRUE) models <- c(models, "gompertz.exp")
      if(input$huang_growth_rerun == TRUE) models <- c(models, "huang")
      if(input$baranyi_growth_rerun == TRUE) models <- c(models, "baranyi")

      control_new$model.type <- models
      showModal(modalDialog("Fitting sample data...", footer = NULL))

      try(results$growth$gcFit$gcFittedModels[[selected_vals_validate_growth$sample_validate_growth_model]] <-
            growth.gcFitModel(acttime, actwell,
                              gcID = gcID,
                              control = control_new))

      # Update gcTable with new results
      res.table.gc <- results$growth$gcFit$gcTable
      fit.summary <- QurvE:::summary.gcFitModel(results$growth$gcFit$gcFittedModels[[selected_vals_validate_growth$sample_validate_growth_model]])

      sample.ndx <- ifelse(is.numeric(selected_vals_validate_growth$sample_validate_growth_model),
                           selected_vals_validate_growth$sample_validate_growth_model,
                           match(selected_vals_validate_growth$sample_validate_growth_model, names(results$growth$gcFit$gcFittedModels)))
      results$growth$gcFit$gcTable[sample.ndx, colnames(res.table.gc) %in% colnames(fit.summary)] <- fit.summary
      results$growth$gcFit$gcTable[sample.ndx, "used.model"] <- results$growth$gcFit$gcFittedModels[[selected_vals_validate_growth$sample_validate_growth_model]]$model

      # Re-run drFit if respective fit type (linear/spline/model) was chosen as dr.parameter
      dr.parameter.fit.method <- gsub(".+\\.", "", results$growth$control$dr.parameter)

      if(length(results$growth$drFit) > 1 && dr.parameter.fit.method == "model"){
        # Store previous drFit in memory
        selected_vals_validate_growth$restore_growth_drFit <- results$growth$drFit

        # Re-run drFit
        if (is.null(input$smoothing_factor_growth_dr) || is.na(input$smoothing_factor_growth_dr) || input$smoothing_factor_growth_dr == "NULL" || input$smoothing_factor_growth_dr == "") {
          smooth.dr = NULL
        } else {
          smooth.dr <- as.numeric(input$smoothing_factor_growth_dr)
        }
        if (is.null(input$number_of_bootstrappings_dr_growth) || is.na(input$number_of_bootstrappings_dr_growth) || input$number_of_bootstrappings_dr_growth == "NULL" || input$number_of_bootstrappings_dr_growth == "") {
          nboot.dr <- 0
        } else {
          nboot.dr <- as.numeric(input$number_of_bootstrappings_dr_growth)
        }
        showModal(modalDialog("Re-calculating dose-response analysis...", footer = NULL))

        try(
          results$growth$drFit <-
            suppressWarnings(
              growth.drFit(gcTable = results$growth$gcFit$gcTable,
                           control = growth.control(dr.method = input$dr_method_growth,
                                                    dr.parameter = results$growth$control$dr.parameter,
                                                    smooth.dr = smooth.dr,
                                                    log.x.dr = input$log_transform_concentration_growth,
                                                    log.y.dr = input$log_transform_response_growth,
                                                    nboot.dr = nboot.dr
                           )
              )
            )
        )
      }

      # Show [Restore fit] button
      show("restore_growth_model")
    }

    removeModal()

  })

  # Restore previous model fit upon click on [Restore Fit]
  observeEvent(input$restore_growth_model, {
    # store previous fit from memory
    results$growth$gcFit$gcFittedModels[[selected_vals_validate_growth$sample_validate_growth_model]] <- selected_vals_validate_growth$restore_growth_model
    dr.parameter.fit.method <- gsub(".+\\.", "", results$growth$control$dr.parameter)
    if(length(results$growth$drFit) > 1 && dr.parameter.fit.method == "model"){
      results$growth$drFit <- selected_vals_validate_growth$restore_growth_drFit
    }
    hide("restore_growth_model")
  })

  output$download_growth_validate_model <- downloadHandler(
    filename = function() {
      paste("model_fit_",  gsub(" \\| ", "_", selected_vals_validate_growth$sample_validate_growth_model), input$format_download_growth_validate_model, sep="")
    },
    content = function(file) {
      if(input$format_download_growth_validate_model == ".pdf"){
        if (requireNamespace("Cairo", quietly = TRUE)) {
          ggsave(filename = file, width = input$width_download_growth_validate_model,
                 height = input$height_download_growth_validate_model,
                 dpi = input$dpi_download_growth_validate_model,
                 device = cairo_pdf)
        } else {
          showModal(modalDialog("Package 'Cairo' must be installed to preserve special characters in the exported PDF image", easyClose = T))
          ggsave(filename = file, width = input$width_download_growth_validate_model,
                 height = input$height_download_growth_validate_model,
                 dpi = input$dpi_download_growth_validate_model,
                 device = pdf)
        }

      } else {
        ggsave(filename = file, width = input$width_download_growth_validate_model,
               height = input$height_download_growth_validate_model,
               dpi = input$dpi_download_growth_validate_model,
               device = png)
      }

    },
    contentType = ifelse(input$format_download_growth_validate_model == ".pdf", "image/pdf", "image/png")

  )

      ### Spline Fits BT ####
  # observe({
  #   if(length(results$growth$drFit) > 1 && length(results$growth$drFit$drTable) > 1){
  #     showTab(inputId = "tabsetPanel_Visualize_Growth", target = "tabPanel_Visualize_Growth_DoseResponse")
  #   } else {
  #     hideTab(inputId = "tabsetPanel_Visualize_Growth", target = "tabPanel_Visualize_Growth_DoseResponse")
  #   }
  # })

  selected_inputs_sample_validate_growth_spline_bt <- reactive({
    results <- results$growth
    if(is.null(results)) return("")
    if(("s" %in% results$control$fit.opt || "a" %in% results$control$fit.opt) && results$control$nboot.gc > 1){
      select_samples <- names(results$gcFit$gcBootSplines)
    } else {
      return("")
    }
    select_samples
  })

  observe({
    updateSelectInput(session,
                      inputId = "sample_validate_growth_spline_bt",
                      choices = selected_inputs_sample_validate_growth_spline_bt(),
                      selected = selected_vals_validate_growth$sample_validate_growth_spline_bt
    )})

  validate_growth_plot_spline_bt <- reactive({
    results <- results$growth

    # Define x- and y-axis limits
    if(any(input$y_range_min_validate_growth_spline_bt == "",
           input$y_range_max_validate_growth_spline_bt == "")){
      y.lim <- NULL
    } else {
      y.lim <- c(as.numeric(input$y_range_min_validate_growth_spline_bt),
                as.numeric(input$y_range_max_validate_growth_spline_bt))
    }

    if(any(input$y_range_min_derivative_validate_growth_spline_bt == "",
           input$y_range_max_derivative_validate_growth_spline_bt == "")){
      ylim.deriv <- NULL
    } else {
      ylim.deriv <- c(as.numeric(input$y_range_min_derivative_validate_growth_spline_bt),
                      as.numeric(input$y_range_max_derivative_validate_growth_spline_bt))
    }

    if(any(input$x_range_min_validate_growth_spline_bt == "",
           input$x_range_max_validate_growth_spline_bt == "")){
      x.lim <- NULL
    } else {
      x.lim <- c(as.numeric(input$x_range_min_validate_growth_spline_bt),
                as.numeric(input$x_range_max_validate_growth_spline_bt))
    }
    if(length(results$gcFit$gcBootSplines[[ifelse(selected_vals_validate_growth$sample_validate_growth_spline_bt == "1"||
                                                  selected_vals_validate_growth$sample_validate_growth_spline_bt == ""||
                                                  is.null(selected_vals_validate_growth$sample_validate_growth_spline_bt), 1, selected_vals_validate_growth$sample_validate_growth_spline_bt)]]) > 1){

      try(
        plot.gcBootSpline(results$gcFit$gcBootSplines[[ifelse(selected_vals_validate_growth$sample_validate_growth_spline_bt == "1"||
                                                                selected_vals_validate_growth$sample_validate_growth_spline_bt == ""||
                                                                is.null(selected_vals_validate_growth$sample_validate_growth_spline_bt), 1, selected_vals_validate_growth$sample_validate_growth_spline_bt)]],
                          pch = input$shape_type_validate_growth_spline_bt,
                          cex.point = input$shape_size_validate_growth_spline_bt,
                          cex.lab = input$axis_size_validate_growth_spline_bt,
                          cex.axis = input$lab_size_validate_growth_spline_bt,
                          lwd = input$line_width_validate_growth_spline_bt,
                          y.lim = y.lim,
                          x.lim = x.lim,
                          y.lim.deriv = ylim.deriv,
                          deriv = input$plot_derivative_growth_spline_bt,
                          combine = TRUE,
                          colSpline = input$color_validate_growth_plot_spline_bt
        )
      )
    }
  })

  output$validate_growth_plot_spline_bt <- renderPlot({
    validate_growth_plot_spline_bt()
  })

  output$download_growth_validate_spline_bt <- downloadHandler(
    filename = function() {
      paste("spline_fit_bootstrap_",  gsub(" \\| ", "_", input$sample_validate_growth_spline_bt), input$format_download_growth_validate_spline_bt, sep="")
    },
    content = function(file) {
      if(input$format_download_growth_validate_spline_bt == ".pdf"){
        if (requireNamespace("Cairo", quietly = TRUE)) {
          Cairo::CairoPDF(file = file,
                          width = input$width_download_growth_validate_spline_bt,
                          height = input$height_download_growth_validate_spline_bt)
        } else {
          showModal(modalDialog("Package 'Cairo' must be installed to preserve special characters in the exported PDF image", easyClose = T))
          grDevices::pdf(file = file,
                         width = input$width_download_growth_validate_spline_bt,
                         height = input$height_download_growth_validate_spline_bt)
        }

      } else {
        grDevices::png(file = file,
            width = input$width_download_growth_validate_spline_bt,
            height = input$height_download_growth_validate_spline_bt,
            units = "in",
            res = input$dpi_download_growth_validate_spline_bt)
      }
      # Generate plot
      results <- results$growth$gcFit$gcBootSplines[[ifelse(selected_vals_validate_growth$sample_validate_growth_spline_bt == "1" ||
                                                              is.null(selected_vals_validate_growth$sample_validate_growth_spline_bt),
                                                            1,
                                                            selected_vals_validate_growth$sample_validate_growth_spline_bt)]]

      ## Define x- and y-axis limits
      if(any(input$y_range_min_validate_growth_spline_bt == "",
             input$y_range_max_validate_growth_spline_bt == "")){
        y.lim <- NULL
      } else {
        y.lim <- c(as.numeric(input$y_range_min_validate_growth_spline_bt),
                  as.numeric(input$y_range_max_validate_growth_spline_bt))
      }

      if(any(input$y_range_min_derivative_validate_growth_spline_bt == "",
             input$y_range_max_derivative_validate_growth_spline_bt == "")){
        ylim.deriv <- NULL
      } else {
        ylim.deriv <- c(as.numeric(input$y_range_min_derivative_validate_growth_spline_bt),
                        as.numeric(input$y_range_max_derivative_validate_growth_spline_bt))
      }

      if(any(input$x_range_min_validate_growth_spline_bt == "",
             input$x_range_max_validate_growth_spline_bt == "")){
        x.lim <- NULL
      } else {
        x.lim <- c(as.numeric(input$x_range_min_validate_growth_spline_bt),
                  as.numeric(input$x_range_max_validate_growth_spline_bt))
      }

      plot.gcBootSpline(results,
                        pch = input$shape_type_validate_growth_spline_bt,
                        cex.point = input$shape_size_validate_growth_spline_bt,
                        cex.lab = input$axis_size_validate_growth_spline_bt,
                        cex.axis = input$lab_size_validate_growth_spline_bt,
                        lwd = input$line_width_validate_growth_spline_bt,
                        y.lim = y.lim,
                        x.lim = x.lim,
                        y.lim.deriv = ylim.deriv,
                        deriv = input$plot_derivative_growth_spline_bt,
                        combine = TRUE
      )

      dev.off()
    },
    contentType = ifelse(input$format_download_growth_validate_spline_bt == ".pdf", "image/pdf", "image/png")
  )
    ## Fluorescence #####
  observe({
    if(!is.null(results$fluorescence)){
      if(!("s" %in% results$fluorescence$control$fit.opt || "a" %in% results$fluorescence$control$fit.opt)){
        hideTab(inputId = "tabsetPanel_Validate_Fluorescence", target = "tabPanel_Validate_Fluorescence_Spline")
      } else {
        showTab(inputId = "tabsetPanel_Validate_Fluorescence", target = "tabPanel_Validate_Fluorescence_Spline")
      }
      if(!("l" %in% results$fluorescence$control$fit.opt || "a" %in% results$fluorescence$control$fit.opt)){
        hideTab(inputId = "tabsetPanel_Validate_Fluorescence", target = "tabPanel_Validate_Fluorescence_Linear")
      } else {
        showTab(inputId = "tabsetPanel_Validate_Fluorescence", target = "tabPanel_Validate_Fluorescence_Linear")
      }
      if(!("s" %in% results$fluorescence$control$fit.opt || "a" %in% results$fluorescence$control$fit.opt) || results$fluorescence$control$nboot.fl <=1){
        hideTab(inputId = "tabsetPanel_Validate_Fluorescence", target = "tabPanel_Validate_Fluorescence_Spline_bt")
      } else {
        showTab(inputId = "tabsetPanel_Validate_Fluorescence", target = "tabPanel_Validate_Fluorescence_Spline_bt")
      }
    }
  })
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
                                                        sample_validate_fluorescence_spline = 1,
                                                        sample_validate_fluorescence_spline_bt = 1
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
  observe({
    req(input$sample_validate_fluorescence_spline_bt)
    selected_vals_validate_fluorescence$sample_validate_fluorescence_spline_bt <- input$sample_validate_fluorescence_spline_bt
  })

      ### Linear Fits ####

  selected_inputs_validate_fluorescence_linear_sample <- reactive({
    results <- results$fluorescence
    if(is.null(results)) return("")
    if("l" %in% results$control$fit.opt || "a" %in% results$control$fit.opt){
      select_samples <- names(results$flFit$flFittedLinear)
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
    # Define x- and y-axis limits
    if(any(input$y_range_min_validate_fluorescence_plot_linear == "",
           input$y_range_max_validate_fluorescence_plot_linear == "")){
      y.lim <- NULL
    } else {
      y.lim <- c(as.numeric(input$y_range_min_validate_fluorescence_plot_linear),
                as.numeric(input$y_range_max_validate_fluorescence_plot_linear))
    }

    if(any(input$y_range_min_derivative_validate_fluorescence_plot_linear == "",
           input$y_range_max_derivative_validate_fluorescence_plot_linear == "")){
      ylim.deriv <- NULL
    } else {
      ylim.deriv <- c(as.numeric(input$y_range_min_derivative_validate_fluorescence_plot_linear),
                      as.numeric(input$y_range_max_derivative_validate_fluorescence_plot_linear))
    }

    if(any(input$x_range_min_validate_fluorescence_plot_linear == "",
           input$x_range_max_validate_fluorescence_plot_linear == "")){
      x.lim <- NULL
    } else {
      x.lim <- c(as.numeric(input$x_range_min_validate_fluorescence_plot_linear),
                as.numeric(input$x_range_max_validate_fluorescence_plot_linear))
    }
    if(length(results$flFit$flFittedLinear[[ifelse(selected_vals_validate_fluorescence$sample_validate_fluorescence_linear == "1"||
                                                   selected_vals_validate_fluorescence$sample_validate_fluorescence_linear == "" ||
                                                   is.null(selected_vals_validate_fluorescence$sample_validate_fluorescence_linear), 1, selected_vals_validate_fluorescence$sample_validate_fluorescence_linear)]]) > 1){

      try(
        suppressWarnings(
          plot.flFitLinear(results$flFit$flFittedLinear[[ifelse(selected_vals_validate_fluorescence$sample_validate_fluorescence_linear == "1" || is.null(selected_vals_validate_fluorescence$sample_validate_fluorescence_linear), 1, selected_vals_validate_fluorescence$sample_validate_fluorescence_linear)]],
                           log = logy_validate_fluorescence_plot_linear(),
                           pch = input$shape_type_validate_fluorescence_plot_linear,
                           cex.point = input$shape_size_validate_fluorescence_plot_linear,
                           cex.lab = input$axis_size_validate_fluorescence_plot_linear,
                           cex.axis = input$lab_size_validate_fluorescence_plot_linear,
                           lwd = input$line_width_validate_fluorescence_plot_linear,
                           y.lim = y.lim,
                           x.lim = x.lim,
                           color = input$color_validate_fluorescence_plot_linear
                           # ADD FURTHER INPUT (see Notion)
          )
        )
      )
      if(input$diagnostics_validate_fluorescence_plot_linear){
        try(
          suppressWarnings(
            plot.flFitLinear(results$flFit$flFittedLinear[[ifelse(selected_vals_validate_fluorescence$sample_validate_fluorescence_linear == "1" ||
                                                                    is.null(selected_vals_validate_fluorescence$sample_validate_fluorescence_linear),
                                                                  1,
                                                                  selected_vals_validate_fluorescence$sample_validate_fluorescence_linear)]],
                             which = "fit_diagnostics",
                             log = logy_validate_fluorescence_plot_linear(),
                             pch = input$shape_type_validate_fluorescence_plot_linear,
                             cex.point = input$shape_size_validate_fluorescence_plot_linear,
                             cex.lab = input$axis_size_validate_fluorescence_plot_linear,
                             cex.axis = input$lab_size_validate_fluorescence_plot_linear,
                             lwd = input$line_width_validate_fluorescence_plot_linear,
                             y.lim = y.lim,
                             x.lim = x.lim,
                             color = input$color_validate_fluorescence_plot_linear
                             # ADD FURTHER INPUT (see Notion)
            )
          )
        )
      }
    }
  })

  # lin.rerun.param <- reactiveValues()

  observeEvent(input$rerun_fluorescence_linear, {
    # display a modal dialog with a header, textinput and action buttons
    if(results$fluorescence$flFit$flFittedLinear[[ifelse(
      selected_vals_validate_fluorescence$sample_validate_fluorescence_linear == "1" ||
      is.null(
        selected_vals_validate_fluorescence$sample_validate_fluorescence_linear
      ),
      1,
      selected_vals_validate_fluorescence$sample_validate_fluorescence_linear
    )]]$control$x_type == "time"){
      showModal(
        modalDialog(
          tags$h2('Please enter adjusted parameters'),
          textInput('t0.lin.rerun.fluorescence', 'Minimum time (t0)', placeholder = paste0("previously: ", results$fluorescence$flFit$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]$control$t0)),
          textInput('tmax.lin.rerun.fluorescence', 'Maximum time (tmax)', placeholder = paste0("previously: ", results$fluorescence$flFit$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]$control$tmax)),
          textAreaInput('quota.rerun.fluorescence', 'Quota', placeholder = HTML(paste0("previously: ", results$fluorescence$flFit$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]$quota,
                                                                                       "\n",
                                                                                       "include regression windows with slope = ", expression(µ[max]), " * quota into the final linear fit."))),
          textInput('lin.h.rerun.fluorescence', 'Sliding window size (h)', placeholder = paste0("previously: ",
                                                                                                ifelse(!is.null(results$fluorescence$flFit$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]$control$lin.h),
                                                                                                       results$fluorescence$flFit$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]$control$lin.h,
                                                                                                       "NULL"))),
          textInput('lin.R2.rerun.fluorescence', 'R2 threshold', placeholder = paste0("previously: ", results$fluorescence$flFit$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]$control$lin.R2)),
          textInput('lin.RSD.rerun.fluorescence', 'RSD threshold for slope', placeholder = paste0("previously: ", results$fluorescence$flFit$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]$control$lin.RSD)),
          footer=tagList(
            fluidRow(
              column(12,
                     div(
                       actionButton(inputId = "submit.rerun.linear.fluorescence",
                                    label = "Submit"),
                       style="float:right"),
                     div(
                       modalButton('cancel'),
                       style="float:right"),
                     div(
                       actionButton(inputId = "tooltip_flFitLinear_validate",
                                    label = "",
                                    icon=icon("question"),
                                    style="padding:2px; font-size:100%"),
                       style="float:left")

              )
            )
          )
        )
      )
    } else {
      showModal(
        modalDialog(
          tags$h2('Please enter adjusted parameters'),
          textInput('min.growth.lin.rerun.fluorescence', 'Minimum growth measurement', placeholder = paste0("previously: ", results$fluorescence$flFit$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]$control$min.growth)),
          textInput('max.growth.lin.rerun.fluorescence', 'Maximum growth measurement', placeholder = paste0("previously: ", results$fluorescence$flFit$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]$control$max.growth)),
          textAreaInput('quota.rerun.fluorescence', 'Quota', placeholder = HTML(paste0("previously: ", results$fluorescence$flFit$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]$quota,
                                                                                       "\n",
                                                                                       "include regression windows with slope = ", expression(µ[max]), " * quota into the final linear fit."))),
          textInput('lin.h.rerun.fluorescence', 'Sliding window size (h)', placeholder = paste0("previously: ",
                                                                                                ifelse(!is.null(results$fluorescence$flFit$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]$control$lin.h),
                                                                                                       results$fluorescence$flFit$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]$control$lin.h,
                                                                                                       "NULL"))),
          textInput('lin.R2.rerun.fluorescence', 'R2 threshold', placeholder = paste0("previously: ", results$fluorescence$flFit$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]$control$lin.R2)),
          textInput('lin.RSD.rerun.fluorescence', 'RSD threshold for slope', placeholder = paste0("previously: ", results$fluorescence$flFit$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]$control$lin.RSD)),
          footer=tagList(
            fluidRow(
              column(12,
                     div(
                       actionButton(inputId = "submit.rerun.linear.fluorescence",
                                    label = "Submit"),
                       style="float:right"),
                     div(
                       modalButton('cancel'),
                       style="float:right"),
                     div(
                       actionButton(inputId = "tooltip_flFitLinear_validate",
                                    label = "",
                                    icon=icon("question"),
                                    style="padding:2px; font-size:100%"),
                       style="float:left")

              )
            )
          )
        )
      )
    }

  })

  # Re-run selected linear fit with user-defined parameters upon click on 'submit'
  observeEvent(input$submit.rerun.linear.fluorescence, {
    if(!is.null(results$fluorescence$flFit)){

      showModal(modalDialog("Fitting sample data...", footer = NULL))
      # store previous fit in memory
      selected_vals_validate_fluorescence$restore_fluorescence_linear <- results$fluorescence$flFit$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]

      # Re-run fit and store in results object
      actwell <- results$fluorescence$flFit$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]$raw.fl
      acttime <- results$fluorescence$flFit$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]$raw.x
      control <- results$fluorescence$flFit$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]$control
      control_new <- control
      ID <- results$fluorescence$flFit$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]]$ID

      lin.h.new <- ifelse(!is.na(as.numeric(input$lin.h.rerun.fluorescence)), as.numeric(input$lin.h.rerun.fluorescence), control$lin.h)
      if(!is.na(lin.h.new)) control_new$lin.h <- lin.h.new
      control_new$lin.R2 <- ifelse(!is.na(as.numeric(input$lin.R2.rerun.fluorescence)), as.numeric(input$lin.R2.rerun.fluorescence), control$lin.R2)
      control_new$lin.RSD <- ifelse(!is.na(as.numeric(input$lin.RSD.rerun.fluorescence)), as.numeric(input$lin.RSD.rerun.fluorescence), control$lin.RSD)
      control_new$t0 <- ifelse(!is.na(input$t0.lin.rerun.fluorescence) && !is.null(input$t0.lin.rerun.fluorescence) && input$t0.lin.rerun.fluorescence != "",
                               as.numeric(input$t0.lin.rerun.fluorescence),
                               control$t0)
      control_new$tmax <- ifelse(!is.na(input$tmax.lin.rerun.fluorescence) && !is.null(input$tmax.lin.rerun.fluorescence) && input$tmax.lin.rerun.fluorescence != "",
                                 as.numeric(input$tmax.lin.rerun.fluorescence),
                                 control$tmax)

      min.growth.lin.new <- ifelse(!is.na(input$min.growth.lin.rerun.fluorescence) && !is.null(input$min.growth.lin.rerun.fluorescence) && input$min.growth.lin.rerun.fluorescence != "",
                                    as.numeric(input$min.growth.lin.rerun.fluorescence),
                                    control$min.growth)
      if(is.numeric(min.growth.lin.new)){
        if(!is.na(min.growth.lin.new) && all(as.vector(actwell) < min.growth.lin.new)){
          showModal(
            modalDialog(paste0("Start growth values need to be greater than 'min.growth'.\nThe minimum start value in your dataset is: ",
                         min(as.vector(actwell)),". 'min.growth' was not adjusted."), easyClose = T)
          )
        } else if(!is.na(min.growth.lin.new)){
          control_new$min.growth <- min.growth.lin.new
        }
      }
      control_new$max.growth <-  ifelse(!is.na(as.numeric(input$max.growth.lin.rerun.fluorescence)), as.numeric(input$max.growth.lin.rerun.fluorescence), control$max.growth)
      quota_new <- ifelse(!is.na(as.numeric(input$quota.rerun.fluorescence)), as.numeric(input$quota.rerun.fluorescence), 0.95)

      if(control$x_type == "time"){
        try(
          results$fluorescence$flFit$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]] <-
            flFitLinear(time = acttime, fl_data = actwell,
                        ID = ID,
                        control = control_new,
                        quota = quota_new)
        )
      } else {
        try(
          results$fluorescence$flFit$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]] <-
            flFitLinear(growth = acttime, fl_data = actwell,
                        ID = ID,
                        control = control_new,
                        quota = quota_new)
        )
      }

      # Update gcTable with new results
      res.table.fl <- results$fluorescence$flFit$flTable
      fit.summary <- QurvE:::summary.flFitLinear(results$fluorescence$flFit$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]])

      sample.ndx <- ifelse(is.numeric(selected_vals_validate_fluorescence$sample_validate_fluorescence_linear),
                           selected_vals_validate_fluorescence$sample_validate_fluorescence_linear,
                           match(selected_vals_validate_fluorescence$sample_validate_fluorescence_linear, names(results$fluorescence$flFit$flFittedLinear)))
      results$fluorescence$flFit$flTable[sample.ndx, colnames(res.table.fl) %in% colnames(fit.summary)] <- fit.summary

      # Re-run drFit if respective fit type (linear/spline) was chosen as dr.parameter
      dr.parameter.fit.method <- gsub(".+\\.", "", results$fluorescence$control$dr.parameter)

      if(length(results$fluorescence$drFit) > 1 && dr.parameter.fit.method == "linfit"){
        # Store previous drFit in memory
        selected_vals_validate_fluorescence$restore_fluorescence_drFit <- results$fluorescence$drFit

        # Re-run drFit
        if (is.null(input$smoothing_factor_fluorescence_dr) || is.na(input$smoothing_factor_fluorescence_dr) || input$smoothing_factor_fluorescence_dr == "NULL" || input$smoothing_factor_fluorescence_dr == "") {
          smooth.dr = NULL
        } else {
          smooth.dr <- as.numeric(input$smoothing_factor_fluorescence_dr)
        }
        if (is.null(input$number_of_bootstrappings_dr_fluorescence) || is.na(input$number_of_bootstrappings_dr_fluorescence) || input$number_of_bootstrappings_dr_fluorescence == "NULL" || input$number_of_bootstrappings_dr_fluorescence == "") {
          nboot.dr <- 0
        } else {
          nboot.dr <- as.numeric(input$number_of_bootstrappings_dr_fluorescence)
        }
        showModal(modalDialog("Re-calculating dose-response analysis...", footer = NULL))

        try(
          results$fluorescence$drFit <-
            suppressWarnings(
              fl.drFit(flTable = results$fluorescence$flFit$flTable,
                       control = fl.control(dr.method = input$dr_method_fluorescence,
                                            dr.parameter = results$fluorescence$control$dr.parameter,
                                            smooth.dr = smooth.dr,
                                            log.x.dr = input$log_transform_concentration_fluorescence,
                                            log.y.dr = input$log_transform_response_fluorescence,
                                            nboot.dr = nboot.dr
                       )
              )
            )
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
    results$fluorescence$flFit$flFittedLinear[[selected_vals_validate_fluorescence$sample_validate_fluorescence_linear]] <- selected_vals_validate_fluorescence$restore_fluorescence_linear
    dr.parameter.fit.method <- gsub(".+\\.", "", results$fluorescence$control$dr.parameter)
    if(length(results$fluorescence$drFit) > 1 && dr.parameter.fit.method == "linfit"){
      results$fluorescence$drFit <- selected_vals_validate_fluorescence$restore_fluorescence_drFit
    }
    hide("restore_fluorescence_linear")
  })

  output$download_fluorescence_validate_linear <- downloadHandler(
    filename = function() {
      paste("linear_fit_",  gsub(" \\| ", "_", selected_vals_validate_fluorescence$sample_validate_fluorescence_linear), input$format_download_fluorescence_validate_linear, sep="")
    },
    content = function(file) {
      if(input$format_download_fluorescence_validate_linear == ".pdf"){
        if (requireNamespace("Cairo", quietly = TRUE)) {
          Cairo::CairoPDF(file = file,
                          width = input$width_download_fluorescence_validate_linear,
                          height = input$height_download_fluorescence_validate_linear)
        } else {
          showModal(modalDialog("Package 'Cairo' must be installed to preserve special characters in the exported PDF image", easyClose = T))
          grDevices::pdf(file = file,
                         width = input$width_download_fluorescence_validate_linear,
                         height = input$height_download_fluorescence_validate_linear)
        }

      } else {
        grDevices::png(file = file,
            width = input$width_download_fluorescence_validate_linear,
            height = input$height_download_fluorescence_validate_linear,
            units = "in",
            res = input$dpi_download_fluorescence_validate_linear)
      }
      if(input$logy_validate_fluorescence_plot_linear) log <- "y"
      else  log <- ""
      results <- results$fluorescence
      # Define x- and y-axis limits
      if(any(input$y_range_min_validate_fluorescence_plot_linear == "",
             input$y_range_max_validate_fluorescence_plot_linear == "")){
        y.lim <- NULL
      } else {
        y.lim <- c(as.numeric(input$y_range_min_validate_fluorescence_plot_linear),
                  as.numeric(input$y_range_max_validate_fluorescence_plot_linear))
      }

      if(any(input$y_range_min_derivative_validate_fluorescence_plot_linear == "",
             input$y_range_max_derivative_validate_fluorescence_plot_linear == "")){
        ylim.deriv <- NULL
      } else {
        ylim.deriv <- c(as.numeric(input$y_range_min_derivative_validate_fluorescence_plot_linear),
                        as.numeric(input$y_range_max_derivative_validate_fluorescence_plot_linear))
      }

      if(any(input$x_range_min_validate_fluorescence_plot_linear == "",
             input$x_range_max_validate_fluorescence_plot_linear == "")){
        x.lim <- NULL
      } else {
        x.lim <- c(as.numeric(input$x_range_min_validate_fluorescence_plot_linear),
                  as.numeric(input$x_range_max_validate_fluorescence_plot_linear))
      }
      if(length(results$flFit$flFittedLinear[[ifelse(selected_vals_validate_fluorescence$sample_validate_fluorescence_linear == "1" ||
                                                     selected_vals_validate_fluorescence$sample_validate_fluorescence_linear == "" ||
                                                     is.null(selected_vals_validate_fluorescence$sample_validate_fluorescence_linear),
                                                     1,
                                                     selected_vals_validate_fluorescence$sample_validate_fluorescence_linear)]]) > 1){

        suppressWarnings(
          plot.flFitLinear(results$flFit$flFittedLinear[[ifelse(selected_vals_validate_fluorescence$sample_validate_fluorescence_linear == "1" || is.null(selected_vals_validate_fluorescence$sample_validate_fluorescence_linear), 1, selected_vals_validate_fluorescence$sample_validate_fluorescence_linear)]],
                           log = log,
                           pch = input$shape_type_validate_fluorescence_plot_linear,
                           cex.point = input$shape_size_validate_fluorescence_plot_linear,
                           cex.lab = input$axis_size_validate_fluorescence_plot_linear,
                           cex.axis = input$lab_size_validate_fluorescence_plot_linear,
                           lwd = input$line_width_validate_fluorescence_plot_linear,
                           y.lim = y.lim,
                           x.lim = x.lim
                           # ADD FURTHER INPUT (see Notion)
          )
        )
        if(input$diagnostics_validate_fluorescence_plot_linear){
          suppressWarnings(
            plot.flFitLinear(results$flFit$flFittedLinear[[ifelse(selected_vals_validate_fluorescence$sample_validate_fluorescence_linear == "1"||
                                                                    selected_vals_validate_fluorescence$sample_validate_fluorescence_linear == ""||
                                                                    is.null(selected_vals_validate_fluorescence$sample_validate_fluorescence_linear), 1, selected_vals_validate_fluorescence$sample_validate_fluorescence_linear)]],
                             which = "fit_diagnostics",
                             log = log,
                             pch = input$shape_type_validate_fluorescence_plot_linear,
                             cex.point = input$shape_size_validate_fluorescence_plot_linear,
                             cex.lab = input$axis_size_validate_fluorescence_plot_linear,
                             cex.axis = input$lab_size_validate_fluorescence_plot_linear,
                             lwd = input$line_width_validate_fluorescence_plot_linear,
                             y.lim = y.lim,
                             x.lim = x.lim
                             # ADD FURTHER INPUT (see Notion)
            )
          )
        }
      }
      dev.off()
    },
    contentType = ifelse(input$format_download_fluorescence_validate_linear == ".pdf", "image/pdf", "image/png")

  )

      ### Spline Fits ####
  selected_inputs_validate_fluorescence_spline_sample <- reactive({
    results <- results$fluorescence
    if(is.null(results)) return("")
    if("s" %in% results$control$fit.opt || "a" %in% results$control$fit.opt){
      select_samples <- names(results$flFit$flFittedSplines)
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
    if(length(results$flFit$flFittedSplines[[ifelse(selected_vals_validate_fluorescence$sample_validate_fluorescence_spline == "1"||
                                                    selected_vals_validate_fluorescence$sample_validate_fluorescence_spline == "" ||
                                                    is.null(selected_vals_validate_fluorescence$sample_validate_fluorescence_spline),
                                                    1,
                                                    selected_vals_validate_fluorescence$sample_validate_fluorescence_spline)]]) > 1){
      showModal(modalDialog("Creating plot...", footer=NULL))
      try(
        suppressWarnings(
          plot.flFitSpline(results$flFit$flFittedSplines[[ifelse(selected_vals_validate_fluorescence$sample_validate_fluorescence_spline == "1" ||
                                                                   is.null(selected_vals_validate_fluorescence$sample_validate_fluorescence_spline),
                                                                 1,
                                                                 input$sample_validate_fluorescence_spline)]],
                           log.y = input$logy_validate_fluorescence_plot_spline,
                           x.lim = c(input$x_range_min_validate_fluorescence_plot_spline, input$x_range_max_validate_fluorescence_plot_spline),
                           y.lim = c(input$y_range_min_validate_fluorescence_plot_spline,input$y_range_max_validate_fluorescence_plot_spline),
                           y.lim.deriv = c(input$y_range_min_derivative_validate_fluorescence_plot_spline, input$y_range_max_derivative_validate_fluorescence_plot_spline),
                           lwd = input$line_width_validate_fluorescence_plot_spline,
                           basesize = input$base_size_validate_fluorescence_plot_spline,
                           cex.point = input$shape_size_validate_fluorescence_plot_spline,
                           n.ybreaks = input$nbreaks__validate_fluorescence_plot_spline,
                           deriv = input$plot_derivative_validate_fluorescence_plot_spline,
                           pch = input$shape_type_validate_fluorescence_plot_spline,
                           colSpline = input$color_validate_fluorescence_plot_spline
          )
        )
      )
      removeModal()
    }
  })

  spline.rerun.param.fluorescence <- reactiveValues()

  observeEvent(input$rerun_fluorescence_spline, {
    # display a modal dialog with a header, textinput and action buttons
    if (results$fluorescence$flFit$flFittedSplines[[selected_vals_validate_fluorescence$sample_validate_fluorescence_spline]]$control$x_type == "time") {
      showModal(
        modalDialog(
          tags$h2('Please enter adjusted parameters'),
          textInput('t0.spline.rerun.fluorescence', 'Minimum time (t0)', placeholder = paste0("previously: ", results$fluorescence$flFit$flFittedSplines[[selected_vals_validate_fluorescence$sample_validate_fluorescence_spline]]$control$t0)),
          textInput('tmax.spline.rerun.fluorescence', 'Maximum time (tmax)', placeholder = paste0("previously: ", results$fluorescence$flFit$flFittedSplines[[selected_vals_validate_fluorescence$sample_validate_fluorescence_spline]]$control$tmax)),
          textInput('smooth.fl.rerun.fluorescence', 'Smoothing factor', placeholder = paste0("previously: ", results$fluorescence$flFit$flFittedSplines[[selected_vals_validate_fluorescence$sample_validate_fluorescence_spline]]$control$smooth.fl)),
          footer=tagList(
            fluidRow(
              column(12,
                     div(
                       actionButton('submit.rerun.spline.fluorescence', 'Submit'),
                       style="float:right"),
                     div(
                       modalButton('cancel'),
                       style="float:right"),
                     div(
                       actionButton(inputId = "tooltip_flFitSpline_validate",
                                    label = "",
                                    icon=icon("question"),
                                    style="padding:2px; font-size:100%"),
                       style="float:left")

              )
            )
          )
        )
      )
    } else {
      showModal(
        modalDialog(
          tags$h2('Please enter adjusted parameters'),
          textInput('min.growth.spline.rerun.fluorescence', 'Minimum growth measurement', placeholder = paste0("previously: ", results$fluorescence$flFit$flFittedSplines[[selected_vals_validate_fluorescence$sample_validate_fluorescence_spline]]$control$min.growth)),
          textInput('max.growth.spline.rerun.fluorescence', 'Maximum growth measurement', placeholder = paste0("previously: ", results$fluorescence$flFit$flFittedSplines[[selected_vals_validate_fluorescence$sample_validate_fluorescence_spline]]$control$max.growth)),
          textInput('smooth.fl.rerun.fluorescence', 'Smoothing factor', placeholder = paste0("previously: ", results$fluorescence$flFit$flFittedSplines[[selected_vals_validate_fluorescence$sample_validate_fluorescence_spline]]$control$smooth.fl)),
          footer=tagList(
            fluidRow(
              column(12,
                     div(
                       actionButton('submit.rerun.spline.fluorescence', 'Submit'),
                       style="float:right"),
                     div(
                       modalButton('cancel'),
                       style="float:right"),
                     div(
                       actionButton(inputId = "tooltip_flFitSpline_validate",
                                    label = "",
                                    icon=icon("question"),
                                    style="padding:2px; font-size:100%"),
                       style="float:left")

              )
            )
          )
        )
      )
    }
  })

  # Re-run selected spline fit with user-defined parameters upon click on 'submit'
  observeEvent(input$submit.rerun.spline.fluorescence, {
    if(!is.null(results$fluorescence$flFit)){

      showModal(modalDialog("Fitting sample data...", footer = NULL))
      # store previous fit in memory
      selected_vals_validate_fluorescence$restore_fluorescence_spline <- results$fluorescence$flFit$flFittedSplines[[selected_vals_validate_fluorescence$sample_validate_fluorescence_spline]]

      # Re-run fit and store in results object
      actwell <- results$fluorescence$flFit$flFittedSplines[[selected_vals_validate_fluorescence$sample_validate_fluorescence_spline]]$fl.in
      acttime <- results$fluorescence$flFit$flFittedSplines[[selected_vals_validate_fluorescence$sample_validate_fluorescence_spline]]$x.in
      control <- results$fluorescence$flFit$flFittedSplines[[selected_vals_validate_fluorescence$sample_validate_fluorescence_spline]]$control
      control_new <- control
      ID <- results$fluorescence$flFit$flFittedSplines[[selected_vals_validate_fluorescence$sample_validate_fluorescence_spline]]$ID

      control_new$smooth.fl <- ifelse(!is.na(as.numeric(input$smooth.fl.rerun.fluorescence)), as.numeric(input$smooth.fl.rerun.fluorescence), control$smooth.fl)
      control_new$t0 <- ifelse(!is.na(input$t0.spline.rerun.fluorescence) && !is.null(input$t0.spline.rerun.fluorescence) && input$t0.spline.rerun.fluorescence != "",
                                as.numeric(input$t0.spline.rerun.fluorescence),
                                control$t0)
      control_new$tmax <- ifelse(!is.na(input$tmax.spline.rerun.fluorescence) && !is.null(input$tmax.spline.rerun.fluorescence) && input$tmax.spline.rerun.fluorescence != "",
                                 as.numeric(input$tmax.spline.rerun.fluorescence),
                                 control$tmax)
      min.growth.spline.new <- ifelse(!is.na(as.numeric(input$min.growth.spline.rerun.fluorescence)) && !is.null(input$min.growth.spline.rerun.fluorescence),
                                       as.numeric(input$min.growth.spline.rerun.fluorescence),
                                       control$min.growth)
      control_new$max.growth <-  ifelse(!is.na(as.numeric(input$max.growth.spline.rerun.fluorescence)) && !is.null(input$max.growth.spline.rerun.fluorescence),
                                         as.numeric(input$max.growth.spline.rerun.fluorescence),
                                         control$max.growth)
      if(is.numeric(min.growth.spline.new)){
        if(!is.na(min.growth.spline.new) && all(as.vector(actwell) < min.growth.spline.new)){
          message(paste0("Start growth values need to be greater than 'min.growth'.\nThe minimum start value in your dataset is: ",
                         min(as.vector(actwell)),". 'min.growth' was not adjusted."), call. = FALSE)
        } else if(!is.na(min.growth.spline.new)){
          control_new$min.growth <- min.growth.spline.new
        }
      }
      if(control$x_type == "time"){
        try(
          results$fluorescence$flFit$flFittedSplines[[selected_vals_validate_fluorescence$sample_validate_fluorescence_spline]] <-
            flFitSpline(time = acttime, fl_data = actwell,
                        ID = ID,
                        control = control_new)
        )
      } else {
        try(
          results$fluorescence$flFit$flFittedSplines[[selected_vals_validate_fluorescence$sample_validate_fluorescence_spline]] <-
            flFitSpline(growth = acttime, fl_data = actwell,
                        ID = ID,
                        control = control_new)
        )
      }
      # Update gcTable with new results
      res.table.fl <- results$fluorescence$flFit$flTable
      fit.summary <- QurvE:::summary.flFitSpline(results$fluorescence$flFit$flFittedSplines[[selected_vals_validate_fluorescence$sample_validate_fluorescence_spline]])

      sample.ndx <- ifelse(is.numeric(selected_vals_validate_fluorescence$sample_validate_fluorescence_spline),
                           selected_vals_validate_fluorescence$sample_validate_fluorescence_spline,
                           match(selected_vals_validate_fluorescence$sample_validate_fluorescence_spline, names(results$fluorescence$flFit$flFittedSplines)))
      results$fluorescence$flFit$flTable[sample.ndx, colnames(res.table.fl) %in% colnames(fit.summary)] <- fit.summary

      # Re-run drFit if respective fit type (linear/spline) was chosen as dr.parameter
      dr.parameter.fit.method <- gsub(".+\\.", "", results$fluorescence$control$dr.parameter)

      if(length(results$fluorescence$drFit) > 1 && dr.parameter.fit.method == "spline"){
        # Store previous drFit in memory
        selected_vals_validate_fluorescence$restore_fluorescence_drFit <- results$fluorescence$drFit

        # Re-run drFit
        if (is.null(input$smoothing_factor_fluorescence_dr) || is.na(input$smoothing_factor_fluorescence_dr) || input$smoothing_factor_fluorescence_dr == "NULL" || input$smoothing_factor_fluorescence_dr == "") {
          smooth.dr = NULL
        } else {
          smooth.dr <- as.numeric(input$smoothing_factor_fluorescence_dr)
        }
        if (is.null(input$number_of_bootstrappings_dr_fluorescence) || is.na(input$number_of_bootstrappings_dr_fluorescence) || input$number_of_bootstrappings_dr_fluorescence == "NULL" || input$number_of_bootstrappings_dr_fluorescence == "") {
          nboot.dr <- 0
        } else {
          nboot.dr <- as.numeric(input$number_of_bootstrappings_dr_fluorescence)
        }
        showModal(modalDialog("Re-calculating dose-response analysis...", footer = NULL))

        try(
          results$fluorescence$drFit <-
            suppressWarnings(
              fl.drFit(flTable = results$fluorescence$flFit$flTable,
                       control = fl.control(dr.method = input$dr_method_fluorescence,
                                            dr.parameter = results$fluorescence$control$dr.parameter,
                                            smooth.dr = smooth.dr,
                                            log.x.dr = input$log_transform_concentration_fluorescence,
                                            log.y.dr = input$log_transform_response_fluorescence,
                                            nboot.dr = nboot.dr
                       )
              )
            )
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
    results$fluorescence$flFit$flFittedSplines[[selected_vals_validate_fluorescence$sample_validate_fluorescence_spline]] <- selected_vals_validate_fluorescence$restore_fluorescence_spline
    dr.parameter.fit.method <- gsub(".+\\.", "", results$fluorescence$control$dr.parameter)
    if(length(results$fluorescence$drFit) > 1 && dr.parameter.fit.method == "spline"){
      results$fluorescence$drFit <- selected_vals_validate_fluorescence$restore_fluorescence_drFit
    }
    hide("restore_fluorescence_spline")
  })

  output$download_fluorescence_validate_spline <- downloadHandler(
    filename = function() {
      paste("spline_fit_",  gsub(" \\| ", "_", selected_vals_validate_fluorescence$sample_validate_fluorescence_spline), input$format_download_fluorescence_validate_spline, sep="")
    },
    content = function(file) {
      if(input$format_download_fluorescence_validate_spline == ".pdf"){
        if (requireNamespace("Cairo", quietly = TRUE)) {
          ggsave(filename = file, width = input$width_download_fluorescence_validate_spline,
                 height = input$height_download_fluorescence_validate_spline,
                 dpi = input$dpi_download_fluorescence_validate_spline,
                 device = cairo_pdf)
        } else {
          showModal(modalDialog("Package 'Cairo' must be installed to preserve special characters in the exported PDF image", easyClose = T))
          ggsave(filename = file, width = input$width_download_fluorescence_validate_spline,
                 height = input$height_download_fluorescence_validate_spline,
                 dpi = input$dpi_download_fluorescence_validate_spline,
                 device = pdf)
        }

      } else {
      ggsave(filename = file, width = input$width_download_fluorescence_validate_spline,
             height = input$height_download_fluorescence_validate_spline,
             dpi = input$dpi_download_fluorescence_validate_spline,
             device = png)
      }
    },
    contentType = ifelse(input$format_download_fluorescence_validate_spline == ".pdf", "image/pdf", "image/png")

  )

  output$download_fluorescence_validate_spline_values <- downloadHandler(
    filename = function() {
      paste("values_spline_fit_",  gsub(" \\| ", "_", selected_vals_validate_fluorescence$sample_validate_fluorescence_spline), ".csv", sep="")
    },
    content = function(file) {
      results <- results$fluorescence
      if(length(results$flFit$flFittedSplines[[ifelse(selected_vals_validate_fluorescence$sample_validate_fluorescence_spline == "1" ||
                                                      is.null(selected_vals_validate_fluorescence$sample_validate_fluorescence_spline),
                                                      1,
                                                      input$sample_validate_fluorescence_spline)]]) > 1) {
        try(p <-
              suppressWarnings(
                plot.flFitSpline(
                  results$flFit$flFittedSplines[[ifelse(selected_vals_validate_fluorescence$sample_validate_fluorescence_spline == "1" ||
                                                          is.null(selected_vals_validate_fluorescence$sample_validate_fluorescence_spline),
                                                        1,
                                                        input$sample_validate_fluorescence_spline)]],
                  export = FALSE,
                  plot = FALSE,
                  log.y = FALSE,
                  deriv = F
                )
              ))
      }
      table <- data.frame("x" = as.numeric(p$data$fit.x), "y" = as.numeric(p$data$fit.fl))
      QurvE:::write.csv.utf8.BOM(table, file)
    },
    contentType = ".csv"
  )

  output$download_fluorescence_validate_spline_deriv_values <- downloadHandler(
    filename = function() {
      paste("values_spline_fit_derivative_",  gsub(" \\| ", "_", selected_vals_validate_fluorescence$sample_validate_fluorescence_spline), ".csv", sep="")
    },
    content = function(file) {
      results <- results$fluorescence
      if (length(results$flFit$flFittedSplines[[ifelse(selected_vals_validate_fluorescence$sample_validate_fluorescence_spline == "1" ||
                                                       is.null(selected_vals_validate_fluorescence$sample_validate_fluorescence_spline),
                                                       1,
                                                       input$sample_validate_fluorescence_spline)]]) > 1) {
        table <-  as.data.frame(results$flFit$flFittedSplines[[ifelse(selected_vals_validate_fluorescence$sample_validate_fluorescence_spline == "1" ||
                                                                        is.null(selected_vals_validate_fluorescence$sample_validate_fluorescence_spline),
                                                                      1,
                                                                      input$sample_validate_fluorescence_spline)]]$spline.deriv1)
      }
      QurvE:::write.csv.utf8.BOM(table, file)
    },
    contentType = ".csv"
  )

      ### Spline Fits BT ####
  selected_inputs_sample_validate_fluorescence_spline_bt <- reactive({
    results <- results$fluorescence
    if(is.null(results)) return("")
    if(("s" %in% results$control$fit.opt || "a" %in% results$control$fit.opt) && results$control$nboot.fl > 1){
      select_samples <- names(results$flFit$flBootSplines)
    } else {
      return("")
    }
    select_samples
  })

  observe({
    updateSelectInput(session,
                      inputId = "sample_validate_fluorescence_spline_bt",
                      choices = selected_inputs_sample_validate_fluorescence_spline_bt(),
                      selected = selected_vals_validate_fluorescence$sample_validate_fluorescence_spline_bt
    )})

  validate_fluorescence_plot_spline_bt <- reactive({
    results <- results$fluorescence$flFit$flBootSplines[[ifelse(selected_vals_validate_fluorescence$sample_validate_fluorescence_spline_bt == "1"||
                                                                   selected_vals_validate_fluorescence$sample_validate_fluorescence_spline_bt == ""  ||
                                                                   is.null(selected_vals_validate_fluorescence$sample_validate_fluorescence_spline_bt),
                                                                 1,
                                                                 selected_vals_validate_fluorescence$sample_validate_fluorescence_spline_bt)]]


    # Define x- and y-axis limits
    if(any(input$y_range_min_validate_fluorescence_spline_bt == "",
           input$y_range_max_validate_fluorescence_spline_bt == "")){
      y.lim <- NULL
    } else {
      y.lim <- c(as.numeric(input$y_range_min_validate_fluorescence_spline_bt),
                as.numeric(input$y_range_max_validate_fluorescence_spline_bt))
    }

    if(any(input$y_range_min_derivative_validate_fluorescence_spline_bt == "",
           input$y_range_max_derivative_validate_fluorescence_spline_bt == "")){
      ylim.deriv <- NULL
    } else {
      ylim.deriv <- c(as.numeric(input$y_range_min_derivative_validate_fluorescence_spline_bt),
                      as.numeric(input$y_range_max_derivative_validate_fluorescence_spline_bt))
    }

    if(any(input$x_range_min_validate_fluorescence_spline_bt == "",
           input$x_range_max_validate_fluorescence_spline_bt == "")){
      x.lim <- NULL
    } else {
      x.lim <- c(as.numeric(input$x_range_min_validate_fluorescence_spline_bt),
                 as.numeric(input$x_range_max_validate_fluorescence_spline_bt))
    }
    try(
      plot.flBootSpline(results,
                        pch = input$shape_type_validate_fluorescence_spline_bt,
                        cex.point = input$shape_size_validate_fluorescence_spline_bt,
                        cex.lab = input$axis_size_validate_fluorescence_spline_bt,
                        cex.axis = input$lab_size_validate_fluorescence_spline_bt,
                        lwd = input$line_width_validate_fluorescence_spline_bt,
                        y.lim = y.lim,
                        x.lim = x.lim,
                        y.lim.deriv = ylim.deriv,
                        deriv = input$plot_derivative_fluorescence_spline_bt,
                        combine = T,
                        colSpline = input$color_validate_fluorescence_spline_bt
      )
    )
  })

  output$validate_fluorescence_plot_spline_bt <- renderPlot({
    validate_fluorescence_plot_spline_bt()
  })

  output$download_fluorescence_validate_spline_bt <- downloadHandler(
    filename = function() {
      paste("spline_fit_bootstrap_",  gsub(" \\| ", "_", input$sample_validate_fluorescence_spline_bt), input$format_download_fluorescence_validate_spline_bt, sep="")
    },
    content = function(file) {
      if(input$format_download_fluorescence_validate_spline_bt == ".pdf"){
        if (requireNamespace("Cairo", quietly = TRUE)) {
          Cairo::CairoPDF(file = file,
                          width = input$width_download_fluorescence_validate_spline_bt,
                          height = input$height_download_fluorescence_validate_spline_bt)
        } else {
          showModal(modalDialog("Package 'Cairo' must be installed to preserve special characters in the exported PDF image", easyClose = T))
          grDevices::pdf(file = file,
                         width = input$width_download_fluorescence_validate_spline_bt,
                         height = input$height_download_fluorescence_validate_spline_bt)
        }

      } else {
        grDevices::png(file = file,
            width = input$width_download_fluorescence_validate_spline_bt,
            height = input$height_download_fluorescence_validate_spline_bt,
            units = "in",
            res = input$dpi_download_fluorescence_validate_spline_bt)
      }
      # Generate plot
      results <-
        results$fluorescence$flFit$flBootSplines[[ifelse(
          selected_vals_validate_fluorescence$sample_validate_fluorescence_spline_bt == "1" ||
            is.null(
              selected_vals_validate_fluorescence$sample_validate_fluorescence_spline_bt
            ),
          1,
          selected_vals_validate_fluorescence$sample_validate_fluorescence_spline_bt
        )]]


      # Define x- and y-axis limits
      if(any(input$y_range_min_validate_fluorescence_spline_bt == "",
             input$y_range_max_validate_fluorescence_spline_bt == "")){
        y.lim <- NULL
      } else {
        y.lim <- c(as.numeric(input$y_range_min_validate_fluorescence_spline_bt),
                  as.numeric(input$y_range_max_validate_fluorescence_spline_bt))
      }

      if(any(input$y_range_min_derivative_validate_fluorescence_spline_bt == "",
             input$y_range_max_derivative_validate_fluorescence_spline_bt == "")){
        ylim.deriv <- NULL
      } else {
        ylim.deriv <- c(as.numeric(input$y_range_min_derivative_validate_fluorescence_spline_bt),
                        as.numeric(input$y_range_max_derivative_validate_fluorescence_spline_bt))
      }

      if(any(input$x_range_min_validate_fluorescence_spline_bt == "",
             input$x_range_max_validate_fluorescence_spline_bt == "")){
        x.lim <- NULL
      } else {
        x.lim <- c(as.numeric(input$x_range_min_validate_fluorescence_spline_bt),
                  as.numeric(input$x_range_max_validate_fluorescence_spline_bt))
      }

      plot.flBootSpline(results,
                        pch = input$shape_type_validate_fluorescence_spline_bt,
                        cex.point = input$shape_size_validate_fluorescence_spline_bt,
                        cex.lab = input$axis_size_validate_fluorescence_spline_bt,
                        cex.axis = input$lab_size_validate_fluorescence_spline_bt,
                        lwd = input$line_width_validate_fluorescence_spline_bt,
                        y.lim = y.lim,
                        x.lim = x.lim,
                        y.lim.deriv = ylim.deriv,
                        deriv = input$plot_derivative_fluorescence_spline_bt
      )
      dev.off()
    },
    contentType = ifelse(input$format_download_fluorescence_validate_spline_bt == ".pdf", "image/pdf", "image/png")
  )
  # Visualize ####
    ## Growth Plots: #####
      ### Group Plots ####

  selected_inputs_visualize_growth_group <- reactive({
    results <- results$growth
    if(is.null(results)) return("")
    if(input$plot_group_averages_growth_group_plot){
      select_samples <- results$expdesign$condition
    } else {
      select_samples <- results$expdesign$label
    }
    select_samples
  })

  observe({
    updateSelectInput(session,
                      inputId = "samples_visualize_growth_group",
                      choices = selected_inputs_visualize_growth_group()
    )
  })

  observe({
    updateSelectInput(session,
                      inputId = "groups_visualize_growth_group",
                      choices = selected_inputs_visualize_growth_group()
    )
  })

  growth_group_plot <- reactive({
    results <- results$growth

    if(is.null(input$custom_colors_group_plot) || is.na(input$custom_colors_group_plot) || input$custom_colors_group_plot == ""){
      cols <- NULL
    } else {
      cols <- toupper(unlist(str_split(input$custom_colors_group_plot, ", |; |,|;")))
    }
    if(input$select_string_visualize_growth_group){
      suppressWarnings(
        plot.grofit(results,
                    data.type = input$data_type_growth_group_plot,
                    IDs = NULL,
                    names = input$select_samples_based_on_string_growth_group_plot,
                    conc = input$select_samples_based_on_concentration_growth_group_plot,
                    exclude.nm = input$exclude_samples_based_on_string_growth_group_plot,
                    exclude.conc = input$exclude_samples_based_on_concentration_growth_group_plot,
                    mean = input$plot_group_averages_growth_group_plot,
                    deriv = input$plot_derivative_growth_group_plot,
                    log.y = input$log_transform_y_axis_growth_group_plot,
                    x.lim = c(input$x_range_min_growth_group_plot, input$x_range_max_growth_group_plot),
                    y.lim = c(input$y_range_min_growth_group_plot,input$y_range_max_growth_group_plot),
                    y.lim.deriv = c(input$y_range_min_derivative_growth_group_plot, input$y_range_max_derivative_growth_group_plot),
                    y.title = input$y_axis_title_growth_group_plot,
                    x.title = input$x_axis_title_growth_group_plot,
                    y.title.deriv = input$y_axis_title_derivative_growth_group_plot,
                    n.ybreaks = input$nbreaks_growth_group_plot,
                    lwd = input$line_width_growth_group_plot,
                    basesize = input$base_size_growth_group_plot,
                    legend.position = input$legend_position_group_plot,
                    legend.ncol = input$legend_ncol_group_plot,
                    color_groups = input$color_groups_group_plot,
                    group_pals = input$color_palettes_group_plot,
                    colors = cols
        )
      )
    }
    else{
      suppressWarnings(
        plot.grofit(results,
                    data.type = input$data_type_growth_group_plot,
                    IDs = if(input$plot_group_averages_growth_group_plot){
                      input$groups_visualize_growth_group
                    }else{
                      input$samples_visualize_growth_group
                    },
                    names = NULL,
                    conc = input$select_samples_based_on_concentration_growth_group_plot,
                    exclude.nm = NULL,
                    exclude.conc = input$exclude_samples_based_on_concentration_growth_group_plot,
                    mean = input$plot_group_averages_growth_group_plot,
                    deriv = input$plot_derivative_growth_group_plot,
                    log.y = input$log_transform_y_axis_growth_group_plot,
                    x.lim = c(input$x_range_min_growth_group_plot, input$x_range_max_growth_group_plot),
                    y.lim = c(input$y_range_min_growth_group_plot,input$y_range_max_growth_group_plot),
                    y.lim.deriv = c(input$y_range_min_derivative_growth_group_plot, input$y_range_max_derivative_growth_group_plot),
                    y.title = input$y_axis_title_growth_group_plot,
                    x.title = input$x_axis_title_growth_group_plot,
                    y.title.deriv = input$y_axis_title_derivative_growth_group_plot,
                    n.ybreaks = input$nbreaks_growth_group_plot,
                    lwd = input$line_width_growth_group_plot,
                    basesize = input$base_size_growth_group_plot,
                    legend.position = input$legend_position_group_plot,
                    color_groups = input$color_groups_group_plot,
                    group_pals = input$color_palettes_group_plot,
                    legend.ncol = input$legend_ncol_group_plot,
                    colors = cols
        )
      )
    }
  })

  output$growth_group_plot <- renderPlot({
    growth_group_plot()
  })


  observe({
    if(input$plot_derivative_growth_group_plot && input$data_type_growth_group_plot == 'spline') h <- 9
    else h <- 6
    updateSelectInput(inputId = "height_download_growth_group_plot",
                      selected = h
    )
  })

  output$download_growth_group_plot <- downloadHandler(
    filename = function() {
      paste("growth_group_plot",  input$format_download_growth_group_plot, sep="")
    },
    content = function(file) {
      if(input$format_download_growth_group_plot == ".pdf"){
        if (requireNamespace("Cairo", quietly = TRUE)) {
          ggsave(filename = file, width = input$width_download_growth_group_plot,
                 height = input$height_download_growth_group_plot,
                 dpi = input$dpi_download_growth_group_plot,
                 device = cairo_pdf)
        } else {
          showModal(modalDialog("Package 'Cairo' must be installed to preserve special characters in the exported PDF image", easyClose = T))
          ggsave(filename = file, width = input$width_download_growth_group_plot,
                 height = input$height_download_growth_group_plot,
                 dpi = input$dpi_download_growth_group_plot,
                 device = pdf)
        }

      } else {
        ggsave(filename = file, width = input$width_download_growth_group_plot,
               height = input$height_download_growth_group_plot,
               dpi = input$dpi_download_growth_group_plot,
               device = png)
      }

    },
    contentType = ifelse(input$format_download_growth_group_plot == ".pdf", "image/pdf", "image/png")
  )

      ### DR Plots Spline ####
  observe({
    if(length(results$growth$drFit) > 1 && length(results$growth$drFit$drTable) > 1 && results$growth$drFit$control$dr.method == "spline"){
      showTab(inputId = "tabsetPanel_Visualize_Growth", target = "tabPanel_Visualize_Growth_DoseResponse_Spline")
    } else {
      hideTab(inputId = "tabsetPanel_Visualize_Growth", target = "tabPanel_Visualize_Growth_DoseResponse_Spline")
    }
  })

  output$more_than_one_drfit_spline <- reactive({
    if(length(results$growth$drFit) > 1 && length(results$growth$drFit$drFittedSplines) > 1) return(TRUE)
    else return(FALSE)
  })
  outputOptions(output, 'more_than_one_drfit_spline', suspendWhenHidden=FALSE)

  dose_response_growth_plot_combined <- reactive({
    results <- results$growth$drFit

    plot.drFit(results,
               combine = TRUE,
               pch = input$shape_type_dose_response_growth_plot,
               names = input$select_samples_based_on_string_dose_response_growth_plot,
               exclude.nm = input$exclude_samples_based_on_string_dose_response_growth_plot,
               y.lim = c(as.numeric(input$y_range_min_dose_response_growth_plot), as.numeric(input$y_range_max_dose_response_growth_plot)),
               x.lim = c(as.numeric(input$x_range_min_dose_response_growth_plot), as.numeric(input$x_range_max_dose_response_growth_plot)),
               y.title = input$y_axis_title_dose_response_growth_plot,
               x.title = input$x_axis_title_dose_response_growth_plot,
               cex.point = input$shape_size_dose_response_growth_plot,
               basesize = input$base_size_dose_response_growth_plot,
               lwd = input$line_width_dose_response_growth_plot,
               ec50line = input$show_ec50_indicator_lines_dose_response_growth_plot,
               log.y = input$log_transform_y_axis_dose_response_growth_plot,
               log.x = input$log_transform_x_axis_dose_response_growth_plot
    )
  })

  rerun_dr_growth_buttons <- reactive({
    list(input$rerun_dr_growth, input$rerun_dr_growth2, input$rerun_dr_growth3)
  })

  observeEvent(rerun_dr_growth_buttons(), {

    if(input$rerun_dr_growth==0 && input$rerun_dr_growth2==0 && input$rerun_dr_growth3==0){
      return()
    }

    select_options <- c()
    if(any("l" %in% results$growth$control$fit.opt)) select_options <- c(select_options, 'mu.linfit', 'lambda.linfit', 'dY.linfit',
                                                                         'A.linfit')
    if(any("s" %in% results$growth$control$fit.opt)) select_options <- c(select_options, 'mu.spline', 'lambda.spline',
                                                                         'A.spline', 'dY.spline', 'integral.spline')
    if(any("m" %in% results$growth$control$fit.opt)) select_options <- c(select_options, 'mu.model', 'lambda.model', 'A.model', 'integral.model')
    select_options

    # display a modal dialog with a header, textinput and action buttons
    showModal(
      modalDialog(
        tags$h2('Please enter adjusted parameters'),

        selectInput(inputId = "dr_method_growth_rerun",
                    label = "Method",
                    choices = c("Dose-response models" = "model",
                                "Response spline fit" = "spline")
        ),
        bsPopover(id = "dr_method_growth_rerun",
                  title = HTML("<em>dr.method</em>"),
                  placement = "right",
                  content = "Fit either a various dose-response models (Ritz et al., 2015) to response-vs.-concentration data and select the best model based on the lowest AIC, or apply a nonparametric (spline) fit."),

        selectInput(inputId = "response_parameter_growth_rerun",
                    label = "Response Parameter",
                    choices = select_options),
        bsPopover(id = "response_parameter_growth_rerun", title = HTML("<em>dr.parameter</em>"), content = "Choose the response parameter to be used for creating a dose response curve.", placement = "top"),

        conditionalPanel(
          condition = 'input.dr_method_growth_rerun == "spline"',
          tags$div(title="Perform a log(x+1) transformation on concentration values.",
                   checkboxInput(inputId = 'log_transform_concentration_growth_rerun',
                                 label = 'Log transform concentration')
          ),

          tags$div(title="Perform a log(y+1) transformation on response values.",
                   checkboxInput(inputId = 'log_transform_response_growth_rerun',
                                 label = 'Log transform response')
          ),

          textInput(
            inputId = 'smoothing_factor_growth_dr_rerun',
            label = 'Smoothing factor dose-response splines',
            value = "",
            placeholder = "NULL (choose automatically)"
          ),
          bsPopover(id = "smoothing_factor_growth_dr_rerun", title = HTML("<em>smooth.dr</em>"), content = "\\'spar\\' argument in the R function smooth.spline() used to create the dose response curve."),

          QurvE:::numberInput(
            inputId = 'number_of_bootstrappings_dr_growth_rerun',
            label = 'Number of bootstrappings',
            value = 0,
            min = NA,
            max = NA,
            placeholder = 0
          ),
          bsPopover(id = "number_of_bootstrappings_dr_growth_rerun", title = HTML("<em>nboot.dr</em>"), content = "Optional: Define the number of bootstrap samples for EC50 estimation. Bootstrapping resamples the values in a dataset with replacement and performs a spline fit for each bootstrap sample to determine the EC50.")
        ), #conditionalPanel(condition = 'input.dr_method_growth_rerun == "spline"')
        footer=tagList(
          fluidRow(
            column(12,
                   div(
                     actionButton('submit.rerun.dr.growth', 'Submit'),
                     style="float:right"),
                   div(
                     modalButton('cancel'),
                     style="float:right")

            )
          )
        )
      )
    )
  })



  # Re-run dose-response analysis with user-defined parameters upon click on 'submit'
  observeEvent(input$submit.rerun.dr.growth, {
    if(!is.null(results$growth$drFit)){

      showModal(modalDialog("Performing dose-reponse analysis...", footer = NULL))

      # store previous fit in memory
      selected_vals_validate_growth$restore_dr_growth <- results$growth$drFit

      # Re-run fit and store in results object
      gcTable <- results$growth$gcFit$gcTable
      control <- results$growth$drFit$control
      control_new <- results$growth$control <- control

      control_new$dr.method <- input$dr_method_growth_rerun
      control_new$dr.parameter <- input$response_parameter_growth_rerun
      control_new$smooth.dr <- as.numeric(input$smoothing_factor_growth_dr_rerun)
      if(control_new$smooth.dr == "" || is.na(control_new$smooth.dr))
        control_new$smooth.dr <- NULL
      control_new$nboot.dr <- input$number_of_bootstrappings_dr_growth_rerun
      control_new$log.x.dr <- input$log_transform_concentration_growth_rerun
      control_new$log.y.dr <- input$log_transform_response_growth_rerun

      try(
        results$growth$drFit <-
          growth.drFit(gcTable, control = control_new)
      )

      # Show [Restore fit] button
      show("restore_dr_growth")
      show("restore_dr_growth2")
      show("restore_dr_growth3")
    }

    removeModal()
  })

  restore_dr_growth_buttons <- reactive({
    list(input$restore_dr_growth, input$restore_dr_growth2, input$restore_dr_growth3)
  })

  # Restore previous linear fit upon click on [Restore Fit]
  observeEvent(restore_dr_growth_buttons(), {
    if(input$restore_dr_growth==0 && input$restore_dr_growth2==0 && input$restore_dr_growth3==0){
      return()
    }
    # store previous fit from memory
    results$growth$drFit <- selected_vals_validate_growth$restore_dr_growth
    hide("restore_dr_growth")
    hide("restore_dr_growth2")
    hide("restore_dr_growth3")
  })

  output$dose_response_growth_plot_combined <- renderPlot({
    dose_response_growth_plot_combined()
  })

  output$download_dose_response_growth_plot_combined <- downloadHandler(
    filename = function() {
      paste("dose_response_growth_combined",  input$format_download_dose_response_growth_plot_combined, sep="")
    },
    content = function(file) {
      if(input$format_download_dose_response_growth_plot_combined == ".pdf"){
        if (requireNamespace("Cairo", quietly = TRUE)) {
          ggsave(filename = file, width = input$width_download_dose_response_growth_plot_combined,
                 height = input$height_download_dose_response_growth_plot_combined,
                 dpi = input$dpi_download_dose_response_growth_plot_combined,
                 device = cairo_pdf)
        } else {
          showModal(modalDialog("Package 'Cairo' must be installed to preserve special characters in the exported PDF image", easyClose = T))
          ggsave(filename = file, width = input$width_download_dose_response_growth_plot_combined,
                 height = input$height_download_dose_response_growth_plot_combined,
                 dpi = input$dpi_download_dose_response_growth_plot_combined,
                 device = pdf)
        }

      } else {
        ggsave(filename = file, width = input$width_download_dose_response_growth_plot_combined,
               height = input$height_download_dose_response_growth_plot_combined,
               dpi = input$dpi_download_dose_response_growth_plot_combined,
               device = png)
      }

    },
    contentType = ifelse(input$format_download_dose_response_growth_plot_combined == ".pdf", "image/pdf", "image/png")
  )

      ### DR Plots Model ####
      observe({
        if(length(results$growth$drFit) > 1 && length(results$growth$drFit$drTable) > 1 && results$growth$drFit$control$dr.method == "model"){
          showTab(inputId = "tabsetPanel_Visualize_Growth", target = "tabPanel_Visualize_Growth_DoseResponse_Model")
        } else {
          hideTab(inputId = "tabsetPanel_Visualize_Growth", target = "tabPanel_Visualize_Growth_DoseResponse_Model")
        }
      })

      output$more_than_one_drfit_model <- reactive({
        if(length(results$growth$drFit) > 1 && length(results$growth$drFit$drFittedModels) > 1) return(TRUE)
        else return(FALSE)
      })
      outputOptions(output, 'more_than_one_drfit_model', suspendWhenHidden=FALSE)

      select_inputs_individual_plots_dose_response_growth_plot_model <- reactive({
        if (length(results$growth$drFit)>1) names(results$growth$drFit$drFittedModels)
        else return("")
      })

      observe({
        updateSelectInput(inputId = "individual_plots_dose_response_growth_plot_model",
                          choices = select_inputs_individual_plots_dose_response_growth_plot_model())
      })

      select_inputs_show_breaks_dose_response_growth_plot_model <- reactive({
        if(!input$log_transform_x_axis_dose_response_growth_plot_model)
          return(FALSE)
        else
          return(TRUE)
      })

      observe({
        updateCheckboxInput(inputId = "show_break_dose_response_growth_plot_model",
                            value  = select_inputs_show_breaks_dose_response_growth_plot_model())
      })

      dose_response_growth_plot_model <- reactive({

        # Define log-transformation of axes
        if(input$log_transform_x_axis_dose_response_growth_plot_model){
          log <- "x"
        } else {
          log <- ""
        }

        # Define x- and y-axis limits
        if(!any(input$y_range_min_dose_response_growth_plot_model == "",
                input$y_range_max_dose_response_growth_plot_model == "") &&
           !any(is.null(input$y_range_min_dose_response_growth_plot_model),
                is.null(input$y_range_max_dose_response_growth_plot_model))){
          y.lim <- c(as.numeric(input$y_range_min_dose_response_growth_plot_model),
                     as.numeric(input$y_range_max_dose_response_growth_plot_model))
        }

        if(!any(input$x_range_min_dose_response_growth_plot_model == "",
                input$x_range_max_dose_response_growth_plot_model == "") &&
           !any(is.null(input$x_range_min_dose_response_growth_plot_model),
                is.null(input$x_range_max_dose_response_growth_plot_model))){
          x.lim <- c(as.numeric(input$x_range_min_dose_response_growth_plot_model),
                     as.numeric(input$x_range_max_dose_response_growth_plot_model))
        }
        results <- results$growth$drFit$drFittedModels[[ifelse(input$individual_plots_dose_response_growth_plot_model == "1" || is.null(input$individual_plots_dose_response_growth_plot_model), 1, input$individual_plots_dose_response_growth_plot_model)]]
        try(
          plot.drFitModel(results,
                          pch = input$shape_type_dose_response_growth_plot_model,
                          cex.point = input$shape_size_dose_response_growth_plot_model,
                          cex.lab = input$axis_size_dose_response_growth_plot_model,
                          cex.axis = input$lab_size_dose_response_growth_plot_model,
                          log = log,
                          lwd = input$line_width_dose_response_growth_plot_model,
                          ec50line = input$show_ec50_indicator_lines_dose_response_growth_plot_model,
                          y.lim = if(!exists("y.lim")){
                            substitute()
                          } else {
                            y.lim
                          },
                          x.lim = if(!exists("x.lim")){
                            substitute()
                          } else {
                            x.lim
                          },
                          broken = input$show_break_dose_response_growth_plot_model,
                          n.xbreaks = input$nbreaks_x_growth_dose_response_plot_model,
                          n.ybreaks = input$nbreaks_y_growth_dose_response_plot_model,
                          xlab = input$x_axis_title_dose_response_growth_plot_model,
                          ylab = input$y_axis_title_dose_response_growth_plot_model,
                          bp = ifelse(is.na(input$bp_dose_response_growth_plot_model)||(input$bp_dose_response_growth_plot_model == ""), rlang::missing_arg(), input$bp_dose_response_growth_plot_model)
          )
        )
      })

      output$dose_response_growth_plot_model <- renderPlot({
        dose_response_growth_plot_model()
      })

      output$download_dose_response_growth_plot_model <- downloadHandler(
        filename = function() {
          paste("dose_response_growth_model",  input$format_download_dose_response_growth_plot_model, sep="")
        },
        content = function(file) {
          if(input$format_download_dose_response_growth_plot_individual == ".pdf"){
            if (requireNamespace("Cairo", quietly = TRUE)) {
              Cairo::CairoPDF(file = file,
                              width = input$width_download_dose_response_growth_plot_model,
                              height = input$height_download_dose_response_growth_plot_model)
            } else {
              showModal(modalDialog("Package 'Cairo' must be installed to preserve special characters in the exported PDF image", easyClose = T))
              grDevices::pdf(file = file,
                             width = input$width_download_dose_response_growth_plot_model,
                             height = input$height_download_dose_response_growth_plot_model)
            }

          } else {
            grDevices::png(file = file,
                width = input$width_download_dose_response_growth_plot_model,
                height = input$height_download_dose_response_growth_plot_model,
                units = "in",
                res = input$dpi_download_dose_response_growth_plot_model)
          }
          # Define log-transformation of axes
          if(input$log_transform_x_axis_dose_response_growth_plot_model){
            log <- "x"
          } else {
            log <- ""
          }

          # Define x- and y-axis limits
          if(!any(input$y_range_min_dose_response_growth_plot_model == "",
                  input$y_range_max_dose_response_growth_plot_model == "") &&
             !any(is.null(input$y_range_min_dose_response_growth_plot_model),
                  is.null(input$y_range_max_dose_response_growth_plot_model))){
            y.lim <- c(as.numeric(input$y_range_min_dose_response_growth_plot_model),
                       as.numeric(input$y_range_max_dose_response_growth_plot_model))
          }

          if(!any(input$x_range_min_dose_response_growth_plot_model == "",
                  input$x_range_max_dose_response_growth_plot_model == "") &&
             !any(is.null(input$x_range_min_dose_response_growth_plot_model),
                  is.null(input$x_range_max_dose_response_growth_plot_model))){
            x.lim <- c(as.numeric(input$x_range_min_dose_response_growth_plot_model),
                       as.numeric(input$x_range_max_dose_response_growth_plot_model))
          }
          results <- results$growth$drFit$drFittedModels[[ifelse(input$individual_plots_dose_response_growth_plot_model == "1" || is.null(input$individual_plots_dose_response_growth_plot_model), 1, input$individual_plots_dose_response_growth_plot_model)]]

          try(
            plot.drFitModel(results,
                            pch = input$shape_type_dose_response_growth_plot_model,
                            cex.point = input$shape_size_dose_response_growth_plot_model,
                            cex.lab = input$axis_size_dose_response_growth_plot_model,
                            cex.axis = input$lab_size_dose_response_growth_plot_model,
                            log = log,
                            lwd = input$line_width_dose_response_growth_plot_model,
                            ec50line = input$show_ec50_indicator_lines_dose_response_growth_plot_model,
                            y.lim = ifelse(!exists("y.lim"), substitute(), y.lim),
                            x.lim = ifelse(!exists("x.lim"), substitute(), x.lim),
                            broken = input$show_break_dose_response_growth_plot_model,
                            n.xbreaks = input$nbreaks_x_growth_dose_response_plot_model,
                            n.ybreaks = input$nbreaks_y_growth_dose_response_plot_model,
                            xlab = input$x_axis_title_dose_response_growth_plot_model,
                            ylab = input$y_axis_title_dose_response_growth_plot_model,
                            bp = ifelse(is.na(input$bp_dose_response_growth_plot_model)||(input$bp_dose_response_growth_plot_model == ""), rlang::missing_arg(), input$bp_dose_response_growth_plot_model)
            )
          )
          dev.off()
        },
        contentType = ifelse(input$format_download_dose_response_growth_plot_model == ".pdf", "image/pdf", "image/png")
      )

      ### DR Plots Spline Individual ####


  dose_response_growth_plot_individual <- reactive({
    results <- results$growth$drFit$drFittedSplines[[ifelse(input$individual_plots_dose_response_growth_plot == "1" || is.null(input$individual_plots_dose_response_growth_plot), 1, input$individual_plots_dose_response_growth_plot)]]

    # Define log-transformation of axes
    if(input$log_transform_y_axis_dose_response_growth_plot &&
       input$log_transform_x_axis_dose_response_growth_plot){
      log <- "xy"
    } else if(input$log_transform_y_axis_dose_response_growth_plot){
      log <- "y"
    } else if(input$log_transform_x_axis_dose_response_growth_plot){
      log <- "x"
    } else {
      log <- ""
    }
    # Define x- and y-axis limits
    if(any(input$y_range_min_dose_response_growth_plot == "",
           input$y_range_max_dose_response_growth_plot == "")){
      y.lim <- NULL
    } else {
      y.lim <- c(as.numeric(input$y_range_min_dose_response_growth_plot),
                as.numeric(input$y_range_max_dose_response_growth_plot))
    }

    if(any(input$x_range_min_dose_response_growth_plot == "",
           input$x_range_max_dose_response_growth_plot == "")){
      x.lim <- NULL
    } else {
      x.lim <- c(as.numeric(input$x_range_min_dose_response_growth_plot),
                as.numeric(input$x_range_max_dose_response_growth_plot))
    }

    plot.drFitSpline(results,
                     combine = FALSE,
                     pch = input$shape_type_dose_response_growth_plot,
                     cex.point = input$shape_size_dose_response_growth_plot,
                     cex.lab = input$axis_size_dose_response_growth_plot,
                     cex.axis = input$lab_size_dose_response_growth_plot,
                     y.title = input$y_axis_title_dose_response_growth_plot,
                     x.title = input$x_axis_title_dose_response_growth_plot,
                     log = log,
                     lwd = input$line_width_dose_response_growth_plot,
                     ec50line = input$show_ec50_indicator_lines_dose_response_growth_plot,
                     y.lim = y.lim,
                     x.lim = x.lim
                     )
  })

  output$dose_response_growth_plot_individual <- renderPlot({
    dose_response_growth_plot_individual()
  })

  output$download_dose_response_growth_plot_individual <- downloadHandler(
    filename = function() {
      paste("dose_response_growth_",  gsub(" \\| ", "_", input$individual_plots_dose_response_growth_plot), input$format_download_dose_response_growth_plot_individual, sep="")
    },
    content = function(file) {
      if(input$format_download_dose_response_growth_plot_individual == ".pdf"){
        if (requireNamespace("Cairo", quietly = TRUE)) {
          Cairo::CairoPDF(file = file,
                          width = input$width_download_dose_response_growth_plot_individual,
                          height = input$height_download_dose_response_growth_plot_individual)
        } else {
          showModal(modalDialog("Package 'Cairo' must be installed to preserve special characters in the exported PDF image", easyClose = T))
          grDevices::pdf(file = file,
                         width = input$width_download_dose_response_growth_plot_individual,
                         height = input$height_download_dose_response_growth_plot_individual)
        }

      } else {
        grDevices::png(file = file,
            width = input$width_download_dose_response_growth_plot_individual,
            height = input$height_download_dose_response_growth_plot_individual,
            units = "in",
            res = input$dpi_download_dose_response_growth_plot_individual)
      }
      results <- results$growth$drFit$drFittedSplines[[ifelse(input$individual_plots_dose_response_growth_plot == "1" || is.null(input$individual_plots_dose_response_growth_plot), 1, input$individual_plots_dose_response_growth_plot)]]

      # Define log-transformation of axes
      if(input$log_transform_y_axis_dose_response_growth_plot &&
         input$log_transform_x_axis_dose_response_growth_plot){
        log <- "xy"
      } else if(input$log_transform_y_axis_dose_response_growth_plot){
        log <- "y"
      } else if(input$log_transform_x_axis_dose_response_growth_plot){
        log <- "x"
      } else {
        log <- ""
      }

      # Define x- and y-axis limits
      if(any(input$y_range_min_dose_response_growth_plot == "",
             input$y_range_max_dose_response_growth_plot == "")){
        y.lim <- NULL
      } else {
        y.lim <- c(as.numeric(input$y_range_min_dose_response_growth_plot),
                  as.numeric(input$y_range_max_dose_response_growth_plot))
      }

      if(any(input$x_range_min_dose_response_growth_plot == "",
             input$x_range_max_dose_response_growth_plot == "")){
        x.lim <- NULL
      } else {
        x.lim <- c(as.numeric(input$x_range_min_dose_response_growth_plot),
                  as.numeric(input$x_range_max_dose_response_growth_plot))
      }

      plot.drFitSpline(results,
                       combine = FALSE,
                       pch = input$shape_type_dose_response_growth_plot,
                       cex.point = input$shape_size_dose_response_growth_plot,
                       cex.lab = input$axis_size_dose_response_growth_plot,
                       cex.axis = input$lab_size_dose_response_growth_plot,
                       y.title = input$y_axis_title_dose_response_growth_plot,
                       x.title = input$x_axis_title_dose_response_growth_plot,
                       log = log,
                       lwd = input$line_width_dose_response_growth_plot,
                       ec50line = input$show_ec50_indicator_lines_dose_response_growth_plot,
                       y.lim = y.lim,
                       x.lim = x.lim
      )
      dev.off()
    },
    contentType = ifelse(input$format_download_dose_response_growth_plot_individual == ".pdf", "image/pdf", "image/png")
  )


      ### DR Plots (Bootstrap) ####
  observe({
    if(length(results$growth$drFit) > 1 && length(results$growth$drFit$drTable) > 1 && results$growth$control$nboot.dr > 1){
      showTab(inputId = "tabsetPanel_Visualize_Growth", target = "tabPanel_Visualize_Growth_DoseResponse_Spline_bt")
    } else {
      hideTab(inputId = "tabsetPanel_Visualize_Growth", target = "tabPanel_Visualize_Growth_DoseResponse_Spline_bt")
    }
  })

  dose_response_growth_plot_individual_bt <- reactive({
    results <- results$growth$drFit$drBootSplines[[ifelse(input$individual_plots_dose_response_growth_plot_bt == "1" || is.null(input$individual_plots_dose_response_growth_plot_bt), 1, input$individual_plots_dose_response_growth_plot_bt)]]

    plot.drBootSpline(results,
                     pch = input$shape_type_dose_response_growth_plot_bt,
                     cex.point = input$shape_size_dose_response_growth_plot_bt,
                     cex.lab = input$axis_size_dose_response_growth_plot_bt,
                     cex.axis = input$lab_size_dose_response_growth_plot_bt,
                     lwd = input$line_width_dose_response_growth_plot_bt,
                     combine = TRUE

    )
  })

  output$dose_response_growth_plot_individual_bt <- renderPlot({
    dose_response_growth_plot_individual_bt()
  })

  output$download_dose_response_growth_plot_individual_bt <- downloadHandler(
    filename = function() {
      paste("dose_response_boot_growth_",  gsub(" \\| ", "_", input$individual_plots_dose_response_growth_plot_bt), input$format_download_dose_response_growth_plot_individual_bt, sep="")
    },
    content = function(file) {
      if(input$format_download_dose_response_growth_plot_individual_bt == ".pdf"){
        if (requireNamespace("Cairo", quietly = TRUE)) {
          Cairo::CairoPDF(file = file,
                          width = input$width_download_dose_response_growth_plot_individual_bt,
                          height = input$height_download_dose_response_growth_plot_individual_bt)
        } else {
          showModal(modalDialog("Package 'Cairo' must be installed to preserve special characters in the exported PDF image", easyClose = T))
          grDevices::pdf(file = file,
                         width = input$width_download_dose_response_growth_plot_individual_bt,
                         height = input$height_download_dose_response_growth_plot_individual_bt)
        }

      } else {
        grDevices::png(file = file,
            width = input$width_download_dose_response_growth_plot_individual_bt,
            height = input$height_download_dose_response_growth_plot_individual_bt,
            units = "in",
            res = input$dpi_download_dose_response_growth_plot_individual_bt)
      }
      results <- results$growth$drFit$drBootSplines[[input$individual_plots_dose_response_growth_plot_bt]]

      plot.drBootSpline(results,
                        pch = input$shape_type_dose_response_growth_plot_bt,
                        cex.point = input$shape_size_dose_response_growth_plot_bt,
                        cex.lab = input$axis_size_dose_response_growth_plot_bt,
                        cex.axis = input$lab_size_dose_response_growth_plot_bt,
                        lwd = input$line_width_dose_response_growth_plot_bt,
                        combine = TRUE

      )
      dev.off()
    },
    contentType = ifelse(input$format_download_dose_response_growth_plot_individual_bt == ".pdf", "image/pdf", "image/png")
  )

      ### Parameter Plots ####
  selected_inputs_visualize_parameter_growth_plot <- reactive({
    results <- results$growth
    if(is.null(results)) return("")

    select_samples <- results$expdesign$condition
    select_samples
  })

  observe({
    updateSelectInput(session,
                      inputId = "samples_visualize_parameter_growth_plot",
                      choices = selected_inputs_visualize_parameter_growth_plot()
    )
  })

  growth_parameter_plot <- reactive({
    results <- results$growth
    if (input$normalize_to_reference_growth_parameter_plot){
      reference.conc <- as.numeric(input$reference_concentration_growth_parameter_plot)
      reference.nm <- input$reference_condition_growth_parameter_plot
    } else {
      reference.conc <- NULL
      reference.nm <- NULL
    }
    if(is.null(input$custom_colors_growth_parameter_plot) || is.na(input$custom_colors_growth_parameter_plot) || input$custom_colors_growth_parameter_plot == ""){
      cols <- NULL
    } else {
      cols <- toupper(unlist(str_split(input$custom_colors_growth_parameter_plot, ", |; |,|;")))
    }
    if(input$select_string_visualize_parameter_growth_plot){
      suppressWarnings(
        plot.parameter(results,
                       param = input$parameter_parameter_growth_plot,
                       IDs = NULL,
                       names = input$select_sample_based_on_string_growth_parameter_plot,
                       conc = input$select_sample_based_on_concentration_growth_parameter_plot,
                       exclude.nm = input$exclude_sample_based_on_strings_growth_parameter_plot,
                       exclude.conc = input$exclude_sample_based_on_concentration_growth_parameter_plot,
                       reference.nm = reference.nm,
                       reference.conc = reference.conc,
                       shape.size = input$shape.size_growth_parameter_plot,
                       basesize = input$basesize_growth_parameter_plot,
                       label.size = input$label.size_growth_parameter_plot,
                       legend.position = input$legend_position_growth_parameter_plot,
                       legend.ncol = input$legend_ncol_growth_parameter_plot,
                       order_by_conc = input$sort_by_conc_growth_parameter_plot,
                       colors = cols
        )
      )
    } else {
      suppressWarnings(
        plot.parameter(results,
                       param = input$parameter_parameter_growth_plot,
                       IDs = input$samples_visualize_parameter_growth_plot,
                       names = NULL,
                       conc = input$select_sample_based_on_concentration_growth_parameter_plot,
                       exclude.nm = NULL,
                       exclude.conc = input$exclude_sample_based_on_concentration_growth_parameter_plot,
                       reference.nm = reference.nm,
                       reference.conc = reference.conc,
                       shape.size = input$shape.size_growth_parameter_plot,
                       basesize = input$basesize_growth_parameter_plot,
                       label.size = input$label.size_growth_parameter_plot,
                       legend.position = input$legend_position_growth_parameter_plot,
                       legend.ncol = input$legend_ncol_growth_parameter_plot,
                       order_by_conc = input$sort_by_conc_growth_parameter_plot,
                       colors = cols

        )
      )
    }
  })

  output$growth_parameter_plot <- renderPlot({
    growth_parameter_plot()
  })

  output$download_growth_parameter_plot <- downloadHandler(
    filename = function() {
      paste("growth_parameter_plot",  input$format_download_growth_parameter_plot, sep="")
    },
    content = function(file) {
      if(input$format_download_growth_parameter_plot == ".pdf"){
        if (requireNamespace("Cairo", quietly = TRUE)) {
          ggsave(filename = file, width = input$width_download_growth_parameter_plot,
                 height = input$height_download_growth_parameter_plot,
                 dpi = input$dpi_download_growth_parameter_plot,
                 device = cairo_pdf)
        } else {
          showModal(modalDialog("Package 'Cairo' must be installed to preserve special characters in the exported PDF image", easyClose = T))
          ggsave(filename = file, width = input$width_download_growth_parameter_plot,
                 height = input$height_download_growth_parameter_plot,
                 dpi = input$dpi_download_growth_parameter_plot,
                 device = pdf)
        }

      } else {
        ggsave(filename = file, width = input$width_download_growth_parameter_plot,
               height = input$height_download_growth_parameter_plot,
               dpi = input$dpi_download_growth_parameter_plot,
               device = png)
      }

    },
    contentType = ifelse(input$format_download_growth_parameter_plot == ".pdf", "image/pdf", "image/png")

  )

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
                           'Maximum growth measurement (Spline)' = 'A.spline',
                           'ΔGrowth measurement (Spline)' = 'dY.spline',
                           'Area under the curve (Spline)' = 'integral.spline')
      } else {
        gc_parameters <- c(gc_parameters,
                           'Growth rate (Spline)' = 'mu.spline',
                           "Doubling time (Spline)" = "tD.spline",
                           'Lag time (Spline)' = 'lambda.spline',
                           'Maximum growth measurement (Spline)' = 'A.spline',
                           'ΔGrowth measurement (Spline)' = 'dY.spline',
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
                           'Maximum growth measurement (linear fit)' = 'A.linfit',
                           'ΔGrowth measurement (linear fit)' = 'dY.linfit')
      } else {
        gc_parameters <- c(gc_parameters,
                           'Growth rate (linear fit)' = 'mu.linfit',
                           "Doubling time (linear fit)" = "tD.linfit",
                           'Lag time (linear fit)' = 'lambda.linfit',
                           'Maximum growth measurement (linear fit)' = 'A.linfit',
                           'ΔGrowth measurement (linear fit)' = 'dY.linfit')
      }
    }
    if("m" %in% results$control$fit.opt || "a" %in% results$control$fit.opt){
      gc_parameters <- c(gc_parameters,
                         'Growth rate (model)' = 'mu.model',
                         'Doubling time (model)' = 'tD.model',
                         'Lag time (model)' = 'lambda.model',
                         'Maximum growth measurement (model)' = 'A.model')
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

  select_inputs_individual_plots_dose_response_growth_plot <- reactive({
    if (length(results$growth$drFit)>1) names(results$growth$drFit$drFittedSplines)
    else return("")
  })


  observe({
    updateSelectInput(inputId = "parameter_parameter_growth_plot",
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

  observe({
    updateSelectInput(inputId = "individual_plots_dose_response_growth_plot_bt",
                      choices = select_inputs_individual_plots_dose_response_growth_plot_bt())
  })

      ### Grid Plot ####
      observe({
        if(!exists("output$more_than_two_conc") || is.null(output$more_than_two_conc))
          return(NULL)
        if(!output$more_than_two_conc){
          updateSelectInput(session = session,
                            inputId = "sort_by_conc_growth_grid_plot",
                            selected = FALSE)
        }
      })

  observe({
    if(input$select_string_visualize_growth_grid)
      updateSelectInput(session = session,
                        inputId = "order_matters_visualize_growth_grid",
                        selected = FALSE)
  })

  selected_inputs_parameter_growth_grid_plot <- reactive({
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
                           'Maximum growth measurement (Spline)' = 'A.spline',
                           'ΔGrowth measurement (Spline)' = 'dY.spline',
                           'Area under the curve (Spline)' = 'integral.spline')
      } else {
        gc_parameters <- c(gc_parameters,
                           'Growth rate (Spline)' = 'mu.spline',
                           "Doubling time (Spline)" = "tD.spline",
                           'Lag time (Spline)' = 'lambda.spline',
                           'Maximum growth measurement (Spline)' = 'A.spline',
                           'ΔGrowth measurement (Spline)' = 'dY.spline',
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
                           'Maximum growth measurement (linear fit)' = 'A.linfit',
                           'ΔGrowth measurement (linear fit)' = 'dY.linfit')
      } else {
        gc_parameters <- c(gc_parameters,
                           'Growth rate (linear fit)' = 'mu.linfit',
                           "Doubling time (linear fit)" = "tD.linfit",
                           'Lag time (linear fit)' = 'lambda.linfit',
                           'Maximum growth measurement (linear fit)' = 'A.linfit',
                           'ΔGrowth measurement (linear fit)' = 'dY.linfit')
      }
    }
    if("m" %in% results$control$fit.opt || "a" %in% results$control$fit.opt){
      gc_parameters <- c(gc_parameters,
                         'Growth rate (model)' = 'mu.model',
                         'Doubling time (model)' = 'tD.model',
                         'Lag time (model)' = 'lambda.model',
                         'Maximum growth measurement (model)' = 'A.model')
    }
    gc_parameters
  })

  observe({
    updateSelectInput(inputId = "parameter_parameter_grid_plot",
                      choices = selected_inputs_parameter_growth_grid_plot()
    )})

  selected_inputs_visualize_growth_grid <- reactive({
    results <- results$growth
    if(is.null(results)) return("")
    if(input$plot_group_averages_growth_grid_plot){
      select_samples <- results$expdesign$condition
    } else {
      select_samples <- results$expdesign$label
    }
    select_samples
  })

  observe({
    updateSelectInput(session,
                      inputId = "samples_visualize_growth_grid",
                      choices = selected_inputs_visualize_growth_grid()
    )
  })

  observe({
    updateSelectInput(session,
                      inputId = "groups_visualize_growth_grid",
                      choices = selected_inputs_visualize_growth_grid()
    )
  })

  growth_grid_plot <- reactive({
    results <- results$growth
    if(input$select_string_visualize_growth_grid){
      suppressWarnings(
        plot.grid(results,
                  data.type = input$data_type_growth_grid_plot,
                  IDs = NULL,
                  sort_by_ID = FALSE,
                  names = input$select_samples_based_on_string_growth_grid_plot,
                  conc = input$select_samples_based_on_concentration_growth_grid_plot,
                  exclude.nm = input$exclude_samples_based_on_string_growth_grid_plot,
                  exclude.conc = input$exclude_samples_based_on_concentration_growth_grid_plot,
                  mean = input$plot_group_averages_growth_grid_plot,
                  log.y = input$log_transform_y_axis_growth_grid_plot,
                  x.lim = c(input$x_range_min_growth_grid_plot, input$x_range_max_growth_grid_plot),
                  y.lim = c(input$y_range_min_growth_grid_plot,input$y_range_max_growth_grid_plot),
                  y.title = input$y_axis_title_growth_grid_plot,
                  x.title = input$x_axis_title_growth_grid_plot,
                  n.ybreaks = input$nbreaks_growth_grid_plot,
                  lwd = input$line_width_growth_grid_plot,
                  basesize = input$base_size_growth_grid_plot,
                  pal = input$color_palettes_grid_plot,
                  invert.pal = input$invert_color_palette_grid_plot,
                  sort_by_conc = input$sort_by_conc_growth_grid_plot,
                  legend.lim = c(input$legend_lim_min_growth_grid_plot, input$legend_lim_max_growth_grid_plot),
                  nrow = input$nrows_growth_grid_plot,
                  param = input$parameter_parameter_grid_plot
        )
      )
    }
    else{
      suppressWarnings(
        plot.grid(results,
                  data.type = input$data_type_growth_grid_plot,
                  IDs = if(input$plot_group_averages_growth_grid_plot){
                    input$groups_visualize_growth_grid
                  }else{
                    input$samples_visualize_growth_grid
                  },
                  sort_by_ID = input$order_matters_visualize_growth_grid,
                  names = NULL,
                  conc = input$select_samples_based_on_concentration_growth_grid_plot,
                  exclude.nm = NULL,
                  exclude.conc = input$exclude_samples_based_on_concentration_growth_grid_plot,
                  mean = input$plot_group_averages_growth_grid_plot,
                  log.y = input$log_transform_y_axis_growth_grid_plot,
                  x.lim = c(input$x_range_min_growth_grid_plot, input$x_range_max_growth_grid_plot),
                  y.lim = c(input$y_range_min_growth_grid_plot,input$y_range_max_growth_grid_plot),
                  y.title = input$y_axis_title_growth_grid_plot,
                  x.title = input$x_axis_title_growth_grid_plot,
                  n.ybreaks = input$nbreaks_growth_grid_plot,
                  lwd = input$line_width_growth_grid_plot,
                  basesize = input$base_size_growth_grid_plot,
                  pal = input$color_palettes_grid_plot,
                  invert.pal = input$invert_color_palette_grid_plot,
                  sort_by_conc = input$sort_by_conc_growth_grid_plot,
                  legend.lim = c(input$legend_lim_min_growth_grid_plot, input$legend_lim_max_growth_grid_plot),
                  nrow = input$nrows_growth_grid_plot,
                  param = input$parameter_parameter_grid_plot
        )
      )
    }
  })

  output$growth_grid_plot <- renderPlot({
    growth_grid_plot()
  })


  output$download_growth_grid_plot <- downloadHandler(
    filename = function() {
      paste("growth_grid_plot",  input$format_download_growth_grid_plot, sep="")
    },
    content = function(file) {
      if(input$format_download_growth_grid_plot == ".pdf"){
        if (requireNamespace("Cairo", quietly = TRUE)) {
          ggsave(filename = file, width = input$width_download_growth_grid_plot,
                 height = input$height_download_growth_grid_plot,
                 dpi = input$dpi_download_growth_grid_plot,
                 device = cairo_pdf)
        } else {
          showModal(modalDialog("Package 'Cairo' must be installed to preserve special characters in the exported PDF image", easyClose = T))
          ggsave(filename = file, width = input$width_download_growth_grid_plot,
                 height = input$height_download_growth_grid_plot,
                 dpi = input$dpi_download_growth_grid_plot,
                 device = pdf)
        }

      } else {
        ggsave(filename = file, width = input$width_download_growth_grid_plot,
               height = input$height_download_growth_grid_plot,
               dpi = input$dpi_download_growth_grid_plot,
               device = png)
      }

    },
    contentType = ifelse(input$format_download_growth_grid_plot == ".pdf", "image/pdf", "image/png")
  )

      ### DR Parameter Plots ####
  observe({
    if(length(results$growth$drFit) > 1 && length(results$growth$drFit$drTable) > 1){
      showTab(inputId = "tabsetPanel_Visualize_Growth", target = "tabPanel_Visualize_Growth_DoseResponseParameters")
    } else {
      hideTab(inputId = "tabsetPanel_Visualize_Growth", target = "tabPanel_Visualize_Growth_DoseResponseParameters")
    }
  })

  growth_dr_parameter_plot <- reactive({
    results <- results$growth

    if (input$normalize_to_reference_growth_dr_parameter_plot){
      reference.nm <- input$reference_condition_growth_dr_parameter_plot
    } else {
      reference.nm <- NULL
    }
    param <- input$parameter_dr_parameter_growth_plot
    suppressWarnings(
      plot.dr_parameter(results,
                        param = param,
                        names = input$select_sample_based_on_string_growth_dr_parameter_plot,
                        exclude.nm = input$exclude_sample_based_on_strings_growth_dr_parameter_plot,
                        reference.nm = reference.nm,
                        basesize = input$basesize_growth_dr_parameter_plot,
                        label.size = input$label.size_growth_dr_parameter_plot
      )
    )
  })

  output$growth_dr_parameter_plot <- renderPlot({
    growth_dr_parameter_plot()
  })

  output$download_growth_dr_parameter_plot <- downloadHandler(
    filename = function() {
      paste("growth_dr_parameter_plot",  input$format_download_growth_dr_parameter_plot, sep="")
    },
    content = function(file) {
      if(input$format_download_growth_dr_parameter_plot == ".pdf"){
        if (requireNamespace("Cairo", quietly = TRUE)) {
          ggsave(filename = file, width = input$width_download_growth_dr_parameter_plot,
                 height = input$height_download_growth_dr_parameter_plot,
                 dpi = input$dpi_download_growth_dr_parameter_plot,
                 device = cairo_pdf)
        } else {
          showModal(modalDialog("Package 'Cairo' must be installed to preserve special characters in the exported PDF image", easyClose = T))
          ggsave(filename = file, width = input$width_download_growth_dr_parameter_plot,
                 height = input$height_download_growth_dr_parameter_plot,
                 dpi = input$dpi_download_growth_dr_parameter_plot,
                 device = pdf)
        }

      } else {
        ggsave(filename = file, width = input$width_download_growth_dr_parameter_plot,
               height = input$height_download_growth_dr_parameter_plot,
               dpi = input$dpi_download_growth_dr_parameter_plot,
               device = png)
      }

    },
    contentType = ifelse(input$format_download_growth_dr_parameter_plot == ".pdf", "image/pdf", "image/png")

  )

  selected_inputs_parameter_growth_dr_parameter_plot <- reactive({
    results <- results$growth
    if(!is.null(results)){
      if(results$control$dr.method == "spline"){
        dr_parameters <- c('Response(EC50)' = 'yEC50','EC50' = 'EC50')
        if(!is.null(input$number_of_bootstrappings_dr_growth) && !is.na(input$number_of_bootstrappings_dr_growth) && input$number_of_bootstrappings_dr_growth != "" && as.numeric(input$number_of_bootstrappings_dr_growth) > 1){
          dr_parameters <- c(dr_parameters, 'Response(EC50) - Bootstrap' = 'drboot.meanEC50', 'EC50 - Bootstrap' = 'drboot.meanEC50y')
        }
        if(input$log_transform_response_growth){
          dr_parameters <- c(dr_parameters, 'Original response(EC50)' = 'yEC50.orig')
        }
        if(input$log_transform_concentration_growth){
          dr_parameters <- c(dr_parameters, 'Original EC50' = 'EC50.orig')
        }
      } else {
        dr_parameters <- c('Response(EC50)' = 'yEC50','EC50' = 'EC50.Estimate')
      }
      dr_parameters
    }
  })

  selected_inputs_reference_condition_growth_dr_parameter_plot <- reactive({
    results <- results$growth
    results$expdesign$condition
  })

  select_inputs_individual_plots_dose_response_growth_plot_bt <- reactive({
    if (length(results$growth$drFit)>1 && results$growth$control$nboot.dr > 1) names(results$growth$drFit$drBootSplines)
    else return("")
  })

  observe({
    updateSelectInput(inputId = "parameter_dr_parameter_growth_plot",
                      choices = selected_inputs_parameter_growth_dr_parameter_plot()
    )})

  observe({
    updateSelectInput(inputId = "reference_condition_growth_dr_parameter_plot",
                      choices = selected_inputs_reference_condition_growth_dr_parameter_plot()
    )})



    ## Fluorescence Plots: #####
      ### Group Plots ####
  selected_inputs_visualize_fluorescence_group <- reactive({
    results <- results$fluorescence
    if(is.null(results)) return("")
    if(input$plot_group_averages_fluorescence_group_plot){
      select_samples <- results$expdesign$condition
    } else {
      select_samples <- results$expdesign$label
    }
    select_samples
  })

  observe({
    updateSelectInput(session,
                      inputId = "samples_visualize_fluorescence_group",
                      choices = selected_inputs_visualize_fluorescence_group()
    )
  })

  observe({
    updateSelectInput(session,
                      inputId = "groups_visualize_fluorescence_group",
                      choices = selected_inputs_visualize_fluorescence_group()
    )
  })

  observe({
    if (length(results$fluorescence$flFit)>1){
      if(input$data_type_fluorescence_group_plot == "raw") y_axis <- "Fluorescence"
      # if(input$data_type_fluorescence_group_plot == "raw2") y_axis <- "Fluorescence 2"
      if(input$data_type_fluorescence_group_plot == "spline" && results$fluorescence$control$norm_fl){
        y_axis <- "Normalized fluorescence"
      }
      if(input$data_type_fluorescence_group_plot == "spline" && !results$fluorescence$control$norm_fl){
        y_axis <- "Fluorescence"
      }
      # if(input$data_type_fluorescence_group_plot == "spline2") y_axis <- "Fluorescence 2"
      if(input$data_type_fluorescence_group_plot == "norm.fl") y_axis <- "Normalized fluorescence"
      # if(input$data_type_fluorescence_group_plot == "norm.fl2") y_axis <- "Normalized fluorescence 2"
    }
    else {
      y_axis <- ""
    }
    updateSelectInput(inputId = "y_axis_title_fluorescence_group_plot",
                      selected = y_axis
    )
  })

  observe({
    if (length(results$fluorescence$flFit)>1){
      if(input$data_type_fluorescence_group_plot == "norm.fl"  || input$data_type_fluorescence_group_plot == "raw" ){ # || data.type == "raw2" || data.type == "norm.fl2"
        x_axis <- "Time"
      } else if (results$fluorescence$control$x_type == "growth"){
        x_axis <- "growth"
      } else {
        x_axis <- "Time"
      }
    }
    else {
      x_axis <- ""
    }
    updateSelectInput(inputId = "x_axis_title_fluorescence_group_plot",
                      selected = x_axis
    )
  })

  observe({
    if (length(results$fluorescence$flFit)>1){
      if(input$data_type_fluorescence_group_plot == "spline" && results$fluorescence$control$x_type == "growth"){
        shinyjs::hide(id = "plot_group_averages_fluorescence_group_plot")
      } else {
        shinyjs::show(id = "plot_group_averages_fluorescence_group_plot")
      }
    }
  })


  selected_inputs_fluorescence_group_plot_data_type <- reactive({
    results <- results$fluorescence
    selection <- c()
    if(length(results$data$fluorescence) > 1){
      selection <- c(selection, "Raw fluorescence" = "raw")
    }
    # if(length(results$data$fluorescence2) > 1){
    #   selection <- c(selection, "Raw fluorescence 2")
    # }
    if(length(results$data$norm.fluorescence) > 1){
      selection <- c(selection, "Normalized FL" = "norm.fl")
    }
    # if(length(results$data$norm.fluorescence2) > 1){
    #   selection <- c(selection, "Normalized FL2")
    # }
    # if(length(results$data$norm.fluorescence2) > 1){
    #   selection <- c(selection, "Normalized FL2")
    # }
    if(length(results$data$fluorescence) > 1 && "s" %in% results$control$fit.opt){
      selection <- c(selection, "Spline fits FL" = "spline")
    }
    # if(length(results$data$norm.fluorescence2) > 1 && "s" %in% results$control$fit.opt){
    #   selection <- c(selection, "Spline fits FL2")
    # }
    selection
  })

  output$fluorescence_group_plot <- renderPlot({

    results <- results$fluorescence
    if(is.null(input$custom_colors_fluorescence_group_plot) || is.na(input$custom_colors_fluorescence_group_plot) || input$custom_colors_fluorescence_group_plot == ""){
      cols <- NULL
    } else {
      cols <- toupper(unlist(str_split(input$custom_colors_fluorescence_group_plot, ", |; |,|;")))
    }
    if(input$data_type_fluorescence_group_plot == "spline" && results$control$x_type == "growth"){
      plot_mean <-  FALSE
    } else {
      plot_mean <- input$plot_group_averages_fluorescence_group_plot
    }
    if(input$select_string_visualize_fluorescence_group){
      suppressWarnings(
        plot.flFitRes(
          results,
          data.type = input$data_type_fluorescence_group_plot,
          IDs = NULL,
          names = input$select_samples_based_on_string_fluorescence_group_plot,
          conc = input$select_samples_based_on_concentration_fluorescence_group_plot,
          exclude.nm = input$exclude_samples_based_on_string_fluorescence_group_plot,
          exclude.conc = input$exclude_samples_based_on_concentration_fluorescence_group_plot,
          mean = plot_mean,
          deriv = input$plot_derivative_fluorescence_group_plot,
          log.y = input$log_transform_y_axis_fluorescence_group_plot,
          x.lim = c(input$x_range_min_fluorescence_group_plot, input$x_range_max_fluorescence_group_plot),
          y.lim = c(input$y_range_min_fluorescence_group_plot,input$y_range_max_fluorescence_group_plot),
          y.lim.deriv = c(input$y_range_min_derivative_fluorescence_group_plot, input$y_range_max_derivative_fluorescence_group_plot),
          y.title = input$y_axis_title_fluorescence_group_plot,
          x.title = input$x_axis_title_fluorescence_group_plot,
          y.title.deriv = input$y_axis_title_derivative_fluorescence_group_plot,
          lwd = input$line_width_fluorescence_group_plot,
          basesize = input$base_size_fluorescence_group_plot,
          color_groups = input$color_groups_fluorescence_group_plot,
          n.ybreaks = input$nbreaks_fluorescence_group_plot,
          legend.position = input$legend_position_fluorescence_group_plot,
          legend.ncol = input$legend_ncol_fluorescence_group_plot,
          group_pals = input$color_palettes_fluorescence_group_plot,
          colors = cols
        )
      )
    } else {
      suppressWarnings(
        plot.flFitRes(
          results,
          data.type = input$data_type_fluorescence_group_plot,
          IDs = if(input$plot_group_averages_fluorescence_group_plot){
            input$groups_visualize_fluorescence_group
          }else{
            input$samples_visualize_fluorescence_group
          },
          names = NULL,
          conc = input$select_samples_based_on_concentration_fluorescence_group_plot,
          exclude.nm = NULL,
          exclude.conc = input$exclude_samples_based_on_concentration_fluorescence_group_plot,
          mean = plot_mean,
          deriv = input$plot_derivative_fluorescence_group_plot,
          log.y = input$log_transform_y_axis_fluorescence_group_plot,
          x.lim = c(input$x_range_min_fluorescence_group_plot, input$x_range_max_fluorescence_group_plot),
          y.lim = c(input$y_range_min_fluorescence_group_plot,input$y_range_max_fluorescence_group_plot),
          y.lim.deriv = c(input$y_range_min_derivative_fluorescence_group_plot, input$y_range_max_derivative_fluorescence_group_plot),
          y.title = input$y_axis_title_fluorescence_group_plot,
          x.title = input$x_axis_title_fluorescence_group_plot,
          y.title.deriv = input$y_axis_title_derivative_fluorescence_group_plot,
          lwd = input$line_width_fluorescence_group_plot,
          basesize = input$base_size_fluorescence_group_plot,
          n.ybreaks = input$nbreaks_fluorescence_group_plot,
          color_groups = input$color_groups_fluorescence_group_plot,
          legend.position = input$legend_position_fluorescence_group_plot,
          legend.ncol = input$legend_ncol_fluorescence_group_plot,
          colors = cols,
          group_pals = input$color_palettes_fluorescence_group_plot
        )
      )
    }
  })

  observe({
    if(input$plot_derivative_fluorescence_group_plot && (input$data_type_fluorescence_group_plot == 'spline' )) h <- 9
    else h <- 6
    updateSelectInput(inputId = "height_download_fluorescence_group_plot",
                      selected = h
    )
  })

  output$download_fluorescence_group_plot <- downloadHandler(
    filename = function() {
      paste("fluorescence_group_plot",  input$format_download_fluorescence_group_plot, sep="")
    },
    content = function(file) {
      if(input$format_download_growth_dr_parameter_plot == ".pdf"){
        if (requireNamespace("Cairo", quietly = TRUE)) {
          ggsave(filename = file, width = input$width_download_fluorescence_group_plot,
                 height = input$height_download_fluorescence_group_plot,
                 dpi = input$dpi_download_fluorescence_group_plot,
                 device = cairo_pdf)
        } else {
          showModal(modalDialog("Package 'Cairo' must be installed to preserve special characters in the exported PDF image", easyClose = T))
          ggsave(filename = file, width = input$width_download_fluorescence_group_plot,
                 height = input$height_download_fluorescence_group_plot,
                 dpi = input$dpi_download_fluorescence_group_plot,
                 device = pdf)
        }

      } else {
        ggsave(filename = file, width = input$width_download_fluorescence_group_plot,
               height = input$height_download_fluorescence_group_plot,
               dpi = input$dpi_download_fluorescence_group_plot,
               device = png)
      }

    },
    contentType = ifelse(input$format_download_fluorescence_group_plot == ".pdf", "image/pdf", "image/png")
  )

  observe({
    updateSelectInput(inputId = "data_type_fluorescence_group_plot",
                      choices = selected_inputs_fluorescence_group_plot_data_type())
  })

      ### DR Plots Spline ####
  # Hide Spline dose-response plot if dr.method != "spline"
  observe({
    if(length(results$fluorescence$drFit) > 1 &&
       length(results$fluorescence$drFit$drTable) > 1 &&
       results$fluorescence$drFit$control$dr.method == "spline"){
      showTab(inputId = "tabsetPanel_Visualize_Fluorescence", target = "tabPanel_Visualize_Fluorescence_DoseResponse_spline")
    } else {
      hideTab(inputId = "tabsetPanel_Visualize_Fluorescence", target = "tabPanel_Visualize_Fluorescence_DoseResponse_spline")
    }

  })

  output$dose_response_plot_fluorescence_combined <- renderPlot({
    results <- results$fluorescence$drFit
    plot.drFit(results,
               combine=TRUE,
               names = input$select_samples_based_on_string_dose_response_fluorescence_plot,
               exclude.nm = input$exclude_samples_based_on_string_dose_response_fluorescence_plot,
               pch = input$shape_type_dose_response_fluorescence_plot,
               cex.point = input$shape_size_dose_response_fluorescence_plot,
               basesize = input$base_size_dose_response_fluorescence_plot,
               lwd = input$line_width_dose_response_fluorescence_plot,
               ec50line = input$show_ec50_indicator_lines_dose_response_fluorescence_plot,
               y.lim = c(as.numeric(input$y_range_min_dose_response_fluorescence_plot), as.numeric(input$y_range_max_dose_response_fluorescence_plot)),
               x.lim = c(as.numeric(input$x_range_min_dose_response_fluorescence_plot), as.numeric(input$x_range_max_dose_response_fluorescence_plot)),
               y.title = input$y_axis_title_dose_response_fluorescence_plot,
               x.title = input$x_axis_title_dose_response_fluorescence_plot,
               log.y = input$log_transform_y_axis_dose_response_fluorescence_plot,
               log.x = input$log_transform_x_axis_dose_response_fluorescence_plot
    )
  })

  rerun_dr_fluorescence_buttons <- reactive({
    list(input$rerun_dr_fluorescence, input$rerun_dr_fluorescence2, input$rerun_dr_fluorescence3)
  })

  observeEvent(rerun_dr_fluorescence_buttons(), {
    if(input$rerun_dr_fluorescence==0 && input$rerun_dr_fluorescence2==0 && input$rerun_dr_fluorescence3==0){
      return()
    }
    select_options <- c()
    if(any("l" %in% results$fluorescence$control$fit.opt)) select_options <- c(select_options, 'max_slope.linfit', 'lambda.linfit', 'dY.linfit',
                                                                         'A.linfit')
    if(any("s" %in% results$fluorescence$control$fit.opt)) select_options <- c(select_options, 'max_slope.spline', 'lambda.spline',
                                                                         'A.spline', 'dY.spline', 'integral.spline')

    # display a modal dialog with a header, textinput and action buttons
    showModal(
      modalDialog(
        tags$h2('Please enter adjusted parameters'),

        selectInput(inputId = "dr_method_fluorescence_rerun",
                    label = "Method",
                    choices = c("Biosensor response model" = "model",
                                "Response spline fit" = "spline")
        ),
        bsPopover(id = "dr_method_fluorescence_rerun",
                  placement = "right",
                  title = HTML("<em>dr.method</em>"),
                  content = "Fit either a biosensor response model (Meyer et al., 2019) to response-vs.-concentration data, or apply a nonparametric (spline) fit."
        ),
        selectInput(inputId = "response_parameter_fluorescence_rerun",
                    label = "Response Parameter",
                    choices = select_options),
        tags$div(title="Perform a log(x+1) transformation on concentration values.",
                 checkboxInput(inputId = 'log_transform_concentration_fluorescence_rerun',
                               label = 'Log transform concentration')
        ),
        tags$div(title="Perform a log(y+1) transformation on response values.",
                 checkboxInput(inputId = 'log_transform_response_fluorescence_rerun',
                               label = 'Log transform response')
        ),
        bsPopover(id = "response_parameter_fluorescence_rerun", title = HTML("<em>dr.parameter</em>"), content = "Choose the response parameter to be used for creating a dose response curve.", placement = "top"),

        conditionalPanel(
          condition = 'input.dr_method_fluorescence_rerun == "spline"',

          textInput(
            inputId = 'smoothing_factor_fluorescence_dr_rerun',
            label = 'Smoothing factor dose-response splines',
            value = "",
            placeholder = "NULL (choose automatically)"
          ),
          bsPopover(id = "smoothing_factor_fluorescence_dr_rerun", title = HTML("<em>smooth.dr</em>"), content = "\\'spar\\' argument in the R function smooth.spline() used to create the dose response curve."),

          QurvE:::numberInput(
            inputId = 'number_of_bootstrappings_dr_fluorescence_rerun',
            label = 'Number of bootstrappings',
            value = 0,
            min = NA,
            max = NA,
            placeholder = 0
          ),
          bsPopover(id = "number_of_bootstrappings_dr_fluorescence_rerun", title = HTML("<em>nboot.dr</em>"), content = "Optional: Define the number of bootstrap samples for EC50 estimation. Bootstrapping resamples the values in a dataset with replacement and performs a spline fit for each bootstrap sample to determine the EC50.")
        ), #conditionalPanel(condition = 'input.dr_method_fluorescence_rerun == "spline"')
        footer=tagList(
          fluidRow(
            column(12,
                   div(
                     actionButton('submit.rerun.dr.fluorescence', 'Submit'),
                     style="float:right"),
                   div(
                     modalButton('cancel'),
                     style="float:right")

            )
          )
        )
      )
    )
  })

  # Re-run selected linear fit with user-defined parameters upon click on 'submit'
  observeEvent(input$submit.rerun.dr.fluorescence, {
    if(!is.null(results$fluorescence$drFit)){

      showModal(modalDialog("Performing dose-reponse analysis...", footer = NULL))

      # store previous fit in memory
      selected_vals_validate_fluorescence$restore_dr_fluorescence <- results$fluorescence$drFit

      # Re-run fit and store in results object
      flTable <- results$fluorescence$flFit$flTable
      control <- results$fluorescence$drFit$control
      control_new <- results$fluorescence$control <- control

      control_new$dr.method <- input$dr_method_fluorescence_rerun
      control_new$dr.parameter <- input$response_parameter_fluorescence_rerun
      control_new$smooth.dr <- input$smoothing_factor_fluorescence_dr_rerun
      if(control_new$smooth.dr == "" || is.na(control_new$smooth.dr))
        control_new$smooth.dr <- NULL
      control_new$nboot.dr <- input$number_of_bootstrappings_dr_fluorescence_rerun
      control_new$log.x.dr <- input$log_transform_concentration_fluorescence_rerun
      control_new$log.y.dr <- input$log_transform_response_fluorescence_rerun

      if(control_new$dr.method == "spline"){
        try(
          results$fluorescence$drFit <-
            growth.drFit(flTable, control = control_new)
        )
      } else {
        try(
          results$fluorescence$drFit <-
            fl.drFit(flTable, control = control_new)
        )
      }

      # Show [Restore fit] button
      show("restore_dr_fluorescence")
      show("restore_dr_fluorescence2")
      show("restore_dr_fluorescence3")
    }

    removeModal()
  })

  restore_dr_fluorescence_buttons <- reactive({
    list(input$restore_dr_fluorescence, input$restore_dr_fluorescence2, input$restore_dr_fluorescence3)
  })

  # Restore previous linear fit upon click on [Restore Fit]
  observeEvent(restore_dr_fluorescence_buttons(), {
    if(input$restore_dr_fluorescence==0 && input$restore_dr_fluorescence2==0 && input$restore_dr_fluorescence3==0){
      return()
    }
    # store previous fit from memory
    results$fluorescence$drFit <- selected_vals_validate_fluorescence$restore_dr_fluorescence
    hide("restore_dr_fluorescence")
    hide("restore_dr_fluorescence2")
    hide("restore_dr_fluorescence3")
  })

  output$download_dose_response_plot_fluorescence_combined <- downloadHandler(
    filename = function() {
      paste("dose_response_fluorescence_combined",  input$format_download_dose_response_plot_fluorescence_combined, sep="")
    },
    content = function(file) {
      if(input$format_download_dose_response_plot_fluorescence_combined == ".pdf"){
        if (requireNamespace("Cairo", quietly = TRUE)) {
          ggsave(filename = file, width = input$width_download_dose_response_plot_fluorescence_combined,
                 height = input$height_download_dose_response_plot_fluorescence_combined,
                 dpi = input$dpi_download_dose_response_plot_fluorescence_combined,
                 device = cairo_pdf)
        } else {
          showModal(modalDialog("Package 'Cairo' must be installed to preserve special characters in the exported PDF image", easyClose = T))
          ggsave(filename = file, width = input$width_download_dose_response_plot_fluorescence_combined,
                 height = input$height_download_dose_response_plot_fluorescence_combined,
                 dpi = input$dpi_download_dose_response_plot_fluorescence_combined,
                 device = pdf)
        }

      } else {
        ggsave(filename = file, width = input$width_download_dose_response_plot_fluorescence_combined,
               height = input$height_download_dose_response_plot_fluorescence_combined,
               dpi = input$dpi_download_dose_response_plot_fluorescence_combined,
               device = png)
      }

    },
    contentType = ifelse(input$format_download_dose_response_plot_fluorescence_combined == ".pdf", "image/pdf", "image/png")
  )

      ### DR Plots Spline Individual ####


      output$dose_response_fluorescence_plot_individual <- renderPlot({
        results <- results$fluorescence$drFit$drFittedSplines[[ifelse(input$individual_plots_dose_response_fluorescence_plot == "1" || is.null(input$individual_plots_dose_response_fluorescence_plot), 1, input$individual_plots_dose_response_fluorescence_plot)]]

        # Define log-transformation of axes
        if(input$log_transform_y_axis_dose_response_fluorescence_plot &&
           input$log_transform_x_axis_dose_response_fluorescence_plot){
          log <- "xy"
        } else if(input$log_transform_y_axis_dose_response_fluorescence_plot){
          log <- "y"
        } else if(input$log_transform_x_axis_dose_response_fluorescence_plot){
          log <- "x"
        } else {
          log <- ""
        }

        # Define x- and y-axis limits
        if(any(input$y_range_min_dose_response_fluorescence_plot == "",
               input$y_range_max_dose_response_fluorescence_plot == "")){
          y.lim <- NULL
        } else {
          y.lim <- c(as.numeric(input$y_range_min_dose_response_fluorescence_plot),
                    as.numeric(input$y_range_max_dose_response_fluorescence_plot))
        }

        if(any(input$x_range_min_dose_response_fluorescence_plot == "",
               input$x_range_max_dose_response_fluorescence_plot == "")){
          x.lim <- NULL
        } else {
          x.lim <- c(as.numeric(input$x_range_min_dose_response_fluorescence_plot),
                    as.numeric(input$x_range_max_dose_response_fluorescence_plot))
        }
        plot.drFitSpline(results,
                         combine=FALSE,
                         pch = input$shape_type_dose_response_fluorescence_plot,
                         cex.point = input$shape_size_dose_response_fluorescence_plot,
                         lwd = input$line_width_dose_response_fluorescence_plot,
                         ec50line = input$show_ec50_indicator_lines_dose_response_fluorescence_plot,
                         cex.lab = input$axis_size_dose_response_fluorescence_plot,
                         cex.axis = input$lab_size_dose_response_fluorescence_plot,
                         y.title = input$y_axis_title_dose_response_fluorescence_plot,
                         x.title = input$x_axis_title_dose_response_fluorescence_plot,
                         log = log,
                         y.lim = y.lim,
                         x.lim = x.lim)
      })

      output$download_dose_response_fluorescence_plot_individual <- downloadHandler(
        filename = function() {
          paste("dose_response_fluorescence_",  gsub(" \\| ", "_", input$individual_plots_dose_response_fluorescence_plot), input$format_download_dose_response_fluorescence_plot_individual, sep="")
        },
        content = function(file) {
          if(input$format_download_dose_response_fluorescence_plot_individual == ".pdf"){
            if (requireNamespace("Cairo", quietly = TRUE)) {
              Cairo::CairoPDF(file = file,
                              width = input$width_download_dose_response_fluorescence_plot_individual,
                              height = input$height_download_dose_response_fluorescence_plot_individual)
            } else {
              showModal(modalDialog("Package 'Cairo' must be installed to preserve special characters in the exported PDF image", easyClose = T))
              grDevices::pdf(file = file,
                             width = input$width_download_dose_response_fluorescence_plot_individual,
                             height = input$height_download_dose_response_fluorescence_plot_individual)
            }

          } else {
            grDevices::png(file = file,
                width = input$width_download_dose_response_fluorescence_plot_individual,
                height = input$height_download_dose_response_fluorescence_plot_individual,
                units = "in",
                res = input$dpi_download_dose_response_fluorescence_plot_individual)
          }
          results <- results$fluorescence$drFit$drFittedSplines[[ifelse(input$individual_plots_dose_response_fluorescence_plot == "1" || is.null(input$individual_plots_dose_response_fluorescence_plot), 1, input$individual_plots_dose_response_fluorescence_plot)]]

          # Define log-transformation of axes
          if(input$log_transform_y_axis_dose_response_fluorescence_plot &&
             input$log_transform_x_axis_dose_response_fluorescence_plot){
            log <- "xy"
          } else if(input$log_transform_y_axis_dose_response_fluorescence_plot){
            log <- "y"
          } else if(input$log_transform_x_axis_dose_response_fluorescence_plot){
            log <- "x"
          } else {
            log <- ""
          }

          # Define x- and y-axis limits
          if(any(input$y_range_min_dose_response_fluorescence_plot == "",
                 input$y_range_max_dose_response_fluorescence_plot == "")){
            y.lim <- NULL
          } else {
            y.lim <- c(as.numeric(input$y_range_min_dose_response_fluorescence_plot),
                      as.numeric(input$y_range_max_dose_response_fluorescence_plot))
          }

          if(any(input$x_range_min_dose_response_fluorescence_plot == "",
                 input$x_range_max_dose_response_fluorescence_plot == "")){
            x.lim <- NULL
          } else {
            x.lim <- c(as.numeric(input$x_range_min_dose_response_fluorescence_plot),
                      as.numeric(input$x_range_max_dose_response_fluorescence_plot))
          }
          plot.drFitSpline(results,
                           combine=FALSE,
                           pch = input$shape_type_dose_response_fluorescence_plot,
                           cex.point = input$shape_size_dose_response_fluorescence_plot,
                           lwd = input$line_width_dose_response_fluorescence_plot,
                           ec50line = input$show_ec50_indicator_lines_dose_response_fluorescence_plot,
                           cex.lab = input$axis_size_dose_response_fluorescence_plot,
                           cex.axis = input$lab_size_dose_response_fluorescence_plot,
                           y.title = input$y_axis_title_dose_response_fluorescence_plot,
                           x.title = input$x_axis_title_dose_response_fluorescence_plot,
                           log = log,
                           y.lim = y.lim,
                           x.lim = x.lim)
          dev.off()
        },
        contentType = ifelse(input$format_download_dose_response_fluorescence_plot_individual == ".pdf", "image/pdf", "image/png")
      )

      select_inputs_individual_plots_dose_response_fluorescence_plot<- reactive({
        if (length(results$fluorescence$drFit)>1) names(results$fluorescence$drFit$drFittedSplines)
        else return("")
      })

      observe({
        updateSelectInput(inputId = "individual_plots_dose_response_fluorescence_plot",
                          choices = select_inputs_individual_plots_dose_response_fluorescence_plot())
      })

      ### DR Plots Model individual ####

      # Hide Model dose-response plot if dr.method != "model"
      observe({
        if(length(results$fluorescence$drFit) > 1 &&
           length(results$fluorescence$drFit$drTable) > 1 &&
           results$fluorescence$drFit$control$dr.method == "model"){
          showTab(inputId = "tabsetPanel_Visualize_Fluorescence", target = "tabPanel_Visualize_Fluorescence_DoseResponse_model")
        } else {
          hideTab(inputId = "tabsetPanel_Visualize_Fluorescence", target = "tabPanel_Visualize_Fluorescence_DoseResponse_model")
        }

      })

      output$dose_response_model_fluorescence_plot_individual <- renderPlot({
        results <- results$fluorescence$drFit$drFittedModels[[ifelse(input$individual_plots_dose_response_model_fluorescence_plot == "1" || is.null(input$individual_plots_dose_response_model_fluorescence_plot), 1, input$individual_plots_dose_response_model_fluorescence_plot)]]
        # Define log-transformation of axes
        if(input$log_transform_y_axis_dose_response_model_fluorescence_plot &&
           input$log_transform_x_axis_dose_response_model_fluorescence_plot){
          log <- "xy"
        } else if(input$log_transform_y_axis_dose_response_model_fluorescence_plot){
          log <- "y"
        } else if(input$log_transform_x_axis_dose_response_model_fluorescence_plot){
          log <- "x"
        } else {
          log <- ""
        }

        # Define x- and y-axis limits
        if(any(input$y_range_min_dose_response_model_fluorescence_plot == "",
           input$y_range_max_dose_response_model_fluorescence_plot == "")){
          y.lim <- NULL
        } else {
          y.lim <- c(as.numeric(input$y_range_min_dose_response_model_fluorescence_plot),
                    as.numeric(input$y_range_max_dose_response_model_fluorescence_plot))
        }

        if(any(input$x_range_min_dose_response_model_fluorescence_plot == "",
               input$x_range_max_dose_response_model_fluorescence_plot == "")){
          x.lim <- NULL
        } else {
          x.lim <- c(as.numeric(input$x_range_min_dose_response_model_fluorescence_plot),
                    as.numeric(input$x_range_max_dose_response_model_fluorescence_plot))
        }

        plot.drFitFLModel(results,
                        pch = input$shape_type_dose_response_model_fluorescence_plot,
                        cex.point = input$shape_size_dose_response_model_fluorescence_plot,
                        lwd = input$line_width_dose_response_model_fluorescence_plot,
                        ec50line = input$show_ec50_indicator_lines_dose_response_model_fluorescence_plot,
                        log = log,
                        cex.lab = input$axis_size_dose_response_model_fluorescence_plot,
                        cex.axis = input$lab_size_dose_response_model_fluorescence_plot,
                        y.lim = y.lim,
                        x.lim = x.lim
        )
      })

      output$download_dose_response_model_fluorescence_plot_individual <- downloadHandler(
        filename = function() {
          paste("biosensor_model_fluorescence_",  gsub(" \\| ", "_", input$individual_plots_dose_response_model_fluorescence_plot), input$format_download_dose_response_model_fluorescence_plot_individual, sep="")
        },
        content = function(file) {
          if(input$format_download_dose_response_model_fluorescence_plot_individual == ".pdf"){
            if (requireNamespace("Cairo", quietly = TRUE)) {
              ggsave(filename = file, width = input$width_download_dose_response_model_fluorescence_plot_individual,
                     height = input$height_download_dose_response_model_fluorescence_plot_individual,
                     dpi = input$dpi_download_dose_response_model_fluorescence_plot_individual,
                     device = cairo_pdf)
            } else {
              showModal(modalDialog("Package 'Cairo' must be installed to preserve special characters in the exported PDF image", easyClose = T))
              ggsave(filename = file, width = input$width_download_dose_response_model_fluorescence_plot_individual,
                     height = input$height_download_dose_response_model_fluorescence_plot_individual,
                     dpi = input$dpi_download_dose_response_model_fluorescence_plot_individual,
                     device = pdf)
            }

          } else {
            ggsave(filename = file, width = input$width_download_dose_response_model_fluorescence_plot_individual,
                   height = input$height_download_dose_response_model_fluorescence_plot_individual,
                   dpi = input$dpi_download_dose_response_model_fluorescence_plot_individual,
                   device = png)
          }

        },
        contentType = ifelse(input$format_download_dose_response_model_fluorescence_plot_individual == ".pdf", "image/pdf", "image/png")
      )

      select_inputs_individual_plots_dose_response_model_fluorescence_plot<- reactive({
        if (length(results$fluorescence$drFit)>1) names(results$fluorescence$drFit$drFittedModels)
        else return("")
      })
      observe({
        updateSelectInput(inputId = "individual_plots_dose_response_model_fluorescence_plot",
                          choices = select_inputs_individual_plots_dose_response_model_fluorescence_plot())
      })


      ### Parameter Plots ####
      selected_inputs_visualize_parameter_fluorescence_plot <- reactive({
        results <- results$fluorescence
        if(is.null(results)) return("")

        select_samples <- results$expdesign$condition
        select_samples
      })

      observe({
        updateSelectInput(session,
                          inputId = "samples_visualize_parameter_fluorescence_plot",
                          choices = selected_inputs_visualize_parameter_fluorescence_plot()
        )
      })

      fluorescence_parameter_plot <- reactive({
        results <- results$fluorescence

        if (input$normalize_to_reference_fluorescence_parameter_plot){
          reference.conc <- as.numeric(input$reference_concentration_fluorescence_parameter_plot)
          reference.nm <- input$reference_condition_fluorescence_parameter_plot
        } else {
          reference.conc <- NULL
          reference.nm <- NULL
        }
        if(is.null(input$custom_colors_fluorescence_parameter_plot) || is.na(input$custom_colors_fluorescence_parameter_plot) || input$custom_colors_fluorescence_parameter_plot == ""){
          cols <- NULL
        } else {
          cols <- toupper(unlist(str_split(input$custom_colors_fluorescence_parameter_plot, ", |; |,|;")))
        }
        if(input$select_string_visualize_parameter_fluorescence_plot){
          suppressWarnings(
            plot.parameter(results,
                           param = input$parameter_fluorescence_parameter_fluorescence_plot,
                           IDs = NULL,
                           names = input$select_sample_based_on_string_fluorescence_parameter_plot,
                           conc = input$select_sample_based_on_concentration_fluorescence_parameter_plot,
                           exclude.nm = input$exclude_sample_based_on_strings_fluorescence_parameter_plot,
                           exclude.conc = input$exclude_sample_based_on_concentration_fluorescence_parameter_plot,
                           reference.nm = reference.nm,
                           reference.conc = reference.conc,
                           shape.size = input$shape.size_fluorescence_parameter_plot,
                           basesize = input$basesize_fluorescence_parameter_plot,
                           label.size = input$label.size_fluorescence_parameter_plot,
                           legend.position = input$legend_position_fluorescence_parameter_plot,
                           legend.ncol = input$legend_ncol_fluorescence_parameter_plot,
                           order_by_conc = input$sort_by_conc_fluorescence_parameter_plot,
                           colors = cols
            )
          )
        } else {
          suppressWarnings(
            plot.parameter(results,
                           param = input$parameter_fluorescence_parameter_fluorescence_plot,
                           IDs = input$samples_visualize_parameter_fluorescence_plot,
                           names = NULL,
                           conc = input$select_sample_based_on_concentration_fluorescence_parameter_plot,
                           exclude.nm = NULL,
                           exclude.conc = input$exclude_sample_based_on_concentration_fluorescence_parameter_plot,
                           reference.nm = reference.nm,
                           reference.conc = reference.conc,
                           shape.size = input$shape.size_fluorescence_parameter_plot,
                           basesize = input$basesize_fluorescence_parameter_plot,
                           label.size = input$label.size_fluorescence_parameter_plot,
                           legend.position = input$legend_position_fluorescence_parameter_plot,
                           legend.ncol = input$legend_ncol_fluorescence_parameter_plot,
                           order_by_conc = input$sort_by_conc_fluorescence_parameter_plot,
                           colors = cols
            )
          )
        }
      })

      output$fluorescence_parameter_plot <- renderPlot({
        fluorescence_parameter_plot()
      })

      output$download_fluorescence_parameter_plot <- downloadHandler(
        filename = function() {
          paste("fluorescence_parameter_plot",  input$format_download_fluorescence_parameter_plot, sep="")
        },
        content = function(file) {
          if(input$format_download_fluorescence_parameter_plot == ".pdf"){
            if (requireNamespace("Cairo", quietly = TRUE)) {
              ggsave(filename = file, width = input$width_download_fluorescence_parameter_plot,
                     height = input$height_download_fluorescence_parameter_plot,
                     dpi = input$dpi_download_fluorescence_parameter_plot,
                     device = cairo_pdf)
            } else {
              showModal(modalDialog("Package 'Cairo' must be installed to preserve special characters in the exported PDF image", easyClose = T))
              ggsave(filename = file, width = input$width_download_fluorescence_parameter_plot,
                     height = input$height_download_fluorescence_parameter_plot,
                     dpi = input$dpi_download_fluorescence_parameter_plot,
                     device = pdf)
            }

          } else {
            ggsave(filename = file, width = input$width_download_fluorescence_parameter_plot,
                   height = input$height_download_fluorescence_parameter_plot,
                   dpi = input$dpi_download_fluorescence_parameter_plot,
                   device = png)
          }

        },
        contentType = ifelse(input$format_download_fluorescence_parameter_plot == ".pdf", "image/pdf", "image/png")

      )

      selected_inputs_parameter_fluorescence_parameter_plot <- reactive({
        results <- results$fluorescence
        gc_parameters <- c()
        if("s" %in% results$control$fit.opt || "a" %in% results$control$fit.opt){
          if(results$control$biphasic){
            gc_parameters <- c(gc_parameters,
                               'Increase rate (Spline)' = 'max_slope.spline',
                               'Increase rate phase 2 (Spline)' = 'max_slope.spline',
                               'Maximum fluorescence (Spline)' = 'A.spline',
                               'ΔFluorescence (Spline)' = 'dY.spline',
                               'Area under the curve (Spline)' = 'integral.spline')
          } else {
            gc_parameters <- c(gc_parameters,
                               'Increase rate (Spline)' = 'max_slope.spline',
                               'Maximum fluorescence (Spline)' = 'A.spline',
                               'ΔFluorescence (Spline)' = 'dY.spline',
                               'Area under the curve (Spline)' = 'integral.spline')
          }
        }
        if("l" %in% results$control$fit.opt || "a" %in% results$control$fit.opt){
          if(results$control$biphasic){
            gc_parameters <- c(gc_parameters,
                               'Increase rate (linear fit)' = 'max_slope.linfit',
                               'Increase rate phase 2 (linear fit)' = 'max_slope.linfit',
                               'Maximum fluorescence (linear fit)' = 'A.linfit',
                               'ΔFluorescence (linear fit)' = 'dY.linfit')
          } else {
            gc_parameters <- c(gc_parameters,
                               'Increase rate (linear fit)' = 'max_slope.linfit',
                               'Maximum fluorescence (linear fit)' = 'A.linfit',
                               'ΔFluorescence (linear fit)' = 'dY.linfit')
          }
        }
        gc_parameters
      })

      selected_inputs_reference_condition_fluorescence_parameter_plot <- reactive({
        results <- results$fluorescence
        results$expdesign$condition
      })

      select_inputs_reference_concentration_fluorescence_parameter_plot <- reactive({
        results <- results$fluorescence
        results$expdesign$concentration
      })

      observe({
        updateSelectInput(inputId = "parameter_fluorescence_parameter_fluorescence_plot",
                          choices = selected_inputs_parameter_fluorescence_parameter_plot()
        )})

      observe({
        updateSelectInput(inputId = "reference_condition_fluorescence_parameter_plot",
                          choices = selected_inputs_reference_condition_fluorescence_parameter_plot()
        )})

      observe({
        updateSelectInput(inputId = "reference_concentration_fluorescence_parameter_plot",
                          choices = select_inputs_reference_concentration_fluorescence_parameter_plot()
        )})

      observe({
        updateSelectInput(inputId = "individual_plots_dose_response_fluorescence_plot",
                          choices = select_inputs_individual_plots_dose_response_fluorescence_plot())
      })

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

      ### Grid Plot ####
      observe({
        if(!exists("output$more_than_two_conc") || is.null(output$more_than_two_conc))
          return(NULL)
        if(!output$more_than_two_conc){
          updateSelectInput(session = session,
                            inputId = "sort_by_conc_fluorescence_grid_plot",
                            selected = FALSE)
        }
      })

      observe({
        if(input$select_string_visualize_fluorescence_grid)
          updateSelectInput(session = session,
                            inputId = "order_matters_visualize_fluorescence_grid",
                            selected = FALSE)
      })

      selected_inputs_fluorescence_grid_plot_data_type <- reactive({
        results <- results$fluorescence
        selection <- c()
        if(length(results$data$fluorescence) > 1){
          selection <- c(selection, "Raw fluorescence" = "raw")
        }
        if(length(results$data$norm.fluorescence) > 1){
          selection <- c(selection, "Normalized FL" = "norm.fl")
        }
        if(length(results$data$fluorescence) > 1 && "s" %in% results$control$fit.opt){
          selection <- c(selection, "Spline fits FL" = "spline")
        }
        selection
      })

      observe({
        updateSelectInput(inputId = "data_type_fluorescence_grid_plot",
                          choices = selected_inputs_fluorescence_grid_plot_data_type())
      })

      selected_inputs_parameter_fluorescence_grid_plot <- reactive({
        results <- results$fluorescence
        gc_parameters <- c()
        if("s" %in% results$control$fit.opt || "a" %in% results$control$fit.opt){
          if(results$control$biphasic){
            gc_parameters <- c(gc_parameters,
                               'Increase rate (Spline)' = 'max_slope.spline',
                               'Increase rate phase 2 (Spline)' = 'max_slope.spline',
                               'Maximum fluorescence (Spline)' = 'A.spline',
                               'ΔFluorescence (Spline)' = 'dY.spline',
                               'Area under the curve (Spline)' = 'integral.spline')
          } else {
            gc_parameters <- c(gc_parameters,
                               'Increase rate (Spline)' = 'max_slope.spline',
                               'Maximum fluorescence (Spline)' = 'A.spline',
                               'ΔFluorescence (Spline)' = 'dY.spline',
                               'Area under the curve (Spline)' = 'integral.spline')
          }
        }
        if("l" %in% results$control$fit.opt || "a" %in% results$control$fit.opt){
          if(results$control$biphasic){
            gc_parameters <- c(gc_parameters,
                               'Increase rate (linear fit)' = 'max_slope.linfit',
                               'Increase rate phase 2 (linear fit)' = 'max_slope.linfit',
                               'Maximum fluorescence (linear fit)' = 'A.linfit',
                               'ΔFluorescence (linear fit)' = 'dY.linfit')
          } else {
            gc_parameters <- c(gc_parameters,
                               'Increase rate (linear fit)' = 'max_slope.linfit',
                               'Maximum fluorescence (linear fit)' = 'A.linfit',
                               'ΔFluorescence (linear fit)' = 'dY.linfit')
          }
        }
        gc_parameters
      })

      observe({
        updateSelectInput(inputId = "parameter_parameter_grid_plot_fluorescence",
                          choices = selected_inputs_parameter_fluorescence_grid_plot()
        )})

      selected_inputs_visualize_fluorescence_grid <- reactive({
        results <- results$fluorescence
        if(is.null(results)) return("")
        if(input$plot_group_averages_fluorescence_grid_plot){
          select_samples <- results$expdesign$condition
        } else {
          select_samples <- results$expdesign$label
        }
        select_samples
      })

      observe({
        updateSelectInput(session,
                          inputId = "samples_visualize_fluorescence_grid",
                          choices = selected_inputs_visualize_fluorescence_grid()
        )
      })

      observe({
        updateSelectInput(session,
                          inputId = "groups_visualize_fluorescence_grid",
                          choices = selected_inputs_visualize_fluorescence_grid()
        )
      })

      fluorescence_grid_plot <- reactive({

        results <- results$fluorescence

        if(input$y_axis_title_fluorescence_grid_plot == "")
          y_axis_title <- NULL
        else
          y_axis_title <- input$y_axis_title_fluorescence_grid_plot

        if(input$x_axis_title_fluorescence_grid_plot == "")
          x_axis_title <- NULL
        else
          x_axis_title <- input$x_axis_title_fluorescence_grid_plot

        if(input$select_string_visualize_fluorescence_grid){
          suppressWarnings(
            plot.grid(results,
                      data.type = input$data_type_fluorescence_grid_plot,
                      IDs = NULL,
                      sort_by_ID = FALSE,
                      names = input$select_samples_based_on_string_fluorescence_grid_plot,
                      conc = input$select_samples_based_on_concentration_fluorescence_grid_plot,
                      exclude.nm = input$exclude_samples_based_on_string_fluorescence_grid_plot,
                      exclude.conc = input$exclude_samples_based_on_concentration_fluorescence_grid_plot,
                      mean = input$plot_group_averages_fluorescence_grid_plot,
                      log.y = input$log_transform_y_axis_fluorescence_grid_plot,
                      x.lim = c(input$x_range_min_fluorescence_grid_plot, input$x_range_max_fluorescence_grid_plot),
                      y.lim = c(input$y_range_min_fluorescence_grid_plot,input$y_range_max_fluorescence_grid_plot),
                      y.title = y_axis_title,
                      x.title = x_axis_title,
                      n.ybreaks = input$nbreaks_fluorescence_grid_plot,
                      lwd = input$line_width_fluorescence_grid_plot,
                      basesize = input$base_size_fluorescence_grid_plot,
                      pal = input$color_palettes_grid_plot_fluorescence,
                      sort_by_conc = input$sort_by_conc_fluorescence_grid_plot,
                      invert.pal = input$invert_color_palette_grid_plot_fluorescence,
                      legend.lim = c(input$legend_lim_min_fluorescence_grid_plot, input$legend_lim_max_fluorescence_grid_plot),
                      nrow = input$nrows_fluorescence_grid_plot,
                      param = input$parameter_parameter_grid_plot_fluorescence
            )
          )
        }
        else{
          suppressWarnings(
            plot.grid(results,
                      data.type = input$data_type_fluorescence_grid_plot,
                      IDs = if(input$plot_group_averages_fluorescence_grid_plot){
                        input$groups_visualize_fluorescence_grid
                      }else{
                        input$samples_visualize_fluorescence_grid
                      },
                      sort_by_ID = input$order_matters_visualize_fluorescence_grid,
                      names = NULL,
                      conc = input$select_samples_based_on_concentration_fluorescence_grid_plot,
                      exclude.nm = NULL,
                      exclude.conc = input$exclude_samples_based_on_concentration_fluorescence_grid_plot,
                      mean = input$plot_group_averages_fluorescence_grid_plot,
                      log.y = input$log_transform_y_axis_fluorescence_grid_plot,
                      x.lim = c(input$x_range_min_fluorescence_grid_plot, input$x_range_max_fluorescence_grid_plot),
                      y.lim = c(input$y_range_min_fluorescence_grid_plot,input$y_range_max_fluorescence_grid_plot),
                      y.title = y_axis_title,
                      x.title = x_axis_title,
                      n.ybreaks = input$nbreaks_fluorescence_grid_plot,
                      lwd = input$line_width_fluorescence_grid_plot,
                      basesize = input$base_size_fluorescence_grid_plot,
                      pal = input$color_palettes_grid_plot_fluorescence,
                      invert.pal = input$invert_color_palette_grid_plot_fluorescence,
                      sort_by_conc = input$sort_by_conc_fluorescence_grid_plot,
                      legend.lim = c(input$legend_lim_min_fluorescence_grid_plot, input$legend_lim_max_fluorescence_grid_plot),
                      nrow = input$nrows_fluorescence_grid_plot,
                      param = input$parameter_parameter_grid_plot_fluorescence
            )
          )
        }
      })

      output$fluorescence_grid_plot <- renderPlot({
        fluorescence_grid_plot()
      })


      output$download_fluorescence_grid_plot <- downloadHandler(
        filename = function() {
          paste("fluorescence_grid_plot",  input$format_download_fluorescence_grid_plot, sep="")
        },
        content = function(file) {
          if(input$format_download_fluorescence_grid_plot == ".pdf"){
            if (requireNamespace("Cairo", quietly = TRUE)) {
              ggsave(filename = file, width = input$width_download_fluorescence_grid_plot,
                     height = input$height_download_fluorescence_grid_plot,
                     dpi = input$dpi_download_fluorescence_grid_plot,
                     device = cairo_pdf)
            } else {
              showModal(modalDialog("Package 'Cairo' must be installed to preserve special characters in the exported PDF image", easyClose = T))
              ggsave(filename = file, width = input$width_download_fluorescence_grid_plot,
                     height = input$height_download_fluorescence_grid_plot,
                     dpi = input$dpi_download_fluorescence_grid_plot,
                     device = pdf)
            }

          } else {
            ggsave(filename = file, width = input$width_download_fluorescence_grid_plot,
                   height = input$height_download_fluorescence_grid_plot,
                   dpi = input$dpi_download_fluorescence_grid_plot,
                   device = png)
          }

        },
        contentType = ifelse(input$format_download_fluorescence_grid_plot == ".pdf", "image/pdf", "image/png")
      )

      ### DR Plots (Bootstrap) ####
      observe({
        if(length(results$fluorescence$drFit) > 1 && length(results$fluorescence$drFit$drTable) > 1 && results$fluorescence$control$nboot.dr > 1){
          showTab(inputId = "tabsetPanel_Visualize_Fluorescence", target = "tabPanel_Visualize_Fluorescence_DoseResponse_bt")
        } else {
          hideTab(inputId = "tabsetPanel_Visualize_Fluorescence", target = "tabPanel_Visualize_Fluorescence_DoseResponse_bt")
        }
      })

      dose_response_fluorescence_plot_individual_bt <- reactive({
        results <- results$fluorescence$drFit$drBootSplines[[ifelse(input$individual_plots_dose_response_fluorescence_plot_bt == "1" || is.null(input$individual_plots_dose_response_fluorescence_plot_bt), 1, input$individual_plots_dose_response_fluorescence_plot_bt)]]

        plot.drBootSpline(results,
                          pch = input$shape_type_dose_response_fluorescence_plot_bt,
                          cex.point = input$shape_size_dose_response_fluorescence_plot_bt,
                          cex.lab = input$axis_size_dose_response_fluorescence_plot_bt,
                          cex.axis = input$lab_size_dose_response_fluorescence_plot_bt,
                          lwd = input$line_width_dose_response_fluorescence_plot_bt,
                          combine = TRUE

        )
      })

      output$dose_response_fluorescence_plot_individual_bt <- renderPlot({
        dose_response_fluorescence_plot_individual_bt()
      })

      output$download_dose_response_fluorescence_plot_individual_bt <- downloadHandler(
        filename = function() {
          paste("dose_response_boot_fluorescence_",  gsub(" \\| ", "_", input$individual_plots_dose_response_fluorescence_plot_bt), input$format_download_dose_response_fluorescence_plot_individual_bt, sep="")
        },
        content = function(file) {
          if(input$format_download_dose_response_fluorescence_plot_individual_bt == ".pdf"){
            if (requireNamespace("Cairo", quietly = TRUE)) {
              Cairo::CairoPDF(file = file,
                              width = input$width_download_dose_response_fluorescence_plot_individual_bt,
                              height = input$height_download_dose_response_fluorescence_plot_individual_bt)
            } else {
              showModal(modalDialog("Package 'Cairo' must be installed to preserve special characters in the exported PDF image", easyClose = T))
              grDevices::pdf(file = file,
                             width = input$width_download_dose_response_fluorescence_plot_individual_bt,
                             height = input$height_download_dose_response_fluorescence_plot_individual_bt)
            }

          } else {
            grDevices::png(file = file,
                width = input$width_download_dose_response_fluorescence_plot_individual_bt,
                height = input$height_download_dose_response_fluorescence_plot_individual_bt,
                units = "in",
                res = input$dpi_download_dose_response_fluorescence_plot_individual_bt)
          }
          results <- results$fluorescence$drFit$drBootSplines[[input$individual_plots_dose_response_fluorescence_plot_bt]]

          plot.drBootSpline(results,
                            pch = input$shape_type_dose_response_fluorescence_plot_bt,
                            cex.point = input$shape_size_dose_response_fluorescence_plot_bt,
                            cex.lab = input$axis_size_dose_response_fluorescence_plot_bt,
                            cex.axis = input$lab_size_dose_response_fluorescence_plot_bt,
                            lwd = input$line_width_dose_response_fluorescence_plot_bt,
                            combine = TRUE

          )
          dev.off()
        },
        contentType = ifelse(input$format_download_dose_response_fluorescence_plot_individual_bt == ".pdf", "image/pdf", "image/png")
      )


      ### Dual Plot ####
      selected_inputs_visualize_dual_plot <- reactive({
        results <- results$fluorescence
        if(is.null(results)) return("")
        if(input$plot_group_averages_dual_plot){
          select_samples <- results$expdesign$condition
        } else {
          select_samples <- results$expdesign$label
        }
        select_samples
      })

      observe({
        updateSelectInput(session,
                          inputId = "samples_visualize_dual_plot",
                          choices = selected_inputs_visualize_dual_plot()
        )
      })

      observe({
        if(length(results$fluorescence$data$growth) > 1 && length(results$fluorescence$data$fluorescence) > 1){
          showTab(inputId = "tabsetPanel_Visualize_Fluorescence", target = "tabPabel_Visualize_Dual")
        } else {
          hideTab(inputId = "tabsetPanel_Visualize_Fluorescence", target = "tabPabel_Visualize_Dual")
        }
      })

      dual_plot <- reactive({
        results <- results$fluorescence
        if(is.null(input$custom_colors_dual_plot) || is.na(input$custom_colors_dual_plot) || input$custom_colors_dual_plot == ""){
          cols <- NULL
        } else {
          cols <- toupper(unlist(str_split(input$custom_colors_dual_plot, ", |; |,|;")))
        }
        if(input$select_string_visualize_dual_plot){
        suppressWarnings(
          plot.dual(results,
                    IDs = NULL,
                    fluorescence = input$fluorescence_type_dual_plot,
                    names = input$select_samples_based_on_string_dual_plot,
                    conc = input$select_samples_based_on_concentration_dual_plot,
                    exclude.nm = input$exclude_samples_based_on_string_dual_plot,
                    exclude.conc = input$exclude_samples_based_on_concentration_dual_plot,
                    mean = input$plot_group_averages_dual_plot,
                    log.y.growth = input$log_transform_y_axis_growth_dual_plot,
                    log.y.fl = input$log_transform_y_axis_fluorescence_dual_plot,
                    x.lim = c(input$x_range_min_dual_plot, input$x_range_max_dual_plot),
                    y.lim.growth = c(input$y_range_min_growth_dual_plot,input$y_range_max_growth_dual_plot),
                    y.lim.fl = c(input$y_range_min_fluorescence_dual_plot,input$y_range_max_fluorescence_dual_plot),
                    y.title.fl = input$y_axis_title_fluorescence_dual_plot,
                    y.title.growth = input$y_axis_title_growth_dual_plot,
                    x.title = input$x_axis_title_dual_plot,
                    n.ybreaks = input$nbreaks_dual_plot,
                    lwd = input$line_width_dual_plot,
                    basesize = input$base_size_dual_plot,
                    legend.position = input$legend_position_dual_plot,
                    legend.ncol = input$legend_ncol_dual_plot,
                    color_groups = input$color_groups_dual_plot,
                    colors = cols,
                    group_pals = input$color_palettes_dual_plot
          )
        )
        } else {
          suppressWarnings(
            plot.dual(results,
                      fluorescence = input$fluorescence_type_dual_plot,
                      IDs = input$samples_visualize_dual_plot,
                      names = NULL,
                      conc = input$select_samples_based_on_concentration_dual_plot,
                      exclude.nm = NULL,
                      exclude.conc = input$exclude_samples_based_on_concentration_dual_plot,
                      mean = input$plot_group_averages_dual_plot,
                      log.y.growth = input$log_transform_y_axis_growth_dual_plot,
                      log.y.fl = input$log_transform_y_axis_fluorescence_dual_plot,
                      x.lim = c(input$x_range_min_dual_plot, input$x_range_max_dual_plot),
                      y.lim.growth = c(input$y_range_min_growth_dual_plot,input$y_range_max_growth_dual_plot),
                      y.lim.fl = c(input$y_range_min_fluorescence_dual_plot,input$y_range_max_fluorescence_dual_plot),
                      y.title.fl = input$y_axis_title_fluorescence_dual_plot,
                      y.title.growth = input$y_axis_title_growth_dual_plot,
                      x.title = input$x_axis_title_dual_plot,
                      n.ybreaks = input$nbreaks_dual_plot,
                      lwd = input$line_width_dual_plot,
                      basesize = input$base_size_dual_plot,
                      color_groups = input$color_groups_dual_plot,
                      legend.position = input$legend_position_dual_plot,
                      legend.ncol = input$legend_ncol_dual_plot,
                      group_pals = input$color_palettes_dual_plot,
                      colors = cols
            )
          )
        }
      })

      output$dual_plot <- renderPlot({
        dual_plot()
      })

      output$download_dual_plot <- downloadHandler(
        filename = function() {
          paste("fluorescence_group_plot",  input$format_download_dual_plot, sep="")
        },
        content = function(file) {
          if(input$format_download_dual_plot == ".pdf"){
            if (requireNamespace("Cairo", quietly = TRUE)) {
              ggsave(filename = file, width = input$width_download_dual_plot,
                     height = input$height_download_dual_plot,
                     dpi = input$dpi_download_dual_plot,
                     device = cairo_pdf)
            } else {
              showModal(modalDialog("Package 'Cairo' must be installed to preserve special characters in the exported PDF image", easyClose = T))
              ggsave(filename = file, width = input$width_download_dual_plot,
                     height = input$height_download_dual_plot,
                     dpi = input$dpi_download_dual_plot,
                     device = pdf)
            }

          } else {
            ggsave(filename = file, width = input$width_download_dual_plot,
                   height = input$height_download_dual_plot,
                   dpi = input$dpi_download_dual_plot,
                   device = png)
          }

        },
        contentType = ifelse(input$format_download_dual_plot == ".pdf", "image/pdf", "image/png")
      )

      selected_inputs_fluorescence_type_dual_plot <- reactive({
        results <- results$fluorescence
        selection <- c()
        if(length(results$data$fluorescence) > 1){
          selection <- c(selection, "Raw fluorescence" = "fl")
        }
        # if(length(results$data$fluorescence2) > 1){
        #   selection <- c(selection, "Raw fluorescence 2" = "fl2")
        # }
        if(length(results$data$norm.fluorescence) > 1){
          selection <- c(selection, "Normalized FL" = "norm.fl")
        }
        # if(length(results$data$norm.fluorescence2) > 1){
        #   selection <- c(selection, "Normalized FL2" = "norm.fl2")
        # }
        selection
      })

      observe({
        updateSelectInput(inputId = "fluorescence_type_dual_plot",
                          choices = selected_inputs_fluorescence_type_dual_plot())
      })

      ### DR Parameter Plots ####
      observe({
        if(length(results$fluorescence$drFit) > 1 && length(results$fluorescence$drFit$drTable) > 1){
          showTab(inputId = "tabsetPanel_Visualize_Fluorescence", target = "tabPanel_Visualize_Fluorescence_DoseResponseParameters")
        } else {
          hideTab(inputId = "tabsetPanel_Visualize_Fluorescence", target = "tabPanel_Visualize_Fluorescence_DoseResponseParameters")
        }
      })

      fluorescence_dr_parameter_plot <- reactive({
        results <- results$fluorescence

        if (input$normalize_to_reference_fluorescence_dr_parameter_plot){
          reference.nm <- input$reference_condition_fluorescence_dr_parameter_plot
        } else {
          reference.nm <- NULL
        }
        plot.dr_parameter(results,
                          param = input$parameter_dr_parameter_fluorescence_plot,
                          names = input$select_sample_based_on_string_fluorescence_dr_parameter_plot,
                          exclude.nm = input$exclude_sample_based_on_strings_fluorescence_dr_parameter_plot,
                          reference.nm = reference.nm,
                          basesize = input$basesize_fluorescence_dr_parameter_plot,
                          label.size = input$label.size_fluorescence_dr_parameter_plot
        )
      })

      output$fluorescence_dr_parameter_plot <- renderPlot({
        fluorescence_dr_parameter_plot()
      })

      output$download_fluorescence_dr_parameter_plot <- downloadHandler(
        filename = function() {
          paste("fluorescence_dr_parameter_plot",  input$format_download_fluorescence_dr_parameter_plot, sep="")
        },
        content = function(file) {
          if(input$format_download_fluorescence_dr_parameter_plot == ".pdf"){
            if (requireNamespace("Cairo", quietly = TRUE)) {
              ggsave(filename = file, width = input$width_download_fluorescence_dr_parameter_plot,
                     height = input$height_download_fluorescence_dr_parameter_plot,
                     dpi = input$dpi_download_fluorescence_dr_parameter_plot,
                     device = cairo_pdf)
            } else {
              showModal(modalDialog("Package 'Cairo' must be installed to preserve special characters in the exported PDF image", easyClose = T))
              ggsave(filename = file, width = input$width_download_fluorescence_dr_parameter_plot,
                     height = input$height_download_fluorescence_dr_parameter_plot,
                     dpi = input$dpi_download_fluorescence_dr_parameter_plot,
                     device = pdf)
            }

          } else {
            ggsave(filename = file, width = input$width_download_fluorescence_dr_parameter_plot,
                   height = input$height_download_fluorescence_dr_parameter_plot,
                   dpi = input$dpi_download_fluorescence_dr_parameter_plot,
                   device = png)
          }

        },
        contentType = ifelse(input$format_download_fluorescence_dr_parameter_plot == ".pdf", "image/pdf", "image/png")

      )

      selected_inputs_parameter_fluorescence_dr_parameter_plot <- reactive({
        results <- results$fluorescence
        if(is.null(results)) return(NULL)
        if(results$control$dr.method == "spline"){
          dr_parameters <- c('Response(EC50)' = 'yEC50','EC50' = 'EC50')
          if(results$control$nboot.dr > 1){
            dr_parameters <- c(dr_parameters, 'Response(EC50) - Bootstrap' = 'drboot.meanEC50', 'EC50 - Bootstrap' = 'drboot.meanEC50y')
          }
          if(input$log_transform_response_fluorescence){
            dr_parameters <- c(dr_parameters, 'Original response(EC50)' = 'yEC50.orig')
          }
          if(input$log_transform_concentration_fluorescence){
            dr_parameters <- c(dr_parameters, 'Original EC50' = 'EC50.orig')
          }
        } else {
          dr_parameters <- c('Leakiness' = 'y.min','Sensitivity' = 'K', 'Fold change' = 'fc', 'Maximum response' = 'y.max')
        }
        dr_parameters
      })

      selected_inputs_reference_condition_fluorescence_dr_parameter_plot <- reactive({
        results <- results$fluorescence
        results$expdesign$condition
      })

      select_inputs_individual_plots_dose_response_fluorescence_plot <- reactive({
        if (length(results$fluorescence$drFit)>1) names(results$fluorescence$drFit$drFittedSplines)
        else return("")
      })

      select_inputs_individual_plots_dose_response_fluorescence_plot_bt <- reactive({
        if (length(results$fluorescence$drFit)>1 && results$fluorescence$control$nboot.dr > 1) names(results$fluorescence$drFit$drBootSplines)
        else return("")
      })

      observe({
        updateSelectInput(inputId = "parameter_dr_parameter_fluorescence_plot",
                          choices = selected_inputs_parameter_fluorescence_dr_parameter_plot()
        )})

      observe({
        updateSelectInput(inputId = "reference_condition_fluorescence_dr_parameter_plot",
                          choices = selected_inputs_reference_condition_fluorescence_dr_parameter_plot()
        )})

      observe({
        updateSelectInput(inputId = "individual_plots_dose_response_fluorescence_plot_bt",
                          choices = select_inputs_individual_plots_dose_response_fluorescence_plot_bt()
        )})

  # Report ####
  volumes <- getVolumes() # this makes the directory at the base of your computer.

  observe({
    if(!is.null(results$growth)){
      showTab(inputId = "tabsetPanel_Report", target = "tabPanel_report_growth")
    } else {
      hideTab(inputId = "tabsetPanel_Report", target = "tabPanel_report_growth")
    }
    if(!is.null(results$fluorescence)){
      showTab(inputId = "tabsetPanel_Report", target = "tabPanel_report_fluorescence")
    } else {
      hideTab(inputId = "tabsetPanel_Report", target = "tabPanel_report_fluorescence")
    }
  })

  observeEvent(input$run_report_fix, {
    # check if pandoc is recognized
    showModal(
      modalDialog("Running report troubleshooting...", footer = NULL)
    )
    pandoc.path <- rmarkdown::find_pandoc()$dir
    Sys.setenv(RSTUDIO_PANDOC = pandoc.path)
    Sys.setenv(PANDOC_PATH = pandoc.path)

    # reinstall tinytex
    tinytex::tlmgr_update()
    tinytex::reinstall_tinytex()
    removeModal()
  })

    ## Report Growth ####

  output$download_report_growth_pdf <- downloadHandler(
    filename = function() {
      paste0("GrowthReport.", input$report_filetype_growth)
    },
    content = function(file) {
      if (!requireNamespace("tinytex", quietly = TRUE)) {
        showModal(
          modalDialog("Please install package 'tinytex' to render PDF reports.", easyClose = T)
        )
      } else if(!tinytex::is_tinytex()){
        # stop("TinyTex was not found on your system. To render PDF reports, please execute tinytex::install_tinytex().")
        showModal(
          modalDialog(HTML("TinyTeX is required to render PDF reports but was not found on your system. Installing TinyTeX...<br><br>(This requires and active internet connection and will take several minutes)"), footer = NULL)
        )
        update.packages(ask = FALSE, checkBuilt = TRUE, repos='http://cran.us.r-project.org')
        tinytex::install_tinytex()
        tinytex::tlmgr_update()
        tinytex::reinstall_tinytex()
        removeModal()
        try(
          suppressWarnings(
            suppressMessages(
              growth.report(grofit = results$growth,
                            out.dir = gsub(paste0("[\\\\|", .Platform$file.sep, "]file.+$"), "", file),
                            out.nm = gsub(paste0("^.+[\\\\|", .Platform$file.sep, "]"), "", file),
                            ec50 = ifelse(length(results$growth$drFit) > 1 && length(results$growth$drFit$drTable) > 1, TRUE, FALSE),
                            format = input$report_filetype_growth,
                            export = FALSE,
                            mean.grp = "all")
            )
          )
        )
      } else {
        try(
          suppressWarnings(
            suppressMessages(
              growth.report(grofit = results$growth,
                            out.dir = gsub(paste0("[\\\\|", .Platform$file.sep, "]file.+$"), "", file),
                            out.nm = gsub(paste0("^.+[\\\\|", .Platform$file.sep, "]"), "", file),
                            ec50 = ifelse(length(results$growth$drFit) > 1 && length(results$growth$drFit$drTable) > 1, TRUE, FALSE),
                            format = input$report_filetype_growth,
                            export = FALSE,
                            mean.grp = "all")
            )
          )
        )
      }
    },
    contentType = paste0(".", input$report_filetype_growth)
  )

  output$download_report_growth_html <- downloadHandler(
    filename = function() {
      paste0("GrowthReport.", input$report_filetype_growth)
    },
    content = function(file) {
      try(
        suppressWarnings(
          suppressMessages(
            growth.report(grofit = results$growth,
                          out.dir = gsub(paste0("[\\\\|", .Platform$file.sep, "]file.+$"), "", file),
                          out.nm = gsub(paste0("^.+[\\\\|", .Platform$file.sep, "]"), "", file),
                          ec50 = ifelse(length(results$growth$drFit) > 1 && length(results$growth$drFit$drTable) > 1, TRUE, FALSE),
                          format = input$report_filetype_growth,
                          export = FALSE,
                          mean.grp = "all")
          )
        )
      )
    },
    contentType = paste0(".", input$report_filetype_growth)
  )



    ## Report Fluorescence ####

  output$download_report_fluorescence_pdf <- downloadHandler(
    filename = function() {
      paste0("FluorescenceReport.", input$report_filetype_fluorescence)
    },
    content = function(file) {
      if (!requireNamespace("tinytex", quietly = TRUE)) {
        showModal(
          modalDialog("Please install package 'tinytex' to render PDF reports.", easyClose = T)
        )
      } else if(!tinytex::is_tinytex()){
        # stop("TinyTex was not found on your system. To render PDF reports, please execute tinytex::install_tinytex().")
        showModal(
          modalDialog(HTML("TinyTeX is required to render PDF reports but was not found on your system. Installing TinyTeX...<br><br>(This requires and active internet connection and will take several minutes)"), footer = NULL)
        )
        update.packages(ask = FALSE, checkBuilt = TRUE, repos='http://cran.us.r-project.org')
        tinytex::install_tinytex()
        tinytex::tlmgr_update()
        tinytex::reinstall_tinytex()
        removeModal()
        try(
          suppressWarnings(
            suppressMessages(
              fl.report(flFitRes = results$fluorescence,
                        out.dir = gsub(paste0("[\\\\|", .Platform$file.sep, "]file.+$"), "", file),
                        out.nm = gsub(paste0("^.+[\\\\|", .Platform$file.sep, "]"), "", file),
                        ec50 = ifelse(length(results$fluorescence$drFit) > 1 && length(results$fluorescence$drFit$drTable) > 1, TRUE, FALSE),
                        format = input$report_filetype_fluorescence,
                        export = FALSE,
                        mean.grp = "all")
            )
          )
        )
      } else {
        try(
          suppressWarnings(
            suppressMessages(
              fl.report(flFitRes = results$fluorescence,
                        out.dir = gsub(paste0("[\\\\|", .Platform$file.sep, "]file.+$"), "", file),
                        out.nm = gsub(paste0("^.+[\\\\|", .Platform$file.sep, "]"), "", file),
                        ec50 = ifelse(length(results$fluorescence$drFit) > 1 && length(results$fluorescence$drFit$drTable) > 1, TRUE, FALSE),
                        format = input$report_filetype_fluorescence,
                        export = FALSE,
                        mean.grp = "all")
            )
          )
        )
      }
    },
    contentType = paste0(".", input$report_filetype_fluorescence)
  )

  output$download_report_fluorescence_html <- downloadHandler(
    filename = function() {
      paste0("FluorescenceReport.", input$report_filetype_fluorescence)
    },
    content = function(file) {
      try(
        suppressWarnings(
          suppressMessages(
            fl.report(flFitRes = results$fluorescence,
                      out.dir = gsub(paste0("[\\\\|", .Platform$file.sep, "]file.+$"), "", file),
                      out.nm = gsub(paste0("^.+[\\\\|", .Platform$file.sep, "]"), "", file),
                      ec50 = ifelse(length(results$fluorescence$drFit) > 1 && length(results$fluorescence$drFit$drTable) > 1, TRUE, FALSE),
                      format = input$report_filetype_fluorescence,
                      export = FALSE,
                      mean.grp = "all")
          )
        )
      )
    },
    contentType = paste0(".", input$report_filetype_fluorescence)
  )

  # Bug report message ####
  github_url <- a("QurvE Github", href="https://github.com/NicWir/QurvE/issues")
  output$bug_report <- renderUI({
    tagList("Please report bugs and user feedback at:", github_url)
  })

  # Export RData files ####
  observe({
    if(!is.null(results$growth)){
      showTab(inputId = "tabsetPanel_Export_Data", target = "tabPanel_export_data_growth")
    } else {
      hideTab(inputId = "tabsetPanel_Export_Data", target = "tabPanel_export_data_growth")
    }
    if(!is.null(results$fluorescence)){
      showTab(inputId = "tabsetPanel_Export_Data", target = "tabPanel_export_data_fluorescence")
    } else {
      hideTab(inputId = "tabsetPanel_Export_Data", target = "tabPanel_export_data_fluorescence")
    }
  })

    ## RData Growth ####
  output$export_RData_growth <- downloadHandler(
    filename = function() {
      paste0("GrowthResults.RData")
    },
    content = function(file) {
      try(
        suppressWarnings(
          suppressMessages(
            QurvE:::export_RData(object = results$growth,
                         out.dir = gsub(paste0("[\\\\|", .Platform$file.sep, "]file.+$"), "", file),
                         out.nm = gsub(paste0("^.+[\\\\|", .Platform$file.sep, "]"), "", file)
            )
          )
        )
      )
    },
    contentType = paste0(".RData")
  )

    ## RData Fluorescence ####
  output$export_RData_fluorescence <- downloadHandler(
    filename = function() {
      paste0("FluorescenceResults.RData")
    },
    content = function(file) {
      try(
        suppressWarnings(
          suppressMessages(
            QurvE:::export_RData(object = results$fluorescence,
                         out.dir = gsub(paste0("[\\\\|", .Platform$file.sep, "]file.+$"), "", file),
                         out.nm = gsub(paste0("^.+[\\\\|", .Platform$file.sep, "]"), "", file)
            )
          )
        )
      )
    },
    contentType = paste0(".RData")
  )

  # Import RData files
    ## Growth
    output$RData_growth_uploaded <- reactive({
    if(is.null(input$import_RData_growth)) return(FALSE)
    else return(TRUE)
    })
    outputOptions(output, 'RData_growth_uploaded', suspendWhenHidden=FALSE)

    observeEvent(input$read_RData_growth,{
      showModal(modalDialog("Reading data file...", footer=NULL))
      try(load(input$import_RData_growth$datapath))
      try(results$growth <- object)
      if(!is.null("results$growth")){
        # ENABLE DISABLED PANELS AFTER RUNNING COMPUTATION
        shinyjs::enable(selector = "#navbar li a[data-value=tabPanel_Export_RData]")
        shinyjs::enable(selector = "#navbar li a[data-value=tabPanel_Report]")
        shinyjs::enable(selector = "#navbar li a[data-value=navbarMenu_Visualize]")
        shinyjs::enable(selector = "#navbar li a[data-value=navbarMenu_Results]")
        shinyjs::enable(selector = "#navbar li a[data-value=navbarMenu_Validate]")
        removeModal()
      } else showModal(modalDialog(geterrmessage(), footer=NULL, easyClose = T))
    })
    ## Fluorescence
    output$RData_fluorescence_uploaded <- reactive({
      if(is.null(input$import_RData_fluorescence)) return(FALSE)
      else return(TRUE)
    })
    outputOptions(output, 'RData_fluorescence_uploaded', suspendWhenHidden=FALSE)

    observeEvent(input$read_RData_fluorescence,{
      showModal(modalDialog("Reading data file...", footer=NULL))
      try(load(input$import_RData_fluorescence$datapath))
      try(results$fluorescence <- object)

      if(!is.null("results$fluorescence")){
        ## ENABLE DISABLED PANELS AFTER RUNNING COMPUTATION
        shinyjs::enable(selector = "#navbar li a[data-value=tabPanel_Export_RData]")
        shinyjs::enable(selector = "#navbar li a[data-value=tabPanel_Report]")
        shinyjs::enable(selector = "#navbar li a[data-value=navbarMenu_Visualize]")
        shinyjs::enable(selector = "#navbar li a[data-value=navbarMenu_Results]")
        shinyjs::enable(selector = "#navbar li a[data-value=navbarMenu_Validate]")
        removeModal()
      } else showModal(modalDialog(geterrmessage(), footer=NULL, easyClose = T))
    })
  # Show content after initializing ####
  load_data()

  # Close the app when the session completes ####
  if(!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }

  # Ensure that the application will stop the websocket server started by shiny::runApp() and the underlying R process when the browser window is closed.
  # session$onSessionEnded(function() {
  #   stopApp()
  # })

}




shinyApp(ui = ui, server = server)
