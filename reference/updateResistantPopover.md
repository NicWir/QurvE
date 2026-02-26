# Create an update-resistant popover for a Shiny element

This function creates a popover that is resistant to updates in the
associated Shiny element. It adds an event listener to the specified
element, which reinstalls the popover whenever a child of the element
changes.

## Usage

``` r
updateResistantPopover(
  id,
  title,
  content,
  placement = "bottom",
  trigger = "hover",
  options = NULL
)
```

## Arguments

- id:

  The id of the Shiny element to which the popover is attached.

- title:

  The title of the popover.

- content:

  The content of the popover.

- placement:

  The placement of the popover relative to the Shiny element (default:
  "bottom"). Possible values are "top", "bottom", "left", and "right".

- trigger:

  The event that triggers the display of the popover (default: "hover").
  Possible values are "hover", "focus", and "click".

- options:

  A list of additional options for the popover.

## Value

A Shiny HTML tag that contains the JavaScript code for creating the
update-resistant popover.

## Author

K. Rohde (stack overflow)

## Examples

``` r
if (FALSE) { # \dontrun{
library(shiny)
library(shinyBS)

ui <- shinyUI(fluidPage(
  selectInput("Main2_1","Label","abc",  selectize = TRUE, multiple = TRUE),
  updateResistantPopover("Main2_1", "Label", "content", placement = "right", trigger = "focus"),
  actionButton("destroy", "destroy!")
))

server <- function(input, output, session){
  observeEvent(input$destroy, {
    updateSelectInput(session, "Main2_1", choices="foo")
  })
}

shinyApp(ui, server)
} # }
```
