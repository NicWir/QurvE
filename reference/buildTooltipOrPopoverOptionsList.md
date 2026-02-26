# Custom tooltip function

This function creates a custom tooltip for a given element in a Shiny
application. The implementation is based on the shinyBS package.

## Usage

``` r
buildTooltipOrPopoverOptionsList(title, placement, trigger, options, content)
```

## Arguments

- title:

  The text for the tooltip's title.

- placement:

  Placement of the tooltip. One of 'top', 'bottom', 'left', or 'right'.

- trigger:

  The events that trigger the tooltip. One or more of 'click', 'hover',
  'focus', or 'manual'.

- options:

  A list of additional options for the tooltip.

- content:

  Optional HTML content for the tooltip.

## Value

A list of tooltip options to be used in the Shiny application.

## See also

<https://CRAN.R-project.org/package=shinyBS>

## Examples
