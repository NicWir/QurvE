# Convert a tidy data frame to a custom QurvE format

This function converts a data frame in "tidy" format into the custom
format used by QurvE (row format). The provided "tidy" data has columns
for "Description", "Concentration", "Replicate", and "Values", with one
row per time point and sample. Alternatively, the function converts data
in custom QurvE column format into row format (if
`data.format = "col"`).

## Usage

``` r
tidy_to_custom(df, data.format = "col")
```

## Arguments

- df:

  A data frame in tidy format, containing "Time", "Description", and
  either "Values" or "Value" columns. Optionally, meta information
  provided in columns "Replicate" and "Concentration" is used.

- data.format:

  (Character string) `"col"` (the default) or `"row"`. Only relevant if
  data is not provided in "tidy" format but has been prepared into the
  custom QurvE data format.

## Value

A data frame in the custom format (row format) used by QurvE.

## Examples

``` r
# Create a tidy data frame with two samples, five concentrations, three
# replicates, and five time points
samples <- c("Sample 1", "Sample 2")
concentrations <- c(0.1, 0.5, 1, 2, 5)
time_points <- c(1, 2, 3, 4, 5)
n_replicates <- 3

df <- expand.grid(
   Description = c("Sample 1", "Sample 2"),
   Concentration = c(0.1, 0.5, 1, 2, 5),
   Time = c(1, 2, 3, 4, 5),
   Replicate = 1:3)

df$Value <- abs(rnorm(nrow(df)))

df_formatted <- tidy_to_custom(df)
```
