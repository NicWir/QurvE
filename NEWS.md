# QurvE 1.1.1

## Enhancements

* `read_data()` now automatically assigns increasing replicate numbers to sample groups (identical "Description" and "Concentration") if all of their replicate values are missing or NA.

## Bug fixes

* fixed bug during the parsing of tidy data if the combinations of Description/Concentration/Replicate were not unique (e.g., multiple 'Blank' samples, Issue #11).
* fixed bug that occured in tidy_to_custom() with format='row'.
* bug fixes in report creation (Issue #9); safer handling of image file creation if erros occur.
* fixed missing values in grouped plots with mean=True if some replicates contained missing measurement (Issue #9).
* In group plots: remove rows in data groups in which all values are NA to avoid gaps in plots.
* bug fix in group plots for data series of unequal lengths.
* Factors are removed from tidy dataframes to prevent incompatibilities.
* fix to correctly assign time vectors to samples during tidy data parsing, if multiple time vectors were present.
* fixed bug while assembling 'gcTable' if only a single sample was processed.
* revised roxygen documentation to comply with new CRAN checks

# QurvE 1.1

## Enhancements

* `read_data()` now accepts files or dataframe objects in tidy format
* the shiny app now allows inspection of code used to run workflows and generate plots
* Parametric fits have Root Mean Squared Error (RMSE) added to the calculated parameters. The RMSE for each model fit has been also added to `gcTable` and the respective table within the shiny app.
* `TinyTeX` is only installed from within the shiny app if the user explicitly approves.
* added a [Q&A and Troubleshooting] vignette
* added citation for publication *Wirth et al. (2023)*

## Bug fixes

* the [sort by concentration] button for parameter plots in the shiny app now also is shown for data parsed from raw inputs, not only data parsed in custom format.
* [Select read for fluorescence normalization] is not shown if Fluorescence is *ignored*.
* Data is parsed correctly from CSV files if values are framed by explicit quotes

# QurvE 1.0.1

## Enhancements

* Report template .Rmd files are now copied into tempdir() before rendering to avoid bugs with restrictive write permissions.
* In shiny app: Don’t show “Select Read for fluorescence normalization” if Fluorescence is [Ignore].

## Bug fixes

* Issue #4: In shiny app, checkbox 'sort by concentration' did not show after loading an old RData file.
* Fixed bug in Gen5/Gen6 parser if only one read type was present.

# QurvE 1.0

* Added a `NEWS.md` file to track changes to the package.
