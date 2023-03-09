# QurvE 1.0.1

## Enhancements

* Report template .Rmd files are now copied into tempdir() before rendering to avoid bugs with restrictive write permissions.
* In shiny app: Don’t show “Select Read for fluorescence normalization” if Fluorescence is [Ignore].

## Bug fixes

* Issue #4: In shiny app, checkbox 'sort by concentration' did not show after loading an old RData file.
* Fixed bug in Gen5/Gen6 parser if only one read type was present.

# QurvE 1.0

* Added a `NEWS.md` file to track changes to the package.
