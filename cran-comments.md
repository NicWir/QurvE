## R CMD check results

0 errors \| 0 warnings \| 2 notes

-   This is a new release.

NOTE 1

> NEW submission
>
> Found the following (possibly) invalid URLs:\
> URL: <https://doi.org/10.1111/1751-7915.14063>\
> From: inst/doc/vignette_fluorescence.html\
> Status: 503\
> Message: Service Unavailable

I cannot understand what causes this note. The indicated URL seems to work fine.
__________

NOTE 2

> installed size is 6.3Mb\
> sub-directories of 1Mb or more:\
> doc 2.9Mb\
> shiny_app 1.4Mb

The package contains three detailed vignettes/manuals formatted in HTML. They are built quickly with images available online but require a certain file size. I hope that this is still acceptable.

The accompanying 'shiny app' represents an extensive front-end solution that provides access to all the funtionalities of the `QurvE`package. The `app.R` alone has a size of 892 KB and two high-quality pictures have a combined file size of 483 KB
