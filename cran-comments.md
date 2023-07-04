>   Size of tarball: 5209386 bytes

> Pls reduce to less than 5 MB for a CRAN package.

Done! I reduced the filesize of some images and removed lines from example files that are not essential to demonstrate QurvE's functionalities. New size: 4.8 Mb

____________________

# Previous submission (version 1.0)

## Publication LaTeX issue

> Error is:
$ R CMD Rd2pdf QurvE
Hmm ... looks like a package
Converting Rd files to LaTeX .......
Creating pdf output from LaTeX ...
Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  :
   Running 'texi2dvi' on 'Rd2.tex' failed.
LaTeX errors:
! Package pdftex.def Error: File `Data\T1\textunderscorelayout.jpg' not
found:
using draft setting.
This is from
$ grep -r layout.jpg QurvE
QurvE/man/read_data.Rd:\figure{Data_layout.jpg}
Can you please rename Data_layout.jpg to, e.g., Data.Layout.jpg or 
Data-layout.jpg and resubmit?

Done! renamed the file to Data-layout.jpg


## Second submission

> Please always write package names, software names and API (application programming interface) names in single quotes in title and description.

thanks, fixed!

> If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file in the form authors (year) <doi:...> authors (year) <arXiv:...> authors (year, ISBN:...) or if those are not available: <https:...> with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for auto-linking. (If you want to add a title as well please put it in quotes: "Title")

Unfortunately, there are no references to include regarding this package (yet).

> Please write TRUE and FALSE instead of T and F. Please don't use "T" or "F" as vector names.

Corrected!

> You write information messages to the console that cannot be easily suppressed. -> R/fluorescence_workflows.R; R/dose-response-analysis.R; R/growth_workflows.R; R/linear_fits.R; R/nonparametric_fits.R; R/parametric_fits.R It is more R like to generate objects that can be used to extract the information a user is interested in, and then print() that object. 
Instead of print()/cat() rather use message()/warning() or
if(verbose)cat(..) (or maybe stop()) if you really have to write text to the console. (except for print, summary, interactive functions)

Changed most instances of cat() to message(). Some calls to cat() remain if the argument 'suppress.messages' is set to FALSE or 'interactive' is set to TRUE by the user. The latter makes the functions in R/growth_workflows.R interactive.

> Please make sure that you do not change the user's options, par or working directory. If you really have to do so within functions, please ensure with an *immediate* call of on.exit() that the settings are reset when the function is exited.

Thanks a lot for the advice with 'on.exit()'! I implemented the suggested solution whenever par() settings are changed to create plots. 

> Please ensure that you do not use more than 2 cores in your examples, vignettes, etc. -> R/fluorescence_workflows.R; R/growth_workflows.R

Parallelization (using the packages ‘parallel’ and ‘doParallel’) is only used if the argument ‘parallelize’ is set to TRUE in the functions growth.workflow(), fl.workflow(), growth.report(), and fl.report(). I made sure that in the examples, this argument is set to FALSE.

> Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace (including the package directory and getwd()). This is not allowed by CRAN policies. 
Please omit any default path in writing functions. In your examples/vignettes/tests you can write to tempdir().

Changed all default directories for functions writing files to tempdir().

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

### Resubmission changes

Removed 'GithubRepo' and 'GithubUsername' fields from Description
