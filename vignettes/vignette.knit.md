---
title: "Quantitiative Growth Curve Evaluation with Package `QurvE`"
author: "Nicolas T. Wirth"
date: "2022-10-19"
output:
  bookdown::pdf_document2:
    fig_caption: yes
    toc: false
    latex_engine: xelatex
  bookdown::html_document2:
    fig_caption: yes
    toc: true
    toc_float: true
    theme: united
vignette: >
  %\VignetteIndexEntry{Quantitiative Growth Curve Evaluation with Package `QurvE`}
  %\VignetteEncoding{UTF-8}
  %\VignetteEngine{rmarkdown::render}
csl: https://raw.githubusercontent.com/citation-style-language/styles/master/research-institute-for-nature-and-forest.csl
link-citations: true
lang: en-US
zotero: true
bibliography: references.bib
---



<strong>Please note:</strong> This vignette will be updated from time to time when new features are implemented. Please find the most recent version at the [QurvE GitHub repository](https://github.com/NicWir/QurvE).

# Introduction

In virtually all disciplines of biology dealing with living organisms, from classical microbiology to applied biotechnology, it is routine to characterize the **growth** of the species under study. `QurvE` provides a suite of analysis tools to make such growth profiling quick, efficient, and reproducible. In addition, it allows the characterization of **fluorescence** data for, e.g., biosensor characterization in plate reader experiments (further discussed in the vignette *Quantitiative Fluorescence Curve Evaluation with Package `QurvE`)*. All computational steps to obtain an in-depth characterization are combined into user-friendly *workflow* functions and a range of plotting functions allow the visualization of fits and the comparison of organism performances.

Any physiological parameter calculated (e.g., growth rate µ, doubling time t~D~, lag time $\lambda$, density increase $\Delta$Y, or equivalent fluorescence parameters) can be used to perform a dose-response analysis to determine the *half-maximal effective concentration* (EC~50~).

The package is build on the foundation of the two R packages from @kahm2010 and @petzoldt2022. `QurvE` was designed to be usable with minimal prior knowledge of the R programming language or programming in general. You will need to be familiar with the idea of running commands from a console or writing basic scripts. For R beginners, [this](https://moderndive.netlify.app/1-getting-started.html) is a great starting point, there are some good resources [here](https://education.rstudio.com/learn/beginner/) and we suggest using the [RStudio application](https://rstudio.com/products/rstudio/). It provides an environment for writing and running R code.

With consideration for `R` novices, `QurvE` establishes a framework in which a complete, detailed growth curve analysis can be performed in two simple steps:

1.  *Read data* in custom format or parse data from a plate reader experiment.

2.  *Run workflow*, including fitting of growth curves, dose-response analysis, and rendering a report that summarizes the results.

All computational results of a workflow are stored in a data container (list) and can be visualized by passing them to the generic function `plot(list_object)`. `QurvE` further extends the user's control over the fits by defining thresholds and quality criteria, allows the direct parsing of data from plate reader result files, and calculates parameters for an additional growth phase (bi-phasic growth).

# Installation

## Development version

Install the must current version with package `devtools`:


```r
install.packages("devtools")
library(devtools)
install_github("NicWir/QurvE")
```

# Growth profiling methods

Three methods are available to characterize growth curves:

1.  Fit parametric growth models to (log-transformed) growth data

2.  Determine maximum growth rates (µ~max~) from the log-linear part of a growth curve using a heuristic approach proposed as the "growth rates made easy"-method by @hall2014. Do do so, `QurvE` uses code from the package @petzoldt2022, but adds user-defined thresholds for (i) R^2^ values of linear fits, (ii) relative standard deviations (RSD) of estimates slopes, and (iii) the minimum fraction of total density increase ($\Delta$Y) a regression window should cover to be considered for the analysis. These thresholds ensure a more robust and reproducible identification of the linear range that best describes the growth curve. Additionally, parameters for a *secondary growth phase* can be extracted for bi-linear growth curves.[^1]\
    The algorithm works as follows:

    i.  Fit linear regressions [with the Theil-Sen estimator [@sen1968; @theil1992]] to all subsets of `h` consecutive, log-transformed data points (sliding window of size `h`). If, for example, `h=5`, fit a linear regression to points 1 $\dots$ 5, 2 $\dots$ 6, 3 $\dots$ 7 and so forth.

    ii. Find the subset with the highest slope $\mu_{max}$. Do the R^2^ and RSD values of the regression meet the defined thresholds and do the data points within the regression window account for at least a defined fraction of the total density increase? If not, evaluate the regression with the second highest slope, and so forth.

    iii. Include also the data points of adjacent subsets that have a slope of at least a $defined \space quota \times \mu_{max}$, e.g., all regression windows that have at least 95% of the maximum slope.

    iv. Fit a new linear model to the extended data window identified in step iii.

    If `biphasic = TRUE`, the following steps are performed to define a second growth phase:

    i.  Perform a smooth spline fit on the data with a smoothing factor of 0.5.

    ii. Calculate the second derivative of the spline fit and perform a smooth spline fit of the derivative with a smoothing factor of 0.4.

    iii. Determine local maxima and minima in the second derivative.

    iv. Find the local minimum following $\mu_{max}$ and repeat the heuristic linear method for later time values.

    v.  Find the local maximum before $\mu_{max}$ and repeat the heuristic linear method for earlier time values.

    vi. Choose the greater of the two independently determined slopes as $\mu_{max}2$.

3.  Perform a smooth spline fit on (log-transformed) growth data and extract µ~max~ as the maximum value of the first derivative^1^.

    If `biphasic = TRUE`, the following steps are performed to define a second growth phase:

    i.  Determine local minima within the first derivative of the smooth spline fit.

    ii. Remove the *'peak'* containing the highest value of the first derivative (i.e., $\mu_{max}$) that is flanked by two local minima.

    iii. Repeat the smooth spline fit and identification of maximum slope for later time values than the local minimum after $\mu_{max}$.

    iv. Repeat the smooth spline fit and identification of maximum slope for earlier time values than the local minimum before $\mu_{max}$.

    v.  Choose the greater of the two independently determined slopes as $\mu_{max}2$.

[^1]: For linear and nonparametric (i.e, smooth spline) fits, the lag time is calculated as the intersect of the tangent at µ~max~ and a horizontal line at the first data point (y~0~). For the analysis of bi-phasic growth curves, the lag time is extracted as the lower of the two obtained $\lambda$ values.

# Data formats

`QurvE` accepts files with the formats *.xls*, *.xlsx*, *.csv*, *.tsv*, and *.txt* (tab separated). The data in the files should be structured as shown in Figure \@ref(fig:data-layout). Alternatively, data parsers are available that allow direct import of raw data from different culture instruments. For a list of currently supported devices, please run `?parse_data`.

## Custom format

To ensure compatibility with any type of measurement and data type (e.g., optical density, cell count, measured dimensions), `QurvE` uses a custom data layout. Here the first column contains *time values* and **'Time'** in the top cell, cells #2 and #3 are ignored. The remaining columns contain *measurement values* and the following sample identifiers in the top three rows:

1.  Sample name; usually a combination of organism and condition, or 'blank'.
2.  Replicate number; replicates are identified by *identical* names and concentration values. If only one type of replicate (biological or technical) was performed, enter numerical values here. If **both** biological and technical replicates of these biological replicates have been performed, the biological replicates shall be indicated with numbers and the technical replicates with letters. The technical replicates are then combined with their average value.
3.  (*optional*) Concentration values of an added compound; this information is used to perform a dose-response analysis.

Several experiments (e.g., runs on different plate readers) can be combined into a single file and analyzed simultaneously. Therefore, different experiments are marked by the presence of separate time columns. Different lengths and values in these time columns are permitted.

\begin{figure}

{\centering \includegraphics{../man/figures/Data_layout} 

}

\caption{\label{fig:data-layout} Custom QurvE data layout}(\#fig:data-layout)
\end{figure}

\
To read data in custom format, run:


```r
input <- read_data(data.density = 'path_to_data_file',
                   csvsep = ';', # or ','
                   dec = '.', # or ','
                   sheet.density = 1, # number (or "name") of the EXCEL file sheet containing data
                   subtract.blank = TRUE,
                   calibration = NULL
)
```

The \textcolor{red}{`data.density`} argument takes the path to the file containing experimental data in custom format. \textcolor{red}{`csvsep`} specifies the separator symbol (only required for .csv files; default: `';'`). \textcolor{red}{`dec`} is the decimal separator (only required for .csv, .tsv, or .txt files; default: `'.'`). If an Excel file format is used, \textcolor{red}{`sheet.density`} specifies the number or name (in quotes) of the sheet containing the data. If \textcolor{red}{`subtract.blank = TRUE`}, columns with name 'blank' will be combined by their row-wise average, and the mean values will be subtracted from the measurements of all remaining samples. For the \textcolor{red}{`calibration`} argument, a formula can be provided in the form *'y = function(x)'* (e.g., `calibration = 'y = x * 2 + 0.5'`) to transform all measurement values.

## Data parser

The data generated by culture devices (e.g., plate readers) from different manufacturers come in different formats. If these data are to be used directly, they must first be "parsed" from the plate reader into the `QurvE` standard format. In this scenario, sample information must be provided in a separate table that *maps* samples with their respective identifiers.The *mapping table* must have the following layout (Figure \@ref(fig:mapping-layout)):

\begin{figure}

{\centering \includegraphics[width=0.4\linewidth]{../man/figures/mapping_layout} 

}

\caption{\label{fig:mapping-layout} Data parser mapping layout}(\#fig:mapping-layout)
\end{figure}

To parse data, run:


```r
input <- parse_data(data.file = 'path_to_data_file',
                    map.file = 'path_to_mapping_file',
                    software = 'used_software_or_device', 
                    csvsep.data = ';', # or ','
                    dec.data = '.', # or ','
                    csvsep.map = ';', # or ','
                    dec.map = '.', # or ','
                    sheet.data = 1, # number (or "name") of the EXCEL file sheet containing data
                    sheet.map = 1, # number (or "name") of the EXCEL file sheet containing 
                                   # mapping information
                    subtract.blank = TRUE,
                    calibration = NULL,
                    convert.time = NULL
)
```

The \textcolor{red}{`data.file`} argument takes the path to the file containing experimental data exported from a culture device, \textcolor{red}{`map.file`} the path to the file with mapping information. With \textcolor{red}{`software`}, you can specify the device (or software) that was used to generate the data. \textcolor{red}{`csvsep.data`} and \textcolor{red}{`csvsep.map`} specify the separator symbol for data and mapping file, respectively (only required for .csv files; default: `';'`). \textcolor{red}{`dec.data`} and \textcolor{red}{`dec.map`} are the decimal separator used in data and mapping file, respectively (only required for .csv, .tsv, or .txt files; default: `'.'`). If an Excel file format is used for both or one of data or mapping file, \textcolor{red}{`sheet.data`} and/or \textcolor{red}{`sheet.map`} specify the number or name (in quotes) of the sheet containing the data or mapping information, respectively. If \textcolor{red}{`subtract.blank = TRUE`}, columns with name 'blank' will be combined by their row-wise average, and the mean values will be subtracted from the measurements of all remaining samples. For the \textcolor{red}{`calibration`} argument, a formula can be provided in the form *'y = function(x)'* (e.g., `calibration = 'y = x * 2 + 0.5'`) to transform all measurement values. Similarly, \textcolor{red}{`convert.time`} accepts a function *'y = function(x)'* to transform time values (e.g., `convert.time = 'y = x/3600'` to convert seconds to hours).

# Run a complete growth analysis workflow

`QurvE` reduces all computational steps required to create a complete growth profiling to two steps, **read data** and **run workflow**.

After loading the package:


```r
library(QurvE)
library(ggplot2) ### REMOVE AFTER QurvE UPDATE!!!
```

we load experimental data from the publication @wirth2021 in which *Pseudomonas putida* KT2440 and an engineered strain were tested for their sensitivity towards the product 2-fluoromuconic acid:

### Load data


```r
input <- read_data(data.density = system.file('2-FMA_toxicity.xlsx', package = 'QurvE')
          )
```

The created object `input` is a list of class `grodata` containing:

-   a `time` matrix with time with 66 rows, each corresponding to one sample in the dataset, and 161 columns, i.e., time values for each sample.

-   a `density` data frame with 66 rows and 161+3 columns. The three additional columns contain the sample identifiers `condition`, `replicate`, and `concentration`.

-   `fluorescence1` (here: `NA`)

-   `fluorescence2` (here: `NA`)

-   `norm.fluorescence1` (here: `NA`)

-   `norm.fluorescence2` (here: `NA`)

-   `expdesign`, a data frame containing the `label`, `condition`, `replicate`, and `concentration` for each sample:


```r
head(input$expdesign)
#>             label condition replicate concentration
#> 1 KT2440 | 1 | 90    KT2440         1            90
#> 2 KT2440 | 1 | 70    KT2440         1            70
#> 3 KT2440 | 1 | 50    KT2440         1            50
#> 4 KT2440 | 1 | 25    KT2440         1            25
#> 5 KT2440 | 1 | 20    KT2440         1            20
#> 6 KT2440 | 1 | 15    KT2440         1            15
```

We can plot the raw data:

```r
plot.grofit(input, data.type = "raw", log.y = FALSE, x.lim = c(NA, 32))
```

![](C:/Users/nicwir/AppData/Local/Temp/RtmpkxqOkv/preview-f9c29b22875.dir/vignette_files/figure-latex/raw-plot-1.pdf)<!-- --> 
