# QurvE App User Manual v.1.1

  
*QurvE* is an R package for analyzing growth and fluorescence data
measured over time. It features a graphical user interface (GUI)
developed as a Shiny app, which has been designed to be user-friendly
and intuitive. *QurvE* offers a variety of analysis tools to streamline
the process of growth profiling and ensure reproducibility. It can also
be used to characterize fluorescence data, such as in plate reader
experiments for biosensor characterization. Overall, *QurvE* aims to
make data analysis and reporting quick, efficient, and reliable.

## Navigation Bar

  

![ The QurvE navigation bar.](https://i.imgur.com/iokQfFJ.png)

The QurvE navigation bar.

  

- ***Data***: Load experimental data. Use either one of several
  available parsers for common cultivation devices or provide data in a
  custom format.

- ***Computation:*** Run computational workflows on entire growth or
  fluorescence datasets to perform curve fitting and dose-response
  analyses. This menu becomes available after successful data parsing.

- ***Validation:*** Confirm the accuracy of the fits for each sample and
  re-run individual fits with adjusted parameters.

- ***Results:*** View tabular summaries of computational results,
  including calculated growth and fluorescence curve parameters and
  results of dose-response analyses.

- ***Visualization:*** Compare samples and conditions and visualize
  computational results using a series of plots.

- ***Report:*** Create a PDF or HTML report summarizing all results from
  a growth or fluorescence curve analysis workflow.

- ***Data Export***: Save raw data, all computational results, and
  fitting options as a .RData object for storage and distribution.

- ***Data Import:*** Load a previously exported dataset in .RData
  format.

## Loading Data

*QurvE* allows loading data in different formats, for both growth
experiments and for fluorescence measurements. Data can be loaded by
using the custom QurvE data layout, by loading raw data from various
measuring equipment or by importing previously analyzed datasets.

### Loading data formatted in the custom QurvE layout

To load data using the custom QurvE layout, select the \[Data\] \>
\[Custom\] window and choose a file in .xls/.xlsx (selecting the
appropriate work sheets) or .csv/.tsv/.txt format (define the correct
decimal separator and, for .csv, field separator in the target file).
The custom QurvE layout includes all data and relative metadata
necessary for QurvE to perform the analysis. This custom layout can be
used to load both growth and fluorescence data. Data from a second
fluorescence channel can be used to normalize the primary fluorescence
values in addition to normalization to growth values.

The data must be formatted as displayed in **Figure 1**:

- The first row contains ‘Time’, ‘Blank’, and the sample identifiers
  (IDs). The ID needs to be identical for replicates and for samples
  analyzed together within a dose-response analysis. The ‘Time’ column
  must be positioned at the first position, ‘Blank’ and the samples
  follow in arbitrary order.
- The second row contains replicate numbers for identical conditions. If
  technical replicates were used in addition to biological replicates,
  indicate technical replicates with identical replicate numbers.
  Samples with identical IDs, replicate numbers, and concentration
  values (if applicable) will be combined by their average values at
  each time point. Leave the row empty if no replicate information is
  available.
- The third row is designated to contain concentration values to perform
  a dose-response analysis, if different concentrations of a compound
  were used in the experiment. Samples are included in the same
  dose-response analysis if they have identical sample identifiers
  (first row). Leave the row empty if no concentration information is
  available or relevant.
- Starting with the fourth row, the table must include data for ‘Time’,
  ‘Blank’ and the different samples.

Several experiments can be combined into a single custom dataset.
Different experiments with differing time values and experiment-specific
blanks are distinguished by an individual ‘Time’ column to the left of
each dataset. Blank values (experiment-specific) are combined as their
average and subtracted from all remaining values if the option
\[Subtract blank\] is selected. The metadata on the second and third row
is optional to perform the analysis; if no such metadata is available,
leave the rows empty.

The data can be further processed by (options indicated in the order in
which they are applied to the data):

1.  converting time values,

2.  applying a calibration equation to measurements.

3.  subtracting the blank values from each sample measurement,

4.  normalizing fluorescence data with a second fluorescence or growth
    dataset, if available.

All these data processing functions are optional.

  

![ Figure 1: QurvE custom data layout.](https://i.imgur.com/LeqGWAR.png)

Figure 1: QurvE custom data layout.

  

### Loading Tidy Data

The `QurvE` Shiny application is not limited to the custom QurvE layout;
it also supports the direct loading of tidy data, a widely used format
in data science.

Tidy data, also known as ‘long’ format, is a standardized way of
organizing data values within a dataset. It provides a straightforward,
flexible structure that is highly suitable for various forms of analysis
and visualization in R.

To upload data in tidy format, navigate to the \[Data\] \> \[Custom\]
window and select the appropriate file in .xls/.xlsx format (specifying
the relevant worksheets) or .csv/.tsv/.txt format (remembering to set
the correct decimal separator and, for .csv, the field separator).

Columns required in tidy data format:

- **“Time”**: Each row in this column contains the time values for your
  observations.
- **“Description”**: Each row in this column contains a sample
  description. This can be a combination of the organism and condition
  or any other relevant descriptor.
- **“Values”**: Each row in this column contains the measured values
  from your experiment (e.g., optical density, cell count, etc.).
- **“Replicate”** (optional): If you have multiple replicate
  measurements for the same condition (“Description” labels), you can
  indicate the replicate number in this column.
- **“Concentration”** (optional): If there’s a compound added to the
  sample, you can record its concentration in this column.

Upon uploading, `QurvE` automatically detects if the data is in tidy
format and processes it accordingly.

Note: For `QurvE` to correctly process tidy data, it’s crucial to ensure
the column headers in your uploaded data match exactly with those
specified above.

The same data processing options described in the previous section are
available also for tidy data.

Using tidy data allows you to leverage the power of `QurvE` with a wide
range of datasets, increasing the versatility of your analysis pipeline.

  

### Loading data from raw experiment files

*QurvE* supports loading directly raw data produced by a variety of
proprietary software for different growth or fluorescence measuring
equipment, such as plate readers and micro-bioreactors[¹](#fn1). This
can be done in the \[Data\] \> \[Parse Raw Data\] window.

After loading the file (1), selecting the correct raw data format by
choosing one of the available equipments/software (2) and assigning the
extracted read channels to growth/fluorescence data (3), the user has to
provide mapping information with metadata in tabular format (in a
.xls/.xlsx/.csv/.tsv/.txt file) (4).

The mapping file should be formatted as shown in **Figure 2**:

- The first column contains the well numbers in the plate and must match
  sample identifiers in the raw data file.
- The second column contains the ID (i.e., organism, condition, etc.) of
  each sample. The ID needs to be identical for replicates and for
  samples analyzed together within a dose-response analysis.
- The third column contains replicate numbers for identical conditions.
  If technical replicates were used in addition to biological
  replicates, indicate technical replicates with the same replicate
  number. Samples with identical IDs, replicate numbers, and
  concentration values (if applicable) will be combined by their average
  values at each time point. Leave the row empty if no replicate
  information is available.
- The fourth column is designated to contain concentration values to
  perform a dose-response analysis, if different concentrations of a
  compound of interest were used in the experiment. Leave the row empty
  if no additional concentration information is available.

The values in ‘Blank’ samples are combined by their averages and
subtracted from all remaining values if the option \[Subtract blank\] is
chosen.

The data can be further processed by (options indicated in the order in
which they are applied to the data):

1.  converting time values,

2.  applying a calibration equation to measurements.

3.  subtracting the blank values from each sample measurement,

4.  normalizing fluorescence data with a second fluorescence or growth
    dataset, if available.

All these data processing functions are optional.

  

![ Figure 2: The \[Parse Raw Data\] window enables loading raw data
exported from a plate reader/bioreactor software. The right panel
illustrates the layout of the additional mapping file with metadata that
must be supplemented to analyze establish relationships between
conditions and replicates.](https://i.imgur.com/ZJURShU.png)

Figure 2: The \[Parse Raw Data\] window enables loading raw data
exported from a plate reader/bioreactor software. The right panel
illustrates the layout of the additional mapping file with metadata that
must be supplemented to analyze establish relationships between
conditions and replicates.

  

### Loading previously processed data

*QurvE* supports exporting processed data and reloading it for further
analysis or inspection. Previously saved growth or fluorescence data in
.RData format can be loaded in the Data Import window (**Figure 3**).

  

![ Figure 3: The figure illustrates the \[Data Import\] window, which
enables the user to load previously exported QurvE data
files.](https://i.imgur.com/dkeUsHh.png)

Figure 3: The figure illustrates the \[Data Import\] window, which
enables the user to load previously exported QurvE data files.

  

## Processing and analyzing data in *QurvE*

Once raw data is loaded, *QurvE* can be used to process the data by
fitting curves, calculating averages and standard deviations for
replicates, as well as fitting the data with different models to obtain
growth parameters such as doubling times, yields, etc., or dose-response
parameters. Moreover, the software allows the user to intuitively create
publication-grade plots.

### Run computations

Once the raw data has been successfully parsed, the \[Computation\]
window will unlock, enabling the user to process either Growth or
Fluorescence data (**Figure 4**). The data can be processed using
different curve fitting methods, which can be selected in the panel on
the left. The default fitting options are the Linear regression fit and
Non-parametric fit (smooth spline). By ticking Parametric fit (not
available for Fluorescence analysis), parametric growth models models
are fit to the data. By ticking Log-transform time the time values are
transformed by Ln(1 + time). With Biphasic growth selected, *QurvE*
tries to identify a second growth phase while performing *linear
regressions* and *non-parametric fits* and extracts corresponding
parameters (e.g., growth rate, lag time until second growth phase).

Dose-response (EC50) analyses can be performed by either performing a
smooth spline fit (response spline fit) on response vs. concentration
data and extracting the *EC₅₀* as the concentration at the midpoint
between the largest and smallest response value, or by applying up to 20
(parametric) dose-response models to choose the best model based on the
Akaike information criterion (AIC). Any computed parameter (e.g., growth
rates obtained via linear regression, maximum fluorescence obtained via
non-parametric fits…) can be chosen as response parameter to quantify
adverse or beneficial effects of a substance. The options to
Log-transform concentration or Log-transform response are only available
for *response spline fits*. In both instances, Ln(x + 1) is applied to
the respective values.

Additional information on all methods and (global or method-specific)
fitting options is available by clicking on the \[?\] signs.

  

![ Figure 4: The \[Computation\] window enables the user to define
parameters for data processing and fitting. Growth data and fluorescence
data must be processed separately.](https://i.imgur.com/BCOLAU5.png)

Figure 4: The \[Computation\] window enables the user to define
parameters for data processing and fitting. Growth data and fluorescence
data must be processed separately.

  

### Visualization and validation of curve fitting

The \[Validation\] window enables the user to verify the correct fitting
of the growth/fluorescence curves (see **Figure 5** for an example).

  

![ Figure 5: Example of a fitted growth curve in the \[Validation\]
window. On the right panel, the figure visualizes the performed fit. In
this case, the red dots and dashed line represent the linear regression
model used to calculate the maximum growth rate and lag
time.](https://i.imgur.com/IKPrwRJ.png)

Figure 5: Example of a fitted growth curve in the \[Validation\] window.
On the right panel, the figure visualizes the performed fit. In this
case, the red dots and dashed line represent the linear regression model
used to calculate the maximum growth rate and lag time.

  

Different tabs enable the user to visualize the fit obtained with
different fitting methods (Linear Fits, Nonparametric fits, etc.). In
the left pane the independent samples can be selected for visualization.
It further contains options to modify the figure, which can be exported
in .png or .pdf formats. In the right pane, the \[Re-run with modified
parameters\] button enables the user to modify the fitting parameters
and rerun the analysis only for the selected sample to obtain a new fit
and updated parameters.

### Tabular overview of results

The \[Results\] window (see **Figure 6**) enables the user to obtain an
overview of the fitted parameters for each applied method. By ticking
the \[Group average\] box the user can visualize the **average ±
standard deviation** of parameters for replicates. If deselected, the
parameters for every single sample are shown. The table can be exported
in .csv format by clicking on the \[Download table\] button.

  

![ Figure 6: The \[Results\] window gives a tabular overview of all
computed parameters for each applied fit method as well as the results
dose-response analyses.](https://i.imgur.com/ojwszNf.png)

Figure 6: The \[Results\] window gives a tabular overview of all
computed parameters for each applied fit method as well as the results
dose-response analyses.

  

### Visualization of results

Once the data has been processed, the \[Visualization\] window enables
the user to visualize the data using different plot formats:

- **Group Plots**: Plot growth data over time for multiple conditions on
  the same plot. The \[Data type\] drop down menu allows the user to
  define the type of data (raw data, smoothing splines, normalized
  fluorescence) to be visualized as a curve chart. The user can easily
  select with the cursor which samples/groups to plot in the
  \[Conditions\] input. Alternatively, by ticking \[(De-)select samples
  based on string\], the user can select or exclude samples by typing
  strings which include the name of the sample to be included/excluded.
  The \[Plot group averages\] box is be used to visualize either
  averages of replicates (± standard deviations) or the individual
  samples independently. Moreover, if concentration metadata was
  provided while loading the data, the user can (de-)select samples
  using these additional parameters. The left pane contains also various
  options to modify the figure, which can be exported in .png or .pdf
  formats.

- **Parameter Plots**: Plot calculated curve parameters (growth rates,
  lag times, yields, etc.) of different samples as a bar plot. The
  \[Parameter\] drop down menu allows the user to select the parameter
  to be plotted. Samples and conditions can be (de-)selected via the
  same methods described for Group Plots. The Plot group averages box
  can be used to visualize either averages (and *95% confidence
  intervals*) of groups or the individual samples independently.

- **Plot Grid**: Plot several curves in separate panels, coloring the
  background based on a chosen parameter in a heatmap-like fashion. The
  \[Data type\] drop down menu allows the user to define the type of
  data to be visualized as a curve chart. The \[Parameter\] drop down
  menu allows the user to select the parameter by which the panel
  backgrounds will be colored. Samples and conditions can be
  (de-)selected via the same methods described for Group Plots. If
  concentration metadata was provided while loading the data, the panels
  can be arranged in a matrix with one concentration per row by ticking
  \[Sort by concentration\].

- **Dose-Response Analysis**: Plot the results of dose-response analyses
  with the chosen response parameter on the y-axis and concentration
  values on the x-axis. The drawn line indicates the fit that was
  applied to the data.

## Export data from QurvE

*QurvE* enables the user to export data or results in different ways:

- **Download Plot**: all figures plotted within QurvE can be exported in
  .png or .pdf formats. Please note that the size of the exported plots
  differs from the figure diplayed within the app which is dependent on
  the size of the *QurvE* window. The size, aspect ratio and resolution
  of each exported plot can be adjusted with the dedicated \[Width\],
  \[Height\], and \[DPI\] fields

- **Report**: The \[Report\] window enables the user to export all the
  computational results and figures for each performed fit in a single
  report file in .pdf or .html format.

- **Data Export**: The \[Data Export\] window enables the user to export
  all processed raw data as well as computation results and fitting
  options as a single data container in .RData format. Please note that
  modifications to figures are not included in the exported .RData file,
  so make sure to export your figures separately.

*In the spirit of good scientific practice (data transparency), we
encourage anyone using QurvE to attach the .RData file and generated
reports to their publication.*

------------------------------------------------------------------------

1.  If *QurvE* does not include a **data parser** for your device, yet,
    please open an issue at <https://github.com/NicWir/QurvE/issues>. We
    are continually working on adding support for more equipment and
    software.
