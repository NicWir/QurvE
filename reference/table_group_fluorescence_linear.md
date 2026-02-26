# Generate a grouped results table for linear fits with average and standard deviations

Generate a grouped results table for linear fits with average and
standard deviations

## Usage

``` r
table_group_fluorescence_linear(flTable, html = FALSE)
```

## Arguments

- flTable:

  An object of class `flTable`

- html:

  (Logical) Should column headers contain html formatting?

## Value

A data frame with grouped linear fit results. Empty cells indicate that
no reliable fit could be determined.

## Examples

``` r
# \donttest{
# load example dataset
input <- read_data(data.growth = system.file("lac_promoters_growth.txt", package = "QurvE"),
                   data.fl = system.file("lac_promoters_fluorescence.txt", package = "QurvE"),
                   csvsep = "\t",
                   csvsep.fl = "\t")
#> Sample data are stored in columns. If they are stored in row format, please run read_data() with data.format = 'row'.

# Run workflow
res <- fl.workflow(grodata = input, ec50 = FALSE, fit.opt = "l",
                   x_type = "time", norm_fl = TRUE,
                   dr.parameter = "max_slope.spline",
                   suppress.messages = TRUE,
                   parallelize = FALSE)

table_group_fluorescence_linear(res$flFit$flTable)
#>             Sample|Conc. slope_max lagtime        dY     Y_max x_start(mumax)
#> 1       pSEVA634-GFP | 0                                                     
#> 2      pSEVA634r-GFP | 0                                                     
#> 3     pSEVA634rk-GFP | 0    75.796  23.666   376.798  2228.283             16
#> 4    pSEVA634-GFP | 0.01   262.266  17.259  5820.567  7503.259             30
#> 5   pSEVA634r-GFP | 0.01                                                     
#> 6  pSEVA634rk-GFP | 0.01   225.688  13.904  3020.995  4880.808             15
#> 7    pSEVA634-GFP | 0.05   658.849  10.171 15562.241     17045             16
#> 8   pSEVA634r-GFP | 0.05   307.497  14.566  3856.069   5224.49           14.5
#> 9  pSEVA634rk-GFP | 0.05   385.864   7.481  6056.345  7883.268             10
#> 10    pSEVA634-GFP | 0.1   786.634   1.892 18027.038 19670.602            2.5
#> 11   pSEVA634r-GFP | 0.1   514.144  11.836  6070.424  6990.424             15
#> 12  pSEVA634rk-GFP | 0.1   404.264   6.785  7467.593  9260.697           15.5
#> 13    pSEVA634-GFP | 0.2   810.584   2.174 18686.014 20250.899            3.5
#> 14   pSEVA634r-GFP | 0.2    540.02  11.918  6008.673  7054.127             15
#> 15  pSEVA634rk-GFP | 0.2   391.915    6.01   7526.82  9138.889             16
#> 16    pSEVA634-GFP | 0.5   820.283   1.388 18860.637 20504.202              5
#> 17   pSEVA634r-GFP | 0.5   494.059  11.004  6225.536  7150.065             15
#> 18  pSEVA634rk-GFP | 0.5    429.55   7.469  7650.517  9370.143           15.5
#> 19      pSEVA634-GFP | 1   877.851   2.083 17785.309 19449.102            3.5
#> 20     pSEVA634r-GFP | 1   559.313  12.303  6148.718  7412.869             16
#> 21    pSEVA634rk-GFP | 1   395.584   6.293  7763.536  9317.992             15
#>    x_end(mumax)
#> 1              
#> 2              
#> 3          19.5
#> 4          34.5
#> 5              
#> 6            20
#> 7            20
#> 8          19.5
#> 9          14.5
#> 10          6.5
#> 11           19
#> 12           20
#> 13          8.5
#> 14         19.5
#> 15           20
#> 16            9
#> 17         19.5
#> 18           20
#> 19            8
#> 20         19.5
#> 21           20

# with HTML formatting
DT::datatable(table_group_fluorescence_linear(res$flFit$flTable, html = TRUE),
              escape = FALSE) # Do not escape HTML entities

{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21"],["pSEVA634-GFP | 0","pSEVA634r-GFP | 0","pSEVA634rk-GFP | 0","pSEVA634-GFP | 0.01","pSEVA634r-GFP | 0.01","pSEVA634rk-GFP | 0.01","pSEVA634-GFP | 0.05","pSEVA634r-GFP | 0.05","pSEVA634rk-GFP | 0.05","pSEVA634-GFP | 0.1","pSEVA634r-GFP | 0.1","pSEVA634rk-GFP | 0.1","pSEVA634-GFP | 0.2","pSEVA634r-GFP | 0.2","pSEVA634rk-GFP | 0.2","pSEVA634-GFP | 0.5","pSEVA634r-GFP | 0.5","pSEVA634rk-GFP | 0.5","pSEVA634-GFP | 1","pSEVA634r-GFP | 1","pSEVA634rk-GFP | 1"],["","","75.796","262.266","","225.688","658.849","307.497","385.864","786.634","514.144","404.264","810.584","540.02","391.915","820.283","494.059","429.55","877.851","559.313","395.584"],["","","23.666","17.259","","13.904","10.171","14.566","7.481","1.892","11.836","6.785","2.174","11.918","6.01","1.388","11.004","7.469","2.083","12.303","6.293"],["","","376.798","5820.567","","3020.995","15562.241","3856.069","6056.345","18027.038","6070.424","7467.593","18686.014","6008.673","7526.82","18860.637","6225.536","7650.517","17785.309","6148.718","7763.536"],["","","2228.283","7503.259","","4880.808","17045","5224.49","7883.268","19670.602","6990.424","9260.697","20250.899","7054.127","9138.889","20504.202","7150.065","9370.143","19449.102","7412.869","9317.992"],["","","16","30","","15","16","14.5","10","2.5","15","15.5","3.5","15","16","5","15","15.5","3.5","16","15"],["","","19.5","34.5","","20","20","19.5","14.5","6.5","19","20","8.5","19.5","20","9","19.5","20","8","19.5","20"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Sample|Conc.<\/th>\n      <th>slope<sub>max<\/sub><\/th>\n      <th>lagtime<\/th>\n      <th>dY<\/th>\n      <th>y<sub>max<\/sub><\/th>\n      <th>x<sub>start<\/sub><br>(mu<sub>max<\/sub>)<\/th>\n      <th>x<sub>end<\/sub><br>(mu<sub>max<\/sub>)<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"Sample|Conc.","targets":1},{"name":"slope<sub>max<\/sub>","targets":2},{"name":"lagtime","targets":3},{"name":"dY","targets":4},{"name":"y<sub>max<\/sub>","targets":5},{"name":"x<sub>start<\/sub><br>(mu<sub>max<\/sub>)","targets":6},{"name":"x<sub>end<\/sub><br>(mu<sub>max<\/sub>)","targets":7}],"order":[],"autoWidth":false,"orderClasses":false},"selection":{"mode":"multiple","selected":null,"target":"row","selectable":null}},"evals":[],"jsHooks":[]}# }
```
