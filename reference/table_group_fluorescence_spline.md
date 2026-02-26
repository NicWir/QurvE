# Generate a grouped results table for spline fits with average and standard deviations

Generate a grouped results table for spline fits with average and
standard deviations

## Usage

``` r
table_group_fluorescence_spline(flTable, html = FALSE)
```

## Arguments

- flTable:

  An object of class `flTable`

- html:

  (Logical) Should column headers contain html formatting?

## Value

A data frame with grouped spline fit results. Empty cells indicate that
no reliable fit could be determined.

## Examples

``` r
# load example dataset
input <- read_data(data.growth = system.file("lac_promoters_growth.txt", package = "QurvE"),
                   data.fl = system.file("lac_promoters_fluorescence.txt", package = "QurvE"),
                   csvsep = "\t",
                   csvsep.fl = "\t")
#> Sample data are stored in columns. If they are stored in row format, please run read_data() with data.format = 'row'.

# Run workflow
res <- fl.workflow(grodata = input, ec50 = FALSE, fit.opt = "s",
                   x_type = "time", norm_fl = TRUE,
                   dr.parameter = "max_slope.spline",
                   suppress.messages = TRUE,
                   parallelize = FALSE)

table_group_fluorescence_spline(res$flFit$flTable)
#>           Sample | Conc. slope_max lagtime     Y_max        dY x(slope_max)
#> 1       pSEVA634-GFP | 0    92.267  15.547  3427.126  2135.701         20.5
#> 2      pSEVA634r-GFP | 0    30.773  31.287  1470.992   282.714           20
#> 3     pSEVA634rk-GFP | 0    64.176  20.813  2196.189   630.596           16
#> 4    pSEVA634-GFP | 0.01   242.558  14.983  7592.801  6072.997         23.5
#> 5   pSEVA634r-GFP | 0.01    64.653  21.671  2384.304  1128.582           19
#> 6  pSEVA634rk-GFP | 0.01    205.58  12.566  4820.287  3196.134           18
#> 7    pSEVA634-GFP | 0.05   509.395    6.96 17121.214 15989.029         21.5
#> 8   pSEVA634r-GFP | 0.05   204.032  13.436  5206.543  3800.827         17.5
#> 9  pSEVA634rk-GFP | 0.05   290.961   4.674  7768.189  6299.664           13
#> 10    pSEVA634-GFP | 0.1   670.985   0.373  20079.01 18869.536            6
#> 11   pSEVA634r-GFP | 0.1   327.195   9.868  6840.378  5652.539           17
#> 12  pSEVA634rk-GFP | 0.1   335.244   3.651  9191.863  7723.924         17.5
#> 13    pSEVA634-GFP | 0.2   728.625   0.554 20481.098 19589.302          6.5
#> 14   pSEVA634r-GFP | 0.2   346.268   9.758  7000.845  5718.068         16.5
#> 15  pSEVA634rk-GFP | 0.2   320.247   3.214  9006.211  7463.562         17.5
#> 16    pSEVA634-GFP | 0.5   774.054   0.247 20704.091 19571.651          5.5
#> 17   pSEVA634r-GFP | 0.5   337.909   8.089  6952.687  6090.024         16.5
#> 18  pSEVA634rk-GFP | 0.5   351.644   4.845  9322.628  7720.204           18
#> 19      pSEVA634-GFP | 1   779.134   0.462 19774.702 18757.659            6
#> 20     pSEVA634r-GFP | 1   353.185   9.572  7189.597  5865.901         16.5
#> 21    pSEVA634rk-GFP | 1   338.827   5.016  9258.531  7551.009           18
#>           AUC
#> 1   82881.406
#> 2   43222.936
#> 3   67033.668
#> 4  151370.676
#> 5   58249.081
#> 6  132414.082
#> 7  367233.852
#> 8  120525.231
#> 9  223919.429
#> 10 490254.829
#> 11 169914.302
#> 12 269744.223
#> 13 497122.375
#> 14 179100.136
#> 15 267724.576
#> 16 520199.731
#> 17 178120.141
#> 18 273698.194
#> 19 508722.732
#> 20 182547.708
#> 21 271383.084

# with HTML formatting
DT::datatable(table_group_fluorescence_spline(res$flFit$flTable, html = TRUE),
              escape = FALSE) # Do not escape HTML entities

{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21"],["pSEVA634-GFP | 0","pSEVA634r-GFP | 0","pSEVA634rk-GFP | 0","pSEVA634-GFP | 0.01","pSEVA634r-GFP | 0.01","pSEVA634rk-GFP | 0.01","pSEVA634-GFP | 0.05","pSEVA634r-GFP | 0.05","pSEVA634rk-GFP | 0.05","pSEVA634-GFP | 0.1","pSEVA634r-GFP | 0.1","pSEVA634rk-GFP | 0.1","pSEVA634-GFP | 0.2","pSEVA634r-GFP | 0.2","pSEVA634rk-GFP | 0.2","pSEVA634-GFP | 0.5","pSEVA634r-GFP | 0.5","pSEVA634rk-GFP | 0.5","pSEVA634-GFP | 1","pSEVA634r-GFP | 1","pSEVA634rk-GFP | 1"],["92.267","30.773","64.176","242.558","64.653","205.58","509.395","204.032","290.961","670.985","327.195","335.244","728.625","346.268","320.247","774.054","337.909","351.644","779.134","353.185","338.827"],["15.547","31.287","20.813","14.983","21.671","12.566","6.96","13.436","4.674","0.373","9.868","3.651","0.554","9.758","3.214","0.247","8.089","4.845","0.462","9.572","5.016"],["3427.126","1470.992","2196.189","7592.801","2384.304","4820.287","17121.214","5206.543","7768.189","20079.01","6840.378","9191.863","20481.098","7000.845","9006.211","20704.091","6952.687","9322.628","19774.702","7189.597","9258.531"],["2135.701","282.714","630.596","6072.997","1128.582","3196.134","15989.029","3800.827","6299.664","18869.536","5652.539","7723.924","19589.302","5718.068","7463.562","19571.651","6090.024","7720.204","18757.659","5865.901","7551.009"],["20.5","20","16","23.5","19","18","21.5","17.5","13","6","17","17.5","6.5","16.5","17.5","5.5","16.5","18","6","16.5","18"],["82881.406","43222.936","67033.668","151370.676","58249.081","132414.082","367233.852","120525.231","223919.429","490254.829","169914.302","269744.223","497122.375","179100.136","267724.576","520199.731","178120.141","273698.194","508722.732","182547.708","271383.084"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Sample|Conc.<\/th>\n      <th>slope<sub>max<\/sub><\/th>\n      <th>lagtime<\/th>\n      <th>dY<\/th>\n      <th>y<sub>max<\/sub><\/th>\n      <th>x(slope<sub>max<\/sub>)<\/th>\n      <th>NA<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"Sample|Conc.","targets":1},{"name":"slope<sub>max<\/sub>","targets":2},{"name":"lagtime","targets":3},{"name":"dY","targets":4},{"name":"y<sub>max<\/sub>","targets":5},{"name":"x(slope<sub>max<\/sub>)","targets":6},{"name":null,"targets":7}],"order":[],"autoWidth":false,"orderClasses":false},"selection":{"mode":"multiple","selected":null,"target":"row","selectable":null}},"evals":[],"jsHooks":[]}
```
