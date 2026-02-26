# Generate a grouped results table for spline fits with average and standard deviations

Generate a grouped results table for spline fits with average and
standard deviations

## Usage

``` r
table_group_growth_spline(gcTable, html = FALSE)
```

## Arguments

- gcTable:

  An object of class `gcTable`

- html:

  (Logical) Should column headers contain html formatting?

## Value

A data frame with grouped spline fit results. Empty cells indicate that
no reliable fit could be determined.

## Examples

``` r
# Create random growth data set
rnd.data <- rdm.data(d = 35, mu = 0.8, A = 5, label = "Test1")

# Run growth curve analysis workflow
res <- growth.workflow(time = rnd.data$time,
                       data = rnd.data$data,
                       fit.opt = "s",
                       ec50 = FALSE,
                       export.res = FALSE,
                       parallelize = FALSE,
                       suppress.messages = TRUE)

table_group_growth_spline(res$gcFit$gcTable)
#>     Sample|Conc.         mumax             tD       lagtime         Y_max
#> 1      Test1 | 0 0.666 ± 0.002  1.041 ± 0.003 4.405 ± 0.246 6.415 ± 0.281
#> 2  Test1 | 0.017 0.623 ± 0.008  1.112 ± 0.015 4.425 ± 0.327 3.889 ± 0.637
#> 3  Test1 | 0.026 0.602 ± 0.004  1.151 ± 0.008 4.447 ± 0.081 3.385 ± 0.289
#> 4  Test1 | 0.039 0.578 ± 0.005    1.2 ± 0.009 4.528 ± 0.214 2.859 ± 0.225
#> 5  Test1 | 0.059  0.53 ± 0.008  1.308 ± 0.019 4.259 ± 0.062 2.276 ± 0.048
#> 6  Test1 | 0.088 0.464 ± 0.006  1.492 ± 0.018 4.437 ± 0.238  1.645 ± 0.11
#> 7   Test1 | 0.13 0.388 ± 0.004   1.788 ± 0.02 4.369 ± 0.067  1.18 ± 0.141
#> 8    Test1 | 0.2  0.27 ± 0.002  2.569 ± 0.021 4.047 ± 0.134 0.827 ± 0.061
#> 9    Test1 | 0.3 0.153 ± 0.003  4.523 ± 0.104 3.244 ± 0.115 0.462 ± 0.046
#> 10  Test1 | 0.44 0.073 ± 0.002  9.531 ± 0.326 2.324 ± 0.736 0.196 ± 0.009
#> 11  Test1 | 0.67 0.036 ± 0.004 19.232 ± 2.109 2.755 ± 1.867 0.096 ± 0.007
#> 12     Test1 | 1                                                         
#>               dY      t(mumax)            AUC
#> 1  6.363 ± 0.277      8 ± 0.25 76.803 ± 2.004
#> 2  3.837 ± 0.632 7.875 ± 0.177 69.031 ± 2.098
#> 3  3.332 ± 0.285 7.833 ± 0.144 66.095 ± 0.124
#> 4  2.803 ± 0.224 7.833 ± 0.289  62.989 ± 1.41
#> 5  2.222 ± 0.046 7.833 ± 0.144 60.538 ± 0.357
#> 6  1.593 ± 0.109 8.167 ± 0.382  54.19 ± 1.047
#> 7   1.13 ± 0.137 8.417 ± 0.144 48.459 ± 0.738
#> 8  0.772 ± 0.061  9.083 ± 0.52 40.109 ± 0.721
#> 9  0.407 ± 0.044  9.75 ± 1.146 28.787 ± 0.687
#> 10  0.145 ± 0.01  12.5 ± 2.634 16.592 ± 0.932
#> 11 0.043 ± 0.005 10.25 ± 7.628  7.333 ± 0.446
#> 12                                           

# with HTML formatting
DT::datatable(table_group_growth_spline(res$gcFit$gcTable, html = TRUE),
              escape = FALSE) # Do not escape HTML entities

{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12"],["Test1 | 0","Test1 | 0.017","Test1 | 0.026","Test1 | 0.039","Test1 | 0.059","Test1 | 0.088","Test1 | 0.13","Test1 | 0.2","Test1 | 0.3","Test1 | 0.44","Test1 | 0.67","Test1 | 1"],["0.666 ± 0.002","0.623 ± 0.008","0.602 ± 0.004","0.578 ± 0.005","0.53 ± 0.008","0.464 ± 0.006","0.388 ± 0.004","0.27 ± 0.002","0.153 ± 0.003","0.073 ± 0.002","0.036 ± 0.004",""],["1.041 ± 0.003","1.112 ± 0.015","1.151 ± 0.008","1.2 ± 0.009","1.308 ± 0.019","1.492 ± 0.018","1.788 ± 0.02","2.569 ± 0.021","4.523 ± 0.104","9.531 ± 0.326","19.232 ± 2.109",""],["4.405 ± 0.246","4.425 ± 0.327","4.447 ± 0.081","4.528 ± 0.214","4.259 ± 0.062","4.437 ± 0.238","4.369 ± 0.067","4.047 ± 0.134","3.244 ± 0.115","2.324 ± 0.736","2.755 ± 1.867",""],["6.415 ± 0.281","3.889 ± 0.637","3.385 ± 0.289","2.859 ± 0.225","2.276 ± 0.048","1.645 ± 0.11","1.18 ± 0.141","0.827 ± 0.061","0.462 ± 0.046","0.196 ± 0.009","0.096 ± 0.007",""],["6.363 ± 0.277","3.837 ± 0.632","3.332 ± 0.285","2.803 ± 0.224","2.222 ± 0.046","1.593 ± 0.109","1.13 ± 0.137","0.772 ± 0.061","0.407 ± 0.044","0.145 ± 0.01","0.043 ± 0.005",""],["8 ± 0.25","7.875 ± 0.177","7.833 ± 0.144","7.833 ± 0.289","7.833 ± 0.144","8.167 ± 0.382","8.417 ± 0.144","9.083 ± 0.52","9.75 ± 1.146","12.5 ± 2.634","10.25 ± 7.628",""],["76.803 ± 2.004","69.031 ± 2.098","66.095 ± 0.124","62.989 ± 1.41","60.538 ± 0.357","54.19 ± 1.047","48.459 ± 0.738","40.109 ± 0.721","28.787 ± 0.687","16.592 ± 0.932","7.333 ± 0.446",""]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Sample|Conc.<\/th>\n      <th>mu<sub>max<\/sub><\/th>\n      <th>t<sub>D<\/sub><\/th>\n      <th>lagtime<\/th>\n      <th>dY<\/th>\n      <th>y<sub>max<\/sub><\/th>\n      <th>t(mu<sub>max<\/sub>)<\/th>\n      <th>NA<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"Sample|Conc.","targets":1},{"name":"mu<sub>max<\/sub>","targets":2},{"name":"t<sub>D<\/sub>","targets":3},{"name":"lagtime","targets":4},{"name":"dY","targets":5},{"name":"y<sub>max<\/sub>","targets":6},{"name":"t(mu<sub>max<\/sub>)","targets":7},{"name":null,"targets":8}],"order":[],"autoWidth":false,"orderClasses":false},"selection":{"mode":"multiple","selected":null,"target":"row","selectable":null}},"evals":[],"jsHooks":[]}
```
