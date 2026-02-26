# Generate a grouped results table for parametric fits with average and standard deviations

Generate a grouped results table for parametric fits with average and
standard deviations

## Usage

``` r
table_group_growth_model(gcTable, html = FALSE)
```

## Arguments

- gcTable:

  An object of class `gcTable`

- html:

  (Logical) Should column headers contain html formatting?

## Value

A data frame with grouped model fit results. Empty cells indicate that
no reliable fit could be determined.

## Examples

``` r
# Create random growth data set
rnd.data <- rdm.data(d = 35, mu = 0.8, A = 5, label = "Test1")


# Run growth curve analysis workflow
res <- growth.workflow(time = rnd.data$time,
                       data = rnd.data$data,
                       fit.opt = "m",
                       ec50 = FALSE,
                       export.res = FALSE,
                       parallelize = FALSE,
                       suppress.messages = TRUE)

table_group_growth_model(res$gcFit$gcTable)
#>     Sample|Conc.         mumax             tD       lagtime         Y_max
#> 1      Test1 | 0 0.803 ± 0.001  0.863 ± 0.001 5.129 ± 0.175  4.78 ± 0.115
#> 2  Test1 | 0.017  0.79 ± 0.011  0.877 ± 0.013 5.223 ± 0.175 4.262 ± 0.085
#> 3  Test1 | 0.026 0.779 ± 0.005   0.89 ± 0.006 5.089 ± 0.234 4.115 ± 0.103
#> 4  Test1 | 0.039 0.764 ± 0.004  0.907 ± 0.005 5.372 ± 0.179 3.943 ± 0.098
#> 5  Test1 | 0.059 0.732 ± 0.003  0.947 ± 0.003 5.311 ± 0.161 3.672 ± 0.106
#> 6  Test1 | 0.088 0.683 ± 0.003  1.014 ± 0.005 5.677 ± 0.079 3.426 ± 0.025
#> 7   Test1 | 0.13 0.591 ± 0.003  1.174 ± 0.007  5.754 ± 0.25 3.166 ± 0.085
#> 8    Test1 | 0.2 0.439 ± 0.009  1.578 ± 0.034 5.978 ± 0.319  2.72 ± 0.055
#> 9    Test1 | 0.3  0.272 ± 0.01  2.549 ± 0.095 6.246 ± 0.243 2.205 ± 0.093
#> 10  Test1 | 0.44 0.141 ± 0.011   4.931 ± 0.39  6.783 ± 0.43 1.668 ± 0.097
#> 11  Test1 | 0.67  0.03 ± 0.003 23.466 ± 2.075 0.566 ± 0.358 1.103 ± 0.298
#> 12     Test1 | 1                                                         
#>               dY
#> 1  4.769 ± 0.104
#> 2  4.264 ± 0.076
#> 3  4.107 ± 0.105
#> 4  3.949 ± 0.086
#> 5  3.689 ± 0.102
#> 6  3.431 ± 0.032
#> 7  3.158 ± 0.088
#> 8  2.706 ± 0.042
#> 9  2.139 ± 0.084
#> 10 1.359 ± 0.013
#> 11 0.554 ± 0.051
#> 12              

# with HTML formatting
DT::datatable(table_group_growth_model(res$gcFit$gcTable, html = TRUE),
              escape = FALSE) # Do not escape HTML entities

{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12"],["Test1 | 0","Test1 | 0.017","Test1 | 0.026","Test1 | 0.039","Test1 | 0.059","Test1 | 0.088","Test1 | 0.13","Test1 | 0.2","Test1 | 0.3","Test1 | 0.44","Test1 | 0.67","Test1 | 1"],["0.803 ± 0.001","0.79 ± 0.011","0.779 ± 0.005","0.764 ± 0.004","0.732 ± 0.003","0.683 ± 0.003","0.591 ± 0.003","0.439 ± 0.009","0.272 ± 0.01","0.141 ± 0.011","0.03 ± 0.003",""],["0.863 ± 0.001","0.877 ± 0.013","0.89 ± 0.006","0.907 ± 0.005","0.947 ± 0.003","1.014 ± 0.005","1.174 ± 0.007","1.578 ± 0.034","2.549 ± 0.095","4.931 ± 0.39","23.466 ± 2.075",""],["5.129 ± 0.175","5.223 ± 0.175","5.089 ± 0.234","5.372 ± 0.179","5.311 ± 0.161","5.677 ± 0.079","5.754 ± 0.25","5.978 ± 0.319","6.246 ± 0.243","6.783 ± 0.43","0.566 ± 0.358",""],["4.78 ± 0.115","4.262 ± 0.085","4.115 ± 0.103","3.943 ± 0.098","3.672 ± 0.106","3.426 ± 0.025","3.166 ± 0.085","2.72 ± 0.055","2.205 ± 0.093","1.668 ± 0.097","1.103 ± 0.298",""],["4.769 ± 0.104","4.264 ± 0.076","4.107 ± 0.105","3.949 ± 0.086","3.689 ± 0.102","3.431 ± 0.032","3.158 ± 0.088","2.706 ± 0.042","2.139 ± 0.084","1.359 ± 0.013","0.554 ± 0.051",""]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Sample|Conc.<\/th>\n      <th>mu<sub>max<\/sub><\/th>\n      <th>t<sub>D<\/sub><\/th>\n      <th>lagtime<\/th>\n      <th>y<sub>max<\/sub><\/th>\n      <th>dY<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"Sample|Conc.","targets":1},{"name":"mu<sub>max<\/sub>","targets":2},{"name":"t<sub>D<\/sub>","targets":3},{"name":"lagtime","targets":4},{"name":"y<sub>max<\/sub>","targets":5},{"name":"dY","targets":6}],"order":[],"autoWidth":false,"orderClasses":false},"selection":{"mode":"multiple","selected":null,"target":"row","selectable":null}},"evals":[],"jsHooks":[]}
```
