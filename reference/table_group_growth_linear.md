# Generate a grouped results table for linear fits with average and standard deviations

Generate a grouped results table for linear fits with average and
standard deviations

## Usage

``` r
table_group_growth_linear(gcTable, html = FALSE)
```

## Arguments

- gcTable:

  An object of class `gcTable`

- html:

  (Logical) Should column headers contain html formatting?

## Value

A data frame with grouped linear fit results. Empty cells indicate that
no reliable fit could be determined.

## Examples

``` r
# \donttest{
# Create random growth data set
rnd.data <- rdm.data(d = 30, mu = 0.6, A = 4.5, label = "Test2")


# Run growth curve analysis workflow
res <- growth.workflow(time = rnd.data$time,
                       data = rnd.data$data,
                       fit.opt = "l",
                       ec50 = FALSE,
                       export.res = FALSE,
                       parallelize = FALSE,
                       suppress.messages = TRUE)

table_group_growth_linear(res$gcFit$gcTable)
#>     Sample|Conc.         mumax            tD       lagtime            dY
#> 1      Test2 | 0 0.468 ± 0.008 1.483 ± 0.027 3.828 ± 0.022 3.825 ± 0.285
#> 2  Test2 | 0.039 0.389 ± 0.004 1.782 ± 0.018 3.964 ± 0.199 1.792 ± 0.148
#> 3  Test2 | 0.059 0.342 ± 0.007 2.027 ± 0.039 4.023 ± 0.234 1.469 ± 0.017
#> 4  Test2 | 0.088 0.288 ± 0.008 2.407 ± 0.067  3.741 ± 0.24 1.159 ± 0.126
#> 5   Test2 | 0.13 0.217 ± 0.007 3.202 ± 0.106 3.452 ± 0.142 0.818 ± 0.115
#> 6    Test2 | 0.2 0.127 ± 0.007 5.476 ± 0.319 3.018 ± 0.779 0.416 ± 0.031
#> 7    Test2 | 0.3 0.015 ± 0.025        15.851         4.091 0.025 ± 0.042
#> 8   Test2 | 0.44                                                        
#> 9   Test2 | 0.67                                                        
#> 10     Test2 | 1                                                        
#>            Y_max t_start(mumax)   t_end(mumax)
#> 1  3.877 ± 0.289           6.75  10.167 ± 0.52
#> 2  1.846 ± 0.151  6.833 ± 0.144    9.75 ± 0.25
#> 3  1.523 ± 0.021     6.75 ± 0.5   10.25 ± 0.75
#> 4  1.211 ± 0.129      6.5 ± 0.5  10.75 ± 0.661
#> 5   0.87 ± 0.117   7.917 ± 0.52  10.583 ± 0.52
#> 6  0.468 ± 0.036    10.75 ± 2.5 13.333 ± 1.528
#> 7          0.129          19.75           21.5
#> 8                                             
#> 9                                             
#> 10                                            

# with HTML formatting
DT::datatable(table_group_growth_linear(res$gcFit$gcTable, html = TRUE),
              escape = FALSE) # Do not escape HTML entities

{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10"],["Test2 | 0","Test2 | 0.039","Test2 | 0.059","Test2 | 0.088","Test2 | 0.13","Test2 | 0.2","Test2 | 0.3","Test2 | 0.44","Test2 | 0.67","Test2 | 1"],["0.468 ± 0.008","0.389 ± 0.004","0.342 ± 0.007","0.288 ± 0.008","0.217 ± 0.007","0.127 ± 0.007","0.015 ± 0.025","","",""],["1.483 ± 0.027","1.782 ± 0.018","2.027 ± 0.039","2.407 ± 0.067","3.202 ± 0.106","5.476 ± 0.319","15.851","","",""],["3.828 ± 0.022","3.964 ± 0.199","4.023 ± 0.234","3.741 ± 0.24","3.452 ± 0.142","3.018 ± 0.779","4.091","","",""],["3.825 ± 0.285","1.792 ± 0.148","1.469 ± 0.017","1.159 ± 0.126","0.818 ± 0.115","0.416 ± 0.031","0.025 ± 0.042","","",""],["3.877 ± 0.289","1.846 ± 0.151","1.523 ± 0.021","1.211 ± 0.129","0.87 ± 0.117","0.468 ± 0.036","0.129","","",""],["6.75","6.833 ± 0.144","6.75 ± 0.5","6.5 ± 0.5","7.917 ± 0.52","10.75 ± 2.5","19.75","","",""],["10.167 ± 0.52","9.75 ± 0.25","10.25 ± 0.75","10.75 ± 0.661","10.583 ± 0.52","13.333 ± 1.528","21.5","","",""]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Sample|Conc.<\/th>\n      <th>mu<sub>max<\/sub><\/th>\n      <th>t<sub>D<\/sub><\/th>\n      <th>lagtime<\/th>\n      <th>dY<\/th>\n      <th>y<sub>max<\/sub><\/th>\n      <th>t<sub>start<\/sub><br>(mu<sub>max<\/sub>)<\/th>\n      <th>t<sub>end<\/sub><br>(mu<sub>max<\/sub>)<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"Sample|Conc.","targets":1},{"name":"mu<sub>max<\/sub>","targets":2},{"name":"t<sub>D<\/sub>","targets":3},{"name":"lagtime","targets":4},{"name":"dY","targets":5},{"name":"y<sub>max<\/sub>","targets":6},{"name":"t<sub>start<\/sub><br>(mu<sub>max<\/sub>)","targets":7},{"name":"t<sub>end<\/sub><br>(mu<sub>max<\/sub>)","targets":8}],"order":[],"autoWidth":false,"orderClasses":false},"selection":{"mode":"multiple","selected":null,"target":"row","selectable":null}},"evals":[],"jsHooks":[]}# }
```
