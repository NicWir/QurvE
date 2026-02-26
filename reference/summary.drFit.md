# Generic summary function for drFit objects

Generic summary function for drFit objects

## Usage

``` r
# S3 method for class 'drFit'
summary(object, ...)
```

## Arguments

- object:

  object of class `drFit`

- ...:

  Additional arguments. This has currently no effect and is only meant
  to fulfill the requirements of a generic function.

## Value

A dataframe with parameters for all samples extracted from the
dose-response analysis.

## Examples

``` r
# \donttest{
# Create random growth data set
rnd.data1 <- rdm.data(d = 35, mu = 0.8, A = 5, label = 'Test1')
rnd.data2 <- rdm.data(d = 35, mu = 0.6, A = 4.5, label = 'Test2')

rnd.data <- list()
rnd.data[['time']] <- rbind(rnd.data1$time, rnd.data2$time)
rnd.data[['data']] <- rbind(rnd.data1$data, rnd.data2$data)

# Run growth curve analysis workflow
gcFit <- growth.gcFit(time = rnd.data$time,
                       data = rnd.data$data,
                       parallelize = FALSE,
                       control = growth.control(fit.opt = 's',
                                                suppress.messages = TRUE))

# Perform dose-response analysis
drFit <- growth.drFit(gcTable = gcFit$gcTable,
                 control = growth.control(dr.parameter = 'mu.spline'))
#> 
#> === EC 50 Estimation ==============================
#> ---------------------------------------------------
#> --> Checking data ...
#> --> Number of distinct tests found: 2 
#> --> Valid datasets per test: 
#>       TestID Number
#>       Test1  35    
#>       Test2  35    

# Inspect results
summary(drFit)
#>    Test log.x log.y Samples EC50.Estimate EC50.Std..Error EC50.Lower EC50.Upper
#> 1 Test1 FALSE FALSE       0     0.1592961     0.002776046  0.1536414  0.1649507
#> 2 Test2 FALSE FALSE       0     0.1158689     0.002262638  0.1112542  0.1204835
#>       yEC50      test model
#> 1 0.3319046 mu.spline  W1.3
#> 2 0.2356760 mu.spline  W1.4
# }
```
