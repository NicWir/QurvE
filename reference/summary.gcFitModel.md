# Generic summary function for gcFitModel objects

Generic summary function for gcFitModel objects

## Usage

``` r
# S3 method for class 'gcFitModel'
summary(object, ...)
```

## Arguments

- object:

  object of class `gcFitModel`

- ...:

  Additional arguments. This has currently no effect and is only meant
  to fulfill the requirements of a generic function.

## Value

A dataframe with parameters extracted from the growth model fit.

## Examples

``` r
# Create random growth dataset
rnd.dataset <- rdm.data(d = 35, mu = 0.8, A = 5, label = 'Test1')

# Extract time and growth data for single sample
time <- rnd.dataset$time[1,]
data <- rnd.dataset$data[1,-(1:3)] # Remove identifier columns

# Perform parametric fit
TestFit <- growth.gcFitModel(time, data, gcID = 'TestFit',
                 control = growth.control(fit.opt = 'm'))
#> --> Try to fit model logistic
#> ....... OK
#> --> Try to fit model richards
#> ....... OK
#> --> Try to fit model gompertz
#> ....... OK
#> --> Try to fit model gompertz.exp
#> ... ERROR in nls(). For further information see help(growth.gcFitModel)
#> --> Try to fit model huang
#> .......... OK
#> --> Try to fit model baranyi
#> ........ OK
#> 
#> Best fitting model: ~baranyi

summary(TestFit)
#>            mu.model          tD.model     lambda.model          A.model
#> 1 0.808438742753108 0.857389860114149 4.78449176808774 4.76738971743057
#>           dY.model     A.orig.model    dY.orig.model   integral.model
#> 1 4.78225651487036 6.67701126419174 6.77701859051578 77.2669846662877
#>   parameter_nu.model parameter_alpha.model parameter_t_shift.model
#> 1               <NA>                  <NA>                    <NA>
#>    parameter_y0.model         stdmu.model    stdlambda.model
#> 1 -0.0148880144665687 0.00259692561628804 0.0165095397345604
#>            stdA.model         RMSE.model reliable_fit.model  ci90.mu.model.lo
#> 1 0.00235587520661927 0.0150497266900256               TRUE 0.804166800114314
#>    ci90.mu.model.up ci90.lambda.model.lo ci90.lambda.model.up  ci90.A.model.lo
#> 1 0.812710685391901     4.75733357522438     4.81164996095109 4.76351430271568
#>    ci90.A.model.up  ci95.mu.model.lo  ci95.mu.model.up ci95.lambda.model.lo
#> 1 4.77126513214546 0.803348768545183 0.813528716961032       4.752133070208
#>   ci95.lambda.model.up ci95.A.model.lo  ci95.A.model.up
#> 1     4.81685046596747 4.7627722020256 4.77200723283554
```
