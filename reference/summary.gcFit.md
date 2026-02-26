# Generic summary function for gcFit objects

Generic summary function for gcFit objects

## Usage

``` r
# S3 method for class 'gcFit'
summary(object, ...)
```

## Arguments

- object:

  object of class `gcFit`

- ...:

  Additional arguments. This has currently no effect and is only meant
  to fulfill the requirements of a generic function.

## Value

A dataframe with parameters extracted from all fits of a workflow.

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
                                                suppress.messages = TRUE,
                                                nboot.gc = 20))
summary(gcFit)
#>    TestId AddId concentration reliability_tag used.model log.x log.y.lin
#> 1   Test1     1         0.000            TRUE         NA FALSE      TRUE
#> 2   Test1     1         0.017            TRUE         NA FALSE      TRUE
#> 3   Test1     1         0.026            TRUE         NA FALSE      TRUE
#> 4   Test1     1         0.039            TRUE         NA FALSE      TRUE
#> 5   Test1     1         0.059            TRUE         NA FALSE      TRUE
#> 6   Test1     1         0.088            TRUE         NA FALSE      TRUE
#> 7   Test1     1         0.130            TRUE         NA FALSE      TRUE
#> 8   Test1     1         0.200            TRUE         NA FALSE      TRUE
#> 9   Test1     1         0.300            TRUE         NA FALSE      TRUE
#> 10  Test1     1         0.440            TRUE         NA FALSE      TRUE
#> 11  Test1     1         0.670            TRUE         NA FALSE      TRUE
#> 12  Test1     1         1.000            TRUE         NA FALSE      TRUE
#> 13  Test1     2         0.000            TRUE         NA FALSE      TRUE
#> 14  Test1     2         0.017            TRUE         NA FALSE      TRUE
#> 15  Test1     2         0.026            TRUE         NA FALSE      TRUE
#> 16  Test1     2         0.039            TRUE         NA FALSE      TRUE
#> 17  Test1     2         0.059            TRUE         NA FALSE      TRUE
#> 18  Test1     2         0.088            TRUE         NA FALSE      TRUE
#> 19  Test1     2         0.130            TRUE         NA FALSE      TRUE
#> 20  Test1     2         0.200            TRUE         NA FALSE      TRUE
#> 21  Test1     2         0.300            TRUE         NA FALSE      TRUE
#> 22  Test1     2         0.440            TRUE         NA FALSE      TRUE
#> 23  Test1     2         0.670            TRUE         NA FALSE      TRUE
#> 24  Test1     2         1.000            TRUE         NA FALSE      TRUE
#> 25  Test1     3         0.000            TRUE         NA FALSE      TRUE
#> 26  Test1     3         0.026            TRUE         NA FALSE      TRUE
#> 27  Test1     3         0.039            TRUE         NA FALSE      TRUE
#> 28  Test1     3         0.059            TRUE         NA FALSE      TRUE
#> 29  Test1     3         0.088            TRUE         NA FALSE      TRUE
#> 30  Test1     3         0.130            TRUE         NA FALSE      TRUE
#> 31  Test1     3         0.200            TRUE         NA FALSE      TRUE
#> 32  Test1     3         0.300            TRUE         NA FALSE      TRUE
#> 33  Test1     3         0.440            TRUE         NA FALSE      TRUE
#> 34  Test1     3         0.670            TRUE         NA FALSE      TRUE
#> 35  Test1     3         1.000            TRUE         NA FALSE      TRUE
#> 36  Test2     1         0.000            TRUE         NA FALSE      TRUE
#> 37  Test2     1         0.017            TRUE         NA FALSE      TRUE
#> 38  Test2     1         0.026            TRUE         NA FALSE      TRUE
#> 39  Test2     1         0.039            TRUE         NA FALSE      TRUE
#> 40  Test2     1         0.059            TRUE         NA FALSE      TRUE
#> 41  Test2     1         0.088            TRUE         NA FALSE      TRUE
#> 42  Test2     1         0.130            TRUE         NA FALSE      TRUE
#> 43  Test2     1         0.200            TRUE         NA FALSE      TRUE
#> 44  Test2     1         0.300            TRUE         NA FALSE      TRUE
#> 45  Test2     1         0.440            TRUE         NA FALSE      TRUE
#> 46  Test2     1         0.670            TRUE         NA FALSE      TRUE
#> 47  Test2     1         1.000            TRUE         NA FALSE      TRUE
#> 48  Test2     2         0.000            TRUE         NA FALSE      TRUE
#> 49  Test2     2         0.017            TRUE         NA FALSE      TRUE
#> 50  Test2     2         0.026            TRUE         NA FALSE      TRUE
#> 51  Test2     2         0.039            TRUE         NA FALSE      TRUE
#> 52  Test2     2         0.059            TRUE         NA FALSE      TRUE
#> 53  Test2     2         0.088            TRUE         NA FALSE      TRUE
#> 54  Test2     2         0.130            TRUE         NA FALSE      TRUE
#> 55  Test2     2         0.200            TRUE         NA FALSE      TRUE
#> 56  Test2     2         0.300            TRUE         NA FALSE      TRUE
#> 57  Test2     2         0.440            TRUE         NA FALSE      TRUE
#> 58  Test2     2         0.670            TRUE         NA FALSE      TRUE
#> 59  Test2     2         1.000            TRUE         NA FALSE      TRUE
#> 60  Test2     3         0.000            TRUE         NA FALSE      TRUE
#> 61  Test2     3         0.026            TRUE         NA FALSE      TRUE
#> 62  Test2     3         0.039            TRUE         NA FALSE      TRUE
#> 63  Test2     3         0.059            TRUE         NA FALSE      TRUE
#> 64  Test2     3         0.088            TRUE         NA FALSE      TRUE
#> 65  Test2     3         0.130            TRUE         NA FALSE      TRUE
#> 66  Test2     3         0.200            TRUE         NA FALSE      TRUE
#> 67  Test2     3         0.300            TRUE         NA FALSE      TRUE
#> 68  Test2     3         0.440            TRUE         NA FALSE      TRUE
#> 69  Test2     3         0.670            TRUE         NA FALSE      TRUE
#> 70  Test2     3         1.000            TRUE         NA FALSE      TRUE
#>    log.y.spline log.y.model nboot.gc mu.linfit tD.linfit lambda.linfit
#> 1          TRUE        TRUE       20         0      <NA>          <NA>
#> 2          TRUE        TRUE       20         0      <NA>          <NA>
#> 3          TRUE        TRUE       20         0      <NA>          <NA>
#> 4          TRUE        TRUE       20         0      <NA>          <NA>
#> 5          TRUE        TRUE       20         0      <NA>          <NA>
#> 6          TRUE        TRUE       20         0      <NA>          <NA>
#> 7          TRUE        TRUE       20         0      <NA>          <NA>
#> 8          TRUE        TRUE       20         0      <NA>          <NA>
#> 9          TRUE        TRUE       20         0      <NA>          <NA>
#> 10         TRUE        TRUE       20         0      <NA>          <NA>
#> 11         TRUE        TRUE       20         0      <NA>          <NA>
#> 12         TRUE        TRUE       20         0      <NA>          <NA>
#> 13         TRUE        TRUE       20         0      <NA>          <NA>
#> 14         TRUE        TRUE       20         0      <NA>          <NA>
#> 15         TRUE        TRUE       20         0      <NA>          <NA>
#> 16         TRUE        TRUE       20         0      <NA>          <NA>
#> 17         TRUE        TRUE       20         0      <NA>          <NA>
#> 18         TRUE        TRUE       20         0      <NA>          <NA>
#> 19         TRUE        TRUE       20         0      <NA>          <NA>
#> 20         TRUE        TRUE       20         0      <NA>          <NA>
#> 21         TRUE        TRUE       20         0      <NA>          <NA>
#> 22         TRUE        TRUE       20         0      <NA>          <NA>
#> 23         TRUE        TRUE       20         0      <NA>          <NA>
#> 24         TRUE        TRUE       20         0      <NA>          <NA>
#> 25         TRUE        TRUE       20         0      <NA>          <NA>
#> 26         TRUE        TRUE       20         0      <NA>          <NA>
#> 27         TRUE        TRUE       20         0      <NA>          <NA>
#> 28         TRUE        TRUE       20         0      <NA>          <NA>
#> 29         TRUE        TRUE       20         0      <NA>          <NA>
#> 30         TRUE        TRUE       20         0      <NA>          <NA>
#> 31         TRUE        TRUE       20         0      <NA>          <NA>
#> 32         TRUE        TRUE       20         0      <NA>          <NA>
#> 33         TRUE        TRUE       20         0      <NA>          <NA>
#> 34         TRUE        TRUE       20         0      <NA>          <NA>
#> 35         TRUE        TRUE       20         0      <NA>          <NA>
#> 36         TRUE        TRUE       20         0      <NA>          <NA>
#> 37         TRUE        TRUE       20         0      <NA>          <NA>
#> 38         TRUE        TRUE       20         0      <NA>          <NA>
#> 39         TRUE        TRUE       20         0      <NA>          <NA>
#> 40         TRUE        TRUE       20         0      <NA>          <NA>
#> 41         TRUE        TRUE       20         0      <NA>          <NA>
#> 42         TRUE        TRUE       20         0      <NA>          <NA>
#> 43         TRUE        TRUE       20         0      <NA>          <NA>
#> 44         TRUE        TRUE       20         0      <NA>          <NA>
#> 45         TRUE        TRUE       20         0      <NA>          <NA>
#> 46         TRUE        TRUE       20         0      <NA>          <NA>
#> 47         TRUE        TRUE       20         0      <NA>          <NA>
#> 48         TRUE        TRUE       20         0      <NA>          <NA>
#> 49         TRUE        TRUE       20         0      <NA>          <NA>
#> 50         TRUE        TRUE       20         0      <NA>          <NA>
#> 51         TRUE        TRUE       20         0      <NA>          <NA>
#> 52         TRUE        TRUE       20         0      <NA>          <NA>
#> 53         TRUE        TRUE       20         0      <NA>          <NA>
#> 54         TRUE        TRUE       20         0      <NA>          <NA>
#> 55         TRUE        TRUE       20         0      <NA>          <NA>
#> 56         TRUE        TRUE       20         0      <NA>          <NA>
#> 57         TRUE        TRUE       20         0      <NA>          <NA>
#> 58         TRUE        TRUE       20         0      <NA>          <NA>
#> 59         TRUE        TRUE       20         0      <NA>          <NA>
#> 60         TRUE        TRUE       20         0      <NA>          <NA>
#> 61         TRUE        TRUE       20         0      <NA>          <NA>
#> 62         TRUE        TRUE       20         0      <NA>          <NA>
#> 63         TRUE        TRUE       20         0      <NA>          <NA>
#> 64         TRUE        TRUE       20         0      <NA>          <NA>
#> 65         TRUE        TRUE       20         0      <NA>          <NA>
#> 66         TRUE        TRUE       20         0      <NA>          <NA>
#> 67         TRUE        TRUE       20         0      <NA>          <NA>
#> 68         TRUE        TRUE       20         0      <NA>          <NA>
#> 69         TRUE        TRUE       20         0      <NA>          <NA>
#> 70         TRUE        TRUE       20         0      <NA>          <NA>
#>    dY.linfit A.linfit tmu.start.linfit tmu.end.linfit r2mu.linfit
#> 1          0     <NA>             <NA>           <NA>        <NA>
#> 2          0     <NA>             <NA>           <NA>        <NA>
#> 3          0     <NA>             <NA>           <NA>        <NA>
#> 4          0     <NA>             <NA>           <NA>        <NA>
#> 5          0     <NA>             <NA>           <NA>        <NA>
#> 6          0     <NA>             <NA>           <NA>        <NA>
#> 7          0     <NA>             <NA>           <NA>        <NA>
#> 8          0     <NA>             <NA>           <NA>        <NA>
#> 9          0     <NA>             <NA>           <NA>        <NA>
#> 10         0     <NA>             <NA>           <NA>        <NA>
#> 11         0     <NA>             <NA>           <NA>        <NA>
#> 12         0     <NA>             <NA>           <NA>        <NA>
#> 13         0     <NA>             <NA>           <NA>        <NA>
#> 14         0     <NA>             <NA>           <NA>        <NA>
#> 15         0     <NA>             <NA>           <NA>        <NA>
#> 16         0     <NA>             <NA>           <NA>        <NA>
#> 17         0     <NA>             <NA>           <NA>        <NA>
#> 18         0     <NA>             <NA>           <NA>        <NA>
#> 19         0     <NA>             <NA>           <NA>        <NA>
#> 20         0     <NA>             <NA>           <NA>        <NA>
#> 21         0     <NA>             <NA>           <NA>        <NA>
#> 22         0     <NA>             <NA>           <NA>        <NA>
#> 23         0     <NA>             <NA>           <NA>        <NA>
#> 24         0     <NA>             <NA>           <NA>        <NA>
#> 25         0     <NA>             <NA>           <NA>        <NA>
#> 26         0     <NA>             <NA>           <NA>        <NA>
#> 27         0     <NA>             <NA>           <NA>        <NA>
#> 28         0     <NA>             <NA>           <NA>        <NA>
#> 29         0     <NA>             <NA>           <NA>        <NA>
#> 30         0     <NA>             <NA>           <NA>        <NA>
#> 31         0     <NA>             <NA>           <NA>        <NA>
#> 32         0     <NA>             <NA>           <NA>        <NA>
#> 33         0     <NA>             <NA>           <NA>        <NA>
#> 34         0     <NA>             <NA>           <NA>        <NA>
#> 35         0     <NA>             <NA>           <NA>        <NA>
#> 36         0     <NA>             <NA>           <NA>        <NA>
#> 37         0     <NA>             <NA>           <NA>        <NA>
#> 38         0     <NA>             <NA>           <NA>        <NA>
#> 39         0     <NA>             <NA>           <NA>        <NA>
#> 40         0     <NA>             <NA>           <NA>        <NA>
#> 41         0     <NA>             <NA>           <NA>        <NA>
#> 42         0     <NA>             <NA>           <NA>        <NA>
#> 43         0     <NA>             <NA>           <NA>        <NA>
#> 44         0     <NA>             <NA>           <NA>        <NA>
#> 45         0     <NA>             <NA>           <NA>        <NA>
#> 46         0     <NA>             <NA>           <NA>        <NA>
#> 47         0     <NA>             <NA>           <NA>        <NA>
#> 48         0     <NA>             <NA>           <NA>        <NA>
#> 49         0     <NA>             <NA>           <NA>        <NA>
#> 50         0     <NA>             <NA>           <NA>        <NA>
#> 51         0     <NA>             <NA>           <NA>        <NA>
#> 52         0     <NA>             <NA>           <NA>        <NA>
#> 53         0     <NA>             <NA>           <NA>        <NA>
#> 54         0     <NA>             <NA>           <NA>        <NA>
#> 55         0     <NA>             <NA>           <NA>        <NA>
#> 56         0     <NA>             <NA>           <NA>        <NA>
#> 57         0     <NA>             <NA>           <NA>        <NA>
#> 58         0     <NA>             <NA>           <NA>        <NA>
#> 59         0     <NA>             <NA>           <NA>        <NA>
#> 60         0     <NA>             <NA>           <NA>        <NA>
#> 61         0     <NA>             <NA>           <NA>        <NA>
#> 62         0     <NA>             <NA>           <NA>        <NA>
#> 63         0     <NA>             <NA>           <NA>        <NA>
#> 64         0     <NA>             <NA>           <NA>        <NA>
#> 65         0     <NA>             <NA>           <NA>        <NA>
#> 66         0     <NA>             <NA>           <NA>        <NA>
#> 67         0     <NA>             <NA>           <NA>        <NA>
#> 68         0     <NA>             <NA>           <NA>        <NA>
#> 69         0     <NA>             <NA>           <NA>        <NA>
#> 70         0     <NA>             <NA>           <NA>        <NA>
#>    reliable_fit.linfit mu2.linfit tD2.linfit tmu2.start.linfit tmu2.end.linfit
#> 1                FALSE       <NA>       <NA>              <NA>            <NA>
#> 2                FALSE       <NA>       <NA>              <NA>            <NA>
#> 3                FALSE       <NA>       <NA>              <NA>            <NA>
#> 4                FALSE       <NA>       <NA>              <NA>            <NA>
#> 5                FALSE       <NA>       <NA>              <NA>            <NA>
#> 6                FALSE       <NA>       <NA>              <NA>            <NA>
#> 7                FALSE       <NA>       <NA>              <NA>            <NA>
#> 8                FALSE       <NA>       <NA>              <NA>            <NA>
#> 9                FALSE       <NA>       <NA>              <NA>            <NA>
#> 10               FALSE       <NA>       <NA>              <NA>            <NA>
#> 11               FALSE       <NA>       <NA>              <NA>            <NA>
#> 12               FALSE       <NA>       <NA>              <NA>            <NA>
#> 13               FALSE       <NA>       <NA>              <NA>            <NA>
#> 14               FALSE       <NA>       <NA>              <NA>            <NA>
#> 15               FALSE       <NA>       <NA>              <NA>            <NA>
#> 16               FALSE       <NA>       <NA>              <NA>            <NA>
#> 17               FALSE       <NA>       <NA>              <NA>            <NA>
#> 18               FALSE       <NA>       <NA>              <NA>            <NA>
#> 19               FALSE       <NA>       <NA>              <NA>            <NA>
#> 20               FALSE       <NA>       <NA>              <NA>            <NA>
#> 21               FALSE       <NA>       <NA>              <NA>            <NA>
#> 22               FALSE       <NA>       <NA>              <NA>            <NA>
#> 23               FALSE       <NA>       <NA>              <NA>            <NA>
#> 24               FALSE       <NA>       <NA>              <NA>            <NA>
#> 25               FALSE       <NA>       <NA>              <NA>            <NA>
#> 26               FALSE       <NA>       <NA>              <NA>            <NA>
#> 27               FALSE       <NA>       <NA>              <NA>            <NA>
#> 28               FALSE       <NA>       <NA>              <NA>            <NA>
#> 29               FALSE       <NA>       <NA>              <NA>            <NA>
#> 30               FALSE       <NA>       <NA>              <NA>            <NA>
#> 31               FALSE       <NA>       <NA>              <NA>            <NA>
#> 32               FALSE       <NA>       <NA>              <NA>            <NA>
#> 33               FALSE       <NA>       <NA>              <NA>            <NA>
#> 34               FALSE       <NA>       <NA>              <NA>            <NA>
#> 35               FALSE       <NA>       <NA>              <NA>            <NA>
#> 36               FALSE       <NA>       <NA>              <NA>            <NA>
#> 37               FALSE       <NA>       <NA>              <NA>            <NA>
#> 38               FALSE       <NA>       <NA>              <NA>            <NA>
#> 39               FALSE       <NA>       <NA>              <NA>            <NA>
#> 40               FALSE       <NA>       <NA>              <NA>            <NA>
#> 41               FALSE       <NA>       <NA>              <NA>            <NA>
#> 42               FALSE       <NA>       <NA>              <NA>            <NA>
#> 43               FALSE       <NA>       <NA>              <NA>            <NA>
#> 44               FALSE       <NA>       <NA>              <NA>            <NA>
#> 45               FALSE       <NA>       <NA>              <NA>            <NA>
#> 46               FALSE       <NA>       <NA>              <NA>            <NA>
#> 47               FALSE       <NA>       <NA>              <NA>            <NA>
#> 48               FALSE       <NA>       <NA>              <NA>            <NA>
#> 49               FALSE       <NA>       <NA>              <NA>            <NA>
#> 50               FALSE       <NA>       <NA>              <NA>            <NA>
#> 51               FALSE       <NA>       <NA>              <NA>            <NA>
#> 52               FALSE       <NA>       <NA>              <NA>            <NA>
#> 53               FALSE       <NA>       <NA>              <NA>            <NA>
#> 54               FALSE       <NA>       <NA>              <NA>            <NA>
#> 55               FALSE       <NA>       <NA>              <NA>            <NA>
#> 56               FALSE       <NA>       <NA>              <NA>            <NA>
#> 57               FALSE       <NA>       <NA>              <NA>            <NA>
#> 58               FALSE       <NA>       <NA>              <NA>            <NA>
#> 59               FALSE       <NA>       <NA>              <NA>            <NA>
#> 60               FALSE       <NA>       <NA>              <NA>            <NA>
#> 61               FALSE       <NA>       <NA>              <NA>            <NA>
#> 62               FALSE       <NA>       <NA>              <NA>            <NA>
#> 63               FALSE       <NA>       <NA>              <NA>            <NA>
#> 64               FALSE       <NA>       <NA>              <NA>            <NA>
#> 65               FALSE       <NA>       <NA>              <NA>            <NA>
#> 66               FALSE       <NA>       <NA>              <NA>            <NA>
#> 67               FALSE       <NA>       <NA>              <NA>            <NA>
#> 68               FALSE       <NA>       <NA>              <NA>            <NA>
#> 69               FALSE       <NA>       <NA>              <NA>            <NA>
#> 70               FALSE       <NA>       <NA>              <NA>            <NA>
#>    r2mu2.linfit reliable_fit2.linfit mu.model tD.model lambda.model A.model
#> 1          <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 2          <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 3          <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 4          <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 5          <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 6          <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 7          <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 8          <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 9          <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 10         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 11         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 12         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 13         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 14         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 15         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 16         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 17         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 18         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 19         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 20         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 21         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 22         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 23         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 24         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 25         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 26         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 27         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 28         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 29         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 30         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 31         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 32         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 33         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 34         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 35         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 36         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 37         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 38         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 39         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 40         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 41         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 42         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 43         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 44         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 45         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 46         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 47         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 48         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 49         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 50         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 51         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 52         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 53         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 54         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 55         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 56         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 57         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 58         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 59         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 60         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 61         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 62         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 63         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 64         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 65         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 66         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 67         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 68         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 69         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#> 70         <NA>                FALSE     <NA>     <NA>         <NA>    <NA>
#>    dY.model A.orig.model dY.orig.model integral.model parameter_nu.model
#> 1      <NA>         <NA>          <NA>           <NA>               <NA>
#> 2      <NA>         <NA>          <NA>           <NA>               <NA>
#> 3      <NA>         <NA>          <NA>           <NA>               <NA>
#> 4      <NA>         <NA>          <NA>           <NA>               <NA>
#> 5      <NA>         <NA>          <NA>           <NA>               <NA>
#> 6      <NA>         <NA>          <NA>           <NA>               <NA>
#> 7      <NA>         <NA>          <NA>           <NA>               <NA>
#> 8      <NA>         <NA>          <NA>           <NA>               <NA>
#> 9      <NA>         <NA>          <NA>           <NA>               <NA>
#> 10     <NA>         <NA>          <NA>           <NA>               <NA>
#> 11     <NA>         <NA>          <NA>           <NA>               <NA>
#> 12     <NA>         <NA>          <NA>           <NA>               <NA>
#> 13     <NA>         <NA>          <NA>           <NA>               <NA>
#> 14     <NA>         <NA>          <NA>           <NA>               <NA>
#> 15     <NA>         <NA>          <NA>           <NA>               <NA>
#> 16     <NA>         <NA>          <NA>           <NA>               <NA>
#> 17     <NA>         <NA>          <NA>           <NA>               <NA>
#> 18     <NA>         <NA>          <NA>           <NA>               <NA>
#> 19     <NA>         <NA>          <NA>           <NA>               <NA>
#> 20     <NA>         <NA>          <NA>           <NA>               <NA>
#> 21     <NA>         <NA>          <NA>           <NA>               <NA>
#> 22     <NA>         <NA>          <NA>           <NA>               <NA>
#> 23     <NA>         <NA>          <NA>           <NA>               <NA>
#> 24     <NA>         <NA>          <NA>           <NA>               <NA>
#> 25     <NA>         <NA>          <NA>           <NA>               <NA>
#> 26     <NA>         <NA>          <NA>           <NA>               <NA>
#> 27     <NA>         <NA>          <NA>           <NA>               <NA>
#> 28     <NA>         <NA>          <NA>           <NA>               <NA>
#> 29     <NA>         <NA>          <NA>           <NA>               <NA>
#> 30     <NA>         <NA>          <NA>           <NA>               <NA>
#> 31     <NA>         <NA>          <NA>           <NA>               <NA>
#> 32     <NA>         <NA>          <NA>           <NA>               <NA>
#> 33     <NA>         <NA>          <NA>           <NA>               <NA>
#> 34     <NA>         <NA>          <NA>           <NA>               <NA>
#> 35     <NA>         <NA>          <NA>           <NA>               <NA>
#> 36     <NA>         <NA>          <NA>           <NA>               <NA>
#> 37     <NA>         <NA>          <NA>           <NA>               <NA>
#> 38     <NA>         <NA>          <NA>           <NA>               <NA>
#> 39     <NA>         <NA>          <NA>           <NA>               <NA>
#> 40     <NA>         <NA>          <NA>           <NA>               <NA>
#> 41     <NA>         <NA>          <NA>           <NA>               <NA>
#> 42     <NA>         <NA>          <NA>           <NA>               <NA>
#> 43     <NA>         <NA>          <NA>           <NA>               <NA>
#> 44     <NA>         <NA>          <NA>           <NA>               <NA>
#> 45     <NA>         <NA>          <NA>           <NA>               <NA>
#> 46     <NA>         <NA>          <NA>           <NA>               <NA>
#> 47     <NA>         <NA>          <NA>           <NA>               <NA>
#> 48     <NA>         <NA>          <NA>           <NA>               <NA>
#> 49     <NA>         <NA>          <NA>           <NA>               <NA>
#> 50     <NA>         <NA>          <NA>           <NA>               <NA>
#> 51     <NA>         <NA>          <NA>           <NA>               <NA>
#> 52     <NA>         <NA>          <NA>           <NA>               <NA>
#> 53     <NA>         <NA>          <NA>           <NA>               <NA>
#> 54     <NA>         <NA>          <NA>           <NA>               <NA>
#> 55     <NA>         <NA>          <NA>           <NA>               <NA>
#> 56     <NA>         <NA>          <NA>           <NA>               <NA>
#> 57     <NA>         <NA>          <NA>           <NA>               <NA>
#> 58     <NA>         <NA>          <NA>           <NA>               <NA>
#> 59     <NA>         <NA>          <NA>           <NA>               <NA>
#> 60     <NA>         <NA>          <NA>           <NA>               <NA>
#> 61     <NA>         <NA>          <NA>           <NA>               <NA>
#> 62     <NA>         <NA>          <NA>           <NA>               <NA>
#> 63     <NA>         <NA>          <NA>           <NA>               <NA>
#> 64     <NA>         <NA>          <NA>           <NA>               <NA>
#> 65     <NA>         <NA>          <NA>           <NA>               <NA>
#> 66     <NA>         <NA>          <NA>           <NA>               <NA>
#> 67     <NA>         <NA>          <NA>           <NA>               <NA>
#> 68     <NA>         <NA>          <NA>           <NA>               <NA>
#> 69     <NA>         <NA>          <NA>           <NA>               <NA>
#> 70     <NA>         <NA>          <NA>           <NA>               <NA>
#>    parameter_alpha.model parameter_t_shift.model parameter_y0.model stdmu.model
#> 1                   <NA>                    <NA>               <NA>        <NA>
#> 2                   <NA>                    <NA>               <NA>        <NA>
#> 3                   <NA>                    <NA>               <NA>        <NA>
#> 4                   <NA>                    <NA>               <NA>        <NA>
#> 5                   <NA>                    <NA>               <NA>        <NA>
#> 6                   <NA>                    <NA>               <NA>        <NA>
#> 7                   <NA>                    <NA>               <NA>        <NA>
#> 8                   <NA>                    <NA>               <NA>        <NA>
#> 9                   <NA>                    <NA>               <NA>        <NA>
#> 10                  <NA>                    <NA>               <NA>        <NA>
#> 11                  <NA>                    <NA>               <NA>        <NA>
#> 12                  <NA>                    <NA>               <NA>        <NA>
#> 13                  <NA>                    <NA>               <NA>        <NA>
#> 14                  <NA>                    <NA>               <NA>        <NA>
#> 15                  <NA>                    <NA>               <NA>        <NA>
#> 16                  <NA>                    <NA>               <NA>        <NA>
#> 17                  <NA>                    <NA>               <NA>        <NA>
#> 18                  <NA>                    <NA>               <NA>        <NA>
#> 19                  <NA>                    <NA>               <NA>        <NA>
#> 20                  <NA>                    <NA>               <NA>        <NA>
#> 21                  <NA>                    <NA>               <NA>        <NA>
#> 22                  <NA>                    <NA>               <NA>        <NA>
#> 23                  <NA>                    <NA>               <NA>        <NA>
#> 24                  <NA>                    <NA>               <NA>        <NA>
#> 25                  <NA>                    <NA>               <NA>        <NA>
#> 26                  <NA>                    <NA>               <NA>        <NA>
#> 27                  <NA>                    <NA>               <NA>        <NA>
#> 28                  <NA>                    <NA>               <NA>        <NA>
#> 29                  <NA>                    <NA>               <NA>        <NA>
#> 30                  <NA>                    <NA>               <NA>        <NA>
#> 31                  <NA>                    <NA>               <NA>        <NA>
#> 32                  <NA>                    <NA>               <NA>        <NA>
#> 33                  <NA>                    <NA>               <NA>        <NA>
#> 34                  <NA>                    <NA>               <NA>        <NA>
#> 35                  <NA>                    <NA>               <NA>        <NA>
#> 36                  <NA>                    <NA>               <NA>        <NA>
#> 37                  <NA>                    <NA>               <NA>        <NA>
#> 38                  <NA>                    <NA>               <NA>        <NA>
#> 39                  <NA>                    <NA>               <NA>        <NA>
#> 40                  <NA>                    <NA>               <NA>        <NA>
#> 41                  <NA>                    <NA>               <NA>        <NA>
#> 42                  <NA>                    <NA>               <NA>        <NA>
#> 43                  <NA>                    <NA>               <NA>        <NA>
#> 44                  <NA>                    <NA>               <NA>        <NA>
#> 45                  <NA>                    <NA>               <NA>        <NA>
#> 46                  <NA>                    <NA>               <NA>        <NA>
#> 47                  <NA>                    <NA>               <NA>        <NA>
#> 48                  <NA>                    <NA>               <NA>        <NA>
#> 49                  <NA>                    <NA>               <NA>        <NA>
#> 50                  <NA>                    <NA>               <NA>        <NA>
#> 51                  <NA>                    <NA>               <NA>        <NA>
#> 52                  <NA>                    <NA>               <NA>        <NA>
#> 53                  <NA>                    <NA>               <NA>        <NA>
#> 54                  <NA>                    <NA>               <NA>        <NA>
#> 55                  <NA>                    <NA>               <NA>        <NA>
#> 56                  <NA>                    <NA>               <NA>        <NA>
#> 57                  <NA>                    <NA>               <NA>        <NA>
#> 58                  <NA>                    <NA>               <NA>        <NA>
#> 59                  <NA>                    <NA>               <NA>        <NA>
#> 60                  <NA>                    <NA>               <NA>        <NA>
#> 61                  <NA>                    <NA>               <NA>        <NA>
#> 62                  <NA>                    <NA>               <NA>        <NA>
#> 63                  <NA>                    <NA>               <NA>        <NA>
#> 64                  <NA>                    <NA>               <NA>        <NA>
#> 65                  <NA>                    <NA>               <NA>        <NA>
#> 66                  <NA>                    <NA>               <NA>        <NA>
#> 67                  <NA>                    <NA>               <NA>        <NA>
#> 68                  <NA>                    <NA>               <NA>        <NA>
#> 69                  <NA>                    <NA>               <NA>        <NA>
#> 70                  <NA>                    <NA>               <NA>        <NA>
#>    stdlambda.model stdA.model RMSE.model reliable_fit.model ci90.mu.model.lo
#> 1             <NA>       <NA>       <NA>              FALSE             <NA>
#> 2             <NA>       <NA>       <NA>              FALSE             <NA>
#> 3             <NA>       <NA>       <NA>              FALSE             <NA>
#> 4             <NA>       <NA>       <NA>              FALSE             <NA>
#> 5             <NA>       <NA>       <NA>              FALSE             <NA>
#> 6             <NA>       <NA>       <NA>              FALSE             <NA>
#> 7             <NA>       <NA>       <NA>              FALSE             <NA>
#> 8             <NA>       <NA>       <NA>              FALSE             <NA>
#> 9             <NA>       <NA>       <NA>              FALSE             <NA>
#> 10            <NA>       <NA>       <NA>              FALSE             <NA>
#> 11            <NA>       <NA>       <NA>              FALSE             <NA>
#> 12            <NA>       <NA>       <NA>              FALSE             <NA>
#> 13            <NA>       <NA>       <NA>              FALSE             <NA>
#> 14            <NA>       <NA>       <NA>              FALSE             <NA>
#> 15            <NA>       <NA>       <NA>              FALSE             <NA>
#> 16            <NA>       <NA>       <NA>              FALSE             <NA>
#> 17            <NA>       <NA>       <NA>              FALSE             <NA>
#> 18            <NA>       <NA>       <NA>              FALSE             <NA>
#> 19            <NA>       <NA>       <NA>              FALSE             <NA>
#> 20            <NA>       <NA>       <NA>              FALSE             <NA>
#> 21            <NA>       <NA>       <NA>              FALSE             <NA>
#> 22            <NA>       <NA>       <NA>              FALSE             <NA>
#> 23            <NA>       <NA>       <NA>              FALSE             <NA>
#> 24            <NA>       <NA>       <NA>              FALSE             <NA>
#> 25            <NA>       <NA>       <NA>              FALSE             <NA>
#> 26            <NA>       <NA>       <NA>              FALSE             <NA>
#> 27            <NA>       <NA>       <NA>              FALSE             <NA>
#> 28            <NA>       <NA>       <NA>              FALSE             <NA>
#> 29            <NA>       <NA>       <NA>              FALSE             <NA>
#> 30            <NA>       <NA>       <NA>              FALSE             <NA>
#> 31            <NA>       <NA>       <NA>              FALSE             <NA>
#> 32            <NA>       <NA>       <NA>              FALSE             <NA>
#> 33            <NA>       <NA>       <NA>              FALSE             <NA>
#> 34            <NA>       <NA>       <NA>              FALSE             <NA>
#> 35            <NA>       <NA>       <NA>              FALSE             <NA>
#> 36            <NA>       <NA>       <NA>              FALSE             <NA>
#> 37            <NA>       <NA>       <NA>              FALSE             <NA>
#> 38            <NA>       <NA>       <NA>              FALSE             <NA>
#> 39            <NA>       <NA>       <NA>              FALSE             <NA>
#> 40            <NA>       <NA>       <NA>              FALSE             <NA>
#> 41            <NA>       <NA>       <NA>              FALSE             <NA>
#> 42            <NA>       <NA>       <NA>              FALSE             <NA>
#> 43            <NA>       <NA>       <NA>              FALSE             <NA>
#> 44            <NA>       <NA>       <NA>              FALSE             <NA>
#> 45            <NA>       <NA>       <NA>              FALSE             <NA>
#> 46            <NA>       <NA>       <NA>              FALSE             <NA>
#> 47            <NA>       <NA>       <NA>              FALSE             <NA>
#> 48            <NA>       <NA>       <NA>              FALSE             <NA>
#> 49            <NA>       <NA>       <NA>              FALSE             <NA>
#> 50            <NA>       <NA>       <NA>              FALSE             <NA>
#> 51            <NA>       <NA>       <NA>              FALSE             <NA>
#> 52            <NA>       <NA>       <NA>              FALSE             <NA>
#> 53            <NA>       <NA>       <NA>              FALSE             <NA>
#> 54            <NA>       <NA>       <NA>              FALSE             <NA>
#> 55            <NA>       <NA>       <NA>              FALSE             <NA>
#> 56            <NA>       <NA>       <NA>              FALSE             <NA>
#> 57            <NA>       <NA>       <NA>              FALSE             <NA>
#> 58            <NA>       <NA>       <NA>              FALSE             <NA>
#> 59            <NA>       <NA>       <NA>              FALSE             <NA>
#> 60            <NA>       <NA>       <NA>              FALSE             <NA>
#> 61            <NA>       <NA>       <NA>              FALSE             <NA>
#> 62            <NA>       <NA>       <NA>              FALSE             <NA>
#> 63            <NA>       <NA>       <NA>              FALSE             <NA>
#> 64            <NA>       <NA>       <NA>              FALSE             <NA>
#> 65            <NA>       <NA>       <NA>              FALSE             <NA>
#> 66            <NA>       <NA>       <NA>              FALSE             <NA>
#> 67            <NA>       <NA>       <NA>              FALSE             <NA>
#> 68            <NA>       <NA>       <NA>              FALSE             <NA>
#> 69            <NA>       <NA>       <NA>              FALSE             <NA>
#> 70            <NA>       <NA>       <NA>              FALSE             <NA>
#>    ci90.mu.model.up ci90.lambda.model.lo ci90.lambda.model.up ci90.A.model.lo
#> 1              <NA>                 <NA>                 <NA>            <NA>
#> 2              <NA>                 <NA>                 <NA>            <NA>
#> 3              <NA>                 <NA>                 <NA>            <NA>
#> 4              <NA>                 <NA>                 <NA>            <NA>
#> 5              <NA>                 <NA>                 <NA>            <NA>
#> 6              <NA>                 <NA>                 <NA>            <NA>
#> 7              <NA>                 <NA>                 <NA>            <NA>
#> 8              <NA>                 <NA>                 <NA>            <NA>
#> 9              <NA>                 <NA>                 <NA>            <NA>
#> 10             <NA>                 <NA>                 <NA>            <NA>
#> 11             <NA>                 <NA>                 <NA>            <NA>
#> 12             <NA>                 <NA>                 <NA>            <NA>
#> 13             <NA>                 <NA>                 <NA>            <NA>
#> 14             <NA>                 <NA>                 <NA>            <NA>
#> 15             <NA>                 <NA>                 <NA>            <NA>
#> 16             <NA>                 <NA>                 <NA>            <NA>
#> 17             <NA>                 <NA>                 <NA>            <NA>
#> 18             <NA>                 <NA>                 <NA>            <NA>
#> 19             <NA>                 <NA>                 <NA>            <NA>
#> 20             <NA>                 <NA>                 <NA>            <NA>
#> 21             <NA>                 <NA>                 <NA>            <NA>
#> 22             <NA>                 <NA>                 <NA>            <NA>
#> 23             <NA>                 <NA>                 <NA>            <NA>
#> 24             <NA>                 <NA>                 <NA>            <NA>
#> 25             <NA>                 <NA>                 <NA>            <NA>
#> 26             <NA>                 <NA>                 <NA>            <NA>
#> 27             <NA>                 <NA>                 <NA>            <NA>
#> 28             <NA>                 <NA>                 <NA>            <NA>
#> 29             <NA>                 <NA>                 <NA>            <NA>
#> 30             <NA>                 <NA>                 <NA>            <NA>
#> 31             <NA>                 <NA>                 <NA>            <NA>
#> 32             <NA>                 <NA>                 <NA>            <NA>
#> 33             <NA>                 <NA>                 <NA>            <NA>
#> 34             <NA>                 <NA>                 <NA>            <NA>
#> 35             <NA>                 <NA>                 <NA>            <NA>
#> 36             <NA>                 <NA>                 <NA>            <NA>
#> 37             <NA>                 <NA>                 <NA>            <NA>
#> 38             <NA>                 <NA>                 <NA>            <NA>
#> 39             <NA>                 <NA>                 <NA>            <NA>
#> 40             <NA>                 <NA>                 <NA>            <NA>
#> 41             <NA>                 <NA>                 <NA>            <NA>
#> 42             <NA>                 <NA>                 <NA>            <NA>
#> 43             <NA>                 <NA>                 <NA>            <NA>
#> 44             <NA>                 <NA>                 <NA>            <NA>
#> 45             <NA>                 <NA>                 <NA>            <NA>
#> 46             <NA>                 <NA>                 <NA>            <NA>
#> 47             <NA>                 <NA>                 <NA>            <NA>
#> 48             <NA>                 <NA>                 <NA>            <NA>
#> 49             <NA>                 <NA>                 <NA>            <NA>
#> 50             <NA>                 <NA>                 <NA>            <NA>
#> 51             <NA>                 <NA>                 <NA>            <NA>
#> 52             <NA>                 <NA>                 <NA>            <NA>
#> 53             <NA>                 <NA>                 <NA>            <NA>
#> 54             <NA>                 <NA>                 <NA>            <NA>
#> 55             <NA>                 <NA>                 <NA>            <NA>
#> 56             <NA>                 <NA>                 <NA>            <NA>
#> 57             <NA>                 <NA>                 <NA>            <NA>
#> 58             <NA>                 <NA>                 <NA>            <NA>
#> 59             <NA>                 <NA>                 <NA>            <NA>
#> 60             <NA>                 <NA>                 <NA>            <NA>
#> 61             <NA>                 <NA>                 <NA>            <NA>
#> 62             <NA>                 <NA>                 <NA>            <NA>
#> 63             <NA>                 <NA>                 <NA>            <NA>
#> 64             <NA>                 <NA>                 <NA>            <NA>
#> 65             <NA>                 <NA>                 <NA>            <NA>
#> 66             <NA>                 <NA>                 <NA>            <NA>
#> 67             <NA>                 <NA>                 <NA>            <NA>
#> 68             <NA>                 <NA>                 <NA>            <NA>
#> 69             <NA>                 <NA>                 <NA>            <NA>
#> 70             <NA>                 <NA>                 <NA>            <NA>
#>    ci90.A.model.up ci95.mu.model.lo ci95.mu.model.up ci95.lambda.model.lo
#> 1             <NA>             <NA>             <NA>                 <NA>
#> 2             <NA>             <NA>             <NA>                 <NA>
#> 3             <NA>             <NA>             <NA>                 <NA>
#> 4             <NA>             <NA>             <NA>                 <NA>
#> 5             <NA>             <NA>             <NA>                 <NA>
#> 6             <NA>             <NA>             <NA>                 <NA>
#> 7             <NA>             <NA>             <NA>                 <NA>
#> 8             <NA>             <NA>             <NA>                 <NA>
#> 9             <NA>             <NA>             <NA>                 <NA>
#> 10            <NA>             <NA>             <NA>                 <NA>
#> 11            <NA>             <NA>             <NA>                 <NA>
#> 12            <NA>             <NA>             <NA>                 <NA>
#> 13            <NA>             <NA>             <NA>                 <NA>
#> 14            <NA>             <NA>             <NA>                 <NA>
#> 15            <NA>             <NA>             <NA>                 <NA>
#> 16            <NA>             <NA>             <NA>                 <NA>
#> 17            <NA>             <NA>             <NA>                 <NA>
#> 18            <NA>             <NA>             <NA>                 <NA>
#> 19            <NA>             <NA>             <NA>                 <NA>
#> 20            <NA>             <NA>             <NA>                 <NA>
#> 21            <NA>             <NA>             <NA>                 <NA>
#> 22            <NA>             <NA>             <NA>                 <NA>
#> 23            <NA>             <NA>             <NA>                 <NA>
#> 24            <NA>             <NA>             <NA>                 <NA>
#> 25            <NA>             <NA>             <NA>                 <NA>
#> 26            <NA>             <NA>             <NA>                 <NA>
#> 27            <NA>             <NA>             <NA>                 <NA>
#> 28            <NA>             <NA>             <NA>                 <NA>
#> 29            <NA>             <NA>             <NA>                 <NA>
#> 30            <NA>             <NA>             <NA>                 <NA>
#> 31            <NA>             <NA>             <NA>                 <NA>
#> 32            <NA>             <NA>             <NA>                 <NA>
#> 33            <NA>             <NA>             <NA>                 <NA>
#> 34            <NA>             <NA>             <NA>                 <NA>
#> 35            <NA>             <NA>             <NA>                 <NA>
#> 36            <NA>             <NA>             <NA>                 <NA>
#> 37            <NA>             <NA>             <NA>                 <NA>
#> 38            <NA>             <NA>             <NA>                 <NA>
#> 39            <NA>             <NA>             <NA>                 <NA>
#> 40            <NA>             <NA>             <NA>                 <NA>
#> 41            <NA>             <NA>             <NA>                 <NA>
#> 42            <NA>             <NA>             <NA>                 <NA>
#> 43            <NA>             <NA>             <NA>                 <NA>
#> 44            <NA>             <NA>             <NA>                 <NA>
#> 45            <NA>             <NA>             <NA>                 <NA>
#> 46            <NA>             <NA>             <NA>                 <NA>
#> 47            <NA>             <NA>             <NA>                 <NA>
#> 48            <NA>             <NA>             <NA>                 <NA>
#> 49            <NA>             <NA>             <NA>                 <NA>
#> 50            <NA>             <NA>             <NA>                 <NA>
#> 51            <NA>             <NA>             <NA>                 <NA>
#> 52            <NA>             <NA>             <NA>                 <NA>
#> 53            <NA>             <NA>             <NA>                 <NA>
#> 54            <NA>             <NA>             <NA>                 <NA>
#> 55            <NA>             <NA>             <NA>                 <NA>
#> 56            <NA>             <NA>             <NA>                 <NA>
#> 57            <NA>             <NA>             <NA>                 <NA>
#> 58            <NA>             <NA>             <NA>                 <NA>
#> 59            <NA>             <NA>             <NA>                 <NA>
#> 60            <NA>             <NA>             <NA>                 <NA>
#> 61            <NA>             <NA>             <NA>                 <NA>
#> 62            <NA>             <NA>             <NA>                 <NA>
#> 63            <NA>             <NA>             <NA>                 <NA>
#> 64            <NA>             <NA>             <NA>                 <NA>
#> 65            <NA>             <NA>             <NA>                 <NA>
#> 66            <NA>             <NA>             <NA>                 <NA>
#> 67            <NA>             <NA>             <NA>                 <NA>
#> 68            <NA>             <NA>             <NA>                 <NA>
#> 69            <NA>             <NA>             <NA>                 <NA>
#> 70            <NA>             <NA>             <NA>                 <NA>
#>    ci95.lambda.model.up ci95.A.model.lo ci95.A.model.up          mu.spline
#> 1                  <NA>            <NA>            <NA>  0.666185304009823
#> 2                  <NA>            <NA>            <NA>  0.612943905976584
#> 3                  <NA>            <NA>            <NA>  0.614284297497893
#> 4                  <NA>            <NA>            <NA>  0.588989039601768
#> 5                  <NA>            <NA>            <NA>  0.535602629960741
#> 6                  <NA>            <NA>            <NA>  0.479613510905437
#> 7                  <NA>            <NA>            <NA>  0.378003193525613
#> 8                  <NA>            <NA>            <NA>  0.266706766016054
#> 9                  <NA>            <NA>            <NA>   0.14878387770882
#> 10                 <NA>            <NA>            <NA> 0.0754629635799424
#> 11                 <NA>            <NA>            <NA> 0.0296040705093421
#> 12                 <NA>            <NA>            <NA>                  0
#> 13                 <NA>            <NA>            <NA>  0.667659190094061
#> 14                 <NA>            <NA>            <NA>  0.612320532615856
#> 15                 <NA>            <NA>            <NA>  0.591397109289114
#> 16                 <NA>            <NA>            <NA>   0.57414797598451
#> 17                 <NA>            <NA>            <NA>  0.525952315446092
#> 18                 <NA>            <NA>            <NA>  0.461470951814229
#> 19                 <NA>            <NA>            <NA>  0.382313296114188
#> 20                 <NA>            <NA>            <NA>  0.266070189199259
#> 21                 <NA>            <NA>            <NA>  0.149221173738177
#> 22                 <NA>            <NA>            <NA> 0.0703594320295498
#> 23                 <NA>            <NA>            <NA> 0.0300201219461153
#> 24                 <NA>            <NA>            <NA>                  0
#> 25                 <NA>            <NA>            <NA>   0.65652638274416
#> 26                 <NA>            <NA>            <NA>  0.605237805580726
#> 27                 <NA>            <NA>            <NA>  0.577891846267645
#> 28                 <NA>            <NA>            <NA>  0.524529813896478
#> 29                 <NA>            <NA>            <NA>  0.466087841972033
#> 30                 <NA>            <NA>            <NA>  0.384311624602979
#> 31                 <NA>            <NA>            <NA>  0.260631961681371
#> 32                 <NA>            <NA>            <NA>    0.1466672376868
#> 33                 <NA>            <NA>            <NA> 0.0728306242801331
#> 34                 <NA>            <NA>            <NA> 0.0344003861475467
#> 35                 <NA>            <NA>            <NA>                  0
#> 36                 <NA>            <NA>            <NA>  0.475928439183671
#> 37                 <NA>            <NA>            <NA>  0.453876110290266
#> 38                 <NA>            <NA>            <NA>  0.424197933384486
#> 39                 <NA>            <NA>            <NA>  0.393083205955063
#> 40                 <NA>            <NA>            <NA>   0.36002048802828
#> 41                 <NA>            <NA>            <NA>  0.295601980401546
#> 42                 <NA>            <NA>            <NA>   0.22112155894673
#> 43                 <NA>            <NA>            <NA>  0.118013781700274
#> 44                 <NA>            <NA>            <NA> 0.0362366507519891
#> 45                 <NA>            <NA>            <NA>                  0
#> 46                 <NA>            <NA>            <NA>                  0
#> 47                 <NA>            <NA>            <NA>                  0
#> 48                 <NA>            <NA>            <NA>  0.480926685694289
#> 49                 <NA>            <NA>            <NA>  0.425031273955219
#> 50                 <NA>            <NA>            <NA>  0.433321562350223
#> 51                 <NA>            <NA>            <NA>  0.390888862608272
#> 52                 <NA>            <NA>            <NA>  0.354746049402425
#> 53                 <NA>            <NA>            <NA>  0.285825019410519
#> 54                 <NA>            <NA>            <NA>  0.214859740430842
#> 55                 <NA>            <NA>            <NA>  0.114787521480258
#> 56                 <NA>            <NA>            <NA> 0.0449708001426715
#> 57                 <NA>            <NA>            <NA>                  0
#> 58                 <NA>            <NA>            <NA>                  0
#> 59                 <NA>            <NA>            <NA>                  0
#> 60                 <NA>            <NA>            <NA>  0.478615045726239
#> 61                 <NA>            <NA>            <NA>  0.417979403998442
#> 62                 <NA>            <NA>            <NA>  0.388121323444942
#> 63                 <NA>            <NA>            <NA>  0.353093994312452
#> 64                 <NA>            <NA>            <NA>  0.292761827976707
#> 65                 <NA>            <NA>            <NA>  0.211263188856813
#> 66                 <NA>            <NA>            <NA>  0.114261029236495
#> 67                 <NA>            <NA>            <NA> 0.0417532208901036
#> 68                 <NA>            <NA>            <NA>                  0
#> 69                 <NA>            <NA>            <NA>                  0
#> 70                 <NA>            <NA>            <NA>                  0
#>           tD.spline tmax.spline        lambda.spline mu2.spline tD2.spline
#> 1  1.04047203741637           8     4.63736448864898       <NA>       <NA>
#> 2  1.13084928947222         7.5     4.16197089421789       <NA>       <NA>
#> 3  1.12838173364235        7.75     4.28933197118306       <NA>       <NA>
#> 4  1.17684223976154           8     4.68857884143938       <NA>       <NA>
#> 5  1.29414446790665        7.75     4.27816444727987       <NA>       <NA>
#> 6  1.44522029675809           8     4.41516311908786       <NA>       <NA>
#> 7  1.83370720785453           8     4.24656892769708       <NA>       <NA>
#> 8  2.59891112218062           9     4.19396325718445       <NA>       <NA>
#> 9  4.65875195104458           9     2.88831508236648       <NA>       <NA>
#> 10 9.18526317649391           9     1.64946042894383       <NA>       <NA>
#> 11 23.4139146622155           5     1.40994098857115       <NA>       <NA>
#> 12             <NA>        <NA>                 <NA>       <NA>       <NA>
#> 13 1.03817515110111        7.75     4.20939229960926       <NA>       <NA>
#> 14 1.13200055140858        8.25      4.7252041521752       <NA>       <NA>
#> 15 1.17205033584479         7.5     4.14103886160159       <NA>       <NA>
#> 16 1.20726225564304        7.75     4.46769232159293       <NA>       <NA>
#> 17 1.31788977860483           8     4.51196342192448       <NA>       <NA>
#> 18 1.50203859600459        8.25     4.66816556814742       <NA>       <NA>
#> 19 1.81303446049368        8.25     4.44708145723655       <NA>       <NA>
#> 20  2.6051290550286         9.5     4.09438537002772       <NA>       <NA>
#> 21 4.64509937293578        9.75     2.87394670406481       <NA>       <NA>
#> 22 9.85151756581598        6.75     1.50587765780861       <NA>       <NA>
#> 23 23.0894192170209         3.5    0.702692023643686       <NA>       <NA>
#> 24             <NA>        <NA>                 <NA>       <NA>       <NA>
#> 25 1.05577962863079        7.75     4.31034462904121       <NA>       <NA>
#> 26 1.14524765995883         7.5     4.17198786598988       <NA>       <NA>
#> 27 1.19944101138766           8     4.65451468203892       <NA>       <NA>
#> 28 1.32146383712851        7.75     4.42684092586831       <NA>       <NA>
#> 29 1.48715996887457        7.75      4.1565740212875       <NA>       <NA>
#> 30 1.80360711512699         8.5     4.26438140732584       <NA>       <NA>
#> 31 2.65948648848883         9.5     4.15131785864316       <NA>       <NA>
#> 32 4.72598510404979        10.5     2.90179183021546       <NA>       <NA>
#> 33 9.51724892393958         7.5      1.7722146014507       <NA>       <NA>
#> 34 20.1494011604105        0.25 -0.00105379045519705       <NA>       <NA>
#> 35             <NA>        <NA>                 <NA>       <NA>       <NA>
#> 36 1.45641050942208           8     3.93272232220545       <NA>       <NA>
#> 37 1.52717264655472         8.5     4.20594587619961       <NA>       <NA>
#> 38 1.63401828724066         8.5     3.83459307809601       <NA>       <NA>
#> 39 1.76335994532207         8.5     3.92812130117288       <NA>       <NA>
#> 40 1.92529926381717         8.5     3.91949733632239       <NA>       <NA>
#> 41 2.34486649791173        9.25     3.89925949243815       <NA>       <NA>
#> 42 3.13468837621088        9.75     3.45651006717252       <NA>       <NA>
#> 43 5.87344266553859       12.25     2.55628995761556       <NA>       <NA>
#> 44 19.1283456438616       16.25     3.07613518688487       <NA>       <NA>
#> 45             <NA>        <NA>                 <NA>       <NA>       <NA>
#> 46             <NA>        <NA>                 <NA>       <NA>       <NA>
#> 47             <NA>        <NA>                 <NA>       <NA>       <NA>
#> 48  1.4412741092944        8.25     3.90335054570826       <NA>       <NA>
#> 49 1.63081453773911        8.25     3.71514765969262       <NA>       <NA>
#> 50 1.59961386827947        8.75     4.28196801645446       <NA>       <NA>
#> 51 1.77325896658913        8.25     3.73281602469846       <NA>       <NA>
#> 52 1.95392501686083        8.75     4.22520559649212       <NA>       <NA>
#> 53 2.42507525054833         9.5     3.77508565835527       <NA>       <NA>
#> 54 3.22604494992887        8.75     3.16650127998405       <NA>       <NA>
#> 55 6.03852380137991          10     2.37719193775097       <NA>       <NA>
#> 56 15.4132721312698          24     6.70930626082653       <NA>       <NA>
#> 57             <NA>        <NA>                 <NA>       <NA>       <NA>
#> 58             <NA>        <NA>                 <NA>       <NA>       <NA>
#> 59             <NA>        <NA>                 <NA>       <NA>       <NA>
#> 60 1.44823525033188        8.75     4.24337673795876       <NA>       <NA>
#> 61 1.65832855382159        8.75      4.0103478772528       <NA>       <NA>
#> 62  1.7859033727073        8.75     4.11618679288304       <NA>       <NA>
#> 63 1.96306703519455        8.75     4.03623738675974       <NA>       <NA>
#> 64 2.36761460792318        9.25      3.9651966519585       <NA>       <NA>
#> 65 3.28096524676496        9.75     3.40967419153788       <NA>       <NA>
#> 66 6.06634812579261        12.5     2.72701739231328       <NA>       <NA>
#> 67 16.6010469559783          24     4.51650569483349       <NA>       <NA>
#> 68             <NA>        <NA>                 <NA>       <NA>       <NA>
#> 69             <NA>        <NA>                 <NA>       <NA>       <NA>
#> 70             <NA>        <NA>                 <NA>       <NA>       <NA>
#>    tmax2.spline lambda2.spline          y0.spline           A.spline
#> 1          <NA>           <NA> 0.0547454883772911   6.97633594780545
#> 2          <NA>           <NA> 0.0568991952930782   3.83989976213284
#> 3          <NA>           <NA> 0.0550423746970474   3.71723198054444
#> 4          <NA>           <NA> 0.0498449340523397   2.77988443205792
#> 5          <NA>           <NA>  0.053022159937985   2.44583256369604
#> 6          <NA>           <NA> 0.0513642458013086   1.64590874855822
#> 7          <NA>           <NA>  0.054822005433649   1.17337037045215
#> 8          <NA>           <NA> 0.0577403327922724  0.898027316460322
#> 9          <NA>           <NA> 0.0506715359626077  0.403188492464833
#> 10         <NA>           <NA> 0.0502321253997889  0.211770027483813
#> 11         <NA>           <NA> 0.0557982674953308 0.0896968471664291
#> 12         <NA>           <NA>               <NA>               <NA>
#> 13         <NA>           <NA> 0.0519387775587781   5.89580872590961
#> 14         <NA>           <NA>  0.047306031865241   3.44058093523416
#> 15         <NA>           <NA> 0.0547341075538874   3.37097153240171
#> 16         <NA>           <NA> 0.0507904851513512   2.50946572177898
#> 17         <NA>           <NA>  0.054515538670159   2.08158319123491
#> 18         <NA>           <NA> 0.0528123515707595   1.66715631125666
#> 19         <NA>           <NA> 0.0556981804139212   1.30157480434584
#> 20         <NA>           <NA>  0.055814778253276  0.837169600555916
#> 21         <NA>           <NA> 0.0482070605720539  0.423178272258683
#> 22         <NA>           <NA> 0.0503478922341105    0.1806022666409
#> 23         <NA>           <NA> 0.0491753444050969  0.084829484679357
#> 24         <NA>           <NA>               <NA>               <NA>
#> 25         <NA>           <NA> 0.0506655210175015    5.6983422384523
#> 26         <NA>           <NA> 0.0476532062527806   3.23424210234577
#> 27         <NA>           <NA> 0.0499745722660419   2.87368953960216
#> 28         <NA>           <NA> 0.0545328662575431   2.06271472532907
#> 29         <NA>           <NA>  0.049470159811297   1.52909884343522
#> 30         <NA>           <NA> 0.0531898459028686   1.20135956609875
#> 31         <NA>           <NA> 0.0490761484249905  0.696801431122222
#> 32         <NA>           <NA> 0.0521441728116661  0.416150932713198
#> 33         <NA>           <NA> 0.0520252817283838  0.198621846557738
#> 34         <NA>           <NA> 0.0532952778742202   0.09613172377119
#> 35         <NA>           <NA>               <NA>               <NA>
#> 36         <NA>           <NA> 0.0525856330176331   3.81295563152493
#> 37         <NA>           <NA>  0.057333757053414   2.79141049265079
#> 38         <NA>           <NA> 0.0556372180543279   2.54943626223866
#> 39         <NA>           <NA> 0.0527895572055841   1.86007927367143
#> 40         <NA>           <NA> 0.0497059425610327   1.30210906341421
#> 41         <NA>           <NA> 0.0473015200777307   1.17786190185491
#> 42         <NA>           <NA> 0.0553657521528903   0.90261255989378
#> 43         <NA>           <NA> 0.0501347734243542  0.425457564887918
#> 44         <NA>           <NA> 0.0517298034037668   0.10442401203221
#> 45         <NA>           <NA>               <NA>               <NA>
#> 46         <NA>           <NA>               <NA>               <NA>
#> 47         <NA>           <NA>               <NA>               <NA>
#> 48         <NA>           <NA> 0.0554347685190431   4.25592038925248
#> 49         <NA>           <NA>  0.050030149504086   2.14450087322009
#> 50         <NA>           <NA> 0.0513230964308278   2.28976736141969
#> 51         <NA>           <NA> 0.0561535440864764   1.93132195770635
#> 52         <NA>           <NA> 0.0562874884637765   1.62018273511576
#> 53         <NA>           <NA> 0.0526524522164216   1.15008486097419
#> 54         <NA>           <NA> 0.0535140865414424  0.836584636080718
#> 55         <NA>           <NA> 0.0523835722985085  0.417563595048547
#> 56         <NA>           <NA>  0.055375049351011  0.120506711931045
#> 57         <NA>           <NA>               <NA>               <NA>
#> 58         <NA>           <NA>               <NA>               <NA>
#> 59         <NA>           <NA>               <NA>               <NA>
#> 60         <NA>           <NA> 0.0468769260074198   3.60593739774369
#> 61         <NA>           <NA> 0.0471012589995921   1.90955813951562
#> 62         <NA>           <NA> 0.0465474930923988   1.64828964045391
#> 63         <NA>           <NA> 0.0497387753743352   1.45217978901893
#> 64         <NA>           <NA> 0.0502271598858766   1.06826430213422
#> 65         <NA>           <NA> 0.0534548299500154  0.835083662387163
#> 66         <NA>           <NA> 0.0522100300730132  0.415561658824066
#> 67         <NA>           <NA> 0.0480212727514768   0.10832573323954
#> 68         <NA>           <NA>               <NA>               <NA>
#> 69         <NA>           <NA>               <NA>               <NA>
#> 70         <NA>           <NA>               <NA>               <NA>
#>             dY.spline  integral.spline reliable_fit.spline reliable_fit2.spline
#> 1    6.92159045942816 75.8698269576953                TRUE                FALSE
#> 2    3.78300056683977 68.2335428598593                TRUE                FALSE
#> 3    3.66218960584739  68.160620332171                TRUE                FALSE
#> 4    2.73003949800558  63.554898166495                TRUE                FALSE
#> 5    2.39281040375806 61.6499051059675                TRUE                FALSE
#> 6    1.59454450275691 55.3523922447667                TRUE                FALSE
#> 7    1.11854836501851 47.4900851031094                TRUE                FALSE
#> 8    0.84028698366805 39.7531290627763                TRUE                FALSE
#> 9   0.352516956502226 28.8673216387232                TRUE                FALSE
#> 10  0.161537902084024 18.4669808041159                TRUE                FALSE
#> 11 0.0338985796710983 6.15467766121257                TRUE                FALSE
#> 12                  0                0               FALSE                FALSE
#> 13   5.84386994835083 76.7425440958304                TRUE                FALSE
#> 14   3.39327490336892 67.4712121256522                TRUE                FALSE
#> 15   3.31623742484782 67.4513687688314                TRUE                FALSE
#> 16   2.45867523662763 62.4499059502654                TRUE                FALSE
#> 17   2.02706765256475 58.1389680132309                TRUE                FALSE
#> 18    1.6143439596859 53.6653884146515                TRUE                FALSE
#> 19   1.24587662393192 48.3128831970255                TRUE                FALSE
#> 20  0.781354822302641 39.5846086322305                TRUE                FALSE
#> 21  0.374971211686629  29.495833177066                TRUE                FALSE
#> 22  0.130254374406789 16.1553761352148                TRUE                FALSE
#> 23 0.0356541402742601 6.93190330246195                TRUE                FALSE
#> 24                  0                0               FALSE                FALSE
#> 25    5.6476767174348  75.527669561894                TRUE                FALSE
#> 26   3.18658889609299 69.1238692057686                TRUE                FALSE
#> 27   2.82371496733612 63.6639384585016                TRUE                FALSE
#> 28   2.00818185907152 57.9052486127322                TRUE                FALSE
#> 29   1.47962868362393 55.5843977595572                TRUE                FALSE
#> 30   1.14816972019588 48.4209428912433                TRUE                FALSE
#> 31  0.647725282697231 38.4598229360074                TRUE                FALSE
#> 32  0.364006759901532 28.5528161581774                TRUE                FALSE
#> 33  0.146596564829354  16.387503709212                TRUE                FALSE
#> 34 0.0428364458969698 7.58542849460561                TRUE                FALSE
#> 35                  0                0               FALSE                FALSE
#> 36    3.7603699985073 66.1270195782982                TRUE                FALSE
#> 37   2.73407673559737 59.9137802271219                TRUE                FALSE
#> 38   2.49379904418433 59.1659517844401                TRUE                FALSE
#> 39   1.80728971646584 54.7369705838903                TRUE                FALSE
#> 40   1.25240312085317 49.7872643453008                TRUE                FALSE
#> 41   1.13056038177718 46.3910153277179                TRUE                FALSE
#> 42   0.84724680774089  39.115019230584                TRUE                FALSE
#> 43  0.375322791463564 26.1188229691728                TRUE                FALSE
#> 44 0.0526942086284431 8.40717783615375                TRUE                FALSE
#> 45                  0                0               FALSE                FALSE
#> 46                  0                0               FALSE                FALSE
#> 47                  0                0               FALSE                FALSE
#> 48   4.20048562073344 67.4482361408933                TRUE                FALSE
#> 49     2.094470723716 58.9080569684425                TRUE                FALSE
#> 50   2.23844426498886 57.5851322275217                TRUE                FALSE
#> 51   1.87516841361987 55.6300672257429                TRUE                FALSE
#> 52   1.56389524665198 49.9514442860146                TRUE                FALSE
#> 53   1.09743240875777 45.0086081317599                TRUE                FALSE
#> 54  0.783070549539275 38.9866340266603                TRUE                FALSE
#> 55  0.365180022750038 24.9550495233751                TRUE                FALSE
#> 56 0.0651316625800344 9.53410454765571                TRUE                FALSE
#> 57                  0                0               FALSE                FALSE
#> 58                  0                0               FALSE                FALSE
#> 59                  0                0               FALSE                FALSE
#> 60   3.55906047173627 65.6346043406477                TRUE                FALSE
#> 61   1.86245688051603 57.2972748084169                TRUE                FALSE
#> 62   1.60174214736151 54.0772575349498                TRUE                FALSE
#> 63    1.4024410136446 50.9767401031793                TRUE                FALSE
#> 64   1.01803714224834 44.6103678561717                TRUE                FALSE
#> 65  0.781628832437148 37.5935554721914                TRUE                FALSE
#> 66  0.363351628751053 25.2872690813821                TRUE                FALSE
#> 67 0.0603044604880637 9.71882856509048                TRUE                FALSE
#> 68                  0                0               FALSE                FALSE
#> 69                  0                0               FALSE                FALSE
#> 70                  0                0               FALSE                FALSE
#>    smooth.spline              mu.bt        lambda.bt               A.bt
#> 1           0.55  0.664775437846225 4.63478244457801   6.97505981833775
#> 2           0.55  0.609549427160117 4.15353845651784   3.84716719572225
#> 3           0.55  0.612553791438614 4.28289748638494   3.71708524911173
#> 4           0.55    0.5885248675473   4.690966534472   2.78842971293627
#> 5           0.55  0.533817604276273 4.25959079797386   2.44151930667529
#> 6           0.55  0.477713756907105 4.39663639596209   1.64892564582807
#> 7           0.55  0.377571135131155 4.26307795082451   1.17470268960755
#> 8           0.55  0.268251784082131 4.21834511086827  0.899825095135623
#> 9           0.55   0.15029135977164 2.94745460560645  0.402212775097059
#> 10          0.55 0.0777231063410791 1.83250007373586  0.206571943185026
#> 11          0.55 0.0312632791101953 2.15742584477387 0.0893836521046412
#> 12          0.55               <NA>             <NA>               <NA>
#> 13          0.55  0.666507684478079 4.20207474668902   5.90488432568839
#> 14          0.55  0.611438174657045 4.72100747186356   3.45377455215852
#> 15          0.55  0.592108090967954 4.14398227107695   3.38104218026316
#> 16          0.55  0.571020931849139 4.45440408662234    2.5225266972024
#> 17          0.55  0.525725927034353 4.50758123866695   2.08664493744271
#> 18          0.55  0.461670487032898 4.66889891831093   1.66948944432887
#> 19          0.55  0.380906193132449 4.43075406555337   1.30372134652561
#> 20          0.55  0.267048385564954 4.11140781174785  0.819740319113844
#> 21          0.55  0.151080579498448  2.9704989306099  0.417553393958022
#> 22          0.55 0.0707684293703707 1.91652084186684  0.179207502207594
#> 23          0.55 0.0330509273387612  1.8772312967836 0.0836692947077107
#> 24          0.55               <NA>             <NA>               <NA>
#> 25          0.55  0.656616445020953 4.31379657802125   5.72896229142306
#> 26          0.55   0.60385957054516 4.15372690660497   3.24965997957705
#> 27          0.55  0.575908268070666 4.64152545552144   2.88331952077775
#> 28          0.55  0.522551274460962 4.42334938279998   2.07325902355259
#> 29          0.55  0.466819905368308  4.1507773857632   1.53539936911314
#> 30          0.55   0.38392150516336 4.26874984378319   1.20697268379672
#> 31          0.55  0.259586627484856 4.15823715374555  0.698214697678521
#> 32          0.55  0.149260084914496 3.00827420005545  0.413691685224928
#> 33          0.55  0.073687860524488  2.2376442339942  0.194988382478644
#> 34          0.55 0.0370407547442648 3.29352092053863 0.0949790972834153
#> 35          0.55               <NA>             <NA>               <NA>
#> 36          0.55  0.477323423975771 3.94696846501871   3.81306843210908
#> 37          0.55  0.452381074004116 4.19552057293526   2.80569715884389
#> 38          0.55  0.423429994070849 3.83110566029957   2.52646769725883
#> 39          0.55  0.392293153895907 3.92997000029989   1.88084581513512
#> 40          0.55   0.36012151655512 3.94449327776672   1.30405969286089
#> 41          0.55  0.295541442625162 3.90068234691541   1.16154092445294
#> 42          0.55  0.219598067549744 3.41961962068452  0.904577906413257
#> 43          0.55  0.119757680172347 2.67316218950633  0.419342761394949
#> 44          0.55 0.0392565859078804 3.17628074915072  0.102432110391235
#> 45          0.55               <NA>             <NA>               <NA>
#> 46          0.55               <NA>             <NA>               <NA>
#> 47          0.55               <NA>             <NA>               <NA>
#> 48          0.55  0.479248568242499 3.87652567250808   4.22724975720158
#> 49          0.55  0.425548427812938 3.73064272832431   2.14913979536182
#> 50          0.55  0.432035634596614 4.27828209540089   2.26823280979444
#> 51          0.55  0.391992407948312 3.73573610625066   1.92357484461339
#> 52          0.55  0.353049663223989 4.21395514979617   1.62707377193542
#> 53          0.55  0.286612378855233 3.80257986093234   1.15636002328337
#> 54          0.55  0.215408328152237 3.17654049305551  0.835622159142962
#> 55          0.55  0.116692905414483  2.6566223427887  0.410252787748813
#> 56          0.55 0.0456876024688047 5.52812698671066   0.11887769411073
#> 57          0.55               <NA>             <NA>               <NA>
#> 58          0.55               <NA>             <NA>               <NA>
#> 59          0.55               <NA>             <NA>               <NA>
#> 60          0.55  0.478825807461758  4.2490314478228   3.61053562448846
#> 61          0.55  0.418657147572709 4.01801480919529   1.92371814922255
#> 62          0.55  0.389260916872153 4.13220108759003   1.64776880321043
#> 63          0.55  0.352190831244319 4.02475212909191   1.45273310943849
#> 64          0.55   0.29392908275809 3.99549749439698   1.06131554142936
#> 65          0.55  0.211182095251522 3.43537200549425  0.828905141971117
#> 66          0.55  0.114958277089102 2.81502693786162  0.408259555244701
#> 67          0.55 0.0445918093775327  3.4127331149945  0.106547683911566
#> 68          0.55               <NA>             <NA>               <NA>
#> 69          0.55               <NA>             <NA>               <NA>
#> 70          0.55               <NA>             <NA>               <NA>
#>                 dY.bt      integral.bt            stdmu.bt       stdlambda.bt
#> 1    6.92019010200862 74.1250408854629 0.00418091972377121 0.0179464688168841
#> 2    3.78993097136299 66.4731703260827 0.00486668051094493 0.0338382062913358
#> 3    3.66187506134901 66.4774892911419 0.00535695273671511 0.0318448050926782
#> 4    2.73838347434849 61.6946846872299 0.00480095337289231 0.0299887236448871
#> 5    2.38863257292352 59.9670581705534 0.00387800305815762 0.0240451524190361
#> 6    1.59758613136343 53.8878570815464 0.00424977937895418 0.0365295738236839
#> 7    1.11945747544555 46.5349249245813 0.00328773208436906 0.0470542276315782
#> 8   0.842092448705797 38.8744927872821 0.00259846109018261 0.0470602882562743
#> 9   0.351639864522843 28.1703479563272 0.00367830639541116   0.15538518189459
#> 10  0.156567927975119 17.8017209598659 0.00233674808058998   0.32743281661688
#> 11 0.0336683494584357 5.95957275587098 0.00283671088944656   1.88846529023331
#> 12               <NA>             <NA>                <NA>               <NA>
#> 13   5.85306111571784 74.9565568028123 0.00459010909457193 0.0181842111002298
#> 14   3.40640240154122 65.7594375682586 0.00573261320493019 0.0351522295418763
#> 15   3.32642359153182 65.9448568171661  0.0039600901507976 0.0309827599907326
#> 16   2.47159796126575 60.4669987243805 0.00562404724099196  0.035724452493801
#> 17    2.0321363008974 56.5990896923792 0.00488000600353543 0.0388477142134233
#> 18   1.61665627122365 52.3849936419712 0.00265006137107055 0.0219603915480106
#> 19   1.24801463181521 47.1697066716674 0.00430478489353599 0.0546439914499986
#> 20   0.76387058072367 38.2827873961497 0.00432346674793811 0.0713922594955829
#> 21  0.369358272859186 28.7843518477997 0.00253885615298951  0.133130724108009
#> 22  0.128836828269574  15.659560835035 0.00218791512314059  0.373874264636114
#> 23 0.0345953605540301 6.69533579315707 0.00259503226840556   2.11727073522137
#> 24               <NA>             <NA>                <NA>               <NA>
#> 25    5.6782187454118 73.6928913583192  0.0034819650843821 0.0178843187688275
#> 26   3.20227320831392 67.5303547304837 0.00425599556102548 0.0270678504200607
#> 27   2.83333629372563 62.4712371256196 0.00494052228900424 0.0267567320156207
#> 28   2.01850066553822 56.5910082441996 0.00408944611233263 0.0294041523192231
#> 29   1.48613577948111 54.5148177079893 0.00440054811883179 0.0396181370860966
#> 30   1.15356116169194 47.1736297477525 0.00473672113651666 0.0578028485956875
#> 31  0.648808740955419 37.3731473057327 0.00358031618930741 0.0808128029643585
#> 32   0.36156578852514 27.8531925345352  0.0049519107291463  0.268764216462562
#> 33  0.142699671619158 15.8029195182304 0.00220310249930427  0.471527416791166
#> 34 0.0417705929069208 7.36428005353092 0.00346148093242695   1.93347378699718
#> 35               <NA>             <NA>                <NA>               <NA>
#> 36   3.76041113944592 64.2612900318186 0.00426087371918279 0.0355292283588151
#> 37   2.74822915317834 58.5057474066266 0.00425151083603308 0.0453674954605962
#> 38   2.47070492390647 57.8242317598133 0.00285470457817067 0.0304256107295185
#> 39   1.82782198054553  53.360441630794 0.00172354656158386 0.0245977189526536
#> 40   1.25396720062853   48.50401431441 0.00246146770090748 0.0361808662015985
#> 41   1.11413615939405 45.3562170128488 0.00331549984275306 0.0626794754534631
#> 42  0.849154387338026 38.1459393603892 0.00240781004376066 0.0644813870507068
#> 43  0.369007130073238 25.4641433631903 0.00244102068507791  0.214208797203975
#> 44 0.0507268782220216 8.15794384231081 0.00257845924239948   1.55656404790238
#> 45               <NA>             <NA>                <NA>               <NA>
#> 46               <NA>             <NA>                <NA>               <NA>
#> 47               <NA>             <NA>                <NA>               <NA>
#> 48   4.17205013447724 65.6987337794917 0.00346950779123876 0.0356862465809687
#> 49   2.09886116252271 57.3307903938713 0.00340315297037337  0.037807582993738
#> 50   2.21677486611015 55.8736044379439 0.00373857871185955 0.0348017325776723
#> 51   1.86765556924971 54.4239902393887 0.00361076017497369 0.0471441118780371
#> 52   1.57053466475851 48.7534170048768 0.00379183469881045 0.0501400606214165
#> 53   1.10346477751447 43.6414907779829 0.00277251934601348 0.0632254196171564
#> 54  0.782179015429835  37.898066661111 0.00221057766492849 0.0561096465350692
#> 55  0.357356790419672    24.2590066204   0.003299534903271  0.252224917689778
#> 56 0.0637570468771189 9.26936803426784 0.00375281823348348   2.57235367552059
#> 57               <NA>             <NA>                <NA>               <NA>
#> 58               <NA>             <NA>                <NA>               <NA>
#> 59               <NA>             <NA>                <NA>               <NA>
#> 60   3.56354766201557 64.2263717926334  0.0060575356081367  0.065585154426498
#> 61   1.87659472838223 56.0713759882059 0.00304029753634521 0.0312568901867696
#> 62   1.60113789142074 52.9203650156141 0.00527398252987859 0.0591292768166562
#> 63   1.40309993628609 49.7829053722744 0.00273141671535777 0.0495440610304552
#> 64   1.01094209927978  43.584020549385 0.00293100989033142 0.0438039539080891
#> 65  0.775069640394312  36.511094809523 0.00285038005271567  0.109041632349662
#> 66  0.355985897994587 24.4066421906268 0.00307787361313216  0.273124026645612
#> 67 0.0584358354740139 9.41676663693256 0.00309420040724433   2.14930581397372
#> 68               <NA>             <NA>                <NA>               <NA>
#> 69               <NA>             <NA>                <NA>               <NA>
#> 70               <NA>             <NA>                <NA>               <NA>
#>                 stdA.bt             stddY.bt    stdintegral.bt reliable_fit.bt
#> 1    0.0202294554667859   0.0202025562444563 0.744646854357586            TRUE
#> 2    0.0326501521568602   0.0326628743086024   1.1377707248001            TRUE
#> 3     0.020353274682762   0.0203618529569191 0.923776486981358            TRUE
#> 4    0.0115454865182667   0.0115490894957845  1.08572948821487            TRUE
#> 5   0.00839382163047802  0.00849469072365868   1.0662704394354            TRUE
#> 6   0.00611585748001787  0.00606063744048479 0.853637269056752            TRUE
#> 7   0.00844785119650475  0.00854913676008194 0.441697975773661            TRUE
#> 8   0.00316122615203272  0.00314639227519733  0.40514583740195            TRUE
#> 9   0.00239431638026588   0.0024001514433774 0.251607176693276            TRUE
#> 10   0.0026536956293868   0.0026856401735613 0.395669595548627            TRUE
#> 11 0.000651036393752612 0.000643553418614602 0.144318646394548            TRUE
#> 12                 <NA>                 <NA>              <NA>            <NA>
#> 13   0.0210141312886451    0.021063048058827  1.04687973474256            TRUE
#> 14   0.0155761408016454   0.0155683512997396  1.06050610817627            TRUE
#> 15   0.0188429409205267   0.0187474527316998  1.22711253024138            TRUE
#> 16   0.0128254258500018   0.0126992687969141  1.38949015100973            TRUE
#> 17  0.00688358207987648  0.00691088522630235 0.823076785253507            TRUE
#> 18  0.00675988716297141   0.0067601124430771 0.765832053005673            TRUE
#> 19  0.00427296927532832   0.0042988977375367 0.544313314383117            TRUE
#> 20  0.00408455750803283  0.00411808597917119  1.03097553281894            TRUE
#> 21  0.00237038313714201  0.00239033995204477 0.261160659510003            TRUE
#> 22  0.00245262742810838  0.00242331824032332 0.269250767874866            TRUE
#> 23 0.000716995109969121 0.000767563756742565 0.170845079082646            TRUE
#> 24                 <NA>                 <NA>              <NA>            <NA>
#> 25   0.0165498589190034   0.0165496207541875  1.03009648739918            TRUE
#> 26    0.016284439054528   0.0162134153813635 0.938426802886525            TRUE
#> 27  0.00897311342815396  0.00899818818511284 0.638929311241928            TRUE
#> 28   0.0115711239168727   0.0115834611081352 0.619818106747628            TRUE
#> 29  0.00406865294689097  0.00416128097644483 0.481201192617411            TRUE
#> 30  0.00647367233926385  0.00647255621696225 0.703350337403176            TRUE
#> 31  0.00233715747726452  0.00242580856299826 0.604532080316876            TRUE
#> 32  0.00167839974179014  0.00174421007212539 0.349815064008708            TRUE
#> 33  0.00172553703621082  0.00165559658538346 0.292572857856563            TRUE
#> 34 0.000967033828123035 0.000926692314889755 0.128910068919975            TRUE
#> 35                 <NA>                 <NA>              <NA>            <NA>
#> 36   0.0201159421003313   0.0201279692420528   1.2330417276179            TRUE
#> 37   0.0165384808622749   0.0164803651389783 0.590230177335129            TRUE
#> 38   0.0142247546932551   0.0141890853564054 0.716377647505916            TRUE
#> 39   0.0067010845051734  0.00671447317955265 0.538379564110941            TRUE
#> 40   0.0083659198973244    0.008438673128667 0.846264329010116            TRUE
#> 41  0.00509020950117968  0.00509410754271894 0.457087875059753            TRUE
#> 42  0.00554136255633509  0.00551780728356174 0.611334230474074            TRUE
#> 43  0.00263082973950769  0.00257756291480463 0.291189792301546            TRUE
#> 44 0.000867087230588412 0.000905030823000752 0.150816943848312            TRUE
#> 45                 <NA>                 <NA>              <NA>            <NA>
#> 46                 <NA>                 <NA>              <NA>            <NA>
#> 47                 <NA>                 <NA>              <NA>            <NA>
#> 48   0.0124770052725929   0.0123743574046859  1.12480775402134            TRUE
#> 49   0.0117480620772804   0.0118105507835043 0.751395432599401            TRUE
#> 50  0.00960466057071061  0.00965190665383293  1.08334764742241            TRUE
#> 51  0.00732733709768043  0.00732864280520598 0.525127603570412            TRUE
#> 52  0.00617051662680381  0.00616969112065439 0.635149077036529            TRUE
#> 53   0.0110827068249446   0.0109829480706824  1.10317539982504            TRUE
#> 54   0.0094769712900181  0.00950908748080956 0.819867325085897            TRUE
#> 55  0.00685252579431225   0.0067168347563777 0.419376510192886            TRUE
#> 56 0.000743377966855592 0.000778862784684277 0.104302275557789            TRUE
#> 57                 <NA>                 <NA>              <NA>            <NA>
#> 58                 <NA>                 <NA>              <NA>            <NA>
#> 59                 <NA>                 <NA>              <NA>            <NA>
#> 60   0.0140622371364576   0.0139780670912479 0.519664976993836            TRUE
#> 61   0.0085255230280402  0.00850702787306375 0.547446920857914            TRUE
#> 62  0.00714996458014339  0.00715522660608241 0.411438308541362            TRUE
#> 63  0.00589654213000614  0.00590592469722283  0.50160103455063            TRUE
#> 64  0.00360515214218571  0.00360018211746587 0.440914467944478            TRUE
#> 65  0.00712908057531419   0.0069756300705789 0.670415843891806            TRUE
#> 66  0.00577198055449742  0.00577992150935653 0.649553398774483            TRUE
#> 67  0.00183464038063249  0.00185021897075422 0.182671273169295            TRUE
#> 68                 <NA>                 <NA>              <NA>            <NA>
#> 69                 <NA>                 <NA>              <NA>            <NA>
#> 70                 <NA>                 <NA>              <NA>            <NA>
#>         ci90.mu.bt.lo      ci90.mu.bt.up  ci90.lambda.bt.lo ci90.lambda.bt.up
#> 1   0.657897824900621  0.671653050791828   4.60526050337423  4.66430438578178
#> 2   0.601543737719613  0.617555116600622   4.09787460716859  4.20920230586708
#> 3   0.603741604186717   0.62136597869051   4.23051278200748  4.33528219076239
#> 4   0.580627299248892  0.596422435845708   4.64163508407616  4.74029798486784
#> 5   0.527438289245603  0.540196919306942   4.22003652224455  4.29914507370318
#> 6   0.470722869828726  0.484704643985485   4.33654524702213  4.45672754490205
#> 7   0.372162815852368  0.382979454409943   4.18567374637056  4.34048215527845
#> 8   0.263977315588781  0.272526252575481    4.1409309366867  4.29575928504984
#> 9   0.144240545751188  0.156342173792091   2.69184598138985  3.20306322982305
#> 10 0.0738791557485086 0.0815670569336496   1.29387309040109  2.37112705707062
#> 11 0.0265968896970557 0.0359296685233349 -0.949099557659935  5.26395124720767
#> 12               <NA>               <NA>               <NA>              <NA>
#> 13  0.658956955017508   0.67405841393865   4.17216171942915   4.2319877739489
#> 14  0.602008025934935  0.620868323379155   4.66318205426717  4.77883288945994
#> 15  0.585593742669892  0.598622439266016   4.09301563089219   4.1949489112617
#> 16  0.561769374137707  0.580272489560571   4.39563736227003  4.51317081097464
#> 17  0.517698317158537  0.533753536910168   4.44367674878587  4.57148572854803
#> 18  0.457311136077487  0.466029837988309   4.63277407421445  4.70502376240741
#> 19  0.373824821982582  0.387987564282315   4.34086469961812  4.52064343148862
#> 20  0.259936282764596  0.274160488365313   3.99396754487762  4.22884807861809
#> 21   0.14690416112678  0.155256997870116   2.75149888945222  3.18949897176757
#> 22 0.0671693089928044 0.0743675497479369   1.30149767654044  2.53154400719325
#> 23 0.0287820992572341 0.0373197554202883  -1.60567906265556  5.36014165622276
#> 24               <NA>               <NA>               <NA>              <NA>
#> 25  0.650888612457144  0.662344277584762   4.28437687364653  4.34321628239597
#> 26  0.596858457847273  0.610860683243047   4.10920029266397  4.19825352054596
#> 27  0.567781108905254  0.584035427236078   4.59751063135575  4.68554027968714
#> 28  0.515824135606174  0.529278413315749   4.37497955223486  4.47171921336511
#> 29   0.45958100371283  0.474058807023786   4.08560555025657  4.21594922126983
#> 30   0.37612959889379  0.391713411432929   4.17366415784328  4.36383552972309
#> 31  0.253697007353446  0.265476247616267   4.02530009286918  4.29117421462192
#> 32  0.141114191765051  0.157405978063942   2.56615706397453  3.45039133613636
#> 33 0.0700637569131325 0.0773119641358436   1.46198163337273  3.01330683461566
#> 34 0.0313466186104225 0.0427348908781072   0.11295654092827    6.474085300149
#> 35               <NA>               <NA>               <NA>              <NA>
#> 36  0.470314286707715  0.484332561243827   3.88852288436845  4.00541404566896
#> 37  0.445387338678842  0.459374809329391   4.12089104290258  4.27015010296794
#> 38  0.418734005039758   0.42812598310194   3.78105553064951  3.88115578994963
#> 39  0.389457919802102  0.395128387989713   3.88950675262277    3.970433247977
#> 40  0.356072402187127  0.364170630923112   3.88497575286509  4.00401080266835
#> 41  0.290087445383834  0.300995439866491   3.79757460979447  4.00379008403636
#> 42  0.215637220027757   0.22355891507173   3.31354773898611  3.52569150238293
#> 43  0.115742201145394    0.1237731591993   2.32078871810579  3.02553566090687
#> 44 0.0350150204541333 0.0434981513616276  0.615732890351299  5.73682860795014
#> 45               <NA>               <NA>               <NA>              <NA>
#> 46               <NA>               <NA>               <NA>              <NA>
#> 47               <NA>               <NA>               <NA>              <NA>
#> 48  0.473541227925911  0.484955908559086   3.81782179688239  3.93522954813378
#> 49  0.419950241176674  0.431146614449203   3.66844925429961    3.792836202349
#> 50  0.425885672615605  0.438185596577623   4.22103324531062  4.33553094549116
#> 51   0.38605270746048  0.397932108436144   3.65818404221129  3.81328817029003
#> 52  0.346812095144446  0.359287231303532   4.13147475007394   4.2964355495184
#> 53  0.282051584531041  0.291173173179425   3.69857404566212  3.90658567620256
#> 54   0.21177192789343  0.219044728411044   3.08424012450532   3.2688408616057
#> 55  0.111265170498602  0.122120640330364   2.24171235318902  3.07153233238838
#> 56 0.0395142164747244  0.051860988462885   1.29660519047929  9.75964878294203
#> 57               <NA>               <NA>               <NA>              <NA>
#> 58               <NA>               <NA>               <NA>              <NA>
#> 59               <NA>               <NA>               <NA>              <NA>
#> 60  0.468861161386373  0.488790453537143   4.14114386879121  4.35691902685439
#> 61  0.413655858125421  0.423658437019997   3.96659722483806  4.06943239355253
#> 62  0.380585215610503  0.397936618133804   4.03493342722663  4.22946874795342
#> 63  0.347697650747555  0.356684011741082   3.94325214869681  4.10625210948701
#> 64  0.289107571488495  0.298750594027685   3.92343999021818  4.06755499857579
#> 65  0.206493220064805  0.215870970438239   3.25599852027906  3.61474549070944
#> 66    0.1098951749955  0.120021379182704   2.36573791402959  3.26431596169366
#> 67 0.0395018497076158 0.0496817690474496 -0.122874948992271  6.94834117898127
#> 68               <NA>               <NA>               <NA>              <NA>
#> 69               <NA>               <NA>               <NA>              <NA>
#> 70               <NA>               <NA>               <NA>              <NA>
#>          ci90.A.bt.lo       ci90.A.bt.up ci90.integral.bt.lo
#> 1    6.94178236409489   7.00833727258062    72.9000968100447
#> 2    3.79345769542421   3.90087669602028    64.6015374837866
#> 3    3.68360411225859   3.75056638596488    64.9578769700575
#> 4    2.76943738761372   2.80742203825882    59.9086596791164
#> 5    2.42771147009315   2.45532714325742    58.2130432976821
#> 6    1.63886506027344    1.6589862313827     52.483623773948
#> 7     1.1608059743893    1.1885994048258    45.8083317544337
#> 8   0.894624878115529  0.905025312155717    38.2080278847559
#> 9   0.398274124651521  0.406151425542596    27.7564541506668
#> 10  0.202206613874684  0.210937272495367    17.1508444751884
#> 11 0.0883126972369182 0.0904546069723643    5.72216858255195
#> 12               <NA>               <NA>                <NA>
#> 13   5.87031607971857   5.93945257165821    73.2344396391608
#> 14   3.42815180053982   3.47939730377723    64.0149050203086
#> 15    3.3500455424489   3.41203881807743     63.926256704919
#> 16   2.50142887167915   2.54362452272566    58.1812874259695
#> 17   2.07532144492131   2.09796842996411    55.2451283806372
#> 18   1.65836942994578   1.68060945871196    51.1251999147769
#> 19   1.29669231206769   1.31075038098353    46.2743112695072
#> 20   0.81302122201313  0.826459416214558    36.5868326446625
#> 21  0.413654113697424  0.421452674218621    28.3547425629057
#> 22  0.175172930088356  0.183242074326832    15.2166433218808
#> 23 0.0824898377518115 0.0848487516636099    6.41429563806611
#> 24               <NA>               <NA>                <NA>
#> 25   5.70173777350129   5.75618680934482    71.9983826365475
#> 26   3.22287207733235   3.27644788182175    65.9866426397353
#> 27   2.86855874918844   2.89808029236706    61.4201984086266
#> 28   2.05422452470933   2.09229352239584    55.5714074585997
#> 29    1.5287064350155   1.54209230321077    53.7232417461337
#> 30   1.19632349279863   1.21762187479481    46.0166184427243
#> 31  0.694370073628421  0.702059321728622    36.3786920336115
#> 32  0.410930717649684  0.416452652800173    27.2777467542409
#> 33  0.192149874054078  0.197826890903211    15.3216371670563
#> 34  0.093388326636153 0.0965698679306777    7.15222299015756
#> 35               <NA>               <NA>                <NA>
#> 36   3.77997770735404   3.84615915686413    62.2329363898871
#> 37   2.77849135782545   2.83290295986233    57.5348187649103
#> 38   2.50306797578843   2.54986741872924    56.6457905296661
#> 39   1.86982253112411   1.89186909914613    52.4748072478314
#> 40   1.29029775462979   1.31782163109198    47.1119094931883
#> 41    1.1531675298235   1.16991431908238    44.6043074583755
#> 42  0.895462365008085  0.913693447818428    37.1402945512593
#> 43  0.415015046473459  0.423670476316439    24.9851361548542
#> 44  0.101005751896917  0.103858468885553    7.90984996968034
#> 45               <NA>               <NA>                <NA>
#> 46               <NA>               <NA>                <NA>
#> 47               <NA>               <NA>                <NA>
#> 48   4.20672508352816   4.24777443087499    63.8484250241266
#> 49   2.12981423324469   2.16846535747895    56.0947449072452
#> 50   2.25243314315562   2.28403247643326     54.091497557934
#> 51   1.91152137508771   1.93562831413908    53.5601553315154
#> 52   1.61692327208433   1.63722427178652    47.7085967731517
#> 53   1.13812897055634   1.17459107601041    41.8267672452707
#> 54  0.820032541370883  0.851211776915042    36.5493849113447
#> 55  0.398980382817169  0.421525192680456    23.5691322611327
#> 56  0.117654837355253  0.120100550866208    9.09779079097528
#> 57               <NA>               <NA>                <NA>
#> 58               <NA>               <NA>                <NA>
#> 59               <NA>               <NA>                <NA>
#> 60   3.58740324439899   3.63366800457793    63.3715229054786
#> 61   1.90969366384142   1.93774263460368    55.1708258033947
#> 62    1.6360071114761   1.65953049494477    52.2435489980636
#> 63   1.44303329763463   1.46243292124235    48.9577716704386
#> 64   1.05538506615546   1.06724601670325    42.8587162496163
#> 65  0.817177804424725  0.840632479517509     35.408260746321
#> 66  0.398764647232553  0.417754463256849    23.3381268496428
#> 67  0.103529700485426  0.109565667337707    9.11627239256907
#> 68               <NA>               <NA>                <NA>
#> 69               <NA>               <NA>                <NA>
#> 70               <NA>               <NA>                <NA>
#>    ci90.integral.bt.up      ci95.mu.bt.lo      ci95.mu.bt.up  ci95.lambda.bt.lo
#> 1     75.3499849608811  0.656580835187633  0.672970040504816   4.59960736569692
#> 2     68.3448031683789  0.600010733358665  0.619088120961569   4.08721557218682
#> 3     67.9971016122262  0.602054164074652  0.623053418802575   4.22048166840329
#> 4     63.4807096953433  0.579114998936431  0.597934736158169   4.63218863612802
#> 5     61.7210730434246  0.526216718282284  0.541418490270262   4.21246229923255
#> 6     55.2920903891447  0.469384189324355  0.486043324489856   4.32503843126767
#> 7      47.261518094729  0.371127180245792  0.384015090016519   4.17085166466661
#> 8     39.5409576898083  0.263158800345373  0.273344767818889   4.12610694588597
#> 9     28.5842417619877  0.143081879236634  0.157500840306645   2.64289964909305
#> 10    18.4525974445434 0.0731430801031227 0.0823031325790355   1.19073175316677
#> 11    6.19697692919001   0.02570332576688 0.0368232324535106  -1.54396612408343
#> 12                <NA>               <NA>               <NA>               <NA>
#> 13    76.6786739664639  0.657511070652718   0.67550429830344   4.16643369293257
#> 14    67.5039701162086  0.600202252775382  0.622674096538709   4.65210910196148
#> 15    67.9634569294132   0.58434631427239  0.599869867663517   4.08325606149511
#> 16    62.7527100227915  0.559997799256795  0.582044064441483   4.38438415973449
#> 17    57.9530510041212  0.516161115267423  0.535290738801282   4.43143971880864
#> 18    53.6447873691655  0.456476366745599  0.466864607320196   4.62585655087683
#> 19    48.0651020738276  0.372468814741118  0.389343571523779   4.32365184231137
#> 20    39.9787421476369  0.258574390738996  0.275522380390913   3.97147898313651
#> 21    29.2139611326936  0.146104421438588  0.156056737558307    2.7095627113582
#> 22    16.1024783481891 0.0664801157290151 0.0750567430117262   1.18372728318006
#> 23    6.97637594824802 0.0279646640926863 0.0381371905848361  -2.27261934425029
#> 24                <NA>               <NA>               <NA>               <NA>
#> 25    75.3874000800908  0.649791793455564  0.663441096586342   4.27874331323435
#> 26     69.074066821232   0.59551781924555  0.612201321844769   4.10067391978165
#> 27    63.5222758426126  0.566224844384218  0.585591691757114   4.58908226077083
#> 28    57.6106090297994   0.51453596008079  0.530566588841134   4.36571724425431
#> 29    55.3063936698449  0.458194831055398  0.475444979681219   4.07312583707445
#> 30    48.3306410527808  0.374637531735787  0.393205478590932   4.15545626053564
#> 31     38.367602577854  0.252569207753814  0.266604047215899   3.99984405993541
#> 32    28.4286383148295   0.13955433988537  0.158965829943623   2.48149633578883
#> 33    16.2842018694044 0.0693697796258517 0.0780059414231244   1.31345049708351
#> 34    7.57633711690428  0.030256252116708 0.0438252573718216 -0.496087701975842
#> 35                <NA>               <NA>               <NA>               <NA>
#> 36      66.28964367375  0.468972111486173  0.485674736465369   3.87733117743543
#> 37    59.4766760483429  0.444048112765491  0.460714035242741   4.10660028183249
#> 38    59.0026729899606  0.417834773097635  0.429025215044064   3.77147146326971
#> 39    54.2460760137565  0.388915002635203  0.395671305156611   3.88175847115269
#> 40    49.8961191356316  0.355297039861341  0.364945993248898   3.87357878001159
#> 41    46.1081265673221  0.289043062933366  0.302039822316958   3.77783057502662
#> 42     39.151584169519  0.214878759863973  0.224317375235515   3.29323610206513
#> 43    25.9431505715263  0.114973279629594    0.1245420807151   2.25331294698654
#> 44    8.40603771494128 0.0342028057927774 0.0443103660229834  0.125415215262048
#> 45                <NA>               <NA>               <NA>               <NA>
#> 46                <NA>               <NA>               <NA>               <NA>
#> 47                <NA>               <NA>               <NA>               <NA>
#> 48    67.5490425348569  0.472448332971671  0.486048803513327   3.80658062920939
#> 49    58.5668358804973  0.418878247991007   0.43221860763487   3.65653986565658
#> 50    57.6557113179538  0.424708020321369  0.439363248871858   4.21007069954865
#> 51    55.2878251472621  0.384915318005364  0.399069497891261   3.64333364696971
#> 52    49.7982372366018   0.34561766721432  0.360481659233657   4.11568063097819
#> 53    45.4562143106951  0.281178240937047  0.292046516773419   3.67865803848272
#> 54    39.2467484108773  0.211075595928977  0.219741060375497   3.06656558584678
#> 55    24.9488809796673  0.110225817004072  0.123159993824894   2.16226150411674
#> 56     9.4409452775604 0.0383320787311771 0.0530431262064323  0.486313782690305
#> 57                <NA>               <NA>               <NA>               <NA>
#> 58                <NA>               <NA>               <NA>               <NA>
#> 59                <NA>               <NA>               <NA>               <NA>
#> 60    65.0812206797883   0.46695303766981  0.490698577253706   4.12048454514687
#> 61    56.9719261730172  0.412698164401473  0.424616130743946   3.95675130442923
#> 62    53.5971810331647  0.378923911113591  0.399597922630715   4.01630770502938
#> 63    50.6080390741102  0.346837254482217   0.35754440800642   3.92764576947221
#> 64    44.3093248491536   0.28818430337304   0.29967386214314   3.90964174473713
#> 65     37.613928872725  0.205595350348199  0.216768840154845   3.22165040608891
#> 66    25.4751575316108  0.108925644807363  0.120990909370841   2.27970384563623
#> 67    9.71726088129606 0.0385271765793338 0.0506564421757316 -0.799906280393993
#> 68                <NA>               <NA>               <NA>               <NA>
#> 69                <NA>               <NA>               <NA>               <NA>
#> 70                <NA>               <NA>               <NA>               <NA>
#>    ci95.lambda.bt.up       ci95.A.bt.lo       ci95.A.bt.up ci95.integral.bt.lo
#> 1    4.6699575234591   6.93541008562285   7.01470955105265     72.665533050922
#> 2   4.21986134084886    3.7831728974948   3.91116149394969    64.2431397054745
#> 3   4.34531330436659   3.67719283073352   3.75697766748995    64.6668873766584
#> 4   4.74974443281598   2.76580055936047   2.81105886651207    59.5666548903287
#> 5   4.30671929671517   2.42506741627955   2.45797119707102      57.87716810926
#> 6   4.46823436065651   1.63693856516723    1.6609127264889    52.2147280341951
#> 7    4.3553042369824    1.1581449012624    1.1912604779527     45.669196892065
#> 8   4.31058327585056  0.893629091877639  0.906021098393607    38.0804069459743
#> 9   3.25200956211984  0.397519914991737   0.40690563520238    27.6771978900084
#> 10  2.47426839430494  0.201370699751427  0.211773186618624    17.0262085525906
#> 11  5.85881781363116 0.0881076207728861 0.0906596834363963    5.67670820893767
#> 12              <NA>               <NA>               <NA>                <NA>
#> 13  4.23771580044547   5.86369662836264   5.94607202301413    72.9046725227169
#> 14  4.78990584176564    3.4232453161873   3.48430378812975    63.6808455962331
#> 15  4.20470848065878   3.34411001605893    3.4179743444674     63.539716257893
#> 16  4.52442401351019    2.4973888625364   2.54766453186841    57.7435980284015
#> 17  4.58372275852526   2.07315311656615   2.10013675831927    54.9858591932823
#> 18  4.71194128574503   1.65624006548945   1.68273882316829    50.8839628180801
#> 19  4.53785628879537   1.29534632674597   1.31209636630525    46.1028525754765
#> 20   4.2513366403592    0.8117345863981  0.827746051829589    36.2620753518246
#> 21   3.2314351498616  0.412907443009224  0.422199344906821      28.27247695516
#> 22  2.64931440055363  0.174400352448501  0.184014651966686    15.1318293300002
#> 23  6.02708193781749 0.0822639842921712 0.0850746051232501    6.36047943815508
#> 24              <NA>               <NA>               <NA>                <NA>
#> 25  4.34884984280815   5.69652456794181    5.7614000149043    71.6739022430168
#> 26  4.20677989342828   3.21774247903017   3.28157748012392    65.6910381968261
#> 27  4.69396865027206   2.86573221845857   2.90090682309693    61.2189356755854
#> 28  4.48098152134566   2.05057962067552   2.09593842642966    55.3761647549742
#> 29  4.22842893445195   1.52742480933723   1.54337392888904    53.5716633704592
#> 30  4.38204342703073   1.19428428601176   1.21966108158167    45.7950630864423
#> 31  4.31663024755569  0.693633869023083   0.70279552633396    36.1882644283116
#> 32  3.53505206432207   0.41040202173102  0.416981348718837    27.1675550090781
#> 33  3.16183797090488  0.191606329887671  0.198370435069618    15.2294767168315
#> 34  7.08312954305311 0.0930837109802942 0.0968744835865365    7.11161631844777
#> 35              <NA>               <NA>               <NA>                <NA>
#> 36  4.01660575260198   3.77364118559243   3.85249567862573    61.8445282456875
#> 37  4.28444086403803   2.77328173635383   2.83811258133395    57.3488962590497
#> 38  3.89073985732943   2.49858717806005   2.55434821645761    56.4201315707017
#> 39  3.97818152944709   1.86771168950498   1.89397994076526    52.3052176851365
#> 40  4.01540777552186   1.28766248986213   1.32045689585964    46.8453362295501
#> 41   4.0235341188042   1.15156411383063   1.17151773507525    44.4603247777317
#> 42   3.5460031393039   0.89371683580284  0.915438977023673      36.94772426866
#> 43  3.09301143202612  0.414186335105514  0.424499187684384    24.8934113702793
#> 44  6.22714628303939  0.100732619419282  0.104131601363188    7.86234263236812
#> 45              <NA>               <NA>               <NA>                <NA>
#> 46              <NA>               <NA>               <NA>                <NA>
#> 47              <NA>               <NA>               <NA>                <NA>
#> 48  3.94647071580678    4.2027948268673   4.25170468753586    63.4941105816099
#> 49  3.80474559099203   2.12611359369035   2.17216599703329    55.8580553459764
#> 50  4.34649349125313   2.24940767507585   2.28705794451304     53.750243048996
#> 51  3.82813856553161   1.90921326390194   1.93793642532485    53.3947401363907
#> 52  4.31222966861414   1.61497955934689   1.63916798452396    47.5085248138852
#> 53  3.92650168338197   1.13463791790648   1.17808212866026    41.4792669943258
#> 54  3.28651540026425  0.817047295414527  0.854197022871398    36.2911267039427
#> 55  3.15098318146066  0.396821837191961  0.423683738305665     23.437028660422
#> 56   10.569940190731  0.117420673295693  0.120334714925767    9.06493557417458
#> 57              <NA>               <NA>               <NA>                <NA>
#> 58              <NA>               <NA>               <NA>                <NA>
#> 59              <NA>               <NA>               <NA>                <NA>
#> 60  4.37757835049874   3.58297363970101   3.63809760927592    63.2078284377255
#> 61  4.07927831396136   1.90700812408759   1.94042817435751    54.9983800233244
#> 62  4.24809447015067   1.63375487263335   1.66178273378751    52.1139459308731
#> 63   4.1218584887116   1.44117588686368    1.4642903320133    48.7997673445551
#> 64  4.08135324405684   1.05424944323067   1.06838163962804    42.7198281922138
#> 65  3.64909360489959  0.814932144043501  0.842878139898733    35.1970797554951
#> 66  3.35035003008702  0.396946473357886  0.419572637131516    23.1335175290288
#> 67  7.62537251038299  0.102951788765527  0.110143579057606    9.05873094152074
#> 68              <NA>               <NA>               <NA>                <NA>
#> 69              <NA>               <NA>               <NA>                <NA>
#> 70              <NA>               <NA>               <NA>                <NA>
#>    ci95.integral.bt.up
#> 1     75.5845487200038
#> 2     68.7032009466909
#> 3     68.2880912056253
#> 4      63.822714484131
#> 5     62.0569482318468
#> 6     55.5609861288976
#> 7     47.4006529570977
#> 8     39.6685786285899
#> 9      28.663498022646
#> 10    18.5772333671412
#> 11    6.24243730280429
#> 12                <NA>
#> 13    77.0084410829078
#> 14    67.8380295402841
#> 15    68.3499973764392
#> 16    63.1903994203596
#> 17    58.2123201914761
#> 18    53.8860244658623
#> 19    48.2365607678583
#> 20    40.3034994404748
#> 21    29.2962267404393
#> 22    16.1872923400697
#> 23    7.03019214815905
#> 24                <NA>
#> 25    75.7118804736215
#> 26    69.3696712641412
#> 27    63.7235385756538
#> 28    57.8058517334249
#> 29    55.4579720455194
#> 30    48.5521964090628
#> 31    38.5580301831538
#> 32    28.5388300599922
#> 33    16.3763623196292
#> 34    7.61694378861407
#> 35                <NA>
#> 36    66.6780518179497
#> 37    59.6625985542034
#> 38    59.2283319489249
#> 39    54.4156655764514
#> 40    50.1626923992698
#> 41    46.2521092479659
#> 42    39.3441544521184
#> 43    26.0348753561013
#> 44     8.4535450522535
#> 45                <NA>
#> 46                <NA>
#> 47                <NA>
#> 48    67.9033569773736
#> 49    58.8035254417661
#> 50    57.9969658268918
#> 51    55.4532403423867
#> 52    49.9983091958684
#> 53      45.80371456164
#> 54    39.5050066182794
#> 55    25.0809845803781
#> 56    9.47380049436111
#> 57                <NA>
#> 58                <NA>
#> 59                <NA>
#> 60    65.2449151475413
#> 61    57.1443719530874
#> 62    53.7267841003552
#> 63    50.7660433999936
#> 64    44.4482129065562
#> 65     37.825109863551
#> 66    25.6797668522248
#> 67    9.77480233234438
#> 68                <NA>
#> 69                <NA>
#> 70                <NA>
# }
```
