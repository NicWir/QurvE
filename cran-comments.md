---
title: "cran-comments"
author: "Nicolas Wirth"
date: "`r Sys.Date()`"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs

## Downstream dependencies
I have also run R CMD check on downstream dependencies of httr 
(https://github.com/wch/checkresults/blob/master/httr/r-release). 
All packages that I could install passed except:

* Ecoengine: this appears to be a failure related to config on 
  that machine. I couldn't reproduce it locally, and it doesn't 
  seem to be related to changes in httr (the same problem exists 
  with httr 0.4).

