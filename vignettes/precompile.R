library(knitr)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

knit("vignette_growth_prettydoc.Rmd.orig", "vignette_growth.Rmd")
knit("vignette_fluorescence_prettydoc.Rmd.orig", "vignette_fluorescence.Rmd")

