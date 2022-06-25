

# Associations Between Poverty and COVID-19 Death Rates Mediated Through Vaccination Rate and Compliance with Stay-at-home Orders in the US

This repository contains the code and data. 

For explanation of variables see `R/varlabels.R`

To run, execute the following:

```r
library(rmarkdown)
rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
rmarkdown::render(paste(rootdir,"rmd/ses.Rmd", sep="/"))
```
