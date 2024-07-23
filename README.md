

# Contribution of vaccinations to reducing socioeconomic disparities in COVID-19 deaths across US counties

This repository contains the code and data for <a href="https://www.sciencedirect.com/science/article/abs/pii/S1047279723001400" target="_blank">Contribution of vaccinations to reducing socioeconomic disparities in COVID-19 deaths across US counties</a>, published in <i>Annals of Epidemiology</i>. 

For explanation of variables see `R/varlabels.R`

To run the main analyses, execute the following:

```r
rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
rmarkdown::render(paste(rootdir,"rmd/ses.Rmd", sep="/"))
```

To run the supplemental analyses, execute the following:

```r
rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
rmarkdown::render(paste(rootdir,"rmd/supplement_confounding.Rmd", sep="/"))
```
