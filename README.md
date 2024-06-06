WKBPLAICE 2024 - ple.27.7e
================

## Plaice (*Pleuronectes platessa*) in Division 7.e (western English Channel) - WKBPLAICE 2024

This repository contains the data preparation for ple.27.7e for the
benchmark workshop WKBPLAICE 2024.

## R packages

The following R packages from CRAN are required to run the assessment:

``` r
icesTAF
icesAdvice
ggplot2
tidyr
dplyr
tibble
stringr
foreach
patchwork
XLConnect
icesDatras
mapdata
foreign
slider
```

They can be installed with:

``` r
### list with required packages
req_pkgs <- c("icesTAF", "icesAdvice", "ggplot2", "tidyr", "dplyr", "tibble", "stringr", "foreach", "patchwork", "XLConnect", "icesDatras", "mapdata", "foreign", "slider")
### install/update packages
install.packages(req_pkgs)
```

Furthermore, the following FLR (<https://www.flr-project.org>,
<https://github.com/flr>) packages are required:

``` r
FLCore
```

They can be installed with

``` r
install.packages("FLCore", repos = c("https://ices-tools-prod.r-universe.dev", "https://cloud.r-project.org"))
```

The SAM R package `stockassessment` is needed to run SAM and can be
installed with

``` r
install.packages("stockassessment", repos = c("https://ices-tools-prod.r-universe.dev", "https://cloud.r-project.org"))
```

## Running the code

The easiest way to run the code is to clone or download this repository,
navigate into the repository with R and run:

``` r
### load the icesTAF package
library(icesTAF)
### load data and install R packages
taf.bootstrap()
### run all scripts
source.all()
```
