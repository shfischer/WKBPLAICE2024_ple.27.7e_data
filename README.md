2021\_ple.27.7e\_assessment
================

## Plaice (*Pleuronectes platessa*) in Division 7.e (western English Channel) - WGCSE 2021

This repository recreates the stock assessment for plaice (*Pleuronectes
platessa*) in Division 7.e (western English Channel) in `R` from WGCSE
2020.

## R packages

The following R packages from CRAN are required to run the assessment:

``` r
icesTAF
icesAdvice
icesSAG
ggplot2
dplyr
reshape
Cairo
tidyr
XML
foreach
doParallel
cowplot
```

They can be installed with:

``` r
### list with required packages
req_pkgs <- c("icesTAF", "icesAdvice", "icesSAG", "ggplot2", "dplyr", "reshape", 
              "Cairo", "tidyr", "XML", "foreach", "doParallel", "cowplot")
### install/update packages
install.packages(req_pkgs)
```

Furthermore, the following FLR (<https://www.flr-project.org>,
<https://github.com/flr>) packages are required:

``` r
FLCore
FLAssess
FLXSA
FLash
ggplotFL
```

For exact reproducibility, it is recommended to use exactly the same
package version as used in the assessment. These FLR package are
automatically installed into a local library when running the TAF
assessment (see below) or by calling

``` r
taf.bootstrap()
```

Alternatively, they can be manually installed from GitHub with

``` r
devtools::install_github("flr/FLCore", ref = "109858c")
devtools::install_github("flr/FLAssess", INSTALL_opts = "--no-multiarch",
                         ref = "14c7752")
devtools::install_github("flr/FLXSA", INSTALL_opts = "--no-multiarch",
                         ref = "40756e4")
devtools::install_github("flr/FLash", INSTALL_opts = "--no-multiarch",
                         ref = "61cb246")
devtools::install_github("flr/ggplotFL", ref = "07c8564")
```

Please note, on Windows `FLAssess`, `FLXSA` and `FLash` can only be
installed and used in the 64-bit version of `R` (previously they only
worked on 32-bit).

## Running the assessment

The easiest way to run the assessment is to clone or download this
repository, navigate into the repository with R and run:

``` r
### load the icesTAF package
library(icesTAF)
### load data and install R packages
taf.bootstrap()
### run all scripts
sourceAll()
```

This code snippet runs the entire data compilation and assessment and
creates the tables and figures presented in the WG report.

## Explore results

To view the results created on the TAF server, browse the [GitHub taf
branch](https://github.com/ices-taf/2021_ple.27.7e_assessment/tree/taf).

Assessments results can also be browsed on the [TAF
application](https://taf.ices.dk/app/stock#!/2021/ple.27.7e).

## Exploratory analyses

This repository contains code for analyses which are not part of the
routine assessment. The scripts for these analyses are not executed when
replicating this stockassessment because the links are commented out in
[`data.R`](data.R), [`model.R`](model.R) and [`report.R`](report.R) and
but can be run manually.

### Discard estimation and assessment

The repository contains code for estimating historical discards (see
[`data_6_discards.R`](data_6_discards.R)), an XSA assessment with total
catch including discards ([`model_4_discards.R`](model_4_discards.R))
and some outputs ([`report_4_discards.R`](report_4_discards.R)).
Furthermore, [`model_5_discards_EqSim.R`](model_5_discards_EqSim.R)
contains code to update the biological reference points for the total
catch assessment. Please note, that this approach has not been reviewed
and is exploratory only. For running this code, the ICES msy R package
is required and can be installed with

``` r
devtools::install_github("ices-tools-prod/msy")
```

### length-based approach

There is code for comparing the catch length distribution to possible
reference points (see [`data_5_length_data.R`](data_5_length_data.R)).

### SPiCT

Exploratory SPiCT assessments are available (see
[`model_3_SPiCT.R`](model_3_SPiCT.R) and
[`report_3_SPiCT.R`](report_3_SPiCT.R)). For running the SPiCT trial
assessment, the R package `spict` needs to be installed from GitHub:

``` r
devtools::install_github("mawp/spict/spict", ref = "4e0937b")
```
