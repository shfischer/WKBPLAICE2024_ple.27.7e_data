### ------------------------------------------------------------------------ ###
### Preprocess data, write TAF data tables ####
### ------------------------------------------------------------------------ ###

## Before:
## After:

library(icesTAF)

### create folder to store data
mkdir("data")

### catch data
sourceTAF("data_catch.R")

### survey data
sourceTAF("data_surveys.R")

### ALKs
sourceTAF("data_ALK.R")
