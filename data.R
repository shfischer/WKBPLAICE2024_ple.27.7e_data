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

### natural mortality
sourceTAF("data_natural_mortality.R")

### maturity
sourceTAF("data_maturity.R")

### prepare data for OM
sourceTAF("data_OM.R")

### run exploratory SAM models
# sourceTAF("model_SAM.R")

