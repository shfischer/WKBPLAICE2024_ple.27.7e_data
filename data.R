### ------------------------------------------------------------------------ ###
### Preprocess data, write TAF data tables ####
### ------------------------------------------------------------------------ ###

## Before:
## After:

library(icesTAF)

### create folder to store data
mkdir("data")

### extract data from InterCatch
sourceTAF("data_1_InterCatch_extract.R")

### processs data from InterCatch and add to time series (VPA files)
sourceTAF("data_2_migration_weights.R")

### extend tuning time series
sourceTAF("data_3_tuning.R")

### load data required for stock assessment
sourceTAF("data_4_assessment.R")

### length data
sourceTAF("data_5_length_data.R")

### prepare discard data
sourceTAF("data_6_discards.R")

### prepare data for rfb rule
# sourceTAF("data_7_rfb.R")

