### ------------------------------------------------------------------------ ###
### Prepare plots and tables for report ####
### ------------------------------------------------------------------------ ###


## Before:
## After:

library(icesTAF)

mkdir("report")
mkdir("report/XSA/")
mkdir("report/SAM/")
mkdir("report/surveys/")
mkdir("report/advice/")

### stock data (weights at age etc)
sourceTAF("report_1_stock_data.R")
### exploratory SAM assessment
sourceTAF("report_3_SAM.R")
### survey data
sourceTAF("report_4_surveys.R")
### prepare ICES advice
### -> moved to separate branch of repository

# done
