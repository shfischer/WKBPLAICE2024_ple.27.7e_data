### ------------------------------------------------------------------------ ###
### create ICES standard graphs for advice sheet ####
### ------------------------------------------------------------------------ ###

## Before: report/tables_sag.csv
## After:  report/standard_graphs/historical.png
##         report/standard_graphs/stock_plot.png
##         report/standard_graphs/stock_status.png

### load packages
suppressPackageStartupMessages(library(TAF))
taf.libPaths()
suppressPackageStartupMessages(library(icesSAG))
suppressPackageStartupMessages(library(Cairo))

### load additional functions
source("utilities.R")

### ------------------------------------------------------------------------ ###
### preparation ####
### ------------------------------------------------------------------------ ###

### ICES standard graphs
### create token for authentication
### go to https://standardgraphs.ices.dk/manage/index.aspx
### login
### click on create token or go directly to 
### https://standardgraphs.ices.dk/manage/CreateToken.aspx
### create new token, save in file
# file.edit("~/.Renviron")
### in the format 
### SG_PAT=some_token_......
### save and restart R

### load token
Sys.getenv("SG_PAT")
options(icesSAG.use_token = TRUE)

### assessment year
ass_yr <- 2021

### check assessments keys
key <- findAssessmentKey("ple.27.7e", year = ass_yr)
key_last <- findAssessmentKey("ple.27.7e", year = ass_yr - 1)

### last year's graphs
plot(getSAGGraphs(key_last))
settings_last <- getSAGSettingsForAStock(key_last)

### list of possible elements:
### https://datsu.ices.dk/web/selRep.aspx?Dataset=126
### allowed units:
### https://vocab.ices.dk/?ref=155

### set up stock info
stk_info <- stockInfo(
  StockCode = "ple.27.7e", 
  AssessmentYear = ass_yr, 
  ContactPerson = "simon.fischer@cefas.co.uk", 
  Purpose = "Advice"
)
#str(stk_info)
### add some more data manually
stk_info$MSYBtrigger <- 2443
stk_info$FMSY <- 0.238
stk_info$Blim <- 1745
stk_info$Flim <- 0.88
stk_info$Bpa <- 2443
stk_info$Fpa <- 0.69 ### updated in 2021, Fp.05 is new basis
stk_info$Fage <- "3-6"
stk_info$RecruitmentAge <- 2
stk_info$RecruitmentDescription <- "Recruitment"
stk_info$RecruitmentUnits <- "NE3" ### NE3 stands for thousands
# stk_info$RecruitmentUnits <- "Relative Recruitment"
stk_info$CatchesLandingsUnits <- "t" ### t for tonnes
stk_info$StockSizeUnits <- "t" ### t for tonnes
stk_info$StockSizeDescription <- "SSB"
# stk_info$StockSizeDescription <- "Stock Size: Relative"
stk_info$FishingPressureDescription <- "F"
# stk_info$FishingPressureDescription <- "Fishing pressure: Relative"
stk_info$Purpose <- "Advice"
stk_info$ModelType <- "A"
stk_info$ModelName <- "XSA"

### load data from assessment/forecast
sag <- read.csv(file = "report/tables_sag.csv", as.is = TRUE)

### set up data
# https://datsu.ices.dk/web/selRep.aspx?Dataset=126  # Record: AF - Fish Data
stk_data <- stockFishdata(
  Year = sag$Year,
  Recruitment = sag$Recruitment,
  TBiomass = sag$TBiomass,
  StockSize = sag$StockSize,
  Landings = sag$Landings,
  Discards = sag$Discards,
  FishingPressure = sag$FishingPressure
)

### upload 
key_new <- uploadStock(info = stk_info, fishdata = stk_data, verbose = TRUE)
key_new <- findAssessmentKey('ple.27.7e', ass_yr, full = TRUE)$AssessmentKey

### check upload
plot(getSAGGraphs(key_new))

### get chart settings 
### should be automatically copied from last year
chart_settings <- getSAGSettingsForAStock(key_new) 
### compare with last years settings
settings_last <- getSAGSettingsForAStock(key_last)
all.equal(chart_settings, settings_last)
### yes, identical (only new assessment key)

### modify chart settings
### possible options listed here: 
### https://standardgraphs.ices.dk/manage/ListSettings.aspx
### ymax landings
# setSAGSettingForAStock(assessmentKey = key_new, chartKey = 1, settingKey = 6,
#                        settingValue = 4.4, copyNextYear = FALSE)
### ymax F
# setSAGSettingForAStock(assessmentKey = key_new, chartKey = 3, settingKey = 6,
#                        settingValue = 2.6, copyNextYear = FALSE)
### unshade last recruitment value (geometric mean)
# setSAGSettingForAStock(assessmentKey = key_new, chartKey = 2, settingKey = 14,
#                        settingValue = ass_yr, copyNextYear = FALSE)
### show historical 5 years
### doesn't work, remove manually in sg.ices.dk
# setSAGSettingForAStock(assessmentKey = key_new, chartKey = 10, settingKey = 22,
#                        settingValue = "", copyNextYear = FALSE)

### check again
getSAGSettingsForAStock(key_new) 
plot(getSAGGraphs(key_new))


### save plot(s) for advice sheet
dir.create(paste0("report/standard_graphs/"), 
           recursive = TRUE)
### stock plot
### individual plots are 450 * 250 px
png(filename = paste0("report/standard_graphs/", "stock_plot.png"), 
    width = 450*2, height = 250*2, units = "px", type = "cairo")
plot(getSAGGraphs(key_new))
dev.off()

### historical assessments
### each 300 * 300
png(filename = paste0("report/standard_graphs/", "historical.png"), 
    width = 3*300, height = 300, units = "px", type = "cairo")
hist <- list(getSSBHistoricalPerformance(key_new)[[1]],
             getFishingMortalityHistoricalPerformance(key_new)[[1]],
             getRecruitmentHistoricalPerformance(key_new)[[1]])
plot.ices_standardgraph_list2(hist, c = 1, r = 3)
dev.off()

### stock status
### it might be neccessary to updated manually to
### remove qualitative evaluation row, add "proxy" suffix etc.
### 2019: manually modify management plan row
###        F: ? and "undefined"
###        Stock size: all green and "Above trigger"
### add "proxy" to MSY
### 991 * 214
png(filename = paste0("report/standard_graphs/", "stock_status_proxy.png"), 
    width = 991, height = 214, units = "px", type = "cairo")
plot(getStockStatusTable(key_new))
dev.off()

### done
