### ------------------------------------------------------------------------ ###
### extend tuning time series ####
### ------------------------------------------------------------------------ ###

## before:  boot/data/tuning/:
##            FSP_numbers.csv, 
##            FSP_biomass.csv,
##            Q1SWBEAM_Index.xls
##          boot/data/vpa_files/PLE7ETU_full.dat
##          boot/data/accessions/LPUE.csv

## after: data/PLE7ETU_full.dat (updated)
##        data/plots_data_LPUE.png 

### ------------------------------------------------------------------------ ###
### prepare ####
### ------------------------------------------------------------------------ ###

### load packages
suppressPackageStartupMessages(library(icesTAF))
taf.libPaths()
suppressPackageStartupMessages(library(XML))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))

if (!exists("verbose")) verbose <- FALSE

### load file with survey indices
idx_file <- readLines(con = "boot/data/vpa_files/PLE7ETU_full.dat")


### ------------------------------------------------------------------------ ###
### update FSP - numbers ####

### load new index data
idx_update <- read.csv(paste0("boot/data/tuning/FSP_numbers.csv"))
### year range
new_years <- idx_update$year
### ages
new_ages <- names(idx_update)[grep(x = names(idx_update), pattern = "AAge*")]
new_ages <- as.numeric(gsub(x = new_ages, pattern = "[[:alpha:]]",
                            replacement = ""))

### format index value for insertion
idx_insert <- idx_update[, c(1, grep(x = names(idx_update),
                                     pattern = "^AAge*"))]
### use first column for effort
idx_insert[, 1] <- 1
### use tab to separate cells
idx_insert <- apply(idx_insert, MARGIN = 1, paste, collapse = "\t")

### find position where FSP starts
fsp_start <- grep(x = idx_file, pattern = "^FSP-7e$")
### find start of index values
fsp_idx_start <- fsp_start + 4
### find end, i.e. start of next index
fsp_idx_end <- fsp_start +
  grep(x = idx_file[-seq(fsp_start)], pattern = "^[a-zA-Z]+")[1] - 1

### update year range
idx_file[fsp_start + 1] <- paste(range(new_years), collapse = " ")
### update age range
idx_file[fsp_start + 3] <- paste(range(new_ages), collapse = " ")

### coerce object into list for easier insertion of values
idx_file <- as.list(idx_file)

### remove old values (apart from first row)
idx_file[(fsp_idx_start + 1):fsp_idx_end] <- NULL
### insert new values
idx_file[fsp_idx_start] <- list(idx_insert)
idx_file <- unlist(idx_file, recursive = TRUE)

### ------------------------------------------------------------------------ ###
### update FSP - biomass ####

### load new index data
idx_update <- read.csv(paste0("boot/data/tuning/FSP_biomass.csv"))
### year range
new_years <- idx_update$year
### ages
new_ages <- names(idx_update)[grep(x = names(idx_update), pattern = "^WAAge*")]
new_ages <- as.numeric(gsub(x = new_ages, pattern = "[[:alpha:]]",
                            replacement = ""))

### format index value for insertion
idx_insert <- idx_update[, c(1, grep(x = names(idx_update),
                                     pattern = "^WAAge*"))]
### use first column for effort
idx_insert[, 1] <- 1
### use tab to separate cells
idx_insert <- apply(idx_insert, MARGIN = 1, paste, collapse = "\t")

### find position where biomass FSP starts
fsp_start <- grep(x = idx_file, pattern = "^FSP-7e-biomass$")
### find start of index values
fsp_idx_start <- fsp_start + 4
### find end, i.e. start of next index
fsp_idx_end <- fsp_start +
  grep(x = idx_file[-seq(fsp_start)], pattern = "^[a-zA-Z]+")[1] - 1
### or if last index, use last data row
if (is.na(fsp_idx_end)) {
  fsp_idx_end <- fsp_start + 
    tail(grep(x = idx_file[-seq(fsp_start)], pattern = "^[0-9]+"), 1)
}

### update year range
idx_file[fsp_start + 1] <- paste(range(new_years), collapse = " ")
### update age range
idx_file[fsp_start + 3] <- paste(range(new_ages), collapse = " ")

### coerce object into list for easier insertion of values
idx_file <- as.list(idx_file)

### remove old values (apart from first row)
idx_file[(fsp_idx_start + 1):fsp_idx_end] <- NULL
### insert new values
idx_file[fsp_idx_start] <- list(idx_insert)
idx_file <- unlist(idx_file, recursive = TRUE)

### ------------------------------------------------------------------------ ###
### update Q1SWBeam ####

### read data
idx_update <- readHTMLTable(doc = paste0("boot/data/tuning/",
                                         "Q1SWBEAM_IndexV2i.xls"),
                            colClasses = "numeric")[[2]]

### year range
new_years <- idx_update[, grep(x = names(idx_update),
                               pattern = "[yY]ear")]
### ages
new_ages <- names(idx_update)[grep(x = names(idx_update), pattern = "AAge*")]
new_ages <- as.numeric(gsub(x = new_ages, pattern = "[[:alpha:]]",
                            replacement = ""))

### format index value for insertion
idx_insert <- idx_update[, c(1, grep(x = names(idx_update),
                                     pattern = "^AAge*"))]
### use first column for effort
idx_insert[, 1] <- 1
### use tab to separate cells
idx_insert <- apply(idx_insert, MARGIN = 1, paste, collapse = "\t")

### find position where survey starts
start <- grep(x = idx_file, pattern = "^Q1SWBeam$")
### find start of index values
idx_start <- start + 4
### find end, i.e. start of next index
idx_end <- start +
  grep(x = idx_file[-seq(start)], pattern = "^[a-zA-Z]+")[1] - 1
### or if last index, use last data row
if (is.na(idx_end)) {
  idx_end <- start +
    tail(grep(x = idx_file[-seq(start)], pattern = "^[0-9]+"), 1)
}

### update year range
idx_file[start + 1] <- paste(range(new_years), collapse = " ")
### update age range
idx_file[start + 3] <- paste(range(new_ages), collapse = " ")

### coerce object into list for easier insertion of values
idx_file <- as.list(idx_file)

### remove old values (apart from first row)
idx_file[(idx_start + 1):idx_end] <- NULL
### insert new values
idx_file[idx_start] <- list(idx_insert)
idx_file <- unlist(idx_file, recursive = TRUE)

### ------------------------------------------------------------------------ ###
### update Q1SWBeam-biomass ####

### read data
idx_update <- readHTMLTable(doc = paste0("boot/data/tuning/",
                                         "Q1SWBEAM_IndexV2i.xls"),
                            colClasses = "numeric")[[2]]

### year range
new_years <- idx_update[, grep(x = names(idx_update),
                               pattern = "[yY]ear")]
### ages
new_ages <- names(idx_update)[grep(x = names(idx_update), pattern = "WWAge*")]
new_ages <- as.numeric(gsub(x = new_ages, pattern = "WWAge_",
                            replacement = ""))

### format index value for insertion
idx_insert <- idx_update[, c(1, grep(x = names(idx_update),
                                     pattern = "^AAge*"))]
### use first column for effort
idx_insert[, 1] <- 1
### use tab to separate cells
idx_insert <- apply(idx_insert, MARGIN = 1, paste, collapse = "\t")

### find position where survey starts
start <- grep(x = idx_file, pattern = "^Q1SWBeam-biomass$")
### find start of index values
idx_start <- start + 4
### find end, i.e. start of next index
idx_end <- start +
  grep(x = idx_file[-seq(start)], pattern = "^[a-zA-Z]+")[1] - 1
### or if last index, use last data row
if (is.na(idx_end)) {
  idx_end <- start +
    tail(grep(x = idx_file[-seq(start)], pattern = "^[0-9]+"), 1)
}

### update year range
idx_file[start + 1] <- paste(range(new_years), collapse = " ")
### update age range
idx_file[start + 3] <- paste(range(new_ages), collapse = " ")

### coerce object into list for easier insertion of values
idx_file <- as.list(idx_file)

### remove old values (apart from first row)
idx_file[(idx_start + 1):idx_end] <- NULL
### insert new values
idx_file[idx_start] <- list(idx_insert)
idx_file <- unlist(idx_file, recursive = TRUE)


### ------------------------------------------------------------------------ ###
### update description ####
idx_file[1] <- "ple.27.7e WGCSE 2024"



### ------------------------------------------------------------------------ ###
### save tuning file

### save changes
writeLines(text = idx_file, con = "data/PLE7ETU_full.dat", 
           sep = "\n")


### ------------------------------------------------------------------------ ###
### commercial effort and LPUE ####
### ------------------------------------------------------------------------ ###

### get data from ADF spreadsheet
LPUE <- read.csv("boot/data/accessions/LPUE.csv")

### reshape
LPUE <- LPUE %>% gather(key = "key", value = "value", 2:5) %>%
  mutate(fleet = sapply(strsplit(key, "_"), "[[", 1),
         type = sapply(strsplit(key, "_"), "[[", 2),
         key = NULL) %>%
  tidyr::spread(key = type, value = value) %>%
  mutate(LPUE = landings / effort) %>%
  gather(key = "type", value = "value", effort, landings, LPUE) %>%
  mutate(fleet = ifelse(fleet == "otter" & year > 2016, "otter_new", fleet))

LPUE$type <- factor(LPUE$type)
levels(LPUE$type) <- c("effort [days fished]", "landings [tonnes]", 
                       "LPUE [tonnes/days fished]")
LPUE$fleet <- factor(LPUE$fleet)
levels(LPUE$fleet) <- c("UK beam\ntrawl", "UK otter\ntrawl", 
                        "UK otter\ntrawl\n(new)")

### plot
p <- ggplot(LPUE, aes(x = year, y = value, colour = fleet, linetype = fleet)) +
  geom_line() +
  theme_bw(base_size = 8, base_family = "serif") +
  facet_wrap(~ type, scales = "free_y", ncol = 1) +
  labs(y = "")
if (isTRUE(verbose)) p
ggsave(file = "data/plots_data_LPUE.png", plot = p,
       width = 15, height = 10, units = "cm", dpi = 300, type = "cairo-png")

