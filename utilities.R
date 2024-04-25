### ------------------------------------------------------------------------ ###
### define ggplot style for figures ####
### ------------------------------------------------------------------------ ###
library(ggplot2)
theme_custom <- theme_grey(base_size = 10, base_family = "serif") +
                theme(panel.background = element_rect(fill = NA,
                                                      color = "black"),
                panel.grid = element_blank(),  ### grid lines
                strip.background = element_blank(), ### remove facet lable border
                strip.text = element_text(face = "bold"),
                legend.background = element_blank(), ### legend background
                legend.key = element_blank()
                )

theme_custom2 <- theme_bw(base_size = 10) +
  theme(legend.background = element_blank(), ### legend background
        legend.key = element_blank())

### ------------------------------------------------------------------------ ###
### function for reading FLQuant ####
### ------------------------------------------------------------------------ ###
readFLQuant <- function(input_file) {
  
  ### establish connection and read input
  con  <- file(input_file, open = "r")
  stream <- readLines(con = con, n = 999)
  close(con)
  
  ### correct positions in case InterCatch added additional code...
  add <- ifelse(grepl(x = stream[1], pattern = "^Catch category:"), 1, 0)
  
  ### extract year(s)
  years <- as.numeric(unlist(strsplit(x = stream[3 + add], split = " ")))
  years <- years[1]:years[2]
  
  ### extract age(s)
  ages <- as.numeric(unlist(strsplit(x = stream[4 + add], split = " ")))
  ages <- ages[1]:ages[2]
  
  ### data format identifier
  dfi <- as.numeric(stream[5 + add])
  ### DFIs
  ### 1: array (by ages & years)
  ### 2: row vector (by ages, without year structure)
  ### 3: scalar value
  ### 4: no data expected
  ### 5: column vector (by year, without age structure)
  
  ### remove age structure, if only one value per year provided
  if (dfi %in% c(3, 5)) ages <- "all"
  
  ### check if units provided and extract if possible
  if (grepl(x = stream[6 + add], pattern = "[[:alpha:]]") &
      !grepl(x = stream[6 + add], pattern = "NA", fixed = TRUE)) {
  
    unit <- stream[6 + add]
    unit <- gsub(x = unit, pattern = "in ", replacement = "")
    add <- add + 1
    
  } else {
  
    unit <- "NA"
    
  }
  
  ### extract values
  values <- stream[c(c(6 + add):length(stream))]
  
  ### check for comments after values and ignore
  pos <- which(grepl(x = values, pattern = "[[:lower:]]"))
  
  if (length(pos) > 0) {values[pos] <- ""}
  
  ### remove commas
  values <- gsub(x = values, pattern = ",", replacement = "")
  ### separate values
  values <- unlist(strsplit(x = values, split = "[[:space:]]"))
  #values <- unlist(strsplit(x = values, split = " "))
  ### remove empty cells
  values <- values[values != ""] 
  ### transform into numbers
  values <- as.numeric(values)
  
  ### create FLQuant
  res <- FLQuant(values, dimnames = list(age = ages, year = years, iter = 1),
                 units = unit)
  
  return(res)

}

### ------------------------------------------------------------------------ ###
### function for setting plusgroup based on weighted mean ####
### ------------------------------------------------------------------------ ###

setPlusGroupWeca <- function(number, weight, plusgroup){

  ### select ages to be condensed
  ages <- plusgroup:dims(number)$max

  ### calculate plusgroup weight (weighted mean)
  res_weight <- quantSums(number[ages,]*weight[ages,])/quantSums(number[ages,])
  
  ### adapt dimensions
  res <- setPlusGroup(weight, plusgroup)
  
  ### enter correct plusgroup weight
  res[plusgroup,] <- res_weight
  
  return(res) 

}

### ------------------------------------------------------------------------ ###
### function for setting plusgroup based on sum ####
### ------------------------------------------------------------------------ ###

setPlusGroupSum <- function(number, plusgroup){

  ### select ages to be condensed
  ages <- plusgroup:dims(number)$max

  ### calculate plusgroup weight (weighted mean)
  res_number <- quantSums(number[ages,])
  
  ### adapt dimensions
  res <- setPlusGroup(number, plusgroup)
  
  ### enter correct plusgroup weight
  res[plusgroup,] <- res_number
  
  return(res) 

}

### ------------------------------------------------------------------------ ###
### function for accessing slots/using functions on FLStocks object ####
### ------------------------------------------------------------------------ ###

### returns list of data frames for each stock in FLStocks
calculate_quants <- function(function_name,  ### e.g. fbar, catch...
                             stocks,         ### FLStocks object
                             return_list = FALSE ### return results as list
                             ){
  
  res <-  tapply(1:length(stocks), 1:length(stocks),
                 function(x){
                   cbind(as.data.frame(get(function_name)(stocks[[x]])),
                                       stock = x,
                                       quant = function_name)
                   })
 
 ### remove list structure if desired
 if(!return_list)
   res <- do.call(rbind, res) 
 
 return(res)

}

### ------------------------------------------------------------------------ ###
### function for extracting table 1 & 2 from InterCatch output ####
### ------------------------------------------------------------------------ ###
extract_tables <- function(file_path, overwrite = TRUE){

  if (!isTRUE(overwrite)){
  
    ### check if files already exist
    if (all(file.exists(paste0(path_IC_i, c("table1.txt", "table2.txt"))))){
      
      ### print message
      cat("table1.txt and table2.txt already exist,\n",
          "if you want to update them, use overwrite = TRUE\n")
          
      ### leave function
      return(invisible(NULL))
      
    }
    
  }
  
  ### connection to input file
  con  <- file(paste0(file_path, "CatchAndSampleDataTables.txt"), open = "r")
  
  ### read file
  stream <- readLines(con)
  
  ### close connection
  close(con)
  
  ### print
  cat("successfully read CatchAndSampleDataTables.txt in", file_path, "\n")
  
  ### find positions for table 1 & 2
  tab1_header <- which(grepl(x = stream, pattern = "^TABLE 1."))+4
  tab1_units <- which(grepl(x = stream, pattern = "^TABLE 1."))+5
  tab1_start <- which(grepl(x = stream, pattern = "^TABLE 1."))+7
  tab1_end <- which(grepl(x = stream, pattern = "^TABLE 2."))-3
  tab2_header <- which(grepl(x = stream, pattern = "^TABLE 2."))+4
  tab2_units <- which(grepl(x = stream, pattern = "^TABLE 2."))+5
  tab2_start <- which(grepl(x = stream, pattern = "^TABLE 2."))+7
  tab2_end <- length(stream)
  
  ### write table 1 to file
  con  <- file(paste0(file_path, "table1.txt"), open = "w")
  writeLines(text = stream[c(tab1_header, tab1_start:tab1_end)],con = con)
  close(con)
  cat("successfully created table1.txt\n")
  
  ### and table 2
  con  <- file(paste0(file_path, "table2.txt"), open = "w")
  writeLines(text = stream[c(tab2_header, tab2_start:tab2_end)],con = con)
  close(con)
  cat("successfully created table2.txt\n")
    
}

### ------------------------------------------------------------------------ ###
### Calculate Mohn's Rho ####
### ------------------------------------------------------------------------ ###

mohns_rho <- function(retro, ### FLStocks object (list of FLStock)
                      quant, ### slot e.g. fbar, ssb... has to be a string!
                      n_years = (length(retro) - 1) ### number of years
                      ){

  ### check object type
  if (!is(retro, "FLStocks")) stop("retro has to be of type FLStocks!")

  ### select slot
  quant <- get(as.character(quant), mode = "function")
  
  ### years
  final_yr <- dims(quant(retro[[1]]))$maxyear ### final year
  used_yrs <- seq(to = final_yr - 1, length.out = n_years)
  
  ### extract F values from final year's run
  F_final <- quant(retro[[1]])[, ac(used_yrs)]
                                  
  ### terminal F values from the retro runs
  F_term <- lapply(retro[-1], function(x){ 
    quant(x)[, length(fbar(x))]
  })
  ### convert to single FLStock object
  F_term <- as.FLQuant(as.data.frame(F_term)[,-8])
  
  ### calculate differences per year
  res <- (F_term/F_final) - 1
  
  ### calculate mean over years
  res <- mean(res)

  ### return result
  return(res)
  
}

### ------------------------------------------------------------------------ ###
### run a retro analysis ####
### ------------------------------------------------------------------------ ###

### function for running the analysis
FLXSA_retro <- function(stock,  ### FLStock
                        index,  ### FLIndex/FLIndices
                        years,  ### number of years for retro analysis
                        control ### fwd control object
                        ){
                        
  ### determine years
  retro_years <- range(stock)[["maxyear"]]:c(range(stock)[["maxyear"]]-years+1)
  
  ### do the retro analysis
  res <- tapply(retro_years, 1:length(retro_years), 
                function(x){
                  window(stock, end = x) + FLXSA(stock = window(stock, end=x),
                                                 control = control,
                                                 indices = window(index, end=x))
                })
                
  ### coerce into FLStocks object
  res <- FLStocks(res)
  
  ### name elements
  names(res) <- retro_years
  
  ### return results
  return(res)
                                        
}

### retro, including stf
FLXSA_retro2 <- function(
  stock,  ### FLStock
  index,  ### FLIndex/FLIndices
  years,  ### number of years for retro analysis
  control, ### fwd control object
  int_yr = FALSE, ### do intermediate year forecast
  int_rec = "geomean", ### recruitment model for intermediate year, geomean only
  int_rec_last = 0, ### last year for calculating geometric mean,
                    ### relative to last data year
  int_rec_first = NULL ### first year for geometric mean, relative
){
  
  ### determine years
  retro_years <- range(stock)[["maxyear"]]:c(range(stock)[["maxyear"]]-years)
  
  ### do the retro analysis
  res <- tapply(retro_years, 1:length(retro_years), 
    function(x){
      window(stock, end = x) + FLXSA(stock = window(stock, end = x),
                                     control = control,
                                     indices = window(index, end = x))
  })
  
  ### coerce into FLStocks object
  res <- FLStocks(res)
  
  ### do forecast, if requested
  if (isTRUE(int_yr)) {
    
    res <- lapply(res, function(retro_i) {
      ### extend stock by 1 year
      tmp <- stf(retro_i, 1)
      ### define control object
      ### target 0 fishing, we are only interested in SSB at beginning of year
      ctrl <- fwdControl(data.frame(year = range(retro_i)[["maxyear"]] + 1, 
                                    val = 0,
                                    quantity = "f"))
      ### define recruitment model
      rec_last <- range(retro_i)[["maxyear"]] + int_rec_last
      if (is.null(int_rec_first)) {
        rec_first <- range(retro_i)[["minyear"]]
      } else {
        rec_first <- range(retro_i)[["maxyear"]] + int_rec_first
      }
      rec_yrs <- rec_first:rec_last
      rec_geomean <- exp(mean(log(rec(retro_i)[, ac(rec_yrs)])))
      rec_model <- list(model = "mean",
                        params = FLPar(rec_geomean))
      ### project forward
      tmp_fwd <- fwd(tmp, ctrl = ctrl, sr = rec_model)
      units(tmp_fwd) <- units(retro_i)
      return(tmp_fwd)
    })
    
  }
  
  ### name elements
  names(res) <- retro_years
  
  ### return results
  return(res)
  
}

### ------------------------------------------------------------------------ ###
### expand age dimensions of FLQuant with desired value ####
### ------------------------------------------------------------------------ ###

expand_age <- function(object, ### FLQuant
                       max_age, ### new max age
                       value ### value, e.g. NA or 0
                       ){
                       
  ### check if new age is larger than existing max age
  if(dims(object)$max >= max_age){stop("new max age is not larger!")}
  
  ### expand age, filled with value of max_age
  res <- setPlusGroup(object, max_age)
  
  ### age range added
  new_ages <- c(dims(object)$max+1):max_age
  
  ### fill expanded ages with desired values
  res[ac(new_ages),] <- value
  
  ### replace NAs, if they were inserted by whatever reason
  if(any(is.na(res[ac(dims(object)$min:dims(object)$max), ]))){
    
    res[ac(dims(object)$min:dims(object)$max), ] <- object[ac(dims(object)$min:dims(object)$max), ]
    
  }
  
  ### return result
  return(res)

}

trim_age <- function(object, ### FLQuant
                     max = dims(object)$max, ### new max age
                     min = dims(object)$min, ### new min age
                     plusgroup = FALSE, ### use plusgroup as last age
                     fill = NA ### fill missing values with (e.g. NA or 0)
){
  
  ### check if new age settings differ
  ages <- as.numeric(dimnames(object)$age)
  ### if identical, return unchanged
  if (max == max(ages) & min == min(ages)) return(object)
  
  res <- object
  
  ### new ages
  ages_new <- min:max

  ### expand upper/lower age
  if (min < min(ages) | max > max(ages)) {
    dms_tmp <- dimnames(object)
    if (min < min(ages)) dms_tmp$age <- min:max(as.numeric(dms_tmp$age))
    if (max > max(ages)) dms_tmp$age <- min(as.numeric(dms_tmp$age)):max
    res <- FLQuant(fill, dimnames = dms_tmp)
    res[ac(ages)] <- object
  }
  
  ### remove lower ages
  if (min > min(ages)) res <- res[ac(min:dims(res)$max), ]
  
  ### remove higher ages
  if (max < max(ages)) {
    ### combine ages into plusgroup
    if (isTRUE(plusgroup)) {
      res <- setPlusGroup(res, max)
    ### or remove data
    } else {
      res <- res[ac(dims(res)$min:max)]
    }
  }
  
  ### restore units
  units(res) <- units(object)

  ### return result
  return(res)
  
}


### ------------------------------------------------------------------------ ###
### smooth weights@age (fit polynomial model) ####
### ------------------------------------------------------------------------ ###

fit_polynomial <- function(canum, ### catch numbers at age
                           weca, ### catch weight at age
                           caton, ### catch tonnage
                           fit_age = NULL, ### ages used in the model fitting
                           catch_age = 0.5, ### add +0.5 for catch age
                           stock_age = 0, ###   stock age at beginning of year
                           poly_degree = 2, ### 2nd degree polynomial fit
                           SOP_correction = TRUE, ### apply SOP correction
                           SOP_data_ages = NULL, ### use input catch weight for
                                                ### for ages for SOP corr.
                                                ### not predicted ones
                           plot_diagnostics = FALSE, ### add diagnostics in plot
                           x_pos = 5.5, y_pos = 0.16 ### position for diagnostics
                           ){
                           
  ### create data.frame with values
  ### catch weight is mid-year age (+0.5)
  data_set <- data.frame(age = as.numeric(dimnames(canum)$age) + catch_age, 
                         weight = c(weca), 
                         source = "data")
  ### extract weights and ages
  weights <- data_set$weight
  ages    <- data_set$age
  ### subset ages for model fitting, if required
  if(!is.null(fit_age)){
    weights <- weights[fit_age]
    ages <- ages[fit_age]
  }
  
  ### fit polynomial model
  model <- lm(weights ~ poly(ages, poly_degree, raw = TRUE))
  #summary(model)    
  
  ### predict values for plotting
  age_pred = seq(from = 0, to = max(data_set$age)+1, by = 0.1)
  predicted <- data.frame(age = age_pred,
                          weight = predict(model, data.frame(ages = age_pred)))
                          
  ### predicted catch and stock weights
  data_set <- rbind(data_set,
                    data.frame(age = data_set$age,
                               weight = predict(model, 
                                               data.frame(ages = data_set$age)),
                               source = "catch weight"),
                    data.frame(age = data_set$age-0.5,
                               weight = predict(model, 
                                           data.frame(ages = data_set$age-0.5)),
                               source = "stock weight"))
  
  ### plot
  p <- ggplot() +
       geom_point(data = data_set, aes(x = age, y = weight, size = source, 
                                       colour = source, shape = source))+
       scale_shape_manual(values = c(16, 1, 1)) + 
       scale_size_manual(values = c(2,1,1)) +
       geom_line(data = predicted, aes(x = age, y = weight)) +
       theme_custom2 +
       labs(x = "age [years]", y = "weight [kg]") +
       ylim(0, NA) + xlim(0, NA)
  
  ### add diagnostics
  if(isTRUE(plot_diagnostics)){
     p <- p +
          annotate("text", x = x_pos, y = y_pos, hjust = 0, size = 1.3, 
                   family = "mono",
                   label = paste0(capture.output(summary(model)), sep = "\n",
                                  collapse = ""))
  }    
  #p

  ### SOP correction, if desired
  if(isTRUE(SOP_correction)){
  
    ### create fitted catch weight values
    ages <- as.numeric(dimnames(canum)$age) + catch_age
    fitted_weca <- weca
    fitted_weca[] <- predict(model, data.frame(ages = ages))
    
    ### use input catch weight for last age
    if(!is.null(SOP_data_ages)){
      fitted_weca[SOP_data_ages] <- weca[SOP_data_ages]
    } 
    
    ### calculate SOP
    fitted_SOP <- quantSums(fitted_weca * canum)
    
    ### calculate SOP ratio
    sop_ratio <- caton / fitted_SOP
    
    ### calculate corrected weights for stock and catch
    stock_weights <- catch_weights <- fitted_weca
    stock_weights[] <- predict(model, 
                               data.frame(ages = ages - catch_age + stock_age))*
                               c(sop_ratio)
    catch_weights[] <- predict(model, data.frame(ages = ages)) * c(sop_ratio)
      
    ### use input catch weight for last age as basis
    if(!is.null(SOP_data_ages)){
      catch_weights[SOP_data_ages] <- fitted_weca[SOP_data_ages] * c(sop_ratio)
      ### and use catch weight as stock weight...
      ### does not make particularly sense, but was in earlier years...
      stock_weights[SOP_data_ages] <- catch_weights[SOP_data_ages]
    }   
  }
  
  
  ### object to be returned
  res <- list(model = model, plot = p, 
              catch_weights = catch_weights, stock_weights = stock_weights)
              
  return(res)

}

### ------------------------------------------------------------------------ ###
### plot age structure barplot ####
### ------------------------------------------------------------------------ ###
plot_age_structure <- function(stock, ### FLStock
                           slots = "landings.n", ### slot(s) for plotting
                           years = NA
                           ){
                           
  ### get slot(s) as data frame
  res <- as.data.frame(stock)
  res <- res[res$slot %in% slots, ]
  
  res$age <- as.numeric(as.character(res$age))
  
  ### subset years, if required
  if(isTRUE(!is.na(years))){
    res <- res[res$year %in% c(years), ]
  }
  
  ### y axis label with units
  y_label <- paste0("numbers [", units(slot(stock, slots)), "]")
  
  ### x axis breaks
  x_breaks <- seq(from = range(stock)[["min"]], to = range(stock)[["max"]],
                  by = 2)
  
  ### plot
  p <- ggplot(data = res, aes(x = age, y = data)) +
       geom_bar(stat = "identity", colour = "black", fill = "grey",
                width = 1) +
       labs(x = "age", y = y_label) +
       facet_wrap(~ year) +
       theme(panel.grid = element_blank()) +
       theme_custom2 +
       scale_x_continuous(breaks = x_breaks)
  p

}

### ------------------------------------------------------------------------ ###
### plot weights at age line plot ####
### ------------------------------------------------------------------------ ###
plot_weights <- function(stock, ### FLStock
                         slots = "stock.wt", ### slot(s) for plotting
                         years = NA
                         ){

  ### get slot(s) as data frame
  res <- as.data.frame(stock)
  res <- res[res$slot %in% slots, ]
  
  res$age <- as.factor(res$age)
  res$age <- factor(res$age, levels = rev(unique(res$age)))
  
  ### subset years, if required
  if(!is.na(years)){
    res <- res[res$year %in% c(years), ]
  }
  
  ### modify name for facetting
  res$slot <- sub(x = res$slot, pattern = ".wt", replacement = "")
  
  ### y axis label with units
  y_label <- paste0("weight at age [", units(slot(stock, slots[1])), "]")  
  
  ### plot
  p <- ggplot(data = res, 
              aes(x = year, y = data, colour = age)) +
       geom_line() +
       geom_text(data = res[res$year == max(res$year), ],
                 aes(x = year+1, y = data, label = age),
                 colour = "black", show.legend = FALSE, size = 2) +
       labs(x = "year", y = y_label) +
       facet_wrap(~ slot) +
       theme(panel.grid = element_blank()) +
       theme_custom2
  p
          
}

### comparison of weights of two stocks
plot_weights_cf <- function(stocks, ### FLStocks
                            slots = "stock.wt", ### slot(s) for plotting
                            years = NA
){
  
  ### get slot(s) as data frame
  res <- as.data.frame(stocks)
  res <- res[res$slot %in% slots, ]
  
  res$age <- as.factor(res$age)
  res$age <- factor(res$age, levels = rev(unique(res$age)))
  
  ### subset years, if required
  if (!is.na(years)) {
    res <- res[res$year %in% c(years), ]
  }
  
  ### modify name for facetting
  res$slot <- sub(x = res$slot, pattern = ".wt", replacement = "")
  
  ### y axis label with units
  y_label <- paste0("weight at age [", units(slot(stock, slots[1])), "]")  
  
  ### plot
  p <- ggplot(data = res, 
              aes(x = year, y = data, colour = age, linetype = cname)) +
    geom_line() +
    geom_text(data = res[res$year == max(res$year) & 
                           res$cname == unique(res$cname)[1], ],
              aes(x = year + 1, y = data, label = age),
              colour = "black", show.legend = FALSE, size = 2) +
    labs(x = "year", y = y_label) +
    facet_wrap(~ slot) +
    theme(panel.grid = element_blank()) +
    theme_custom2
  p
  
}

### ------------------------------------------------------------------------ ###
### plot catch curves ####
### ------------------------------------------------------------------------ ###

plot_catch_curve <- function(input, ### FLStock? or FLIndex/FLIndices
                             slots = "landings.n", ### slots if input is FLStock
                             cohort = FALSE, ### track cohorts
                             standardize = FALSE, ### standardize cohorts
                             log = FALSE, ### y-axis on log-scale?
                             rm_ages = FALSE, ### remove some ages?
                             total = FALSE, ### sum over all ages
                             y_label = "", ### axis label
                             wide = FALSE ### plot in wide format
                             ){
                             
  ### extract slots
  ### if input is FLIndices
  if(is(input, "FLIndices")){
  
    res <- lapply(input, index)
  
  ### if input is one index
  } else if(is(input, "FLIndex")){
  
    res <- FLQuants(survey = index(input))
  
  ### if index is FLStock, extract desired slots
  } else if(is(input, "FLStock")){
  
    res <- lapply(slots, function(x){get(x)(input)})
    res <- FLQuants(res)
    names(res) <- slots
  
  }
  
  ### extract cohort structure if requested
  if(isTRUE(cohort)){
  
    res <- lapply(res, function(x){
      as(x, "FLCohort")
    })
  
  }
  
  ### standardize if requested
  if(isTRUE(standardize)){
    res <- lapply(res, function(x){
      sweep(x, 1, apply(x, 1, mean, na.rm = TRUE), "/")
    })
  }
  
  ### coerce into data frame
  res_df <- as.data.frame(res)
  
  ### add total over all ages, if requested
  if(isTRUE(total)){
    
    ### sum up
    res_temp <- lapply(res, quantSums)
    ### convert to data frame
    res_temp <- as.data.frame(res_temp)
    ### add to age data
    res_df <- rbind(res_df, res_temp)
    
  }
  
  if (!isFALSE(rm_ages)) 
    res_df <- res_df[res_df$age != rm_ages, ]
  
  ### standardize names of grouping
  names(res_df)[ncol(res_df)] <- "group"
  #names(res_df)[2] <- "x_axis"
  if (isTRUE(cohort)) {
    res_df$year <- res_df$cohort + as.numeric(as.character(res_df$age))
    res_df$cohort <- as.factor(res_df$cohort)
  }

  ### set order
  res_df$age <- as.factor(res_df$age)
  levels <- rev(sort(as.numeric(as.character(levels(res_df$age)))))
  if(length(levels) < length(unique(res_df$age))){
    levels <- append(setdiff(unique(res_df$age), levels), levels)
  }
  res_df$age <- factor(res_df$age, levels = levels)
  res_df$group <- as.factor(res_df$group)
  res_df$group <- factor(res_df$group, levels = sort(levels(res_df$group)))
  

  
  ### plot
  if (isTRUE(cohort)) {
    p <- ggplot(data = res_df,
                aes(x = year, y = data, colour = cohort))
  } else {
    p <- ggplot(data = res_df,
                aes(x = year, y = data, colour = age))
  }
  p <- p +
       geom_line() +
       geom_text(aes(label = age),
                 colour = "black", show.legend = FALSE, size = 2) +
       
       facet_wrap(~ group, ncol = 1, scales = "free_y") +
       theme_custom2 +
       labs(x = ifelse(isTRUE(cohort), "year class", "year"),
            y = y_label)# +
       #theme(panel.grid = element_blank())
  if(isTRUE(total)){
    p <- p + facet_wrap(~ group + (age == "all"), ncol = 2, scales = "free_y")
  }
  ### plot in wide format, if requested
  if(isTRUE(wide)){
    p <- p + facet_wrap(~ (age == "all") + group, nrow = 2, scales = "free_y")
  }
  ### log scale
  if (isTRUE(log)) p <- p + scale_y_log10()
  p

    
}

### ------------------------------------------------------------------------ ###
### plot stock comparison (FLQuants) ####
### ------------------------------------------------------------------------ ###

plot_stock_comparison <- function(
  stocks, ### FLStocks object
  label = "", ### label for elements
  rem_slots = NA, ### remove slots, e.g. landings
  ncol = NULL, nrow = NULL,
  forecast = FALSE, ### if TRUE, show one year
  ### more for SSB and recruitment
  quants = c("landings", "rec", "fbar", "ssb"),
  refpts = list(Blim = NA, Bpa = NA, MSYBtrigger = NA,
                Flim = NA, Fpa = NA, Fmsy = NA)
) {

  ### set labels
  labels <- quants
  labels[labels == "landings"] <- paste0("Landings [", 
                                         units(landings(stocks[[1]])), "]")
  labels[labels == "catch"] <- paste0("Catch [", 
                                      units(catch(stocks[[1]])), "]")
  labels[labels == "fbar"] <- paste0("F (ages ",  
                                     range(stocks[[1]])[["minfbar"]], " - ",
                                     range(stocks[[1]])[["maxfbar"]], ")")
  labels[labels == "ssb"] <- paste0("SSB [", units(catch(stocks[[1]])), "]")
  labels[labels == "rec"] <- paste0("Recruitment (age ", 
                                    range(stocks[[1]])[["min"]], ")\n[",
                                    units(catch.n(stocks[[1]])), "]")

  ### extract wanted slots and coerce into data frame
  res_df <- lapply(names(stocks), function(name_i) {
    quants_i <- lapply(quants, function(quant_i) {
      get(quant_i, mode = "function")(stocks[[name_i]])
    })
    names(quants_i) <- labels
    quants_i <- FLQuants(quants_i)
    quants_df <- cbind(as.data.frame(quants_i), stock = name_i)
  })
  res_df <- do.call(rbind, res_df)
  
  ### remove slots as requested
  if (!is.na(rem_slots)) {
    res_df <- res_df[!res_df$qname %in% labels[quants %in% rem_slots], ]
  }
  
  ### remove last years values for landings & F, if forecast = TRUE
  if (isTRUE(forecast)) {
    res_df$data[res_df$year == max(res_df$year) &
                  res_df$qname %in% labels[quants %in% c("landings", "fbar",
                                                         "catch")]] <- NA
  }
  
  ### plot
  p <- ggplot(data = res_df,
              aes(x = year, y = data, colour = stock)) +
    geom_line() +
    scale_colour_discrete(label) +
    facet_wrap(~ qname, scale = "free_y", nrow = nrow, ncol = ncol,
               strip.position = "left") +
    labs(x = "year", y = "") +
    scale_y_continuous(limits = c(0, NA)) +
    theme_custom2 +
    theme(strip.placement = "outside",
          strip.background = element_blank())
  
  ### add reference points
  if (isTRUE(any(!is.na(refpts)))) {
    df_refpts <- data.frame(quant = c(rep("ssb", 3), rep("fbar", 3)),
                            refpt = rep(c("limit", "PA", "MSY"), 2),
                            value = c(refpts$Blim, refpts$Bpa, 
                                      refpts$MSYBtrigger,
                                      refpts$Flim, refpts$Fpa, refpts$Fmsy))
    df_refpts <- merge(df_refpts, data.frame(qname = labels, quant = quants))
    p <- p + geom_hline(data = df_refpts, aes(yintercept = value, linetype = refpt),
                   alpha = 0.5)
    
  }
  p
  
}

### ------------------------------------------------------------------------ ###
### plot index residuals ####
### ------------------------------------------------------------------------ ###

plot_index_residuals <- function(xsa_, index_names){

  ### add correct names for surveys
  names(index.res(xsa_)) <- index_names
  
  ### extract residuals
  idx_residuals <- as.data.frame(index.res(xsa_))
  
  ### remove NAs
  idx_residuals <- idx_residuals[!is.na(idx_residuals$data),]
  
  ### duplicate entries for easier plotting
  idx_residuals <- rbind(cbind(idx_residuals, blubb = 1),
                         cbind(idx_residuals, blubb = 2))
  
  ### mark positive and negative residuals
  idx_residuals <- cbind(idx_residuals, 
                         sign = sapply(idx_residuals$data, FUN = function(x){
                           if(x>0) return("positive") else return("negative")}))
  
  ### reverse order of ages                           
  idx_residuals$age <- factor(idx_residuals$age, 
                             levels = rev(levels(as.factor(idx_residuals$age))))
  
  ### bubble plot
  p <- ggplot(data = idx_residuals, aes(x = year, y = age, size = abs(data), 
                                        fill = sign)) +
      geom_point(shape = 21, colour = "black") +
      scale_fill_manual(name = "", values = c("white","grey")) +
      #p <- p + scale_size_continuous(name = "log(residuals)")
      scale_size_continuous("log residuals", breaks = c(0.01,0.1,0.5,1,2,3,4),
                            guide = guide_legend(order = 1),
                            range = c(0.1, 5)) +
      #scale_x_continuous(breaks = seq(2003,2015,2)) +
      #p <- p + scale_y_discrete(limits = rev(levels(idx_residuals$age)))
      facet_wrap(facets = ~qname, nrow = 1) +
      labs(x = "", y = "age") +
      theme_bw(base_size = 9) +
      theme(legend.key = element_blank(),
            panel.grid = element_blank(),
            #axis.text.x = element_text(angle = 30, vjust = 0.5, hjust = 0.5),
            #axis.text.x = element_blank(),
            #axis.ticks.x = element_blank(),
            plot.margin = unit(c(0,0,0,0.2),"cm"),
            legend.key.size = unit(10,"pt")) +
      theme_custom2
  p
                    
  ### lines
  q <- ggplot(data = idx_residuals, 
              aes(x = year, y = data, linetype = age, shape = age, 
                  colour = age)) +
      geom_point(size = 0.3) + geom_line(size = 0.3) +
      #scale_x_continuous(breaks = seq(2003,2015,2)) +
      facet_wrap(facets = ~qname, nrow = 1) +
      labs(x = "year", y = "log residuals") +
      theme_bw(base_size = 9) +
      theme(legend.key = element_blank(),
            panel.grid = element_blank(),
            #axis.text.x = element_text(angle = 30, vjust = 0.5, hjust = 0.5),
            plot.margin = unit(c(0,1.5,0.1,0),"cm"),
            legend.key.size = unit(10,"pt")) +
    theme_custom2
  q

  ### combine both plots
  pq <- plot_grid(p, q, ncol = 1, align = "vh")
  
  return(pq)

}

### ------------------------------------------------------------------------ ###
### plot weights of indices from XSA diagnostics ####
### ------------------------------------------------------------------------ ###

plot_xsa_weights <- function(object, ### FLXSA or list of FLXSA
                             n_year_classes = NA ### number of year classes 
                             ){

  ### coerce into list, if input object is FLXSA only
  if(is(xsa_input, "FLXSA")){
    object <- list(object)
    names(object) <- range(object)[["maxyear"]]+1
  }

  res <- lapply(seq_along(object), function(x){
    
    ### extract diagnostics
    diags <- object[[x]]@diagnostics
    
    ### sum w per year class
    diags_sums_yrcls <- aggregate(w ~ yrcls, data = diags, FUN = sum)
    names(diags_sums_yrcls)[2] <- "w_yrcls"
    ### sum w per year class and survey
    diags_sums_srvy <- aggregate(w ~ yrcls + source, data = diags, FUN = sum)
    names(diags_sums_srvy)[3] <- "w_yrcls_srvy"
    ### merge w per year class with w per years class and survey
    diags_merged <- merge(diags_sums_yrcls, diags_sums_srvy, all = TRUE)
    ### relative w per year class
    diags_merged$w_rel <- diags_merged$w_yrcls_srvy / diags_merged$w_yrcls
    
    ### add name
    diags_merged$assessment_year <- names(object)[x]
    
    ### subset to requested year range
    if(!is.na(n_year_classes)){
      diags_merged <- subset(diags_merged, yrcls %in% 
                             tail(unique(diags_merged$yrcls), n_year_classes))
    }
    
    ### return
    return(diags_merged)
    
  })
  res <- do.call(rbind, res)
  
  ### sort source factor levels
  res$source <- as.factor(res$source)
  res$source <- factor(res$source,
                       levels = rev(levels(res$source)))
                           
  p <- ggplot(data = res,
              aes(x = yrcls, y = w_rel, fill = source)) +
       geom_bar(stat = "identity") +
       facet_wrap(~ assessment_year, ncol = 1) +
       labs(x = "year class", y = "weight") +
       theme_custom2
  p
 
}


### ------------------------------------------------------------------------ ###
### survey index internal consistency
### ------------------------------------------------------------------------ ###
### plot correlation of age vs age+1 next year
idx_cor <- function(idx, ### FLIndex or FLIndices
                    nrow = NULL, ncol = NULL ### rows/columns for facetting
                    ) {

  ### "loop" through surveys
  res <- foreach(idx_tmp = FLIndices(idx), index_name = names(idx), 
    .final = function(x) { 
      ### combine
      res_data <- do.call(rbind, lapply(x, function(y) y$res_data))
      res_cor <- do.call(rbind, lapply(x, function(y) y$res_cor))
      ### sort
      levels <- unique(res_data$ages)
      levels <- levels[order(as.numeric(unlist(lapply(strsplit(levels, " "), 
                                                      "[[", 2))))]
      res_data$ages <- factor(as.character(res_data$ages), levels = levels)
      res_cor$ages <- factor(as.character(res_cor$ages), levels = levels)
      return(list(res_data = res_data, res_cor = res_cor))
     
    }) %do% {
    
    ### select pairs of ages
    age_pairs <- lapply((dims(idx_tmp)$min:(dims(idx_tmp)$max - 1)), 
                        function(x) {
      seq(from = x, length.out = 2)
    })
    
    ### create cohorts
    idx_cohort <- FLCohort(index(idx_tmp))
    
    ### go through age pairs
    res_data <- lapply(age_pairs, function(ages) {
      tmp <- data.frame(age1 = as.numeric(idx_cohort[ac(ages[1]), ]),
                        age2 = as.numeric(idx_cohort[ac(ages[2]), ]),
                        age1_log = log(as.numeric(idx_cohort[ac(ages[1]), ])),
                        age2_log = log(as.numeric(idx_cohort[ac(ages[2]), ])),
                        ages = paste("age ", ages, sep = "", collapse = " vs. "),
                        index = index_name)
      tmp[!is.na(tmp$age1) & !is.na(tmp$age2) &
            is.finite(tmp$age1_log) & is.finite(tmp$age2_log), ]
    })
    res_data <- do.call(rbind, res_data)
    
    ### correlation
    res_cor <- lapply(split(res_data, res_data$ages), function(x) {
      ### test correlation
      model <- cor.test(x = x$age1, y = x$age2)
      model_log <- cor.test(x = x$age1_log, y = x$age2_log)
      ### return results
      data.frame(ages = as.character(x$ages[1]),
                 age1_min = min(x$age1, na.rm = TRUE),
                 age1_max = max(x$age1, na.rm = TRUE),
                 age2_min = min(x$age2, na.rm = TRUE),
                 age2_max = max(x$age2, na.rm = TRUE),
                 age1_min_log = min(x$age1_log, na.rm = TRUE),
                 age1_max_log = max(x$age1_log, na.rm = TRUE),
                 age2_min_log = min(x$age2_log, na.rm = TRUE),
                 age2_max_log = max(x$age2_log, na.rm = TRUE),
                 cor = c(model$estimate),
                 p = model$p.value,
                 cor_log = c(model_log$estimate),
                 p_log = model_log$p.value,
                 index = index_name)
    })
    res_cor <- do.call(rbind, res_cor)
    
    ### return data and correlation test results
    return(list(res_data = res_data, res_cor = res_cor))
    
  }
  
  ### plot
  ggplot() +
    geom_point(data = res$res_data, aes(x = age1_log, y = age2_log),
               size = 0.5) +
    geom_text(data = res$res_cor,
              aes(x = -Inf,
                  y = Inf, hjust = -0.10, vjust = 1.5,
                  label = paste0("italic(rho)==", round(cor, 3),
                                 "~~~italic(p)==", round(p, 3), "")), 
              parse = TRUE, size = 2) +
    facet_wrap(~ index + ages, scales = "free", nrow = nrow, ncol = ncol) +
    geom_smooth(data = res$res_data, size = 0.5, 
                aes(x = age1_log, y = age2_log), method = "lm") +
    labs(x = expression(log(numbers["age=a,year=y"])),
         y = expression(log(numbers["age=a+1,year=y+1"]))) + 
    theme_custom2 +
    theme(strip.text = element_text(size = 6),
          strip.text.x = element_text(margin = margin(t = 2, b = 2)))
    

}


### ------------------------------------------------------------------------ ###
### update VPA input files with FLQuant object ####
### ------------------------------------------------------------------------ ###

update_vpa <- function(file, ### file path of VPA file to be updated
                       update, ### FLQuant object with updates
                       year_range_only = FALSE ### update only year range,
                                               ### don't touch data
                       ) {
  
  ### load data
  object <- readLines(con = file)
  
  ### years in VPA file
  yrs <- as.numeric(unlist(strsplit(object[3], " ")))
  
  ### years to update
  yrs_update <- as.numeric(dimnames(update)$year)
  
  ### years to add to file
  yrs_add <- setdiff(yrs_update, yrs[1]:yrs[2])
  
  ### update year range in object
  object[3] <- paste(min(c(yrs, yrs_add)), max(c(yrs, yrs_add)))
  
  ### proceed and update data, if required
  if (!isTRUE(year_range_only)) {
  
    ### positions with data
    data_pos <- seq(from = 6, length.out = yrs[2] - yrs[1] + 1)
    
    ### ages, if age structure required
    if (object[5] == "1") {
      
      ### ages in VPA file
      ages <- as.numeric(unlist(strsplit(object[4], " ")))
      
      ### ages in update object
      ages_update <- c(dims(update)$min, dims(update)$max)
      
      ### keep only ages that exist in VPA file
      update <- update[ac(ages[1]:ages[2]), ]
      
    }
    
    ### add elements for years which need to be added
    if (length(yrs_add) > 0) {
      
      object <- c(object[c(1:tail(data_pos, 1))], rep("", length(yrs_add)),
                  object[c((max(data_pos) + 1):length(object))])
      
    }
    
    ### positions to update
    pos <- yrs_update - yrs[1] + 6
    
    ### update/add values
    object[pos] <- apply(update@.Data, 2, function(x) {
      paste0(x, collapse = "\t")
    })
    
  }
  
  ### write to file
  writeLines(object, con = file)
  
}

### ------------------------------------------------------------------------ ###
### icesTAF plotting ####
### ------------------------------------------------------------------------ ###
### modifed from https://github.com/ices-tools-prod/icesSAG/blob/a80b86583582019746dac8fa6b5f95c39662b776/R/utilities.R#L198-L218
plot.ices_standardgraph_list2 <- function(x, y = NULL, ..., r, c) {
  grid::grid.newpage()
  x_loc <- rep((1:r) / r - 1 / (2 * r), c)
  y_loc <- rep((c:1) / c  - 1 / (2 * c), each = r)
  for (i in seq_along(x)) {
    if (!is.null(x[[i]]))
      grid::grid.raster(x[[i]], x = x_loc[i], y = y_loc[i], width = 1 / r, height = 1 / c)
  }
}
### ------------------------------------------------------------------------ ###
### SPiCT: extract time series ####
### ------------------------------------------------------------------------ ###

extrct_tmsrs <- function(model, ### SPiCT object
                         get_B = TRUE,     ### biomass
                         get_F = TRUE,     ### fishing mortality
                         get_BBmsy = TRUE,  ### B / Bmsy
                         get_FFmsty = TRUE ### F / Fmsy
){
  
  ### extract values
  res <- as.data.frame(rbind(
    cbind(get.par("logB", model, exp = TRUE), quant = "B"),
    cbind(get.par("logBBmsy", model, exp = TRUE), quant = "B/B[MSY]"),
    cbind(get.par("logF", model, exp = TRUE), quant = "F"),
    cbind(get.par("logFFmsy", model, exp = TRUE), quant = "F/F[MSY]")
  ))
  ### year stored in row names
  res$year <- row.names(res)
  res$year <- gsub(x = res$year, pattern = "X", replacement = "")
  res$year <- gsub(x = res$year, pattern = "\\.[0-9]$", replacement = "")
  res$year <- as.numeric(res$year)
  res$ll <- as.numeric(as.character(res$ll))
  res$est <- as.numeric(as.character(res$est))
  res$ul <- as.numeric(as.character(res$ul))
  res$sd <- as.numeric(as.character(res$sd))
  res$cv <- as.numeric(as.character(res$cv))
  row.names(res) <- NULL
  
  ### return
  res
  
}

### wrapper for list
extrct_tmsrs_lst <- function(models, ### list of SPiCT objects
                             get_B = TRUE,     ### biomass
                             get_F = TRUE,     ### fishing mortality
                             get_BBmsy = TRUE,  ### B / Bmsy
                             get_FFmsty = TRUE ### F / Fmsy
){
  
  ### check if list is result from retro analysis
  if (is(models, "spictcls") & is(models$retro, "list")) {
    
    ### get final input year
    last_year <- models$inp$timeC[length(models$inp$timeC)]
    
    ### create list with last years assessment first
    res <- list(models)
    ### add retro runs
    res[2:c(length(models$retro) + 1)] <- models$retro
    
    ### set names
    names(res) <- rev(as.character(seq(to = last_year, 
                                       length.out = length(res))))
    
    models <- res
    
  }
  
  ### extract timeseries
  res <- lapply(seq_along(models), function(x){
    
    ### extract values
    res_temp <- extrct_tmsrs(models[[x]])
    ### set names
    res_temp$model <- names(models)[x]
    ### return
    res_temp
    
  })
  
  ### combine
  res <- do.call(rbind, res)
  
  ### return
  return(res)
  
}

### ------------------------------------------------------------------------ ###
### plot XSA catchability ####
### ------------------------------------------------------------------------ ###

XSA_plot_q <- function(xsa) {
  qs <- xsa@q.hat
  qs <- as.data.frame(qs)
  qs$q_est <- "estimated"
  qs$q_est[qs$age <= xsa@control@rage] <- "two parameter model"
  qs$q_est[qs$age >= xsa@control@qage] <- "plateau"
  p <- ggplot(data = qs, aes(x = age, y = data)) +
    geom_line() +
    geom_point(aes(colour = q_est)) +
    facet_wrap(~ qname, scale = "free") +
    theme_custom2 +
    labs(x = "age", y = "catchability") +
    scale_colour_discrete("catchability\nestimation") +
    ylim(0, NA)
  return(p)
}


