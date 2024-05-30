
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
  ages <- ac(plusgroup:dims(number)$max)
  
  ### calculate plusgroup weight (weighted mean)
  res_weight <- quantSums(number[ages,]*weight[ages,])/quantSums(number[ages,])
  
  ### adapt dimensions
  res <- setPlusGroup(weight, plusgroup)
  
  ### enter correct plusgroup weight
  res[ac(plusgroup),] <- res_weight
  
  return(res) 
  
}

### ------------------------------------------------------------------------ ###
### function for setting plusgroup based on sum ####
### ------------------------------------------------------------------------ ###

setPlusGroupSum <- function(number, plusgroup){
  
  ### select ages to be condensed
  ages <- ac(plusgroup:dims(number)$max)
  
  ### calculate plusgroup weight (weighted mean)
  res_number <- quantSums(number[ages,])
  
  ### adapt dimensions
  res <- setPlusGroup(number, plusgroup)
  
  ### enter correct plusgroup weight
  res[ac(plusgroup),] <- res_number
  
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
    theme_bw(base_size = 8) +
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


