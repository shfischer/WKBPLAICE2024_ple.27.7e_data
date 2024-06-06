### ------------------------------------------------------------------------ ###
### Maturity data from Q1SWBeam survey ####
### ------------------------------------------------------------------------ ###

library(icesTAF)
library(tidyr)
library(dplyr)
library(tibble)
library(stringr)
library(ggplot2)
library(lme4)
library(slider)
library(FLCore)

mkdir("data/maturity")
mkdir("data/maturity/plots")

if (!exists("verbose")) verbose <- FALSE

### ------------------------------------------------------------------------ ###
### data extract ####
### ------------------------------------------------------------------------ ###

mat <- read.csv("boot/initial/data/maturity/Cefas_maturity.csv")
table(mat$fldMainSpeciesCode)
mat_ <- mat %>% 
  filter(fldMainSpeciesCode == "PLE") %>%
  filter(!is.na(fldFishMaturity) & fldFishMaturity != "")
### keep only Q1SWBeam data - CEND XX/YY
mat_ <- mat_ %>%
  filter(grepl(x = fldCruiseName, pattern = "^CEND"))
mat_ <- mat_ %>%
  mutate(year = gsub(x = fldCruiseName, pattern = "\\D", replacement = "")) %>%
  mutate(year = str_sub(year, -2, -1)) %>%
  mutate(year = as.numeric(year) + 2000)

table(mat$fldCruiseName)
table(mat_$year)
#table(mat_$fldCruiseName[mat_$year == 2023])
table(mat_$fldCruiseName, mat_$year)
table(mat_$fldFishMaturity)
table(mat_$fldCruiseName, mat_$fldFishMaturity)
table(mat_$year, mat_$fldFishMaturity)

### cleaning
summary(mat_)
table(mat_$fldFishLength)
mat_ <- mat_ %>%
  filter(fldFishLength < 1000)

### maturity - immature or mature
mat_ <- mat_ %>% 
  mutate(maturity = case_when(
    fldFishMaturity %in% c(1, "I") ~ "immature",
    fldFishMaturity %in% c(2, 3, 4, 5, "M", "H", "R", "S") ~ "mature",
    fldFishMaturity %in% c(6, "U") ~ "unknown",
    .default = "unknown"
  ))
table(mat_$maturity)
### remove unknown/abnormal/NA
mat_ <- mat_%>%
  filter(maturity %in% c("immature", "mature"))
### proportion mature
mat_smry <- mat_ %>%
  select(year, length = fldFishLength, maturity) %>%
  mutate(tmp = 1) %>%
  group_by(year, length, maturity) %>%
  summarise(count = sum(tmp, na.rm = TRUE)) %>%
  mutate(count = ifelse(!is.na(count), count, 0)) %>%
  pivot_wider(names_from = maturity, values_from = count) %>%
  mutate(immature = ifelse(!is.na(immature), immature, 0),
         mature = ifelse(!is.na(mature), mature, 0)) %>%
  mutate(total = immature + mature) %>%
  mutate(prop_mature = mature/total) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(prop_length = total/sum(total))

###

### plot
p <- mat_smry %>%
  ggplot(aes(x = length/10, y = prop_mature, alpha = prop_length)) +
  geom_point(size = 0.4) +
  facet_wrap(~ year) +
  scale_alpha("Proportion of\nmeasured\nindividuals") +
  labs(x = "Length (cm)", y = "Proportion mature") +
  xlim(c(0, 60)) + 
  theme_bw(base_size = 8) +
  theme(legend.key.height = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave("data/maturity/plots/Q1SWBeam_maturity_prop.png", 
       width = 15, height = 10, units = "cm", dpi = 300, plot = p)

mat_ %>%
  filter(year == 2021 & fldFishLength <= 170)


### ------------------------------------------------------------------------ ###
### fit model ####
### ------------------------------------------------------------------------ ###
### logistic regression

glm_2023 <- glm(prop_mature ~ length, data = mat_smry %>% filter(year == 2023), 
                family = binomial(link = "logit"), weights = total)
### length at 50% maturity
lrPerc <- function(cf, p) (log(p/(1-p))-cf[[1]])/cf[[2]]
lrPerc(coef(glm_2023), 0.5)

### for all years
range(mat_smry$year)

yrs <- unique(mat_smry$year)
names(yrs) <- yrs
glms <- lapply(yrs, function(yr) {
  glm(prop_mature ~ length, data = mat_smry %>% filter(year == yr), 
      family = binomial(link = "logit"), weights = total)
})
L50_all <- sapply(glms, function(x) {
  lrPerc(coef(x), 0.5)
})
L50_all <- data.frame(year = as.numeric(names(L50_all)), L50 = L50_all)

### predict values for all years
lngths_pred <- 0:600
pred <- lapply(yrs, function(yr) {#browser()
  data.frame(length = lngths_pred,
             prop_mature = predict(glms[[as.character(yr)]], 
                                   data.frame(length = lngths_pred), 
                                   type = "response"),
             year = yr)
})
pred <- do.call(rbind, pred)

### add fit to plot
### plot
p <-  ggplot() +
  geom_point(data = mat_smry,
             mapping = aes(x = length/10, y = prop_mature, alpha = prop_length),
             size = 0.4) +
  geom_line(data = pred,
            mapping = aes(x = length/10, y = prop_mature),
            colour = "red", linewidth = 0.3) +
  facet_wrap(~ year) +
  scale_alpha("Proportion of\nmeasured\nindividuals") +
  xlim(c(0, 60)) + 
  labs(x = "Length (cm)", y = "Proportion mature") +
  theme_bw(base_size = 8) +
  theme(legend.key.height = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave("data/maturity/plots/Q1SWBeam_maturity_prop_fit.png", 
       width = 15, height = 10, units = "cm", dpi = 300, plot = p)

### plot L50 time series
p <- L50_all %>%
  ggplot(aes(x = year, y = L50/10)) +
  geom_line() +
  labs(x = "Year", y = "L50 (cm)") +
  ylim(c(0, NA)) +
  theme_bw(base_size = 8)
if (isTRUE(verbose)) p
ggsave("data/maturity/plots/Q1SWBeam_fit_L50.png", 
       width = 10, height = 6, units = "cm", dpi = 300, plot = p)

### ------------------------------------------------------------------------ ###
### data - by sex ####
### ------------------------------------------------------------------------ ###

### proportion mature
mat_smry_sex <- mat_ %>%
  select(year, sex = fldFishSex, length = fldFishLength, maturity) %>%
  mutate(tmp = 1) %>%
  group_by(year, sex, length, maturity) %>%
  summarise(count = sum(tmp, na.rm = TRUE)) %>%
  mutate(count = ifelse(!is.na(count), count, 0)) %>%
  pivot_wider(names_from = maturity, values_from = count) %>%
  mutate(immature = ifelse(!is.na(immature), immature, 0),
         mature = ifelse(!is.na(mature), mature, 0)) %>%
  mutate(total = immature + mature) %>%
  mutate(prop_mature = mature/total) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(prop_length = total/sum(total))

### plot
p <- mat_smry_sex %>%
  ggplot(aes(x = length/10, y = prop_mature, alpha = prop_length, 
             colour = sex)) +
  geom_point(size = 0.4) +
  facet_wrap(~ year) +
  scale_alpha("Proportion of\nmeasured\nindividuals") +
  scale_colour_discrete("Sex") + 
  labs(x = "Length (cm)", y = "Proportion mature") +
  xlim(c(0, 60)) + 
  theme_bw(base_size = 8) +
  theme(legend.key.height = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave("data/maturity/plots/Q1SWBeam_maturity_prop_sex.png", 
       width = 15, height = 10, units = "cm", dpi = 300, plot = p)

### fit model by sex
glm_F <- lapply(yrs, function(yr) {
  glm(prop_mature ~ length, 
      data = mat_smry_sex %>% 
        filter(year == yr & sex == "F"), 
      family = binomial(link = "logit"), weights = total)
})
L50_F <- sapply(glm_F, function(x) {
  lrPerc(coef(x), 0.5)
})
glm_M <- lapply(yrs, function(yr) {
  glm(prop_mature ~ length, 
      data = mat_smry_sex %>% 
        filter(year == yr & sex == "M"), 
      family = binomial(link = "logit"), weights = total)
})
L50_M <- sapply(glm_M, function(x) {
  lrPerc(coef(x), 0.5)
})

### predict values for all years by sex
lngths_pred <- 0:600
pred_F <- lapply(yrs, function(yr) {#browser()
  data.frame(length = lngths_pred,
             prop_mature = predict(glm_F[[as.character(yr)]], 
                                   data.frame(length = lngths_pred), 
                                   type = "response"),
             year = yr,
             sex = "F")
})
pred_F <- do.call(rbind, pred_F)
pred_M <- lapply(yrs, function(yr) {#browser()
  data.frame(length = lngths_pred,
             prop_mature = predict(glm_M[[as.character(yr)]], 
                                   data.frame(length = lngths_pred), 
                                   type = "response"),
             year = yr,
             sex = "M")
})
pred_M <- do.call(rbind, pred_M)
### combine all/F/M
pred_all <- bind_rows(pred %>% mutate(sex = "all"), pred_F, pred_M)

### add fit to plot
### plot
p <-  ggplot() +
  geom_point(data = mat_smry_sex,
             mapping = aes(x = length/10, y = prop_mature, alpha = prop_length,
                           colour = sex),
             size = 0.4) +
  geom_line(data = pred_all,
            mapping = aes(x = length/10, y = prop_mature, colour = sex,
                          linetype = sex),
            linewidth = 0.3) +
  facet_wrap(~ year) +
  scale_alpha("Proportion of\nmeasured\nindividuals") +
  scale_colour_manual("Sex", 
                      values = c("F" = "#F8766D", "M" = "#00BFC4", 
                                 "all" = "grey")) +
  scale_linetype_manual("Sex", values = c("F" = "solid", "M" = "solid",
                                          "all" = "2121")) +
  xlim(c(0, 60)) + 
  labs(x = "Length (cm)", y = "Proportion mature") +
  theme_bw(base_size = 8) +
  theme(legend.key.height = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave("data/maturity/plots/Q1SWBeam_maturity_prop_fit_sex.png", 
       width = 15, height = 10, units = "cm", dpi = 300, plot = p)

### L50 by sex
L50_combined <- L50_all %>%
  rename(all = L50) %>%
  mutate("F" = c(L50_F), "M" = c(L50_M)) %>%
  pivot_longer(-year)
### plot
p <- L50_combined %>%
  ggplot(aes(x = year, y = value/10, colour = name, linetype = name)) +
  geom_line() +
  labs(x = "Year", y = "L50 (cm)") +
  scale_colour_manual("Sex", 
                      values = c("F" = "#F8766D", "M" = "#00BFC4", 
                                 "all" = "grey")) +
  scale_linetype_manual("Sex", values = c("F" = "solid", "M" = "solid",
                                          "all" = "2121")) +
  ylim(c(0, NA)) +
  theme_bw(base_size = 8) +
  theme(legend.key.height = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave("data/maturity/plots/Q1SWBeam_fit_L50_sex.png", 
       width = 10, height = 6, units = "cm", dpi = 300, plot = p)

### ------------------------------------------------------------------------ ###
### fit model - year as random effect ####
### ------------------------------------------------------------------------ ###
### logistic regression

glm_ <- glmer(prop_mature ~ length + (1 | year), 
              data = mat_smry_sex %>% filter(sex == "F") %>% mutate(length = length/10),
              family = binomial(link = "logit"), weights = total)
plot(glm_)
summary(glm_)

pred_df <- expand.grid(length = lngths_pred/10, year = as.vector(yrs))
pred_year <- predict(glm_,pred_df, type = "response")
pred_df$prop_mature <- pred_year

p <-  ggplot() +
  geom_point(data = mat_smry_sex %>% filter(sex == "F"),
             mapping = aes(x = length/10, y = prop_mature, alpha = prop_length,
                           colour = sex),
             size = 0.4) +
  geom_line(data = pred_df,
            mapping = aes(x = length, y = prop_mature, sex = "F"),
            linewidth = 0.3) +
  facet_wrap(~ year) +
  scale_alpha("Proportion of\nmeasured\nindividuals") +
  scale_colour_manual("Sex", 
                      values = c("F" = "#F8766D")) +
  xlim(c(0, 60)) + 
  labs(x = "Length (cm)", y = "Proportion mature") +
  theme_bw(base_size = 8) +
  theme(legend.key.height = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave("data/maturity/plots/Q1SWBeam_maturity_prop_fit_F_year.png", 
       width = 15, height = 10, units = "cm", dpi = 300, plot = p)

### L50
### find manually...
p <- pred_df %>%
  mutate(diff = abs(prop_mature - 0.5)) %>%
  group_by(year) %>%
  summarise(length = length[diff == min(diff)]) %>%
  ggplot(aes(x = year, y = length)) +
  geom_line() +
  labs(x = "Year", y = "L50 (cm)") +
  ylim(c(0, NA)) +
  theme_bw(base_size = 8)
if (isTRUE(verbose)) p
ggsave("data/maturity/plots/Q1SWBeam_fit_L50_F_year.png", 
       width = 10, height = 6, units = "cm", dpi = 300, plot = p)

### ------------------------------------------------------------------------ ###
### convert to maturity at age - apply Q1SWBeam ALKs ####
### ------------------------------------------------------------------------ ###

### get ALK for females from DATRAS
if (isTRUE(verbose)) {
  Q1 <- lapply(2006:2023, getCAdata, survey = "BTS", quarter = 1)
  Q1 <- lapply(Q1, function(x) {
    x$LngtCode <- as.character(x$LngtCode)
    return(x)
  })
  Q1 <- do.call(bind_rows, Q1)
  ### select plaice
  ### keep only females
  Q1_ALK_F <- Q1 %>%
    filter(Survey == "BTS" &
             Ship == "74E9" & ### Cefas Endeavour - name changed in 2023...
             Quarter == 1 &
             Country == "GB" &
             SpecCode == 127143 & ### plaice
             !is.na(Age) &
             Sex == "F"
    ) %>%
    select(Year, Age, LngtClass, CANoAtLngt) %>%
    group_by(Year, Age, LngtClass) %>%
    summarise(CANoAtLngt = sum(CANoAtLngt)) %>%
    select(year = Year, age = Age, length = LngtClass, count = CANoAtLngt)
  saveRDS(object = Q1_ALK_F, 
          file = "boot/initial/data/ALK/Q1SWBeam_ple_ALK_F.rds")
}
Q1_ALK_F <- readRDS("boot/initial/data/ALK/Q1SWBeam_ple_ALK_F.rds")

### prepare predicted maturity
pred_df_join <- pred_df %>%
  filter(length %% 1 == 0)

### merge with maturity at length with ALK
mat_age <- pred_df_join %>% 
  inner_join(Q1_ALK_F, by = c("length", "year")) %>%
  group_by(year, age) %>%
  select(-length) %>% 
  ### maturity: average at year, weighted by individuals at age
  summarise(prop_mature = weighted.mean(x = prop_mature, w = count))
### average
mat_age_mean <- mat_age %>%
  group_by(age) %>%
  summarise(prop_mature = mean(prop_mature, na.rm = TRUE)) %>%
  mutate(year = "average")

### plot by year
p <- bind_rows(mat_age %>% 
                 mutate(year = as.character(year)), 
               mat_age_mean) %>%
  ggplot(aes(x = age, y = prop_mature)) +
  geom_line() +
  facet_wrap(~ year) +
  labs(x = "Age (years)", y = "Proportion mature") +
  coord_cartesian(xlim = c(0, 15)) +
  theme_bw(base_size = 8)
if (isTRUE(verbose)) p
ggsave("data/maturity/plots/maturity_age.png", 
       width = 15, height = 10, units = "cm", dpi = 300, plot = p)

### plot time series
cols <- scales::hue_pal()(length(0:10))
cols <- cols[c(seq(from = 1, to = length(cols), by = 3),
               seq(from = 2, to = length(cols), by = 3),
               seq(from = 3, to = length(cols), by = 3))]
p <- mat_age %>%
  filter(age <= 10) %>%
  mutate(age = factor(age, levels = 10:0)) %>%
  ggplot(aes(x = year, y = prop_mature, colour = age)) +
  geom_line() + geom_point(size = 0.3) +
  geom_hline(data = mat_age_mean %>%
               filter(age <= 10) %>%
               mutate(age = factor(age, levels = 10:0)),
             aes(yintercept = prop_mature, colour = age),
             linetype = "1212", alpha = 0.5) +
  scale_colour_manual("Age (years)", values = cols) +
  labs(x = "Year", y = "Proportion mature") +
  #coord_cartesian(xlim = c(0, 15)) +
  theme_bw(base_size = 8) +
  theme(legend.key.height = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave("data/maturity/plots/maturity_age_timeseries.png", 
       width = 15, height = 10, units = "cm", dpi = 300, plot = p)

### ------------------------------------------------------------------------ ###
### convert to maturity at age - apply vB model fit ####
### ------------------------------------------------------------------------ ###

### define ages
ages <- 0:15
### estimate length at age
### von Bertalanffy growth function
vB_length <- function(L_inf, k, t, t_0 = 0){
  L_inf * (1 - exp(-k * (t - t_0)))
}
### get von Bertalanffy growth parameters
vB_pars <- read.csv("data/ALKs/vB_pars_q.csv")
### use values estimated with last five years of data
vB_Linf <- vB_pars$Linf[vB_pars$year == "all"]
vB_k <- vB_pars$k[vB_pars$year == "all"]
vB_t0 <- vB_pars$t0[vB_pars$year == "all"]

### lengths corresponding to ages
lengths <- vB_length(L_inf = vB_Linf, k = vB_k, t_0 = vB_t0, t = ages)

### predict maturity at these lengths
pred_ALK <- expand.grid(length = lengths, year = as.vector(yrs))
pred_ALK_values <- predict(glm_, pred_ALK, type = "response")
pred_ALK$prop_mature <- pred_ALK_values
pred_ALK$age <- ages

### average
pred_ALK_mean <- pred_ALK %>%
  group_by(age) %>%
  summarise(prop_mature = mean(prop_mature, na.rm = TRUE)) %>%
  mutate(year = "average")

### plot by year
p <- bind_rows(pred_ALK %>% 
                 mutate(year = as.character(year)), 
               pred_ALK_mean) %>%
  ggplot(aes(x = age, y = prop_mature)) +
  geom_line() +
  facet_wrap(~ year) +
  labs(x = "Age (years)", y = "Proportion mature") +
  coord_cartesian(xlim = c(0, 15)) +
  theme_bw(base_size = 8)
if (isTRUE(verbose)) p
ggsave("data/maturity/plots/maturity_vB_age.png", 
       width = 15, height = 10, units = "cm", dpi = 300, plot = p)

### plot time series
cols <- scales::hue_pal()(length(0:10))
cols <- cols[c(seq(from = 1, to = length(cols), by = 3),
               seq(from = 2, to = length(cols), by = 3),
               seq(from = 3, to = length(cols), by = 3))]
p <- pred_ALK %>%
  filter(age <= 10) %>%
  mutate(age = factor(age, levels = 10:0)) %>%
  ggplot(aes(x = year, y = prop_mature, colour = age)) +
  geom_line() + geom_point(size = 0.3) +
  geom_hline(data = pred_ALK_mean %>%
               filter(age <= 10) %>%
               mutate(age = factor(age, levels = 10:0)),
             aes(yintercept = prop_mature, colour = age),
             linetype = "1212", alpha = 0.5) +
  scale_colour_manual("Age (years)", values = cols) +
  labs(x = "Year", y = "Proportion mature") +
  #coord_cartesian(xlim = c(0, 15)) +
  theme_bw(base_size = 8) +
  theme(legend.key.height = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave("data/maturity/plots/maturity_vB_age_timeseries.png", 
       width = 15, height = 10, units = "cm", dpi = 300, plot = p)

### annual vB model fits
### lengths corresponding to ages
lengths_yr <- lapply(yrs, function(yr) {#  browser()
  vB_Linf_i <- vB_pars$Linf[vB_pars$year == yr]
  vB_k_i <- vB_pars$k[vB_pars$year == yr]
  vB_t0_i <- vB_pars$t0[vB_pars$year == yr]
  lengths_i <- vB_length(L_inf = vB_Linf_i, k = vB_k_i, t_0 = vB_t0_i, t = ages)
  data.frame(age = ages,
             length = lengths_i,
             year = yr)
})
lengths_yr <- do.call(rbind, lengths_yr)

### predict maturity at these lengths
pred_ALK_yr <- lengths_yr
pred_ALK_yr_values <- predict(glm_, pred_ALK_yr, type = "response")
pred_ALK_yr$prop_mature <- pred_ALK_yr_values
pred_ALK_yr$age <- ages

### average
pred_ALK_yr_mean <- pred_ALK_yr %>%
  group_by(age) %>%
  summarise(prop_mature = mean(prop_mature, na.rm = TRUE)) %>%
  mutate(year = "average")

### plot by year
p <- bind_rows(pred_ALK_yr %>% 
                 mutate(year = as.character(year)), 
               pred_ALK_yr_mean) %>%
  ggplot(aes(x = age, y = prop_mature)) +
  geom_line() +
  facet_wrap(~ year) +
  labs(x = "Age (years)", y = "Proportion mature") +
  coord_cartesian(xlim = c(0, 15)) +
  theme_bw(base_size = 8)
if (isTRUE(verbose)) p
ggsave("data/maturity/plots/maturity_vB_yr_age.png", 
       width = 15, height = 10, units = "cm", dpi = 300, plot = p)

p <- pred_ALK_yr %>%
  filter(age <= 10) %>%
  mutate(age = factor(age, levels = 10:0)) %>%
  ggplot(aes(x = year, y = prop_mature, colour = age)) +
  geom_line() + geom_point(size = 0.3) +
  geom_hline(data = pred_ALK_yr_mean %>%
               filter(age <= 10) %>%
               mutate(age = factor(age, levels = 10:0)),
             aes(yintercept = prop_mature, colour = age),
             linetype = "1212", alpha = 0.5) +
  scale_colour_manual("Age (years)", values = cols) +
  labs(x = "Year", y = "Proportion mature") +
  #coord_cartesian(xlim = c(0, 15)) +
  theme_bw(base_size = 8) +
  theme(legend.key.height = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave("data/maturity/plots/maturity_vB_yr_age_timeseries.png", 
       width = 15, height = 10, units = "cm", dpi = 300, plot = p)


### moving average - 3-year average
head(pred_ALK_yr)
pred_ALK_yr_ma3 <- pred_ALK_yr %>%
  select(year, age, prop_mature) %>%
  pivot_wider(names_from = age, values_from = prop_mature) %>%
  column_to_rownames("year") %>%
  ### moving average for all ages
  mutate(across(everything(), ~ slide(.x, .f = mean, .before = 2, .after = 0,
                                      .complete = FALSE))) %>%
  rownames_to_column("year") %>%
  pivot_longer(-year, names_to = "age", values_to = "prop_mature") %>%
  mutate(year = as.numeric(year),
         age = as.numeric(age)) %>%
  mutate(prop_mature = sapply(prop_mature, 
                              function(x) {ifelse(is.null(x), NA, x)}))
### 5-year average
pred_ALK_yr_ma5 <- pred_ALK_yr %>%
  select(year, age, prop_mature) %>%
  pivot_wider(names_from = age, values_from = prop_mature) %>%
  column_to_rownames("year") %>%
  ### moving average for all ages
  mutate(across(everything(), ~ slide(.x, .f = mean, .before = 4, .after = 0,
                                      .complete = FALSE))) %>%
  rownames_to_column("year") %>%
  pivot_longer(-year, names_to = "age", values_to = "prop_mature") %>%
  mutate(year = as.numeric(year),
         age = as.numeric(age)) %>%
  mutate(prop_mature = sapply(prop_mature, 
                              function(x) {ifelse(is.null(x), NA, x)}))
### plot
p <- bind_rows(pred_ALK_yr_ma3 %>% mutate(source = "3-year average"),
          pred_ALK_yr_ma5 %>% mutate(source = "5-year average"),
          pred_ALK_yr %>% mutate(source = "annual")) %>%
  mutate(source = factor(source, levels = c("annual", "3-year average",
                                            "5-year average"))) %>%
  filter(age <= 10) %>%
  mutate(age = factor(age, levels = 10:0)) %>%
  ggplot(aes(x = year, y = prop_mature, colour = age, linetype = source,
             alpha = source)) +
  geom_line() +
  scale_colour_manual("Age (years)", values = cols) +
  scale_linetype_manual("", values = c("annual" = "solid",
                                       "3-year average" = "3131",
                                       "5-year average" = "1111")) +
  scale_alpha_manual("", values = c("annual" = 1,
                                       "3-year average" = 1,
                                       "5-year average" = 0.5)) + 
  labs(x = "Year", y = "Proportion mature") +
  theme_bw(base_size = 8) +
  theme(legend.key.height = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave("data/maturity/plots/maturity_vB_yr_age_timeseries_smoothed.png", 
       width = 15, height = 10, units = "cm", dpi = 300, plot = p)

### ------------------------------------------------------------------------ ###
### final data for OM ####
### ------------------------------------------------------------------------ ###
### maturity values from Q1SWBeam survey
### - females only
### - modelled with logistic regression, year as a random effect
### - converted to age with annual von Bertalanffy growth model fit
### - smoothed with 3-year moving average
### - ages 2 - 10
### - before maturity time series: mean of first three years

### FLQuant template
flq_mat <- FLQuant(NA, dimnames = list(ages = 2:10, year = 1980:2023))

### fill in values from survey
mat_tmp_annual <- pred_ALK_yr_ma3 %>%
  pivot_wider(names_from = age, values_from = prop_mature) %>%
  #filter(year >= 2008) %>%
  select(year, as.character(2:10)) %>%
  column_to_rownames("year") %>%
  t()
flq_mat[, ac(2006:2023)] <- mat_tmp_annual

### historical year: mean of first 3 years
mat_tmp_mean <- pred_ALK_yr %>%
  select(age, year, prop_mature) %>%
  filter(year %in% 2006:2008) %>%
  group_by(age) %>%
  summarise(prop_mature = mean(prop_mature)) %>%
  filter(age %in% 2:10) %>%
  select(prop_mature) %>%
  unlist()
flq_mat[, ac(1980:2005)] <- mat_tmp_mean

### save
mkdir("data/OM")
saveRDS(flq_mat, "data/OM/maturity.rds")
