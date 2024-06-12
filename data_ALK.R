### ------------------------------------------------------------------------ ###
### age-length keys (ALKs) ####
### ------------------------------------------------------------------------ ###

library(icesTAF)
taf.libPaths()
library(tidyr)
library(dplyr)
library(ggplot2)
library(icesDatras)

mkdir("data/ALKs")
mkdir("data/ALKs/plots")

if (!exists("verbose")) verbose <- FALSE

### ------------------------------------------------------------------------ ###
### commercial ALK ####
### ------------------------------------------------------------------------ ###

### load ALKS (historical and current)
alks <- read.csv("boot/initial/data/ALK/ALKs_commercial.csv")
alks <- alks %>% mutate(across(starts_with("X"), as.numeric)) %>%
  pivot_longer(starts_with("X"), names_prefix = "X", names_to = "age", 
               values_to = "count", values_transform = as.numeric) %>%
  mutate(age = as.numeric(age)) %>%
  group_by(year, data, length, age) %>%
  summarise(count = sum(count, na.rm = TRUE))

### plot ALKs
p <- ggplot(data = alks %>% filter(count > 0), 
            aes(x = age, y = length, size = count, colour = data)) +
  geom_point(alpha = 0.2, shape = 1) +
  scale_colour_discrete("") + 
  labs(x = "Age (years)", y = "Length (cm)") +
  ylim(0, NA) + xlim(0, NA) +
  facet_wrap(~ year) +
  theme_bw(base_size = 8)
if (isTRUE(verbose)) p

### ------------------------------------------------------------------------ ###
### commercial ALK - using quarterly ALK ####
### ------------------------------------------------------------------------ ###

### load ALKS (historical and current)
alks_q <- read.csv("boot/initial/data/ALK/ALKs_commercial.csv")
alks_q <- alks_q %>% mutate(across(starts_with("X"), as.numeric)) %>%
  pivot_longer(starts_with("X"), names_prefix = "X", names_to = "age", 
               values_to = "count", values_transform = as.numeric) %>%
  mutate(age = as.numeric(age)) %>%
  group_by(year, data, quarter, length, age) %>%
  summarise(count = sum(count, na.rm = TRUE)) %>%
  filter(count > 0 & !is.na(count)) %>%
  ### assume age is middle of quarter
  mutate(delta_year = case_when(quarter == "1" ~ mean(1:3)/12,
                                quarter == "2" ~ mean(4:6)/12,
                                quarter == "3" ~ mean(7:9)/12,
                                quarter == "4" ~ mean(10:12)/12,
                                quarter == "all" ~ 0.5
                                )) %>%
  mutate(age = age + delta_year)

### plot ALKs
p <- ggplot(data = alks_q %>% filter(count > 0), 
            aes(x = age, y = length, size = count, colour = data)) +
  geom_point(alpha = 0.2, shape = 1) +
  scale_colour_discrete("") + 
  labs(x = "Age (years)", y = "Length (cm)") +
  ylim(0, NA) + xlim(0, NA) +
  facet_wrap(~ year) +
  theme_bw(base_size = 8)
if (isTRUE(verbose)) p

### ------------------------------------------------------------------------ ###
### ALK from Q1SWBeam ####
### ------------------------------------------------------------------------ ###

if (isTRUE(verbose)) {
  Q1 <- lapply(2006:2023, getCAdata, survey = "BTS", quarter = 1)
  Q1 <- lapply(Q1, function(x) {
    x$LngtCode <- as.character(x$LngtCode)
    return(x)
  })
  Q1 <- do.call(bind_rows, Q1)
  ### select plaice
  Q1_ALK <- Q1 %>%
    filter(Survey == "BTS" &
             Ship == "74E9" & ### Cefas Endeavour - name changed in 2023...
             Quarter == 1 &
             Country == "GB" &
             SpecCode == 127143 & ### plaice
             !is.na(Age)
    ) %>%
    select(Year, Age, LngtClass, CANoAtLngt) %>%
    group_by(Year, Age, LngtClass) %>%
    summarise(CANoAtLngt = sum(CANoAtLngt)) %>%
    select(year = Year, age = Age, length = LngtClass, count = CANoAtLngt)
  saveRDS(object = Q1_ALK, 
          file = "boot/initial/data/ALK/Q1SWBeam_ple_ALK.rds")
}
### load ALK from bootstrap directory
Q1_ALK <- readRDS("boot/initial/data/ALK/Q1SWBeam_ple_ALK.rds")

### plot ALKs
p <- ggplot(data = Q1_ALK %>% filter(count > 0), 
            aes(x = age, y = length, size = count)) +
  geom_point(alpha = 0.2, shape = 1) +
  scale_colour_discrete("") + 
  labs(x = "Age (years)", y = "Length (cm)") +
  ylim(0, NA) + xlim(0, NA) +
  facet_wrap(~ year) +
  theme_bw(base_size = 8)
if (isTRUE(verbose)) p

### ------------------------------------------------------------------------ ###
### ALK from Q1SWBeam - assume age is middle of Q1 ####
### ------------------------------------------------------------------------ ###

### load ALK from bootstrap directory
Q1_ALK_q <- readRDS("boot/initial/data/ALK/Q1SWBeam_ple_ALK.rds")
Q1_ALK_q <- Q1_ALK_q %>% 
  mutate(age = age + 2/12)

### plot ALKs
p <- ggplot(data = Q1_ALK_q %>% filter(count > 0), 
            aes(x = age, y = length, size = count)) +
  geom_point(alpha = 0.2, shape = 1) +
  scale_colour_discrete("") + 
  labs(x = "Age (years)", y = "Length (cm)") +
  ylim(0, NA) + xlim(0, NA) +
  facet_wrap(~ year) +
  theme_bw(base_size = 8)
if (isTRUE(verbose)) p

### ------------------------------------------------------------------------ ###
### combine ALKs ####
### ------------------------------------------------------------------------ ###

ALK_combined <- bind_rows(
  alks %>%
    group_by(year, age, length) %>%
    summarise(count = sum(count, na.rm = TRUE)) %>%
    filter(count > 0) %>%
    mutate(data = "catch"),
  Q1_ALK %>%
    group_by(year, age, length) %>%
    filter(count > 0) %>%
    mutate(data = "Q1SWBeam")) %>%
  ungroup()

### number of fish age per year and data source
if (isTRUE(verbose))
  ALK_combined %>%
  group_by(year, data) %>%
  summarise(count = sum(count)) %>%
  print(n = Inf)

### plot
p <- ALK_combined %>% 
  filter(year >= 2013) %>%
  group_by(year, data, age, length) %>%
  summarise(count = sum(count)) %>%
  ungroup() %>%
  group_by(year, data) %>%
  mutate(freq = count/sum(count)) %>%
  ggplot(aes(x = age, y = length, size = freq, colour = data)) +
  geom_point(alpha = 0.5, shape = 21) +
  labs(x = "Age (years)", y = "Length (cm)") +
  ylim(0, NA) + xlim(0, NA) +
  scale_size_continuous("freq") +
  scale_colour_discrete("") +
  facet_wrap(~ year) +
  theme_bw(base_size = 8)
if (isTRUE(verbose)) p
ggsave("data/ALKs/plots/ALKs_combined.png", 
       width = 15, height = 10, units = "cm", dpi = 300, plot = p)

### ------------------------------------------------------------------------ ###
### combine ALKs - with quarterly ages ####
### ------------------------------------------------------------------------ ###

ALK_combined_q <- bind_rows(
  alks_q %>%
    select(-delta_year) %>%
    group_by(year, age, length) %>%
    summarise(count = sum(count, na.rm = TRUE)) %>%
    filter(count > 0) %>%
    mutate(data = "catch"),
  Q1_ALK_q %>%
    group_by(year, age, length) %>%
    filter(count > 0) %>%
    mutate(data = "Q1SWBeam")) %>%
  ungroup()

### number of fish age per year (commercial data)
if (isTRUE(verbose))
  alks_q %>%
  group_by(year, data, quarter) %>%
  summarise(count = sum(count)) %>%
  print(n = Inf)
### number of fish age per year and data source
if (isTRUE(verbose))
  ALK_combined_q %>%
  group_by(year, data, quarter) %>%
  summarise(count = sum(count)) %>%
  print(n = Inf)

### plot
p <- ALK_combined_q %>% 
  filter(year >= 2013) %>%
  group_by(year, data, age, length) %>%
  summarise(count = sum(count)) %>%
  ungroup() %>%
  group_by(year, data) %>%
  mutate(freq = count/sum(count)) %>%
  ggplot(aes(x = age, y = length, size = freq, colour = data)) +
  geom_point(alpha = 0.5, shape = 21) +
  labs(x = "Age (years)", y = "Length (cm)") +
  ylim(0, NA) + xlim(0, NA) +
  scale_size_continuous("freq") +
  scale_colour_discrete("") +
  facet_wrap(~ year) +
  theme_bw(base_size = 8)
if (isTRUE(verbose)) p
ggsave("data/ALKs/plots/ALKs_combined_q.png", 
       width = 15, height = 10, units = "cm", dpi = 300, plot = p)

### ------------------------------------------------------------------------ ###
### fit von Bertalanffy growth model ####
### ------------------------------------------------------------------------ ###

### von Bertalanffy growth function
vB_length <- function(L_inf, k, t, t_0 = 0){
  L_inf * (1 - exp(-k * (t - t_0)))
}

### weight commercial and survey data equally
ALK_fit <- ALK_combined_q %>%
  ### standardise counts per data source
  group_by(year, data) %>%
  mutate(freq = count/sum(count)) %>%
  ungroup() %>%
  group_by(year, data, age, length) %>%
  summarise(freq = sum(freq)) %>%
  ungroup() %>%
  ### combine both data source and standardise again
  group_by(year, age, length) %>%
  summarise(freq = sum(freq))
ALK_fit <- as.data.frame(ALK_fit)
df_i <- ALK_fit %>% filter(year == 2017)
### fit in normal scale
if (isTRUE(verbose))
  nls(length ~ vB_length(L_inf, k, t = age, t_0),
      data = df_i,
      weight = df_i$freq,
      start = list(L_inf = 50, k = 0.1, t_0 = 0))

### fit in log-scale
if (isTRUE(verbose))
  nls(log(length) ~ log(vB_length(L_inf, k, t = age, t_0)),
      data = df_i,
      weight = df_i$freq,
      start = list(L_inf = 50, k = 0.1, t_0 = 0))

### fit model by year
yrs_alk <- as.list(unique(ALK_fit$year))
names(yrs_alk) <- yrs_alk
yrs_alk <- append(yrs_alk, list("all" = c(2006:2023), "last5" = c(2019:2023)))

model_fits <- lapply(yrs_alk, function(x) {
  df_i <- ALK_fit %>%
    filter(year %in% x & age > 0)
  df_i <- as.data.frame(df_i)
  nls(log(length) ~ log(vB_length(L_inf, k, t = age, t_0)),
      data = df_i, weight = df_i$freq,
      start = list(L_inf = 50, k = 0.1, t_0 = 0))
})
### get parameters
if (isTRUE(verbose)) {
  sapply(model_fits, function(x) x$m$getPars()[["L_inf"]])
  sapply(model_fits, function(x) x$m$getPars()[["k"]])
  sapply(model_fits, function(x) x$m$getPars()[["t_0"]])
}

### save vB parameters
vB_pars <- data.frame(
  year = names(model_fits),
  k = sapply(model_fits, function(x) x$m$getPars()[["k"]]),
  Linf = sapply(model_fits, function(x) x$m$getPars()[["L_inf"]]),
  t0 = sapply(model_fits, function(x) x$m$getPars()[["t_0"]]))
write.csv(x = vB_pars, file = "data/ALKs/vB_pars_q.csv", row.names = FALSE)

### get estimates
ages <- seq(0, 26, 0.1)
length_predicted <- lapply(names(model_fits),
  function(x) {
    model_tmp <- model_fits[[x]]
    lengths_tmp <- vB_length(
      L_inf = summary(model_tmp)$parameters["L_inf", "Estimate"],
      k = summary(model_tmp)$parameters["k", "Estimate"],
      t_0 = summary(model_tmp)$parameters["t_0", "Estimate"], 
      t = ages)
    data.frame(age = ages,
               length = lengths_tmp,
               year = x)
})
length_predicted_years <- do.call(rbind, 
                                  length_predicted[-length(length_predicted)])
length_predicted_last5 <- lapply(yrs_alk$last5, function(x) {
  length_predicted[[length(length_predicted)]] %>% mutate(year = x)
})
length_predicted_last5 <- do.call(rbind, length_predicted_last5)

### add curves to plot
p <- ggplot() +
  geom_point(data = ALK_combined_q %>% 
               filter(year >= 2013) %>%
               group_by(year, data, age, length) %>%
               summarise(count = sum(count)) %>%
               ungroup() %>%
               group_by(year, data) %>%
               mutate(freq = count/sum(count)) %>%
               mutate(data = factor(data, 
                                    levels = c("Q1SWBeam", "catch"),
                                    labels = c("Q1SWBeam", 
                                               "Commercial\ncatch"))), 
             aes(x = age, y = length, size = freq, colour = data),
             alpha = 0.5, shape = 21) +
  ### curve by year
  geom_line(data = length_predicted_years %>% 
              filter(year >= 2013) %>%
              mutate(year = as.numeric(as.character(year))),
            aes(x = age, y = length)) +
  ### last 5 years
  geom_line(data = length_predicted_last5 %>%
              mutate(year = as.numeric(as.character(year))),
            aes(x = age, y = length),
            linetype = "dashed") +
  labs(x = "Age (years)", y = "Length (cm)") +
  ylim(0, NA) + xlim(0, NA) +
  scale_size_continuous("Frequency") +
  scale_colour_discrete("") +
  ylim(c(0, 78.5)) +
  facet_wrap(~ year) +
  theme_bw(base_size = 8)
if (isTRUE(verbose)) p
ggsave("data/ALKs/plots/ALKs_combined_fitted_q.png", 
       width = 15, height = 10, units = "cm", dpi = 300, plot = p)

### ------------------------------------------------------------------------ ###
### Prepare ALKs for MSE ####
### ------------------------------------------------------------------------ ###

### load commercial ALKs
alks <- read.csv("boot/initial/data/ALK/ALKs_commercial.csv")
alks <- alks %>% mutate(across(starts_with("X"), as.numeric)) %>%
  pivot_longer(starts_with("X"), names_prefix = "X", names_to = "age", 
               values_to = "count", values_transform = as.numeric) %>%
  mutate(age = as.numeric(age)) %>%
  ### 1cm bins in 2017, 2cm bins in other years 
  ### -> standardise to 2cm in 2017
  mutate(length = ifelse((length %% 2) == 0, length - 1, length)) %>%
  group_by(year, length, age) %>%
  summarise(count = sum(count, na.rm = TRUE)) %>%
  filter(count > 0)

### check number of fish measured per year
if (isTRUE(verbose)) 
  alks %>% 
    group_by(year) %>%
    summarise(count = sum(count))
# year count
# 2013  2149
# 2014  2616
# 2015  2124
# 2016  1998
# 2017  2525
# 2018  1675
# 2019  2331
# 2020  1992
# 2021  2454
# 2022  2773
# 2023  2322
if (isTRUE(verbose)) 
  alks %>% 
    group_by(year) %>%
    summarise(count = sum(count)) %>%
    ungroup() %>%
    summarise(average = mean(count))
# 2269

### Q1SWBeam ALK
Q1_ALK <- readRDS("boot/initial/data/ALK/Q1SWBeam_ple_ALK.rds")
### standardise to 2cm length classes (same as for commercial data)
Q1_ALK <- Q1_ALK %>%
  mutate(length = ifelse((length %% 2) == 0, length - 1, length))

### check number of fish measured per year
if (isTRUE(verbose)) 
  Q1_ALK %>% 
    group_by(year) %>%
    summarise(count = sum(count))
# year count
# 2006   276
# 2007   269
# 2008   200
# 2009   329
# 2010   375
# 2011   541
# 2012   394
# 2013   522
# 2014   938
# 2015   812
# 2016   870
# 2017   902
# 2018   835
# 2019   669
# 2020   371
# 2021   570
# 2022   430
# 2023   396

### year ranges
yrs <- intersect(unique(alks$year), unique(Q1_ALK$year))

### combine 
ALKs <- bind_rows(alks %>% 
                    filter(year %in% yrs) %>%
                    mutate(data = "commercial"),
                  Q1_ALK %>% 
                    filter(year %in% yrs) %>%
                    mutate(data = "Q1SWBeam"))

### standardise length frequencies by age - 1st by data source
ALKs <- ALKs %>%
  group_by(data, year, age) %>%
  mutate(freq = count / sum(count))
### then - combine data source and standardise again
### (equal weight for commercial and survey data)
ALKs <- ALKs %>%
  ungroup() %>%
  group_by(year, age) %>%
  mutate(freq = freq/sum(freq))
### remove redundant columns
ALKs <- ALKs %>%
  ungroup() %>%
  group_by(year, age, length) %>%
  summarise(freq = sum(freq))

### check total freq per year & age
if (isTRUE(verbose)) 
  ALKs %>% 
    group_by(year, age) %>%
    summarise(total = sum(freq)) %>%
    summary()

### trim ages for OM 2-10
ALKs <- ALKs %>%
  ### remove ages < 2 because not modelled in OM
  filter(age >= 2) %>% 
  #mutate(age = ifelse(age < 2, 2, age)) %>%
  ### plusgroup 10+
  mutate(age = ifelse(age > 10, 10, age))
### standardise again
ALKs <- ALKs %>%
  ungroup() %>%
  group_by(year, age) %>%
  mutate(freq = freq/sum(freq))

### save ALK for MSE
saveRDS(ALKs, file = "data/ALKs/ALK_MSE.rds")
# saveRDS(ALKs, file = "../../../data-limited/MSE_risk_comparison_WKBPLAICE/input/ple.27.7e/preparation/ALK_MSE.rds")

### check reproduction of length frequencies for historical period ####
stk <- readRDS("data/OM/stk_d100.rds")
cn <- as.data.frame(catch.n(stk)[, ac(yrs)])

### catch numbers at age
cn <- cn %>%
  select(year, age, iter, data) %>%
  rename("caa" = "data") %>%
  mutate(iter = as.numeric(as.character(iter)))
### match ALKs with data
cn <- left_join(cn, ALKs)
### calculate numbers at length
cn <- cn %>%
  mutate(cal = caa * freq)

p <- cn %>% group_by(year, length) %>%
  summarise(cal = sum(cal)) %>%
  ggplot(aes(x = length, y = cal)) +
  geom_line() +
  facet_wrap(~ year)
if (isTRUE(verbose)) p

### mean catch length above Lc
Lc <- 26.4
if (isTRUE(verbose)) 
  cn %>%
    filter(length >= Lc) %>%
    #filter(!is.na(length) & !is.na(cal)) %>%
    group_by(year) %>%
    summarise(L = weighted.mean(x = length, w = cal))

### sample
if (isTRUE(verbose)) 
  cn %>%
    group_by(year, iter) %>%
    filter(length >= Lc) %>%
    summarise(data = mean(sample(x = length, prob = cal, 
                                 size = 2000, replace = TRUE))) %>%
    arrange(as.numeric(as.character(iter)))



