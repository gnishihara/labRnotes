# Fitting the Bayesian Ordinal GAM
# 2021 Aug 11
# Greg Nishihara

# Set up options for brms
options(
  mc.cores = parallel::detectCores(),
  brms.backend = "rstan"
)

# Load packages
library(tidyverse)
library(lubridate)
library(readxl)
library(brms)
library(future)
plan(multisession)

# Load data from the excel sheet
filename = "~/Lab_Data/kabeyamam/arikawa_amamo_line.xlsx"
esheet = excel_sheets(filename)
dset = tibble(filename, esheet) |>  mutate(data = map2(filename, esheet,  read_xlsx, col_types = "text"))

# Define functions to process data
pivot_longer_get_line = function(df) {
  tomatch = "ライン|距離|時刻|アマモ"
  df |> 
    select(matches(tomatch)) |> 
    pivot_longer(cols = everything()) |> 
    mutate(line = str_extract(name,"[0-9]+$"),
           line = as.numeric(line),
           name = str_remove(name,"[0-9]+$"))
}

pivot_wider_clean_data = function(df) {
  df |> 
    pivot_wider(id_cols = line,
                names_from = name, 
                values_from = value,
                values_fn = list) |> 
    unnest(everything()) |> 
    mutate(distance = str_extract(距離,"[0-9]+"),
           line = 5 * (line - 1),
           amamo = factor(アマモ),
           distance = as.numeric(distance)) |> 
    select(line, distance, amamo) |>
    mutate(amamo = str_remove(amamo,"orC|orB"),
           amamo = ifelse(str_detect(amamo,"A|B|C|D|E"), amamo, NA))
}

# Apply the functions and define month as a factor

dset = dset |> 
  mutate(data = map(data, pivot_longer_get_line)) |>
  mutate(data = map(data, pivot_wider_clean_data)) |> 
  mutate(date = ymd(esheet),
         month = month(date),
         month_fct = factor(month, 
                            levels = c(4:12, 1:3),
                            labels = month.abb[c(4:12, 1:3)])) 

# Clean up the tibble and define amamo as an ordered factor
dset = dset |> select(-filename) |> unnest(data)
dset = dset |> mutate(amamo = factor(amamo, ordered = T))

# Define the null (bmodel0) and full (bmodel1) GAM models

bmodel0 = bf(amamo ~ t2(line, distance, bs = c("ts", "ts"), k = c(10, 30), m = 2))
bmodel1 = bf(amamo ~ t2(line, distance, bs = c("ts", "ts"), k = c(10, 30), m = 2) + 
               t2(line, distance, month_fct, bs = c("ts", "ts", "re"), k = c(10, 30, 3), m = 1)) 

# Define the parameters for the sample

seed = 2020
chains = 4
cores = chains
threads = 4
ctrl = list(adapt_delta = 0.95, max_treedepth = 15)

# Define the prior distribution

myprior = c(prior(student_t(3, 0, 2.5), class = Intercept),
            prior(student_t(3, 0, 2.5), class = sds))

# Sample from the null model posterior (requires about 3 hours)
bout0 = brm(bmodel0, 
            data = dset,
            family = cumulative("probit"),
            prior = myprior,
            chains = chains, cores = cores,
            seed = seed, control = ctrl,
            file = "brms_bout0")

# Sample from the full model posterior (requires about 4 hours)
bout1 = brm(bmodel1, 
            data = dset,
            family = cumulative("probit"),
            prior = myprior,
            chains = chains, cores = cores,
            seed = seed, control = ctrl,
            file = "brms_bout1")

# Define the path and name of the kfold results
kname0 = rprojroot::find_rstudio_root_file("brms_kfold0.rds")
kname1 = rprojroot::find_rstudio_root_file("brms_kfold1.rds")

# K-fold cross validation using 4 out of 10 folds
# After sampling, save the kfold results to file
kfold0 = bout0 |> brms::kfold(K = 10, Ksub = 4, cores = 10, control = list(adapt_delta = 0.999999, max_treedepth = 20))
kfold0 |> write_rds(kname0)

kfold1 = bout1 |> kfold(K = 10, group = "month_fct", folds = "stratified", Ksub = 4, cores = 10, control = list(adapt_delta = 0.999999, max_treedepth = 20))
kfold1 |> write_rds(kname1)


