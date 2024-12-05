## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----libraries, message=FALSE-------------------------------------------------
library(PHEindicatormethods)
library(dplyr)

## ----create test data1--------------------------------------------------------
df <- data.frame(
        area = rep(c("Area1","Area2","Area3","Area4"), 2),
        year = rep(2015:2016, each = 4),
        obs = sample(100, 2 * 4, replace = TRUE),
        pop = sample(100:200, 2 * 4, replace = TRUE))
df

## ----Execute phe_proportion and phe_rate--------------------------------------
# default proportion
phe_proportion(df, obs, pop)

# specify confidence level for proportion
phe_proportion(df, obs, pop, confidence = 99.8)

# specify multiplier to output proportions as percentages
phe_proportion(df, obs, pop, multiplier = 100)

# specify multiplier for proportion, confidence level and remove metadata columns
phe_proportion(df, obs, pop, confidence = 99.8, multiplier = 100, type = "standard")

# default rate
phe_rate(df, obs, pop)

# specify multiplier for rate and confidence level
phe_rate(df, obs, pop, confidence = 99.8, multiplier = 100)

# specify multiplier for rate, confidence level and remove metadata columns
phe_rate(df, obs, pop, type = "standard", confidence = 99.8, multiplier = 100)

## ----Execute phe_proportion and phe_rate grouped------------------------------
# default proportion - grouped
df %>%
  group_by(year) %>%
  phe_proportion(obs, pop)

# default rate - grouped
df %>%
  group_by(year) %>%
  phe_rate(obs, pop)

## ----Execute phe_mean---------------------------------------------------------
# default mean
phe_mean(df,obs)

# multiple means in a single execution with 99.8% confidence
df %>%
    group_by(year) %>%
        phe_mean(obs, confidence = 0.998)

# multiple means in a single execution with 99.8% confidence and data-only output
df %>%
    group_by(year) %>%
        phe_mean(obs, type = "standard", confidence = 0.998)

## ----create test data2--------------------------------------------------------
df_std <- data.frame(
            area = rep(c("Area1", "Area2", "Area3", "Area4"), each = 19 * 2 * 5),
            year = rep(2006:2010, each = 19 * 2),
            sex = rep(rep(c("Male", "Female"), each = 19), 5),
            ageband = rep(c(0, 5,10,15,20,25,30,35,40,45,
                           50,55,60,65,70,75,80,85,90), times = 10),
            obs = sample(200, 19 * 2 * 5 * 4, replace = TRUE),
            pop = sample(10000:20000, 19 * 2 * 5 * 4, replace = TRUE))
head(df_std)

## ----Execute calculate_dsr----------------------------------------------------

# Append the standard populations to the data frame
# calculate separate dsrs for each area, year and sex
df_std %>%
    mutate(refpop = rep(esp2013, 40)) %>%
    group_by(area, year, sex) %>%
    calculate_dsr(obs,pop, stdpop = refpop)


# Append the standard populations to the data frame
# calculate separate dsrs for each area, year and sex and drop metadata fields from output
df_std %>%
    mutate(refpop = rep(esp2013, 40)) %>%
    group_by(area, year, sex) %>%
    calculate_dsr(obs,pop, stdpop = refpop, type = "standard")

# calculate for under 75s by filtering out records for 75+ from input data frame and standard population
df_std %>%
  filter(ageband <= 70) %>%
  mutate(refpop = rep(esp2013[1:15], 40)) %>%
  group_by(area, year, sex) %>%
  calculate_dsr(obs, pop, stdpop = refpop)


## ----create reference data----------------------------------------------------
df_ref <- df_std %>%
    filter(year == 2006) %>%
    group_by(ageband) %>%
    summarise(obs = sum(obs),
              pop = sum(pop),
              .groups = "drop_last")
    
head(df_ref)

## ----Execute calculate_ISRatio------------------------------------------------
# calculate separate smrs for each area, year and sex
# standardised against the all-year, all-sex, all-area reference data
df_std %>%
    group_by(area, year, sex) %>%
    calculate_ISRatio(obs, pop, df_ref$obs, df_ref$pop)

# calculate the same smrs by appending the reference data to the data frame
# and drop metadata columns from output
df_std %>%
    mutate(refobs = rep(df_ref$obs,40),
           refpop = rep(df_ref$pop,40)) %>%
    group_by(area, year, sex) %>%
    calculate_ISRatio(obs, pop, refobs, refpop, refpoptype = "field",
                      type = "standard")


## ----Execute calculate_ISRate-------------------------------------------------
# calculate separate indirectly standardised rates for each area, year and sex
# standardised against the all-year, all-sex, all-area reference data
df_std %>%
    group_by(area, year, sex) %>%
    calculate_ISRate(obs, pop, df_ref$obs, df_ref$pop)

# calculate the same indirectly standardised rates by appending the reference data to the data frame
# and drop metadata columns from output
df_std %>%
    mutate(refobs = rep(df_ref$obs,40),
           refpop = rep(df_ref$pop,40)) %>%
    group_by(area, year, sex) %>%
    calculate_ISRate(obs, pop, refobs, refpop, refpoptype = "field",
                     type = "standard")


