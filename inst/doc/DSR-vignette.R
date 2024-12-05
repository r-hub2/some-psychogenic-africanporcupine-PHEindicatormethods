## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----libraries, message=FALSE-------------------------------------------------
library(PHEindicatormethods)
library(dplyr)
library(tidyr)


## ----Execute SQL Query and load results into r object-------------------------
pops <- data.frame(
  indicator = rep(c("Ind1", "Ind2", "Ind3", "Ind4"), each = 19 * 2 * 5),
  period    = rep(2012:2016, each = 19 * 2, times = 4),
  region    = rep(rep(c("Area1", "Area2"), each = 19), times = 20),
  ageband   = rep(c(0,  5, 10, 15, 20, 25, 30, 35, 40, 45, 50,
                   55, 60, 65, 70, 75, 80, 85, 90), times = 40),
  pop       = sample(10000:20000, 19 * 2 * 5 * 4, replace = TRUE),
  esp2013   = rep(esp2013, times = 40))
head(pops)


deaths <- data.frame(
  indicator = rep(c("Ind1", "Ind2", "Ind3", "Ind4"), each = 19 * 2 * 5),
  period = rep(2012:2016, each = 19 * 2),
  region = rep(rep(c("Area1", "Area2"), each = 19), times = 5),
  ageband = rep(c(0,  5, 10, 15, 20, 25, 30, 35, 40, 45, 50,
                 55, 60, 65, 70, 75, 80, 85, 90), times = 10),
  dths = sample(200, 19 * 2 * 5 * 4, replace = TRUE))
head(deaths)


## ----create reference column--------------------------------------------------
df <- left_join(pops,
                deaths, 
                by = c("indicator", "period", "region", "ageband")) %>%
  group_by(indicator, period, region)


## ----calculate DSRs-----------------------------------------------------------
calculate_dsr(
  data = df, 
  x = dths, # name of field containing count of events
  n = pop, # name of field containing population denominators
  stdpop = esp2013 # name of field containing standard populations
)


## ----alternative dsr----------------------------------------------------------
calculate_dsr(
  data = df, 
  x = dths, 
  n = pop, 
  stdpop = esp2013, 
  type = "standard", 
  confidence = 99.8, 
  multiplier = 10000
)


## ----specify stdpop as field name---------------------------------------------
# duplicate data for males and females and apply different standard populations
# to each sex
df_f <- df %>%
  mutate(
    sex = "F",
    esp_dummy = c(5000, 5500, 5500, 5500, 6000, 6000, 6500, 7000, 7000, 7000,
                  7000, 6500, 5500, 5000, 4500, 4000, 3000, 2000, 1500)
  )

df_m <- df %>%
  mutate(
    sex = "M",
    esp_dummy = c(5000, 5500, 5500, 5500, 6000, 6000, 6500, 7000, 7000, 7000,
                  7000, 6500, 6500, 6000, 5500, 4000, 2000, 1000, 500)
  )

df_mf <- df_f %>%
  bind_rows(df_m) %>%
  group_by(sex, .add = TRUE) %>%
  select(!"esp2013")

# add sex to the grouping variables then calculate the DSRs
dsrs_mf <- calculate_dsr(
  df_mf, 
  x = dths, 
  n = pop, 
  stdpop = esp_dummy
)

head(dsrs_mf)


## -----------------------------------------------------------------------------
# Generate some dummy data
# breakdown original dataset to show event frequencies and to count unique people
df_person_freq <- df %>%
 mutate(
   f3 = floor((dths * 0.1) / 3),           # 10% of events in persons with 3 events
   f2 = floor((dths * 0.2) / 2),           # 20% of events in persons with 2 events
   f1 = (dths - (3 * f3) - (2 * f2))) %>%  # 70% of events in persons with 1 event
 select(!"dths") %>%
 pivot_longer(
   cols = c("f1", "f2", "f3"),
   names_to = "eventfrequency",
   values_to = "uniquepeople",
   names_prefix = "f") %>%
   mutate(eventfrequency = as.integer(eventfrequency)
 )

# calculate the dsrs - notice that output DSRs values match those calculated
# earlier for the same data frame but confidence intervals are wider
df_person_freq %>%
  group_by(eventfrequency, .add = TRUE) %>%
  calculate_dsr(
    x = uniquepeople, # count of unique individuals experiencing the frequency of events in eventfreq
    n = pop,
    stdpop = esp2013,
    independent_events = FALSE, # calculate CIs assuming events are not independent
    eventfreq = eventfrequency, # name of column containing the event frequencies (e.g. 1, 2, ...)
    ageband = ageband # name of column containing age bands
  )


## ----test data----------------------------------------------------------------
pops2   <- data.frame(
  ageband = c( 0, 5, 10, 15, 20, 25, 30, 35, 40, 45,
               50, 55, 60, 65, 70, 75, 80, 85, 90),
  pop     = c(30, 35, 35, 35, 40, 40, 45, 50, 50, 50,
              60, 60, 70, 75, 70, 60, 20, 20, 15),
  esp2013 = esp2013
)

deaths2 <- data.frame(
  ageband = c(0, 5, 25, 30, 35, 40, 45, 50, 55, 
              60, 65, 70, 75, 80, 85, 90),
  dths    = c(1, 1, 1, 1, 3, 3, 3, 3, 10, 
              10, 10, 10, 8, 8, 8, 8)
)


df2 <- left_join(pops2, deaths2, by = "ageband")

head(df2)

calculate_dsr(
  df2,
  x = dths,
  n = pop,
  stdpop = esp2013
)

