## ----output, include=FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----setup, message = FALSE, warning = FALSE----------------------------------
# source functions required
library(PHEindicatormethods)
library(dplyr)

## ----default_example----------------------------------------------------------

# Pass data through SII function ---------------------------------------
LE_data_SII <- LE_data %>%
        # Group the input dataframe to create subgroups to calculate the SII for
        group_by(Sex, GeoCode) %>% 
        # Run SII function on grouped dataset
        phe_sii(quantile = Decile,
                population = Pop ,
                value = LifeExp,
                value_type = 0, # specify default indicator type
                confidence = c(0.95, 0.998),
                se = SE,
                repetitions = 1000,
                rii = FALSE,
                type = "full") # use smaller no. of repetitions e.g. for testing

# View first 10 rows of results
knitr::kable(head(LE_data_SII, 10))  


## ----rate_example-------------------------------------------------------------

# Pass data through SII function ---------------------------------------
DSR_data_SII <- DSR_data %>%
        # Group the input dataframe to create subgroups to calculate the SII for
        group_by(Period) %>% 
        # Run SII function on grouped dataset
        phe_sii(quantile = Quintile,
                population = total_pop ,
                value = value,
                value_type = 1, # specifies indicator is a rate
                lower_cl = lowercl,
                upper_cl = uppercl,
                transform = TRUE,
                rii = TRUE, # returns RII as well as SII (default is FALSE)
                reliability_stat = TRUE) # returns reliability stats (default is FALSE)

# View results
knitr::kable(DSR_data_SII)  


## ----proportion_example-------------------------------------------------------

# Pass data through SII function ---------------------------------------
prevalence_SII <- prevalence_data %>%
          # Group the input dataframe to create subgroups to calculate the SII for
        group_by(Period, SchoolYear, AreaCode) %>% 
          # Format prevalences to be between 0 and 1
        mutate(Rate = Rate/100,
               LCL = LCL/100,
               UCL = UCL/100) %>% 
           # Run SII function on grouped dataset
        phe_sii(quantile = Decile,
                        population = Measured,
                        value = Rate,
                        value_type = 2, # specifies indicator is a proportion
                        lower_cl = LCL,
                        upper_cl = UCL,
                        transform = TRUE,
                        multiplier = -100) # negative multiplier to scale SII outputs

# View first 10 rows of results
knitr::kable(head(prevalence_SII,10)) 



