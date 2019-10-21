library(tidyverse)
library(here)
library(glue)
library(xlsx)
library(readxl)

here()
setwd(here())
captures <- read_excel("2007-2019 SMI FOX GRID DATA_10.9.2019 export.xlsx",
                       col_types = c("text", "numeric", "text", 
                                     "date", "numeric", "numeric", "numeric", 
                                     "numeric", "text", "text", "text", 
                                     "text", "text", "text", "text", "text", 
                                     "text", "numeric", "text", "text", 
                                     "text", "text", "text", "skip"))


# Look at distinct ageclass values over time
pittags_all_years_distinct <- captures %>%
  filter(!is.na(AgeClass), !is.na(Pittag)) %>% 
  group_by(SamplingYear, Pittag) %>% 
  summarise(nDist = n_distinct(AgeClass)) %>% 
  spread(SamplingYear, nDist) %>% 
  filter_if(is.numeric, any_vars(. > 1)) # Optional


# Get the max value of those ageclass values and put them sequentially to
# see if any foxes accidentally got marked getting younger over time.
pittags_all_years_maxValue <- captures %>%
  filter(!is.na(AgeClass), !is.na(Pittag)) %>% 
  group_by(SamplingYear, Pittag) %>% 
  summarise(max = max(AgeClass)) %>% 
  spread(SamplingYear, max) 

write.xlsx(pittags_all_years_maxValue,"pittags_all_years_maxValue.xlsx")


# Get all sex values for pittags.  Are any accidentally both male and female?
pittags_all_years_sex <- captures %>%
  filter(!is.na(Sex), !is.na(Pittag)) %>% 
  group_by(SamplingYear, Pittag) %>% 
  summarise(LasVal = last(Sex)) %>% 
  spread(SamplingYear, LasVal)

write.xlsx(pittags_all_years_sex,"pittags_all_years_sex.xlsx")


# Count of all trap results for all years
trapResults <- captures %>% 
  group_by(SamplingYear, TrapName) %>% 
  count(TrapResult) %>% 
  spread(SamplingYear, n)

write.csv(trapResults,"All years trap results by Grid Name.csv")
