# Purpose: Processes the secr fit models from step 2 and produce an output population estimate. 
# Authors: Rocky Rudolph, GISP, Channel Islands National Park
#          Adam Dillon, PhD Candidate - Colorado State University
# Date: 1/30/2019


library(glue)
library(tidyverse)
library(janitor)
library(xlsx)
library(here)

# Clear any environment variables in memory
rm(list = ls())

# Load functions
here()
source(here("functions.R"))

# Set variables and paths specific to island and year
setwd(here("SRI","2018","Adults and Pups"))
island <- "SRI"
year <- "2018"

if (island == "SMI"){
  islandArea <- 3766
  
} else if (island == "SRI"){
  islandArea <- 21553
}


# Get a list of those Rdata files
allFiles <- list.files(path = ".", pattern = "Rdata")

# loop through them and load them into the workspace
for (file in allFiles){
  dataName <- tools::file_path_sans_ext(file)
  print(glue("Loading {dataName} into workspace"))
  assign(glue("{dataName}"), loadRData(file))
}



#########################################
#  Colin Rundel's technique using purrr.#
#########################################
allModels <- map(ls(pattern = "Model"), get)
modelNames <- tools::file_path_sans_ext(allFiles)
names(allModels) <- modelNames

# Using purrr
allModels_predict <- allModels %>% 
  mutate(predict = purrr::map(allModels, ~predict(.x, newdata = NULL, type = c('response','link'), se.fit = TRUE, alpha = .2))
         ) %>%
  mutate(
    # Adult females are always in the 1st model
    females_adults = purrr::map(predict, 1),
    # Adult females are always in the 1st model
    males_adults   = purrr::map(predict, 2),
    # Pups data can be in either 3 or 5 for femals and 4 or 6 for males
    # If the list is of length 4, it's the 3rd, otherwise 5th
    females_pups   = ifelse(purrr::map(predict, get_len) == 4,
                            purrr::map(predict, 3), 
                            purrr::map(predict, 5)),
    # If the list is of length 4, it's the 4th, otherwise 6th
    males_pups     = ifelse(purrr::map(predict, get_len) == 4,
                            purrr::map(predict, 4), 
                            purrr::map(predict, 6))
    
  )


# Print out into a formatted list so you can use them all in the AIC function. 
# This is a total hack because I can't figure out how to loop on the AIC function.
for (model in modelNames){
  print(glue("{model},"))
}


# Cut and paste the output from the above list list of models into the AIC function. 
AIC <- AIC(Model001_session_1_1,
           Model002_session_1_session,
           Model003_session_1_h2,
           Model004_session_1_b,
           Model005_session_1_B,
           Model006_session_1_session_h2,
           Model007_session_1_session_b,
           Model008_session_1_session_B,
           Model009_session_1_h2_b,
           Model010_session_1_h2_B,
           Model011_session_1_session_h2_b,
           Model012_session_1_session_h2_B,
           Model013_session_session_1,
           Model014_session_session_session,
           Model015_session_session_h2,
           Model016_session_session_b,
           Model017_session_session_B,
           Model018_session_session_session_h2,
           Model019_session_session_session_b,
           Model020_session_session_session_B,
           Model021_session_session_h2_b,
           Model022_session_session_h2_B,
           Model023_session_session_session_h2_b,
           Model024_session_session_session_h2_B,
           Model025_session_h2_1,
           Model026_session_h2_session,
           Model027_session_h2_h2,
           Model028_session_h2_b,
           Model029_session_h2_B,
           Model030_session_h2_session_h2,
           Model031_session_h2_session_b,
           Model032_session_h2_session_B,
           Model033_session_h2_h2_b,
           Model034_session_h2_h2_B,
           Model035_session_h2_session_h2_b,
           Model036_session_h2_session_h2_B,
           Model037_session_b_1,
           Model038_session_b_session,
           Model039_session_b_h2,
           Model040_session_b_b,
           Model041_session_b_session_h2,
           Model042_session_b_session_b,
           Model043_session_b_h2_b,
           Model044_session_b_session_h2_b,
           Model045_session_B_1,
           Model046_session_B_session,
           Model047_session_B_h2,
           Model048_session_B_B,
           Model049_session_B_session_h2,
           Model050_session_B_session_B,
           Model051_session_B_h2_B,
           Model052_session_B_session_h2_B,
           Model053_session_session_h2_1,
           Model054_session_session_h2_session,
           Model055_session_session_h2_h2,
           Model056_session_session_h2_b,
           Model057_session_session_h2_B,
           Model058_session_session_h2_session_h2,
           Model059_session_session_h2_session_b,
           Model060_session_session_h2_session_B,
           Model061_session_session_h2_h2_b,
           Model062_session_session_h2_h2_B,
           Model063_session_session_h2_session_h2_b,
           Model064_session_session_h2_session_h2_B,
           Model065_session_session_b_1,
           Model066_session_session_b_session,
           Model067_session_session_b_h2,
           Model068_session_session_b_b,
           Model069_session_session_b_session_h2,
           Model070_session_session_b_session_b,
           Model071_session_session_b_h2_b,
           Model072_session_session_b_session_h2_b,
           Model073_session_session_B_1,
           Model074_session_session_B_session,
           Model075_session_session_B_h2,
           Model076_session_session_B_B,
           Model077_session_session_B_session_h2,
           Model078_session_session_B_session_B,
           Model079_session_session_B_h2_B,
           Model080_session_session_B_session_h2_B,
           Model081_session_h2_b_1,
           Model082_session_h2_b_session,
           Model083_session_h2_b_h2,
           Model084_session_h2_b_b,
           Model085_session_h2_b_session_h2,
           Model086_session_h2_b_session_b,
           Model087_session_h2_b_h2_b,
           Model088_session_h2_b_session_h2_b,
           Model089_session_h2_B_1,
           Model090_session_h2_B_session,
           Model091_session_h2_B_h2,
           Model092_session_h2_B_B,
           Model093_session_h2_B_session_h2,
           Model094_session_h2_B_session_B,
           Model095_session_h2_B_h2_B,
           Model096_session_h2_B_session_h2_B,
           Model097_session_session_h2_b_1,
           Model098_session_session_h2_b_session,
           Model099_session_session_h2_b_h2,
           Model100_session_session_h2_b_b,
           Model101_session_session_h2_b_session_h2,
           Model102_session_session_h2_b_session_b,
           Model103_session_session_h2_b_h2_b,
           Model104_session_session_h2_b_session_h2_b,
           Model105_session_session_h2_B_1,
           Model106_session_session_h2_B_session,
           Model107_session_session_h2_B_h2,
           Model108_session_session_h2_B_B,
           Model109_session_session_h2_B_session_h2,
           Model110_session_session_h2_B_session_B,
           Model111_session_session_h2_B_h2_B,
           Model112_session_session_h2_B_session_h2_B)

AIC

# Female adults
females_adults_D <- generate_table("females_adults", 1)  %>% 
  rbind(., as.numeric(.[nrow(.),]) * islandArea)
rownames(females_adults_D)[nrow(females_adults_D) - 1]<-"Total"
rownames(females_adults_D)[nrow(females_adults_D)]<-"IslandArea" 

write.xlsx(females_adults_D,"females_adults_D.xlsx")


females_adults_g0 <- generate_table("females_adults", 2) 
females_adults_sigma <- generate_table("females_adults", 3) 

# Female pups
females_pups_D <- generate_table("females_pups", 1)  %>% 
  rbind(., as.numeric(.[nrow(.),]) * islandArea)
rownames(females_pups_D)[nrow(females_pups_D) - 1]<-"Total"
rownames(females_pups_D)[nrow(females_pups_D)]<-"IslandArea" 


females_pups_g0 <- generate_table("females_pups", 2) 
females_pups_sigma <- generate_table("females_pups", 3)


# Male adults
males_adults_D <- generate_table("males_adults", 1)  %>% 
  rbind(., as.numeric(.[nrow(.),]) * islandArea)
rownames(males_adults_D)[nrow(males_adults_D) - 1]<-"Total"
rownames(males_adults_D)[nrow(males_adults_D)]<-"IslandArea" 


males_adults_g0 <- generate_table("males_adults", 2) 
males_adults_sigma <- generate_table("males_adults", 3) 

# Male pups
males_pups_D <- generate_table("males_pups", 1)  %>% 
  rbind(., as.numeric(.[nrow(.),]) * islandArea)
rownames(males_pups_D)[nrow(males_pups_D) - 1]<-"Total"
rownames(males_pups_D)[nrow(males_pups_D)]<-"IslandArea" 


males_pups_g0 <- generate_table("males_pups", 2) 
males_pups_sigma <- generate_table("males_pups", 3)






