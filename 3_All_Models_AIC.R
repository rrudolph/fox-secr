# Purpose: Processes the secr fit models from step 2 and produce an output population estimate. 
# Authors: Rocky Rudolph, GISP, Channel Islands National Park
#          Adam Dillon, PhD Candidate - Colorado State University
# Date: 8/8/2019


library(glue)
library(tidyverse)
library(janitor)
library(xlsx)
library(here)
library(secr)
library(fs)

# Clear any environment variables in memory.
rm(list = ls())

# Load functions.
here()
source(here("functions.R"))

# Set variables and paths specific to island and year.
setwd(here("SMI", "2018", "Adult models" ))
island <- "SMI"
year <- "2018"

if (island == "SMI"){
  islandArea <- 3766
  
} else if (island == "SRI"){
  islandArea <- 21553
}

# Get a list of Rdata files in the specified working directory.
allFiles <- list.files(path = ".", pattern = "Rdata")

# Load all data files into the workspace. 
for (file in allFiles){
  dataName <- tools::file_path_sans_ext(file)
  print(glue("Loading {dataName} into workspace"))
  assign(glue("{dataName}"), loadRData(file))
}


# Create one variable containing a list of all models. 
allModels <- map(ls(pattern = "Model"), get)

# Name the models properly. 
modelNames <- tools::file_path_sans_ext(allFiles)
names(allModels) <- modelNames

# Apply the predict function to all models.
allModels_predict <- purrr::map(allModels, 
                                ~predict(.x, 
                                         newdata = NULL, 
                                         type = c('response','link'), 
                                         se.fit = TRUE, 
                                         alpha = .2))

# Generate AIC table. The secrlist finction puts the data into a format that
# allows it to be entered into a variable. 
all_AIC_temp <- secrlist(allModels)
names(all_AIC_temp) <- modelNames
all_AIC <- AIC(all_AIC_temp)


# Set the output to full numbers and not exponents.
options("scipen"=100, "digits"=4)

# ### Generate the tables, optionally spit out an excel spreadsheet if desired.
# Choices are 'female', 'male', 'adult', 'pup',  'D', 'g0', or 'sigma'

# Females 
females_D <- generate_table("female", "adult", "D")  %>% 
  rbind(., as.numeric(.[nrow(.),]) * islandArea)
females_D[nrow(females_D),1]<-"*IslandArea" 
write.xlsx(females_D, glue("{island}_{year}.xlsx"), sheetName="females_adults_D", append=T)

females_g0 <- generate_table("female", "adult", "g0")
write.xlsx(females_g0,glue("{island}_{year}.xlsx"), sheetName="females_adults_g0", append=T)

females_sigma <- generate_table("female", "adult", "sigma")
write.xlsx(females_sigma,glue("{island}_{year}.xlsx"), sheetName="females_adults_sigma", append=T)


# Males 
male_D <- generate_table("male", "adult", "D")  %>% 
  rbind(., as.numeric(.[nrow(.),]) * islandArea)
male_D[nrow(male_D),1]<-"*IslandArea" 
write.xlsx(male_D, glue("{island}_{year}.xlsx"), sheetName="male_adults_D", append=T)

male_g0 <- generate_table("male", "adult", "g0")
write.xlsx(male_g0,glue("{island}_{year}.xlsx"), sheetName="male_adults_g0", append=T)

male_sigma <- generate_table("male", "adult", "sigma")
write.xlsx(male_sigma,glue("{island}_{year}.xlsx"), sheetName="male_adults_sigma", append=T)


# Pups
females_D_pups <- generate_table("female", "pup", "D")  %>% 
  rbind(., as.numeric(.[nrow(.),]) * islandArea)
females_D_pups[nrow(females_D_pups),1]<-"*IslandArea" 
write.xlsx(females_D_pups, glue("{island}_{year}.xlsx"), sheetName="females_pups_D", append=T)

females_g0pups <- generate_table("female", "pup", "g0")
write.xlsx(females_g0,glue("{island}_{year}.xlsx"), sheetName="females_pups_g0", append=T)

females_sigmapups <- generate_table("female", "pup", "sigma")
write.xlsx(females_sigma,glue("{island}_{year}.xlsx"), sheetName="females_pups_sigma", append=T)

male_D_pups <- generate_table("male", "pup", "D")  %>% 
  rbind(., as.numeric(.[nrow(.),]) * islandArea)
male_D_pups[nrow(male_D_pups),1]<-"*IslandArea" 
write.xlsx(male_D_pups, glue("{island}_{year}.xlsx"), sheetName="male_pups_D", append=T)

male_g0_pups <- generate_table("male", "pup", "g0")
write.xlsx(male_g0,glue("{island}_{year}.xlsx"), sheetName="male_pups_g0", append=T)

male_sigma_pups <- generate_table("male", "pup", "sigma")
write.xlsx(male_sigma,glue("{island}_{year}.xlsx"), sheetName="male_pups_sigma", append=T)


# TODO: Add a for loop to make the tables.  If model == 28, use adults, if model == 112, use pups. 
# if (length(allModels_predict) == 112){
#   print("Model list has pups")
#   modelCriteria = c()
# }else{
#   
# }



