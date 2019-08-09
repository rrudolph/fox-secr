# Purpose: Processes the secr fit models from step 2 and produce an output population estimate. 
# Authors: R. Rudolph, GISP; Adam Dillon
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
setwd(here("ExampleData"))
island <- "SMI"
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


allModels <- map(ls(pattern = "Model"), get)
modelNames <- tools::file_path_sans_ext(allFiles)
names(allModels) <- modelNames


# Run map on the models to get a new variable with the predict for each sex and or age
allModels_predict <- purrr::map(allModels, ~predict(.x, newdata = NULL, type = c('response','link'), se.fit = TRUE, alpha = .2))



# Print out into a formatted list so you can use them all in the AIC function. 
# This is a total hack because I can't figure out how to loop on the AIC function.
for (model in modelNames){
  print(glue("{model},"))
}


# Cut and paste the output from the above list list of models into the AIC function. 
AIC <- AIC(Model01_1_1_1,
           Model02_1_1_h2,
           Model03_1_1_b,
           Model04_1_1_B,
           Model05_1_1_h2_b,
           Model06_1_1_h2_B,
           Model07_1_h2_1,
           Model08_1_h2_h2,
           Model09_1_h2_b,
           Model10_1_h2_B,
           Model11_1_h2_h2_b,
           Model12_1_h2_h2_B,
           Model13_1_b_1,
           Model14_1_b_h2,
           Model15_1_b_b,
           Model16_1_b_h2_b,
           Model17_1_B_1,
           Model18_1_B_h2,
           Model19_1_B_B,
           Model20_1_B_h2_B,
           Model21_1_h2_b_1,
           Model22_1_h2_b_h2,
           Model23_1_h2_b_b,
           Model24_1_h2_b_h2_b,
           Model25_1_h2_B_1,
           Model26_1_h2_B_h2,
           Model27_1_h2_B_B,
           Model28_1_h2_B_h2_B)

AIC

# Set the output to full numbers and not exponents.
options("scipen"=100, "digits"=4)

# Generate the tables, optionally spit out an excel spreadsheet if desired. 

# Females 1
females_D <- generate_table(1, 1)  %>% 
  rbind(., as.numeric(.[nrow(.),]) * islandArea)
females_D[nrow(females_D),1]<-"IslandArea" 

write.xlsx(females_D, glue("{island}_{year}_females_D.xlsx"))


females_g0 <- generate_table(1, 2) 

females_sigma <- generate_table(1, 3) 



# Males 2
males_D <- generate_table(2, 1) %>% 
  rbind(., as.numeric(.[nrow(.),]) * islandArea)

rownames(males_D)[nrow(males_D) - 1]<-"Total"
rownames(males_D)[nrow(males_D)]<-"IslandArea" 
write.xlsx(males_D, glue("{island}_{year}_males_D.xlsx"))


males_g0 <- generate_table(2, 2) 
males_sigma <- generate_table(2, 3) 


