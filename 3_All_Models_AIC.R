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


# Clear any environment variables in memory.
rm(list = ls())

# Load functions.
here()
source(here("functions.R"))

# Set variables and paths specific to island and year.
island <- "SMI"
year <- "2019"
setwd(here(island, year, "First run"))

island_areas <- tribble(
  ~island, ~area_km2,
  "SMI", 3766,
  "SRI", 21553)

# Get island area based on user input from variables
islandArea <- island_areas %>% 
  filter(island == !!island) %>% 
  pull(area_km2)


# Get a list of Rdata files in the specified working directory.
allFiles <- list.files(path = ".", pattern = "rds") # change patteren if .rds or .Rdata

# Load all data files into the workspace. 
for (file in allFiles){
  dataName <- tools::file_path_sans_ext(file)
  print(glue("Loading {dataName} into workspace"))
  assign(glue("{dataName}"), readRDS(file)) # loadRData() function for .Rdata files, readRDS() for .rds files.
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

# Sex and params are the same for both pups and adults.
sex <- c("male", "female")
param <- c("D", "g0", "sigma")

# Get the length of the models to determin if it includes pups.
modelLength <- length(allModels_predict)

# Include pups if the 112 model. Use expand.grid to make a matrix of inputs.
if (modelLength == 28){
  age <- c("adult")
  param_matrix <- as.data.frame.matrix(expand.grid(sex, age, param))
} else if (modelLength == 112){
  age <- c("adult", "pup")
  param_matrix <- as.data.frame.matrix(expand.grid(sex, age, param))
}


# Get elements from the parameter matrix generate above. 
# For each one run the write_table function to make an excel output.
for (row in 1:nrow(param_matrix)){
  sex <- param_matrix[row, 1]
  age <- param_matrix[row, 2]
  param <- param_matrix[row, 3]
  print(glue("Writing table for {sex}_{age}_{param}"))
  write_table(sex, age, param)
  
}


