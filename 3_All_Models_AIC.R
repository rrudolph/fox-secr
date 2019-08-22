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
setwd(here("SRI", "2018", "Adults new buffer"))
island <- "SRI"
year <- "2018"

island_areas <- tribble(
  ~island, ~area_km2,
  "SMI", 3766,
  "SRI", 21553)

# Get island area based on user input from variables
islandArea <- island_areas %>% 
  filter(island == !!island) %>% 
  pull(area_km2)


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

export_summary_xls(allModels_predict)


