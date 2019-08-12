# Purpose: Processes the secr fit models from step 2 and produce an output population estimate. 
# Authors: Rocky Rudolph, GISP, Channel Islands National Park
#          Adam Dillon, PhD Candidate - Colorado State University
# Date: 8/8/2019


library(glue)
library(tidyverse)
library(janitor)
library(readxl)
library(here)
library(secr)
library(fs)

# Clear any environment variables in memory.
rm(list = ls())

# Load functions.
here()
source(here("functions.R"))

# Set variables and paths specific to island and year.
setwd(here("SRI", "2018", "Adults and pups" ))
island <- "SRI"
year <- "2018"

island_areas <- tribble(
  ~island, ~area_km2,
  "SMI", 3766,
  "SRI", 21553)
islandArea <- island_areas %>% 
  filter(island == !!island) %>% 
  pull(area_km2)

# Get a list of Rdata files in the specified working directory.
#allFiles <- list.files(path = ".", pattern = "Rdata")
allFiles <- list.files(path = "ExampleData", pattern = "Rdata")

# Load all data files into the workspace. 
for (file in allFiles){
  dataName <- tools::file_path_sans_ext(file)
  print(glue("Loading {dataName} into workspace"))
  assign(glue("{dataName}"), loadRData(file))
}

# rds preferred over rdata b/c explicit value saved and doesn't corrupt namespace with who knows what
rdatas <- list.files("ExampleData", "Rdata$", full.names=T)
dir.create("data_rds", showWarnings = F)
for (rdata in rdatas){ # rdata = rdatas[1]
  mdl_rds <- file.path("data_rds", path_ext_set(basename(rdata), "rds"))
  load(rdatas[1])
  saveRDS(fit, mdl_rds)
}

mdls <- tibble(
  rds  = list.files("data_rds", "rds$", full.names=T),
  name = fs::path_ext_remove(basename(rdata)),
  fit  = map(rds, readRDS),
  pred = map(
    fit,
    predict, 
    newdata = NULL, type = c('response','link'), se.fit = TRUE, alpha = .2),
  aic = map(fit, secrlist))

# Set the output to full numbers and not exponents.
options("scipen"=100, "digits"=4)

# ### Generate the tables, optionally spit out an excel spreadsheet if desired.
# Choices are 'female', 'male', 'adult', 'pup',  'D', 'g0', or 'sigma'

# Females 
females_D <- generate_table("female", "adult", "D")  %>% 
  rbind(., as.numeric(.[nrow(.),]) * islandArea)
females_D[nrow(females_D),1]<-"*IslandArea" 
females_g0 <- generate_table("female", "adult", "g0")
females_sigma <- generate_table("female", "adult", "sigma")


# Males 
males_D <- generate_table("male", "adult", "D") %>% 
  rbind(., as.numeric(.[nrow(.),]) * islandArea)
males_g0 <- generate_table("male", "adult", "g0")
males_sigma <- generate_table("male", "adult", "sigma") 

# Pups

females_pups_D <- generate_table("female", "pup", "D") %>% 
  rbind(., as.numeric(.[nrow(.),]) * islandArea)

males_pups_D <- generate_table("male", "pup", "D") %>% 
  rbind(., as.numeric(.[nrow(.),]) * islandArea)
