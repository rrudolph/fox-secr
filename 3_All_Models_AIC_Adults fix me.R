# Purpose: Processes the secr fit models from step 2 and produce an output population estimate. 
# Authors: R. Rudolph, GISP; Adam Dillon
# Date: 1/30/2019


library(glue)
library(tidyverse)
library(janitor)
library(xlsx)


# Clear any environment variables in memory
rm(list = ls())

###################
#  Set functions  #
###################

# Function for returning the length of a model. Used for the adults/pups step 3
get_len <- function(model){
  return(length(model))
}


# Make a function that loads .Rdata files by the name of the file
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}


# Sort a table with AIC score 
sort_with_aic <- function(table){
  temp_sort <- NULL
  for(i in 1:nrow(AIC)){
    temp.row.name <- rownames(AIC)[i]
    temp.row.estimate <- table[rownames(table)==temp.row.name, ]
    temp_sort <- rbind(temp_sort,temp.row.estimate)
  }
  return(temp_sort)
}

# Generate a table based on the list of models. This function
# requires the sex (the name of the model in allModels) and the row within that model that contains the data. 
generate_table <- function(sex, row){
  tempList = list()
  i <- 1
  for (model in modelNames) {
    elem <- as_tibble(allModels_predict[[glue("{sex}")]][[glue("{model}")]][row,], rownames = NULL) %>% 
      select(-link) %>%
      mutate(ModelName = model) %>%
      column_to_rownames(var = "ModelName")
    
    tempList[[i]] <- elem # add it to your list
    i <- i + 1
  }
  
  combined_table <- do.call(rbind, tempList)
  
  sorted_table <- combined_table %>% 
    sort_with_aic() %>%
    mutate(AICcwt = AIC[ ,8]) %>% 
    mutate(AIC_estimate = estimate * AICcwt) %>% 
    mutate(AIC_SE.estimate = SE.estimate * AICcwt) %>% 
    mutate(AIC_lcl = lcl * AICcwt) %>% 
    mutate(AIC_ucl = ucl * AICcwt) %>%  
    adorn_totals("row", name = "-999") 
  
  return(sorted_table)
  
}


# Set variables and paths specific to island and year
setwd("C:/Users/RRudolph/Documents/R Projects/Fox secr/SRI/2018/Adults new buffer")
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

# Use purrr to make a big list of lists of all the models
allModels_predict <- allModels %>% 
  mutate(predict = purrr::map(allModels, ~predict(.x, newdata = NULL, type = c('response','link'), se.fit = TRUE, alpha = .2))
         ) %>%
  mutate(
    females = purrr::map(predict, 1),
    males   = purrr::map(predict, 2)
  )

# Trying to fix it, suddenly totally F'ed up
allModels_predict <- dplyr::tibble(
  index = 1:28,
  models = list(allModels)
)
   %>% 
 dplyr::mutate(predict = purrr::map(models, ~predict(.x, newdata = NULL, type = c('response','link'), se.fit = TRUE, alpha = .2))
  ) %>%
  mutate(
    females = purrr::map(predict, 1),
    males   = purrr::map(predict, 2)
  )



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

# Females 
females_D <- generate_table("females", 1)  %>% 
  rbind(., as.numeric(.[nrow(.),]) * islandArea)

rownames(females_D)[nrow(females_D) - 1]<-"Total"
rownames(females_D)[nrow(females_D)]<-"IslandArea" 
write.xlsx(females_D, glue("{island}_{year}_females_D.xlsx"))



females_g0 <- generate_table("females", 2) 

females_sigma <- generate_table("females", 3) 



# Males
males_D <- generate_table("males", 1) %>% 
  rbind(., as.numeric(.[nrow(.),]) * islandArea)

rownames(males_D)[nrow(males_D) - 1]<-"Total"
rownames(males_D)[nrow(males_D)]<-"IslandArea" 
write.xlsx(males_D, glue("{island}_{year}_males_D.xlsx"))


males_g0 <- generate_table("males", 2) 
males_sigma <- generate_table("males", 3) 


##### Illustrate single row extract

exampleModel2 <- as_tibble(allModels[["females"]][["Model01_1_1_1"]][2,], rownames = NULL) %>% 
  select(-link) %>%
  mutate(ModelName = model) %>%
  column_to_rownames(var = "ModelName")

##### Illustrate single row extract for all models
tempList = list()
i <- 1
for (model in modelNames) {
  elem <- as_tibble(allModels[["females"]][[glue("{model}")]][2,], rownames = NULL) %>% 
    select(-link) %>%
    mutate(ModelName = model) %>%
    column_to_rownames(var = "ModelName")
  
  tempList[[i]] <- elem # add it to your list
  i <- i + 1
}

combined_table <- do.call(rbind, tempList)

sorted_table <- combined_table %>% 
  sort_with_aic() %>%
  mutate(AICcwt = AIC[ ,8]) %>% 
  mutate(AIC_estimate = estimate * AICcwt) %>% 
  mutate(AIC_SE.estimate = SE.estimate * AICcwt) %>% 
  mutate(AIC_lcl = lcl * AICcwt) %>% 
  mutate(AIC_ucl = ucl * AICcwt) %>%  
  adorn_totals("row", name = "-999") 
