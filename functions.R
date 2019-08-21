# Functions for processing secr data.
# Authors: Rocky Rudolph, GISP, Channel Islands National Park
#          Adam Dillon, PhD Candidate - Colorado State University
# Date: 8/8/2019


###################
#  Set functions  #
###################

# Load .Rdata files by the name of the file.
# This is only needed for .Rdata files. rds files only need the readRDS function. 
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

# Print out trap grid codes if needed in script 1. 
print_trap_code_list <- function(df_field){
  trapNames <- unique(df_field)
  for (name in trapNames){
    print(glue('levels(captures$GridCode)[levels(captures$GridCode) == "{name}"] <- ""'))
  }
  
}


# To convert to Nad83 UTM Zone 10. EPSG source: http://spatialreference.org/ref/epsg/3157/
to_nad83z10 <- function(x, y, crs_str){
  st_as_sf(data_frame(x=x, y=y), crs=crs_str, coords = c("x", "y")) %>%
    st_transform(crs=3157) %>%
    st_coordinates()
}

# Sort a table with AIC score.
sort_with_aic <- function(table){
  temp_sort <- NULL
  for(i in 1:nrow(all_AIC)){
    temp.row.name <- rownames(all_AIC)[i]
    temp.row.estimate <- table[rownames(table)==temp.row.name, ]
    temp_sort <- rbind(temp_sort,temp.row.estimate)
  }
  return(temp_sort)
}

# Generate a table, extracting from a list of all models.
generate_table <- function(sex, age, param){
  print(glue("Input criteria: Sex {sex} Age {age} Param {param}"))
  
  stopifnot(param %in% c('D', 'g0','sigma'))
  
  tempList = list()
  i <- 1
  # Loop through the models, compiling data based on the row in each table
  # of the master list of data. 
  for (model in modelNames) {
    
    modelLen <- length(allModels_predict[[glue("{model}")]])
    
    # Use logic to get the correct data. If the models include pups,
    # some will have 8 sets of data, so any model that has more than 4
    # lists will need to be able to select the correct row based on user input. 
    if (age == "adult" & sex == "female") {
      index = 1
    }else if (age == "adult" & sex == "male"){
      index = 2
    }else if (age == "pup" & sex == "female" & modelLen == 4){
      index = 3
    }else if (age == "pup" & sex == "male" & modelLen == 4){
      index = 4
    }else if (age == "pup" & sex == "female" & modelLen > 4){
      index = 5
    }else if (age == "pup" & sex == "male" & modelLen > 4){
      index = 6
    }else stop(glue("Error, please check your adults/pups parameters and try again \n
               Or are you running this on a model with no pups?? Model length is {modelLen}"))
    
    print(glue("Index selected: {index} Model: {model}"))
    
    # Using above logic choice and model loop, compile the row of data and add it to the list. 
    elem <- as_tibble(allModels_predict[[glue("{model}")]][[index]][param,], rownames = NULL) %>% 
      select(-link) %>% # remove the unnecessary link column.
      mutate(ModelName = model) %>% # insert model name into the table.
      column_to_rownames(var = "ModelName") # convert model name to row name.
    
    tempList[[i]] <- elem # add the row to the list.
    i <- i + 1
  }
  
    # Merge all the data into a single table.
  combined_table <- do.call(rbind, tempList)
  
  # Sort the table with AIC, add model names to the table, and add some needed
  # calculations to the table.
  sorted_table <- combined_table %>% 
    sort_with_aic() %>%
    rownames_to_column('ModelName') %>%
    mutate(AICcwt = all_AIC[ ,8]) %>% 
    mutate(AIC_estimate = estimate * AICcwt) %>% 
    mutate(AIC_SE.estimate = SE.estimate * AICcwt) %>% 
    mutate(AIC_lcl = lcl * AICcwt) %>% 
    mutate(AIC_ucl = ucl * AICcwt) %>%  
    adorn_totals("row") # add a row to the bottom with totals.
  
  return(sorted_table)
  
}

export_summary_xls <- function(model){
  
  if (length(model) == 28){
    modelType = "Adults"
    #Females 
    females_D <- generate_table("female", "adult", "D")  %>% 
      rbind(., as.numeric(.[nrow(.),]) * islandArea)
    females_D[nrow(females_D),1]<-"*IslandArea" 
    write.xlsx(females_D, glue("{island}_{year}_{modelType}.xlsx"), sheetName="females_adults_D", append=T)
    
    females_g0 <- generate_table("female", "adult", "g0")
    write.xlsx(females_g0,glue("{island}_{year}_{modelType}.xlsx"), sheetName="females_adults_g0", append=T)
    
    females_sigma <- generate_table("female", "adult", "sigma")
    write.xlsx(females_sigma,glue("{island}_{year}_{modelType}.xlsx"), sheetName="females_adults_sigma", append=T)
    
    
    # Males 
    male_D <- generate_table("male", "adult", "D")  %>% 
      rbind(., as.numeric(.[nrow(.),]) * islandArea)
    male_D[nrow(male_D),1]<-"*IslandArea" 
    write.xlsx(male_D, glue("{island}_{year}_{modelType}.xlsx"), sheetName="male_adults_D", append=T)
    
    male_g0 <- generate_table("male", "adult", "g0")
    write.xlsx(male_g0,glue("{island}_{year}_{modelType}.xlsx"), sheetName="male_adults_g0", append=T)
    
    male_sigma <- generate_table("male", "adult", "sigma")
    write.xlsx(male_sigma,glue("{island}_{year}_{modelType}.xlsx"), sheetName="male_adults_sigma", append=T)
    
    

  }else if (length(model) == 112){
    modelType = "Adults_and_Pups"
    #Females 
    females_D <- generate_table("female", "adult", "D")  %>% 
      rbind(., as.numeric(.[nrow(.),]) * islandArea)
    females_D[nrow(females_D),1]<-"*IslandArea" 
    write.xlsx(females_D, glue("{island}_{year}_{modelType}.xlsx"), sheetName="females_adults_D", append=T)
    
    females_g0 <- generate_table("female", "adult", "g0")
    write.xlsx(females_g0,glue("{island}_{year}_{modelType}.xlsx"), sheetName="females_adults_g0", append=T)
    
    females_sigma <- generate_table("female", "adult", "sigma")
    write.xlsx(females_sigma,glue("{island}_{year}_{modelType}.xlsx"), sheetName="females_adults_sigma", append=T)
    
    
    # Males 
    male_D <- generate_table("male", "adult", "D")  %>% 
      rbind(., as.numeric(.[nrow(.),]) * islandArea)
    male_D[nrow(male_D),1]<-"*IslandArea" 
    write.xlsx(male_D, glue("{island}_{year}_{modelType}.xlsx"), sheetName="male_adults_D", append=T)
    
    male_g0 <- generate_table("male", "adult", "g0")
    write.xlsx(male_g0,glue("{island}_{year}_{modelType}.xlsx"), sheetName="male_adults_g0", append=T)
    
    male_sigma <- generate_table("male", "adult", "sigma")
    write.xlsx(male_sigma,glue("{island}_{year}_{modelType}.xlsx"), sheetName="male_adults_sigma", append=T)
    
    
    # Pups
    females_D_pups <- generate_table("female", "pup", "D")  %>% 
      rbind(., as.numeric(.[nrow(.),]) * islandArea)
    females_D_pups[nrow(females_D_pups),1]<-"*IslandArea" 
    write.xlsx(females_D_pups, glue("{island}_{year}_{modelType}.xlsx"), sheetName="females_pups_D", append=T)
    
    females_g0pups <- generate_table("female", "pup", "g0")
    write.xlsx(females_g0,glue("{island}_{year}_{modelType}.xlsx"), sheetName="females_pups_g0", append=T)
    
    females_sigmapups <- generate_table("female", "pup", "sigma")
    write.xlsx(females_sigma,glue("{island}_{year}_{modelType}.xlsx"), sheetName="females_pups_sigma", append=T)
    
    male_D_pups <- generate_table("male", "pup", "D")  %>% 
      rbind(., as.numeric(.[nrow(.),]) * islandArea)
    male_D_pups[nrow(male_D_pups),1]<-"*IslandArea" 
    write.xlsx(male_D_pups, glue("{island}_{year}_{modelType}.xlsx"), sheetName="male_pups_D", append=T)
    
    male_g0_pups <- generate_table("male", "pup", "g0")
    write.xlsx(male_g0,glue("{island}_{year}_{modelType}.xlsx"), sheetName="male_pups_g0", append=T)
    
    male_sigma_pups <- generate_table("male", "pup", "sigma")
    write.xlsx(male_sigma,glue("{island}_{year}_{modelType}.xlsx"), sheetName="male_pups_sigma", append=T)
  }else stop("Model length is neither 28 or 112. Check your predict model input and try again.")
  
  print(glue("File written: {island}_{year}_{modelType}.xlsx"))
  
}
