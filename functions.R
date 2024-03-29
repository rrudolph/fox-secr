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
    print(glue('"{name}" = "",'))
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
    
    modelLen <- length(allModels_80percent[[glue("{model}")]])

    # Use logic to  get the correct data. If the models include pups,
    # some will have 8 sets of data, so any model that has more than 4
    # lists will need to be able to select the correct row based on user input. 
    if (age == "adult" & sex == "female") {
      index = 1
    }else if (age == "adult" & sex == "male"){
      index = 2
    }else if (age == "pup" & sex == "female" & modelLen == 4){
      index = 3
    }else if (age == "pup" & sex == "male"   & modelLen == 4){
      index = 4
    }else if (age == "pup" & sex == "female" & modelLen > 4){
      index = 5
    }else if (age == "pup" & sex == "male"   & modelLen > 4){
      index = 6
    }else stop(glue("Error, please check your adults/pups parameters and try again \n
               Or are you running this on a model with no pups?? Model length is {modelLen}"))
    
    print(glue("Index selected: {index} Model: {model}"))
    
    # Using above logic choice and model loop, compile the row of data and add it to the list. 
    selectedRow <- as_tibble(allModels_80percent[[glue("{model}")]][[index]][param,], rownames = NULL) %>% 
       select(-link) %>% # remove the unnecessary link column.
      mutate(ModelName = model) %>% # insert model name into the table.
      column_to_rownames(var = "ModelName") # convert model name to row name.
    
    tempList[[i]] <- selectedRow # add the row to the list.
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
    mutate(ModelAveraged_estimate = estimate * AICcwt) %>% 
    mutate(ModelAveraged_SE.estimate = SE.estimate * AICcwt) %>% 
    mutate(ModelAveraged_lcl = lcl * AICcwt) %>% 
    mutate(ModelAveraged_ucl = ucl * AICcwt) %>%  
    adorn_totals("row") # add a row to the bottom with totals.
  
  return(sorted_table)
  
}

# Export an excel spreadsheet based on inputs.  
# If Density is the input, add the island area calculation at the bottom of the table. 
write_table <- function(sex, age, param){
  temp <- generate_table(sex, age, param) 
  if (param == "D"){
    temp <- rbind(temp, as.numeric(temp[nrow(temp),]) * islandArea)
    temp[nrow(temp),1]<-"*IslandArea"
  }
  # Removing this because of the difficulties with Java
  # write_xlsx(temp, glue("{island}_{year}.xlsx"), sheetName=glue("{sex}_{age}_{param}"), append=T)
  write_xlsx(temp, glue("{island}_{year}_{sex}_{age}_{param}.xlsx"))
}
