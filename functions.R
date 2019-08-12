# Functions for processing secr data.
# Authors: Rocky Rudolph, GISP, Channel Islands National Park
#          Adam Dillon, PhD Candidate - Colorado State University
# Date: 8/8/2019


###################
#  Set functions  #
###################

# Load .Rdata files by the name of the file.
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
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
  
  # Set what row to get the data from based on user input
  if (param == "D"){
    tableRow = 1
  }else if (param == "g0"){
    tableRow = 2
  }else if (param == "sigma"){
    tableRow = 3
  } else stop("Error, please select 'D', 'g0', or 'sigma'")
  
  print(glue("Table row selected: {tableRow}"))
  
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
    elem <- as_tibble(allModels_predict[[glue("{model}")]][[index]][tableRow,], rownames = NULL) %>% 
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
