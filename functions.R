# Functions for processing secr data.
# Authors: Rocky Rudolph, GISP, Channel Islands National Park
#          Adam Dillon, PhD Candidate - Colorado State University
# Date: 8/8/2019


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
  for(i in 1:nrow(all_AIC)){
    temp.row.name <- rownames(all_AIC)[i]
    temp.row.estimate <- table[rownames(table)==temp.row.name, ]
    temp_sort <- rbind(temp_sort,temp.row.estimate)
  }
  return(temp_sort)
}

# Generate a table based on the list of models. This function
# requires the sex (the name of the model in allModels) and the row within that model that contains the data. 
generate_table <- function(sex, age, param){
  print(glue("Sex: {sex} Age: {age} Param: {param}"))
  
  # Set what row to get the data from based on user input
  if (param == "D"){
    tableRow = 1
  }else if (param == "g0"){
    tableRow = 2
  }else if (param == "sigma"){
    tableRow = 3
  } else stop("Error, please select 'D', 'g0', or 'sigma'")
  
  
  tempList = list()
  i <- 1
  # Loop through the models to get the row for each one.
  for (model in modelNames) {
    
    modelLen <- length(allModels_predict[[glue("{model}")]])
    
    # Use some logic to get the right data. The pups models have 8 tables, so
    # there needs to be a way to get the right ones based on what the user needs
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
    }else stop("Error, please check your adults/pups parameters and try again")
    
    # Get a row based on user input and the logic above
    elem <- as_tibble(allModels_predict[[glue("{model}")]][[index]][tableRow,], rownames = NULL) %>% 
      select(-link) %>% # remove the link row that nobody needs.
      mutate(ModelName = model) %>% # put the model name in the table
      column_to_rownames(var = "ModelName") # turn the model name int the row name.
    
    tempList[[i]] <- elem # add it to your list
    i <- i + 1
  }
  
  # Merge all the data into one table
  combined_table <- do.call(rbind, tempList)
  
  # Sort the table with AIC, add model names to the table, and add some needed
  # calculations to the table
  sorted_table <- combined_table %>% 
    sort_with_aic() %>%
    rownames_to_column('ModelName') %>%
    mutate(AICcwt = all_AIC[ ,8]) %>% 
    mutate(AIC_estimate = estimate * AICcwt) %>% 
    mutate(AIC_SE.estimate = SE.estimate * AICcwt) %>% 
    mutate(AIC_lcl = lcl * AICcwt) %>% 
    mutate(AIC_ucl = ucl * AICcwt) %>%  
    adorn_totals("row") # add a row to the bottom with totals
  
  return(sorted_table)
  
}
