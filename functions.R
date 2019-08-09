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
    elem <- as_tibble(allModels_predict[[glue("{model}")]][[sex]][row,], rownames = NULL) %>% 
      select(-link) %>%
      mutate(ModelName = model) %>%
      column_to_rownames(var = "ModelName")
    
    tempList[[i]] <- elem # add it to your list
    i <- i + 1
  }
  
  combined_table <- do.call(rbind, tempList)
  
  sorted_table <- combined_table %>% 
    sort_with_aic() %>%
    rownames_to_column('ModelName') %>%
    mutate(AICcwt = AIC[ ,8]) %>% 
    mutate(AIC_estimate = estimate * AICcwt) %>% 
    mutate(AIC_SE.estimate = SE.estimate * AICcwt) %>% 
    mutate(AIC_lcl = lcl * AICcwt) %>% 
    mutate(AIC_ucl = ucl * AICcwt) %>%  
    adorn_totals("row") 
  
  return(sorted_table)
  
}
