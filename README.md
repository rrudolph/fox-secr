# fox-secr
Uses the [secr R package](https://www.otago.ac.nz/density/SECRinR.html) for analysis of fox capture data at Channel Islands National Park.  

## Script 1
Imports raw capture data for detailed inspection and UTM coordinate checks. Exports two text files that are needed for secr to run models. Raw data is not provided by this github repo. 

## Script 2
Uses the text files from script 1 as input. Needs 900 m buffer around grids which can be found in Master Grid Buffers folder.
Formula lists are provided by the formulaList_28.csv (adults models) and formulaList_112.csv (adults and pups). A for loop iterates over all formula combinations provided by the csv files and exports a rds file for each model. 

## Script 3
Uses all models generated by script 2 to perform density estimates. functions.R contains the functions that do the real work for this step. Based on weather the input was adults (28 models) or pups and adults (112 models), the script will extract males, females, density, 
g0, sigma, adult, and pup parameters out of the list of models for each model and compare them to the AIC results. Once grouped by type, they are exported to an excel spreadsheet for easy viewing. 
