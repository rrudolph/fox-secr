# Purpose: Input Detection and Capture files from previous step and perform secr.fit model on 
# a series of fomulas. A for loop performs the fit to all combinatons of model formula variables. 
# Authors: Rocky Rudolph, GISP, Channel Islands National Park
#          Adam Dillon, PhD Candidate - Colorado State University
# Date: 1/30/2019

library(secr)
library(sp)
library(maptools)
library(readr)
library(glue)
library(tools)
library(dplyr)
library(here)
here()

# Clear any environment variables in memory
rm(list = ls())

# Set variables
setwd(here("SRI", "2018"))
inputBufferDir <- here("Master Grid Buffers")
island <- "SRI"
year <- "2018"
adultsOnly <- T

# Set pixel spacing. 100m for Rosa, 50m for Miguel. 
if (island == "SMI"){
  pixelSpacing <- 50
  inputBufferShape <- "SMI_clip_900m_UTM10"
  
} else if (island == "SRI"){
  pixelSpacing <- 100
  inputBufferShape <- "SRI_clip_900m_UTM10"
  
}

# Formula list is a list of combinations of formulas for input into the secr.fit function. 
if (adultsOnly == T){
  formulaList <- read_csv(here("Formulas", "formulaList_28.csv"))
} else {
  formulaList <- read_csv(here("Formulas", "formulaList_112.csv"))
}

formulaList$Density <- as.character(formulaList$Density)

formulaList <- formulaList %>%
  mutate(LongName = paste(ModelName, Density, g0, s, sep = "_"))


Capthist <- read.capthist(captfile="Capture_File.txt", 
                          trapfile="Detection_File.txt", 
                          detector = "multi", # physical traps
                          fmt="trapID", #  ID field links the two txt input files
                          noccasions=6, # Number of nights of trapping per grid
                          covnames = c("sex")) # anything after the basic fields is a co-variate

# Inspect the data, this is optional
par(mar = c(1,1,1,1)) # reduce margins
plot(Capthist, tracks = T)
summary(Capthist)

m <- unlist(moves(Capthist))
hist(m,  xlab = "Movement m", main = "")
plot(ecdf(m))
initialsigma <- RPSV(Capthist, CC = TRUE)
cat("Quick and biased estimate of sigma =", initialsigma, "m\n")


Polygon <- rgdal::readOGR(inputBufferDir, inputBufferShape) # 900 meter buffer for both SRI and SMI clipped to shoreline. 
plot(Polygon,col = 'red')

#  Specify session=c(1,1) if including pups
if (adultsOnly == T){
  print("Selected Adults Mask Version")
  Mask <- make.mask(traps(Capthist), 
                    spacing = pixelSpacing,  
                    type = "polygon",
                    poly = Polygon)
} else {
  print("Selected Pups Mask Version")
  Mask <- make.mask(traps(Capthist), 
                    spacing = pixelSpacing,
                    session=c(1,1),
                    type = "polygon",
                    poly = Polygon)
}


plot(Mask, add = T, col = 'blue')
# Look at the capture data again, just for fun.
plot(Capthist, tracks = T, add = T)
summary(Mask)

# png(filename="Model_Test_Plot.png", width = 1920, height = 1080)
# plot(Capthist, tracks = T)
# dev.off()

# Enable logging
dateStamp <- format(Sys.time(), "%Y%m%d")
sink(glue("log_{island}_{year}_{dateStamp}.txt"), append = T, split = T)

# Using the values of the formulaList data frame, loop through all combinations of models.
for (row in 1:nrow(formulaList)) {  
  modelName <-  formulaList[row, "LongName"]
  Density <- formulaList[row, "Density"]
  group <- formulaList[row, "g0"]
  sig <- formulaList[row, "s"]
  
  print(glue("Running {modelName}"))
  
  modelList <- list(as.formula(glue("D ~ {Density}")), as.formula(glue("g0 ~ {group}")), as.formula(glue("sigma ~ {sig}")))
  
  fit <- secr.fit(Capthist,
                  model = modelList ,
                  trace = TRUE,  # Trace shows the verbose output of the fit.
                  mask=Mask,
                  detectfn=0, # detectfn 0 is halfnormal.
                  hcov='sex') # hcov is the h covariate

  # Remove the '+' signs in the file name
  modelNameSave <- gsub("\\+", "_", glue("{modelName}"))

  saveRDS(fit, file=glue('{modelNameSave}.rds'))
}

# Disable logging.
sink()



