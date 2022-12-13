# Purpose: To process raw data from Channel Islands National Park Island Fox capture database
# into a format that the secr package can read. Output files are "Detection_File.txt" and
# "Capture_File.txt". The Catpure file will need to be modified slightly to account for any
# double captures or other issues that this script will help to detect.
# Authors: Rocky Rudolph, GISP, Channel Islands National Park
#          Adam Dillon, PhD Candidate - Colorado State University
# Date: 1/30/2019


renv::init()



library(readxl)
library(tidyverse)
library(glue)
library(sp)
library(mapview)
library(leaflet)
library(sf)
library(rgdal)
library(rgeos)
library(here)




# Clear any environment variables in memory
rm(list = ls())

here()
# Bring in the custom functions
source(here("functions.R"))

# Set variables and paths specific to island and year
island <- "SMI"
year <- "2022"

setwd(here(island, year))
adultsOnly = T

# 
# captures <- read_excel("2019 SRI GRID DATA_12.2.2019 export.xlsx",
#                        col_types = c("text", "numeric", "text",
#                                      "date", "numeric", "numeric", "numeric",
#                                      "numeric", "text", "text", "text",
#                                      "skip", "text", "text", "text", "text", "text",
#                                      "text", "numeric", "text", "text",
#                                      "text", "text", "text", "skip", "skip"))

captures <- read_excel("SMI_Grids_2022_12.6.22.xlsx")

### Take a close look at the data ----

# Inspect the data as needed
table(captures$TrapResult, useNA = "always")
table(captures$Datum, useNA = 'always')
table(captures$Sex, useNA = "always")
table(captures$Animal, useNA = "always")
table(captures$SamplingYear, useNA = "always")
table(captures$AgeClass, useNA = "always")

table(captures$TrapResult, captures$Animal, useNA = "always")



# Any missing spatial data?  SRI had a blank entry. 
# If any exist, it will get removed in the UTM inspection processes
captures %>%
  filter(is.na(UTME), is.na(UTMN), is.na(Datum)) 



### Generate the grid code ----

# OPTIONAL: Get trap names to turn them into short codes. 
# This is a helper function to make writing the codes easier. 
# print_trap_code_list(captures$TrapName)

# Any issues with the trap names?
table(captures$TrapName)

# Make a new field called GridCode and populate it with the trap names
captures$GridCode <- captures$TrapName

# Shorten those names to a code

if (island == "SMI"){
  # # SMI
  captures$GridCode <- recode(captures$GridCode,
                              "CARDWELL POINT GRID" = "CP",
                              "CHARCOAL CANYON GRID" = "CC",
                              "JACKASS FLATS GRID" = "JF",
                              "SANDBLAST PASS GRID" = "SP")
}else if (island == "SRI"){
  captures$GridCode <- recode(captures$GridCode,
                              "Arlington Canyon Grid" = "AC",
                              "Arlington Springs Grid" = "AS",
                              "Bee Canyon Grid" = "BC",
                              "Burma Road Grid" = "BR",
                              "Carrington Point Grid" = "CP",
                              "China Camp Grid" = "CC",
                              "Dry Canyon Grid" = "DC",
                              "Johnsons Lee Grid" = "JL",
                              "Lighthouse Road Grid" = "LR",
                              "Old Ranch Canyon Grid" = "OR",
                              "Pocket Field Grid" = "PF",
                              "Quemada Canyon Grid" = "QC",
                              "Sierra Pablo Grid" = "SP",
                              "Signal Road Grid" = "SR",
                              "Telephone Road Grid" = "TR",
                              "Trancion Canyon Grid" = "TC",
                              "Verde Canyon Grid" = "VC",
                              "Wreck Canyon Grid" = "WC")
}else {
  stop("The island variable only accepts 'SMI' or 'SRI'")  
}
  
table(captures$GridCode)

# OPTIONAL filtering of certain grids, if needed for testing. 
# grids_to_use <- c("Carrington Point Grid",
#                   "Telephone Road Grid",
#                   "Old Ranch Canyon Grid",
#                   "Quemada Canyon Grid",
#                   "Wreck Canyon Grid",
#                   "Signal Road Grid",
#                   "Lighthouse Road Grid",
#                   "China Camp Grid",
#                   "Bee Canyon Grid",
#                   "Pocket Field Grid",
#                   "Arlington Canyon Grid",
#                   "Verde Canyon Grid")

# # Reduce the number of grids to just the ones we want
# captures <- captures %>%
#   filter(TrapName %in% grids_to_use)

### Inspect the UTM's ----

# Fix UTMs that are in Zone 11 (mostly an issue on SRI)
# Flag records that are in UTM Zone 11
captures <- captures %>%
  mutate(UTM_Zone = ifelse(UTME < 700000, "11", "10"))
  # mutate(UTM_Zone =  "10") # Look what happens when it's all assumed zone 10

# How many points are in each UTM?
table(captures$UTM_Zone)


# Add new fields that are zone 10 only
captures <- captures %>%
  filter(!is.na(UTME), !is.na(UTMN), !is.na(Datum)) %>%
  mutate(
    crs_str = glue("+proj=utm +zone={UTM_Zone} +datum={toupper(Datum)} +units=m"),
    utm_convert = pmap(list(UTME, UTMN, crs_str), to_nad83z10),
    X_NAD83z10     = map_dbl(utm_convert, ~.[,1]),
    Y_NAD83z10     = map_dbl(utm_convert, ~.[,2])) %>%
  dplyr::select(-utm_convert, -crs_str) %>%
  tibble()



# Put the data into a format so mapView can display it. 
captures_view <- st_as_sf(x = captures, 
                              coords = c("X_NAD83z10", "Y_NAD83z10"),
                              crs = "+proj=utm +zone=10 +datum=NAD83 +units=m") 

# View the data to see if it looks normal. 
mapView(captures_view)


# Make the TrapID using dplyr unite function
captures <- unite(captures, TrapID, IslandCode, GridCode, TrapNumber, sep="-", remove=F)

# Take a peak at the data
captures[c("TrapID", "Pittag", "UTME", "UTMN", "X_NAD83z10", "Y_NAD83z10")]


# Write detection file ----
# Get only distinct Trap ID's and keep the UTM fields, then combine them into one field.
detection_file <- captures %>%
  select(TrapID, X_NAD83z10, Y_NAD83z10) %>%
  distinct(TrapID, .keep_all=T) %>%
  unite(Detection, TrapID, X_NAD83z10, Y_NAD83z10, sep=" ", remove=F)


# Write detection file to disk, no header, no row names
write.table(detection_file$Detection,"Detection_File.txt",
            sep=" ", 
            row.names=F, 
            quote=F, 
            col.names = glue("#{island} {year} Detection file\n
                             #TrapID UTM_E UTM_N"))


### Filter to only captured foxes ----
captures_fox <- captures %>%
  filter(TrapResult == "FOX") 

# Inspect the fox only table just for fun
table(captures_fox$TrapResult, useNA = "always")
table(captures_fox$Animal, useNA = "always")

# Shorten the sex to a code
captures_fox$Sex <- recode(captures_fox$Sex,
                                "Male" = "M",
                                "Female" = "F")
                                

# Convert age class 0 to P (pups) and 1, 2, 3, 4 to A (adults)
captures_fox$AgeClass <- recode(captures_fox$AgeClass,
                                "0" = "P",
                                "1" = "A",
                                "2" = "A",
                                "3" = "A",
                                "4" = "A")


# Check number of pups and adults.
table(captures_fox$AgeClass, useNA = "always")

# Fill in Sex and Age Class grouped by pittag
captures_fill <- captures_fox %>%
  group_by(Pittag) %>%
  fill(Sex, .direction = "up") %>%
  fill(AgeClass, .direction =  "up") %>%
  fill(Sex, .direction = "down") %>%
  fill(AgeClass, .direction =  "down")


# See how the numbers of each changed after the fill.
table(captures_fill$AgeClass, useNA = "always")


# Only keep the adults if specified at the top of the script
if (adultsOnly == T){
  captures_fill <- captures_fill %>%
    filter(AgeClass == "A")
}
  

### Make a captures file ----
# Unite the fields to make the txt file needed for secr. 
capture_file <- captures_fill %>%
  unite(CaptureFile, AgeClass, Pittag, NightNumber, TrapID, Sex, sep=" ", remove=F)

# Write capture file to disk
write.table(capture_file$CaptureFile,"Capture_File_adults_only.txt",
            sep=" ",
            row.names=F, 
            quote=F, 
            col.names = glue("#{island} {year} Capture file\n
                             #Session FoxID Occasion TrapID Sex"))

### Manual Capture file inspection ----
# Do some checks for repeat offenders and foxes that have been to more than one grid in a day.
# MANUALLY DELETE OR ALTER THE PIT TAG ENTRIES FROM THE BELOW OUTPUT


# Show which pit tag is marked unknown, if any. Delete? Guess the sex?
captures_fill %>% filter(Sex == "Unknown") %>% 
  select(Pittag, Sex, AgeClass)


# Show records that have NA in the Sex or AgeClass columns. Delete? Guess the sex or age?
age_na <- captures_fill %>%
  group_by(Pittag) %>%
  filter(is.na(AgeClass) | is.na(Sex)) %>% 
  select(TrapName, TrapDate, Pittag, Sex, AgeClass)


# Make a table of pit tags and NightNumber. Look for anything more than 1. If so, fix it in the captures file.
multi_captures_per_day <- as.data.frame.matrix(table(captures_fill$Pittag, captures_fill$NightNumber)) %>% 
  rownames_to_column(var = "Pittag") %>%
  filter_if(is.numeric, any_vars(. > 1)) 

# Display it.
multi_captures_per_day 

# check if any foxes have more than one initial capture (P or N)
as.data.frame.matrix(table(captures_fill$Pittag, captures_fill$CaptureType)) %>%
  rownames_to_column(var = "Pittag") %>%
  filter(N + P > 1)

# make sure all foxes have at least one initial capture (P or N)
as.data.frame.matrix(table(captures_fill$Pittag, captures_fill$CaptureType)) %>%
  rownames_to_column(var = "Pittag") %>%
  filter(N + P < 1)

# Check for any foxes that have been seen in multiple grids.
multi_grid_fox <- as.data.frame.matrix(table(captures_fill$Pittag, captures_fill$GridCode))
# multi_grid_fox[4,1] <- 10 # Test it to show it works

# If there is anything greater than 1 in the MultiTrapped column, then a fox has been to more than one grid.
multi_grid_fox$MultiTrapped <- apply(multi_grid_fox, 1, function(x) sum(x > 0))

# Show those foxes that have spanned multiple traps (<0 rows> is good and means they only stay in one grid).
multi_grid_fox %>% 
  rownames_to_column(var="Pittag") %>% 
  filter(MultiTrapped > 1)

