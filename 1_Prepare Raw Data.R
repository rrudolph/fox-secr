# Purpose: To process raw data from Channel Islands National Park Island Fox capture database 
# into a format that the secr package can read. Output files are "Detection_File.txt" and
# "Capture_File.txt". The Catpure file will need to be modified slightly to account for any
# double captures or other issues that this script will help to detect.
# Authors: Rocky Rudolph, GISP, Channel Islands National Park
#          Adam Dillon, PhD Candidate - Colorado State University
# Date: 1/30/2019

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
here()


# Clear any environment variables in memory
rm(list = ls())

# Set variables and paths specific to island and year
setwd(here("SRI", "2018"))

island <- "SRI"
year <- "2018"
adultsOnly = T

captures <- read_excel("2018 SRI GRID DATA_3.6.2019 export.xlsx", 
                           col_types = c("text", "numeric", "text", 
                                         "date", "numeric", "numeric", "numeric", 
                                         "numeric", "text", "text", "text", 
                                         "text", "text", "text", "text", "text", 
                                         "text", "numeric", "text", "text", 
                                         "text", "text", "text", "skip"))
View(captures)

# captures_sub <- captures %>%
#   filter(Pittag == 982000363814353 |
#            Pittag ==982000364301500 |
#            Pittag == 989001000416330 |
#            Pittag == 989001000416334 )
# 
# options("scipen"=100, "digits"=4) 
# 
# write.table(captures_sub,"Female foxes.csv", sep="," ,  row.names=F,   quote=F)


# Inspect the data as needed
table(captures$TrapResult)
table(captures$Datum)
table(captures$Sex)
table(captures$Animal)
table(captures$SamplingYear)
table(captures$AgeClass)
str(captures)
summary(captures)


missing_data <- captures %>%
  filter(is.na(Datum))
# found a bogus entry. Remove it.
captures <- captures %>%
  filter(!is.na(Datum))


# Select the fields to be converted to factors
fieldList <- c('IslandCode', 'SamplingYear', 'TrapName',  'TrapResult', 'Animal',
                'CaptureType', 'Sex', 'AgeClass', 'Weight_units',
               'BodyCondition', 'ReproductiveStatus', 'BloodTaken', 'Vaccinations', 'Datum')


# Apply as.factor to those fields
captures[fieldList] <- lapply(captures[fieldList], as.factor)


# Get trap names to turn them into short codes. 
# This is a helper function to make writing the codes easier. 
# trapNames <- unique(captures$TrapName)
# for (name in trapNames){
#   print(glue('levels(captures$GridCode)[levels(captures$GridCode) == "{name}"] <- ""'))
# }

# Make a new field called GridCode and populate it with the trap names
captures$GridCode <- captures$TrapName

# Shorten those names to a code

# # SMI
levels(captures$GridCode)[levels(captures$GridCode) == "CARDWELL POINT GRID"] <- "CP"
levels(captures$GridCode)[levels(captures$GridCode) == "CHARCOAL CANYON GRID"] <- "CC"
levels(captures$GridCode)[levels(captures$GridCode) == "JACKASS FLATS GRID"] <- "JF"
levels(captures$GridCode)[levels(captures$GridCode) == "SANDBLAST PASS GRID"] <- "SP"


# # SRI
levels(captures$GridCode)[levels(captures$GridCode) == "Arlington Canyon Grid"] <- "AC"
levels(captures$GridCode)[levels(captures$GridCode) == "Arlington Springs Grid"] <- "AS"
levels(captures$GridCode)[levels(captures$GridCode) == "Bee Canyon Grid"] <- "BC"
levels(captures$GridCode)[levels(captures$GridCode) == "Burma Road Grid"] <- "BR"
levels(captures$GridCode)[levels(captures$GridCode) == "Carrington Point Grid"] <- "CP"
levels(captures$GridCode)[levels(captures$GridCode) == "China Camp Grid"] <- "CC"
levels(captures$GridCode)[levels(captures$GridCode) == "Dry Canyon Grid"] <- "DC"
levels(captures$GridCode)[levels(captures$GridCode) == "Johnsons Lee Grid"] <- "JL"
levels(captures$GridCode)[levels(captures$GridCode) == "Lighthouse Road Grid"] <- "LR"
levels(captures$GridCode)[levels(captures$GridCode) == "Old Ranch Canyon Grid"] <- "OR"
levels(captures$GridCode)[levels(captures$GridCode) == "Pocket Field Grid"] <- "PF"
levels(captures$GridCode)[levels(captures$GridCode) == "Quemada Canyon Grid"] <- "QC"
levels(captures$GridCode)[levels(captures$GridCode) == "Sierra Pablo Grid"] <- "SP"
levels(captures$GridCode)[levels(captures$GridCode) == "Signal Road Grid"] <- "SR"
levels(captures$GridCode)[levels(captures$GridCode) == "Telephone Road Grid"] <- "TR"
levels(captures$GridCode)[levels(captures$GridCode) == "Trancion Canyon Grid"] <- "TC"
levels(captures$GridCode)[levels(captures$GridCode) == "Verde Canyon Grid"] <- "VC"
levels(captures$GridCode)[levels(captures$GridCode) == "Wreck Canyon Grid"] <- "WC"

table(captures$GridCode)

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


# Fix UTMs that are in Zone 11 (mostly an issue on SRI) ----

# To convert to Nad83 UTM Zone 10. EPSG source: http://spatialreference.org/ref/epsg/3157/
to_nad83z10 <- function(x, y, crs_str){
  st_as_sf(data_frame(x=x, y=y), crs=crs_str, coords = c("x", "y")) %>%
    st_transform(crs=3157) %>%
    st_coordinates()
}

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
    lon_lat = pmap(list(UTME, UTMN, crs_str), to_nad83z10),
    X_NAD83z10     = map_dbl(lon_lat, ~.[,1]),
    Y_NAD83z10     = map_dbl(lon_lat, ~.[,2])) %>%
  dplyr::select(-lon_lat)



# View the data to see if it looks normal. 
captures_view <- st_as_sf(x = captures, 
                              coords = c("X_NAD83z10", "Y_NAD83z10"),
                              crs = "+proj=utm +zone=10 +datum=NAD83 +units=m")

mapView(captures_view)


# Make the TrapID using dplyr unite function
captures <- unite(captures, TrapID, IslandCode, GridCode, TrapNumber, sep="-", remove=F)

# Take a peak at the data
captures[c("TrapID", "Pittag", "UTME", "UTMN", "X_NAD83z10", "Y_NAD83z10")]

# Get only distinct Trap ID's and keep the UTM fields, then combine them into one field.
detection_file <- captures %>%
  select(TrapID, X_NAD83z10, Y_NAD83z10) %>%
  distinct(TrapID, .keep_all=T) %>%
  unite(Detection, TrapID, X_NAD83z10, Y_NAD83z10, sep=" ", remove=F)


# Write detection file to disk, no header, no row names
#TODO: Ask Adam/Laura: Does secr read anything commented? Or totally ignored?
write.table(detection_file$Detection,"Detection_File.txt",
            sep=" ", 
            row.names=F, 
            quote=F, 
            col.names = glue("#{island} {year} Detection file\n#TrapID UTM_E UTM_N"))


# Make dataframe of just the ones with Foxes found
captures_fox <- captures %>%
  filter(TrapResult == "FOX") 

# Inspect the fox only table just for fun
table(captures_fox$TrapResult)
table(captures_fox$Animal)

# Shorten the sex to a code
levels(captures_fox$Sex)[levels(captures_fox$Sex) == "Male"] <- "M"
levels(captures_fox$Sex)[levels(captures_fox$Sex) == "Female"] <- "F"


# ---- Do some checks for repeat offenders and foxes that have been to more than one grid in a day.

# Make a table of pit tags and NightNumber. Look for anything more than 1. If so, fix.
table(captures_fox$Pittag, captures_fox$NightNumber)

# Show number of occurances that are greater than 1. Address these pit tag numbers
n_occur <- as.data.frame(table(captures_fox$Pittag, captures_fox$NightNumber))
dplyr::filter(n_occur, Freq > 1)


# Check for any foxes that have been seen in multiple grids.
multi_grid_fox <- as.data.frame(table(captures_fox$Pittag, captures_fox$GridCode))
# If there is anything greater than 1 in the TRUE column, then a fox has been to more than one grid
table(multi_grid_fox$Var1, multi_grid_fox$Freq >0)


#MANUALLY DELETE OR ALTER THE PIT TAG NUMBERS FROM THE ABOVE OUTPUT


# Convert age class 0-4 to only 0 pup or 1 adult
levels(captures_fox$AgeClass)[levels(captures_fox$AgeClass) == "2"] <- "1"
levels(captures_fox$AgeClass)[levels(captures_fox$AgeClass) == "3"] <- "1"
levels(captures_fox$AgeClass)[levels(captures_fox$AgeClass) == "4"] <- "1"

# Convert age class 0 or 1 to P and A
levels(captures_fox$AgeClass)[levels(captures_fox$AgeClass) == "0"] <- "P"
levels(captures_fox$AgeClass)[levels(captures_fox$AgeClass) == "1"] <- "A"

# Check number of pups and adults.
table(captures_fox$AgeClass)

# Check for any "unknown" foxes. Delete them manually if so.
table(captures_fox$Sex)

# Show which pit tag is marked unknown, if any
captures_fox %>% filter(Sex == "Unknown") %>% 
  select(Pittag, Sex, AgeClass)


# Fill in Sex and Age Class grouped by pittag
captures_fill <- captures_fox %>%
  group_by(Pittag) %>%
  fill(Sex, .direction = "up") %>%
  fill(AgeClass, .direction =  "up") %>%
  fill(Sex, .direction = "down") %>%
  fill(AgeClass, .direction =  "down")

table(captures_fill$AgeClass)


# Only keep the adults if specified above
if (adultsOnly == T){
  captures_fill <- captures_fill %>%
    filter(AgeClass == "A")
}
  

# Make a table of records that have NA in the Sex or AgeClass columns
captures_is_na <- captures_fill %>%
  group_by(Pittag) %>%
  filter(is.na(AgeClass) | is.na(Sex))

# Show them.  Deal with them as needed. Delete?
captures_is_na[c("TrapName", "TrapDate", "Pittag", "Sex", "AgeClass")]

# Unite the fields to make the txt file needed for secr. 
##Session FoxID Occasion TrapID Sex
capture_file <- captures_fill %>%
  unite(CaptureFile, AgeClass, Pittag, NightNumber, TrapID, Sex, sep=" ", remove=F)

# Write capture file to disk
write.table(capture_file$CaptureFile,"Capture_File.txt",
            sep=" ",
            row.names=F, 
            quote=F, 
            col.names = glue("#{island} {year} Capture file\n#Session FoxID Occasion TrapID Sex"))


# For convenience, show the issues needing addressing again:
#Any foxes with NA sex
captures_is_na[c("TrapName", "TrapDate", "Pittag", "Sex", "AgeClass")]

# Same fox in the same night number
dplyr::filter(n_occur, Freq > 1)
