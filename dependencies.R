# If upgrading to a new version of R, or starting out for the first time,
# run this to get all packages needed. 


pkgList <- c(
  "glue",
  "tidyverse",
  "janitor",
  "xlsx",
  "here",
  "sp",
  "mapview",
  "leaflet",
  "sf",
  "rgdal",
  "rgeos",
  "secr",
  "tools"
)

# Install the packages if they are not already.
inst <- pkgList %in% installed.packages()
if (length(pkgList[!inst]) > 0) install.packages(pkgList[!inst]))

# Optional to load them, but typically just load them in the scripts.
# lapply(pkgList, library, character.only = TRUE)

