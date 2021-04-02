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


inst <- pkgList %in% installed.packages()
if (length(pkgList[!inst]) > 0) install.packages(pkgList[!inst])
# lapply(pkgList, library, character.only = TRUE)

