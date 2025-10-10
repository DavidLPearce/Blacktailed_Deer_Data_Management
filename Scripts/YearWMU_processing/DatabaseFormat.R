# Author: David L. Pearce
# Description:
#       A blank data frame for consistent formatting 
 



# Create a blank data frame with all of the desired columns 
cbtd_data <- data.frame(
  
  # Metadata
  ODFW_ID = character(), 
  OSU_ID = character(),
  Tray_ID = character(),
  Year = numeric(), 
  WMU = character(),
  MgmtArea = character(), 
  Latitude = numeric(), 
  Longitude = numeric(),
  Species = character(),  
  Sex = character(), 
  DAN = character(),
  Collection_method = character(),
  Sample_Quality = character(),
  Pellet_Length_inches = numeric(), 
  Pellet_Width_inches = numeric(),
  Processor = character(),
  Extractor = character(),
  Processing_Date = as.POSIXct(character(), tz = "UTC"),
  Extraction_Date = as.POSIXct(character(), tz = "UTC"), 
  Extraction_Method = character(),

  # Notes
  Collection_Notes = character(),
  OSU_Notes = character(),
  Extraction_Notes = character(),
  Processing_Notes = character(),
  Marker_Notes = character(), 
  Location_Notes = character(),
  Other_Notes = character(),
  
  # Standardized markers or "original 7 loci"
  standardized_markers = character(),
  Nmarkers = numeric(),
  C273.1 = numeric(), 
  C273.2 = numeric(),  
  C89.1 = numeric(), 
  C89.2 = numeric(),
  OdhE.1 = numeric(), 
  OdhE.2 = numeric(), 
  SBTD05.1 = numeric(), 
  SBTD05.2 = numeric(), 
  SBTD06.1 = numeric(), 
  SBTD06.2 = numeric(),
  T159s.1 = numeric(), 
  T159s.2 = numeric(),
  T7.1 = numeric(), 
  T7.2 = numeric(), 
  
  # Markers in 2012 that weren't in later years
  markers_2012 = character(),
  SBTD04.1 = numeric(),
  SBTD04.2 = numeric(), 
  SBTD07.1 = numeric(),
  SBTD07.2 = numeric(),
  
  # Additional markers ran in 2016
  markers_2016 = character(),
  B.1 = numeric(), 
  B.2 = numeric(),
  C.1 = numeric(), 
  C.2 = numeric(),
  H.1 = numeric(), 
  H.2 = numeric(),
  N.1 = numeric(), 
  N.2 = numeric(),
  R.1 = numeric(), 
  R.2 = numeric(),
  V.1 = numeric(), 
  V.2 = numeric(),
  
  # Ensure character columns are treated as strings
  stringsAsFactors = FALSE  
)

# ----------------------------- End of Script -----------------------------