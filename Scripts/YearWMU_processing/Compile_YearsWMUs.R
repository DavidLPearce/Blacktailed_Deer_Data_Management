# Author: David L. Pearce
# Description:
#       Compiling data from 2012-2024 for Columbia black-tailed deer 
#        microsatellite genetic data
#              
#              
#                           

# ------------------------------------------------------------------------------
#
#                               Load Packages
#
# ------------------------------------------------------------------------------

# Clear things
rm(list = ls(all.names = TRUE)) # environment
gc() # memory

# Load packages
library(tidyverse)

# Set working directory
setwd("E:/Projects/Current_Projects/Blacktailed_Deer_Genetics/Msat_Genetic_Data_Management/R")

# Set seed, scientific notation
set.seed(123)
options(scipen = 9999)

# ------------------------------------------------------------------------------
#
#                                 Load Data
#
# ------------------------------------------------------------------------------


# Directory containing the data files
directory <- "./Data/1_YearWMU_processed/rds"

# List all files in the directory
files <- list.files(directory, pattern = "\\.rds$", full.names = TRUE)


# ------------------------------------------------------------------------------
#
#                               Compile Data
#
# ------------------------------------------------------------------------------

# Create a blank data frame with all of the desired columns 
cbtd_data <- data.frame(
                  ODFW_ID = character(), 
                  OSU_ID = character(), 
                  Year = numeric(), 
                  WMU = character(),
                  Collection_method = character(),
                  Latitude = numeric(), 
                  Longitude = numeric(),
                  Sex = character(), 
                  DAN = character(),
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
                  markers_2012 = character(),
                  SBTD04.1 = numeric(),
                  SBTD04.2 = numeric(), 
                  SBTD07.1 = numeric(),
                  SBTD07.2 = numeric(),
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
                  Marker_notes = character(), 
                  Geo_notes = character(),
                  Other_notes = character(),
                  stringsAsFactors = FALSE  # Ensure character columns are treated as strings
)

# Take a look
str(cbtd_data)


# ----------------------------
# Extracting data from files
# ----------------------------

# Loop
for (i in seq_along(files)) {
  
    # Extract path
    file_path <- files[i]
    
    # Read file
    file_data <- readRDS(file_path)
    
    # Some files had varying formatting to what cbtd_data was specified as
    # making everything as a character fixes this. Format after compiling
    file_data <- mutate(file_data, across(everything(), as.character))
    cbtd_data <- mutate(cbtd_data, across(everything(), as.character))
    
    # Combine 
    cbtd_data <- dplyr::bind_rows(cbtd_data, file_data)
  
}# End loop

# Take a look
View(cbtd_data)

# ----------------------------
# Further Formatting
# ----------------------------

# Check formatting 
str(cbtd_data) # everything is a character

# as numeric
names(cbtd_data)
num_cols <- c("Year", "Latitude", "Longitude",
              "C273.1", "C273.2", 
              "C89.1", "C89.2", "OdhE.1", "OdhE.2", 
              "SBTD05.1", "SBTD05.2", "SBTD06.1", "SBTD06.2",             
              "T159s.1","T159s.2","T7.1", "T7.2", "SBTD04.1",               
              "SBTD04.2", "SBTD07.1", "SBTD07.2", "B.1", "B.2",                 
               "C.1","C.2", "H.1", "H.2","N.1", "N.2",                   
              "R.1", "R.2", "V.1", "V.2" 
)  

# Checking for what is not numeric in numeric columns
for (col in num_cols) {
  non_numeric <- cbtd_data[[col]][!grepl("^-?[0-9.]+$", cbtd_data[[col]]) & 
                                    !is.na(cbtd_data[[col]])]
  if (length(non_numeric) > 0) {
    cat("\nColumn:", col, "\n")
    cat("Non-numeric values:", unique(non_numeric)[1:min(10, length(unique(non_numeric)))], "\n")
  }
}

# NAs are from those columns, rows
cbtd_data <- cbtd_data %>%
  mutate(across(all_of(num_cols), as.numeric))

# Recheck structure
str(cbtd_data)

# Sex from f, m, F, M to Female, Male
cbtd_data <- cbtd_data %>%
  mutate(Sex = case_when(
    Sex %in% c("f", "F") ~ "Female",
    Sex %in% c("m", "M") ~ "Male",
    TRUE ~ Sex  # Keep any other values as-is
  ))

# Calculate Nmarkers for standardized markers
marker_cols <- list(
                c("C273.1", "C273.2"),
                c("C89.1", "C89.2"),
                c("OdhE.1", "OdhE.2"),
                c("SBTD05.1", "SBTD05.2"),
                c("SBTD06.1", "SBTD06.2"),
                c("T159s.1", "T159s.2"),
                c("T7.1", "T7.2")
)
cbtd_data <- cbtd_data %>%
  rowwise() %>%
  mutate(Nmarkers = sum(sapply(marker_cols, function(cols) {
    any(c_across(all_of(cols)) > 0, na.rm = TRUE)
  }))) %>%
  ungroup()

# Take a look
View(cbtd_data)

# Order by Year, WMU, DAN
cbtd_data <- cbtd_data %>%
  arrange(desc(Year), WMU, DAN)

# -----------------------
# Exporting
# -----------------------

saveRDS(cbtd_data, file = "./Data/2_For_Across_Year_Assignments/Compiled_YearWMU.rds")
write.csv(cbtd_data, file = "./Data/2_For_Across_Year_Assignments/Compiled_YearWMU.csv")
