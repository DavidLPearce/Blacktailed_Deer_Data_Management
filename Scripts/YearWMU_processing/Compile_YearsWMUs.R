# Author: David L. Pearce
# Description:
#       Compiling data from 2012-2024 for Columbia black-tailed deer microsatellite genetic data
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
View(cbtd_data)


# ----------------------------
# Extracting data from files
# ----------------------------
# Initialize progress bar
total_files <- length(files)
pb <- txtProgressBar(min = 0, max = total_files, style = 3)

for (i in seq_along(files)) {
  
  # Extract path
  file_path <- files[i]

  # Read file
  file_data <- readRDS(file_path)

  # Combining with dataset
  cbtd_data <- data.frame(rbind(cbtd_data, file_data))  
  
  # Update progress bar
  setTxtProgressBar(pb, i)
  
}# end loop

# Take a look
View(cbtd_data)

# ----------------------------
# Further Formatting
# ----------------------------

# Calculate Nloci for standardized markers
