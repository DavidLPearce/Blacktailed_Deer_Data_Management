# Author: David L. Pearce
# Description:
#       Data wrangling for Columbia black-tailed deer in the Rogue WMU in 2023
#              
#              
#              
#                           

# ------------------------------------------------------------------------------
#
#                               Load Packages
#
# ------------------------------------------------------------------------------

# Load packages
library(tidyverse)
library(readxl)

# Load Functions
source("./Scripts/Functions/AlleleID_Suffix_Function.R")

# Set seed, scientific notation, and workplace
set.seed(123)
options(scipen = 9999)
setwd(".")

# ------------------------------------------------------------------------------
#
#                                 Load Data
#
# ------------------------------------------------------------------------------

# Path to Excel file
path <- "./Data/Raw/2023-Rogue_Dog_17June24.xlsx"

# Each sheet in Excel File
sheets <- excel_sheets(path)

# Read all sheets into a list of data frames
df_list <- lapply(sheets, function(x) readxl::read_excel(path, sheet = x))

# Name each df according to sheet name
names(df_list) <- sheets

# Take a look
print(df_list)

# ------------------------------------------------------------------------------
#
#                               Data Wrangling
#
# ------------------------------------------------------------------------------

# -----------------------
# Separating
# -----------------------

# Extract Genetic, and Assignment into individual df
data_gen <- df_list$`2023 All Rogue Genotypes`
data_geo <- df_list$`2023 Rogue Dog Samples`
data_assn <- df_list$`2023 RoD Deer Assignment`

# Inspect each df
View(data_gen)
View(data_geo)
View(data_assn)

# -----------------------
# Cleaning
# -----------------------

# Columns were not named correctly due to notes above each sheet, column names
# are actually row 1
colnames(data_gen) <- as.character(unlist(data_gen[1, ])) # row 1 as column name
data_gen <- data_gen[-1, ]# Drop the first row
print(data_gen)

# The genetic data has identical column names for each allele call. Adding a 
# .1 and .2 to the allele to fix this
names(data_gen) <- fix_alleles(names(data_gen))
print(data_gen)

# data_geo's headers are okay

# data_assn is not - repeat
colnames(data_assn) <- as.character(unlist(data_assn[1, ])) # row 1 as column name
data_assn <- data_assn[-1, ]
print(data_assn)

# -----------------------
# Merging Together
# -----------------------

# Merging Deer Assignment Number from data_assn to data_gen 
data_merge <- data_gen %>%
  left_join(
    data_assn %>% select(`OSU Label`, `Deer Assignment Number`),
    by = c("OSU Label" = "OSU Label")
  )%>%
  # Merge Latitude and Longitude from data_geo
  left_join(
    data_geo %>% select(`OSU Sample Number`, Latitude, Longitude),
    by = c("OSU Label" = "OSU Sample Number")
  )%>%
  # Ensuring Deer Assignment Number is numeric
  mutate(`Deer Assignment Number` = as.numeric(`Deer Assignment Number`)
  ) %>%
  # Order by Deer Assignment Number
  arrange(`Deer Assignment Number`) 

# Take a look
View(data_merge)

# -----------------------
# Reorganizing and Renaming
# -----------------------

# Add in a column for WMU for later on when all years/WMUs are compiled together
data_merge$WMU <- "Rogue"

# Add in a year column
data_merge$Year <- 2023

# Renaming column names for consistency across years. 
names(data_merge) <- gsub(" ", "_", names(data_merge)) # spaces to underscores

# Naming Scheme and columns to retain 
# ODFW_ID
# OSU_ID
# All markers
# Nloci
# Sex
# DAN
# Latitude
# Longitude
# WMU
# Year
print(names(data_merge))
data_merge <- data_merge %>% # Manual changes
  rename(
    "ODFW_ID" = "ODFW_Sample_#",
    "OSU_ID" = "OSU_Label",
    "Nloci" = "#_loci_typed_(original_7_markers)", 
    "DAN" = "Deer_Assignment_Number"
  )
print(names(data_merge)) # Take a look

data_merge <- data_merge %>% # Retain
  select(
    ODFW_ID, OSU_ID, 
    Year, WMU, 
    Latitude, Longitude,
    Sex, DAN, Nloci,
    `C273.1`, `C273.2`, 
    `C89.1`, `C89.2`, 
    `OdhE.1`, `OdhE.2`,
    `SBTD05.1`, `SBTD05.2`, 
    `SBTD06.1`, `SBTD06.2`, 
    `T159s.1`, `T159s.2`,
    `T7.1`, `T7.2`,    
  )
print(names(data_merge)) # Take a look
View(data_merge)

# -----------------------
# Exporting
# -----------------------

saveRDS(data_merge, file = "./Data/1_YearWMU_processed/rds/2023Rogue.rds")

# ----------------------------- End of Script -----------------------------