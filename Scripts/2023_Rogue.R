# Author: David L. Pearce
# Description:
#       Data wrangling for Columbia black-tailed deer in the Dixon WMU in 2023
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
    data_assn %>% select(`ODFW Sample #`, `Deer Assignment Number`),
    by = c("ODFW Sample #" = "ODFW Sample #")
  )%>%
  # Merge Latitude and Longitude from data_geo
  left_join(
    data_geo %>% select(`ODFW Sample #`, Latitude, Longitude),
    by = c("ODFW Sample #" = "ODFW Sample #")
  )

# Take a look
View(data_merge)
