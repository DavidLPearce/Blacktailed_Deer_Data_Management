# Author: David L. Pearce
# Description:
#       Data wrangling for Columbian black-tailed deer in the Santiam WMU in 2020
#              
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
library(readxl)
library(sf)

# Set working directory
setwd("E:/Projects/Current_Projects/Blacktailed_Deer_Genetics/Msat_Genetic_Data_Management/R")

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
path <- "./Data/0_Raw/2020_Santaim_Dog.xlsx"

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
data_geo <- df_list$`Santiam Dog sample info` 
data_gen <- df_list$`All 2020 Santiam Dog Genotypes` 
data_assn <- df_list$`2020 SaD Deer Assignment` 

# Inspect each df
View(data_geo)
View(data_gen)
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
head(as.data.frame(data_gen[,1:ncol(data_gen)]))

# data_geo's headers are okay
print(data_geo)

# data_assn is not - repeat
colnames(data_assn) <- as.character(unlist(data_assn[1, ])) # row 1 as column name
data_assn <- data_assn[-1, ]
print(data_assn)

# coords are in easting and northing changing to lat/long
# First, ensuring coords are numeric
# Note: column names may change run > 
names(data_geo)
data_geo <- data_geo %>%
  mutate(
    `UTM Easting (NAD 83)` = na_if(`UTM Easting (NAD 83)`, "NA"),
    `UTM Northing`         = na_if(`UTM Northing`, "NA")
  ) %>%
  mutate(
    `UTM Easting (NAD 83)` = as.numeric(`UTM Easting (NAD 83)`),
    `UTM Northing`         = as.numeric(`UTM Northing`)
  )

# Now checking to see if there are any missing coords
missing_coords <- data_geo[is.na(data_geo$`UTM Easting (NAD 83)`) | # check col names
                             is.na(data_geo$`UTM Northing`), ]
print(as.data.frame(missing_coords))

# Removing sample(s) with missing coords from geo data
# when merging back in with coords changed to Lat/Long
# sampling will have NA for coords - not a big deal,
# coords can be interpolated or sample can be removed if it 
# didn't amplify.
data_geo <- data_geo[!is.na(data_geo$`UTM Easting (NAD 83)`) & 
                       !is.na(data_geo$`UTM Northing`), ]

# Now easting/northing to lat/long
coords_sf <- st_as_sf( #  convert to a sf object
  data_geo,
  coords = c("UTM Easting (NAD 83)", "UTM Northing"),
  crs = 26910
) 
coords_latlong <- st_transform(coords_sf, crs = 4326) # to lat/long
coords_latlong <- st_coordinates(coords_latlong)
colnames(coords_latlong) <- c("Longitude", "Latitude")
data_geo <- cbind(data_geo, coords_latlong)
head(data_geo)

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
    data_geo %>% select(`OSU Label`, Latitude, Longitude),
    by = c("OSU Label" = "OSU Label")
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
data_merge$WMU <- "Santiam"

# Add in a year column
data_merge$Year <- 2020

# Add in categorical of who collected the sample, Human or Dog
data_merge$Collection_method <- "Dog"

# Renaming column names for consistency across years. 
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
    "ODFW_ID" = "ODFW Sample #",
    "OSU_ID" = "OSU Label",
    "Nmarkers" = "# loci typed (original 7 markers)", 
    "DAN" = "Deer Assignment Number"
  )
print(names(data_merge)) # Take a look

data_merge <- data_merge %>% # Retain
  select(
    ODFW_ID, OSU_ID, 
    Year, WMU, Collection_method,
    Latitude, Longitude,
    Sex, DAN, Nmarkers,
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

saveRDS(data_merge, file = "./Data/1_YearWMU_processed/2020Santiam.rds")

# ----------------------------- End of Script -----------------------------