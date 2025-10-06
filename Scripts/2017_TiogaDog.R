# Author: David L. Pearce
# Description:
#       Data wrangling for Columbia black-tailed deer in the Tioga WMU in 2017
#              Samples were collected by humans and dogs
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
setwd("E:/Projects/Current_Projects/Blacktailed_Deer_Genetic_Data_Management/R")

# Load Functions
source("./Scripts/Functions/AlleleID_Suffix_Function.R")

# Set seed, scientific notation
set.seed(123)
options(scipen = 9999)

# ------------------------------------------------------------------------------
#
#                                 Load Data
#
# ------------------------------------------------------------------------------

# Path to Excel file
path <- "./Data/Raw/2017Tioga Dog.xlsx"

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
data_geo <- df_list$`Tioga Dog sample info`
data_gen <- df_list$`All 2017 Tioga Dog Genotypes`
data_assn <- df_list$`2017 TiD Deer Assignment`

# Inspect each df
str(data_geo)
str(data_gen) 
str(data_assn)

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
print(data_geo)

# data_assn is not
colnames(data_assn) <- as.character(unlist(data_assn[1, ])) # row 1 as column name
data_assn <- data_assn[-1, ]
print(data_assn)

# Removing NAs from coords
# First sandardizing how NA could have been entered
# Then converting to numeric
# Lastly removing NAs
names(data_geo) # Check column naming

data_geo <- data_geo %>%
  mutate(
    # To character and trim whitespace
    `UTM Easting (NAD 83)` = as.character(`UTM Easting (NAD 83)`) %>% trimws(),
    `UTM Northing`            = as.character(`UTM Northing`) %>% trimws()
  ) %>%
  mutate(
    # Standardize NA
    `UTM Easting (NAD 83)` = ifelse(`UTM Easting (NAD 83)` %in% c("", "NA", "na", "Na", "NULL"), NA, `UTM Easting (NAD 83)`),
    `UTM Northing`            = ifelse(`UTM Northing` %in% c("", "NA", "na", "Na", "NULL"), NA, `UTM Northing`)
  ) %>%
  mutate(
    # To numeric
    `UTM Easting (NAD 83)` = as.numeric(`UTM Easting (NAD 83)`),
    `UTM Northing`            = as.numeric(`UTM Northing`)
  ) %>%
  # Remove NAs
  filter(!is.na(`UTM Easting (NAD 83)`), !is.na(`UTM Northing`))

# If there is an error by as.numeric it is because there are other entries
# for NA or missing data that the standardize pipe did not catch
# Checking for any NAs
data_geo %>%
  summarise(
    Easting_NAs  = sum(is.na(`UTM Easting (NAD 83)`)),
    Northing_NAs = sum(is.na(`UTM Northing`))
  )

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

# In case there is a sample with replicate OSU IDs 
# In the data, this will determine how many times it appears
# Should be zero
data_gen %>% 
  count(`OSU ID`) %>% 
  filter(n > 1)

# --- use this when needed, when above is >0
# # Which rows have same OSU ID
# data_gen %>% 
#   filter(`OSU ID` == "ApD10800")%>% 
#   print(width = Inf)
 
# # Both rows of the same sample ID aplified for all loci
# # this could just be a clerical error.
# # Removing one of these rows.
# data_gen <- data_gen %>%
#   group_by(`OSU ID`) %>%
#   filter(!(row_number() > 1 & `OSU ID` == "ApD10800")) %>%
#   ungroup()

# # Check for duplicates again
# data_gen %>% 
#   count(`OSU ID`) %>% 
#   filter(n > 1)

# # none :)

# Merging Deer Assignment Number from data_assn to data_gen 
names(data_gen)# Check column naming
names(data_assn)
names(data_geo) 

data_merge <- data_gen %>%
  left_join(
    data_assn %>% select(`OSU ID`, `Deer Assignment Number`),
    by = c("OSU ID" = "OSU ID")
  )%>%
  # Merge Latitude and Longitude from data_geo
  left_join(
    data_geo %>% select(`OSU label`, Latitude, Longitude),
    by = c("OSU ID" = "OSU label")
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
data_merge$WMU <- "Tioga"

# Add in a year column
data_merge$Year <- 2017

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

# Manual changes
data_merge <- data_merge %>% 
  rename(
    "ODFW_ID" = "ODFW Sample #",
    "OSU_ID" = "OSU ID",
    "Nloci" = "# loci typed (original 7 markers)", 
    "DAN" = "Deer Assignment Number"
  )

# Retain
data_merge <- data_merge %>% 
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
# Take a look
print(names(data_merge)) 
# View(data_merge)

# -----------------------
# Exporting
# -----------------------

saveRDS(data_merge, file = "./Data/1_YearWMU_processed/rds/2017TiogaDog.rds")

# ----------------------------- End of Script -----------------------------
