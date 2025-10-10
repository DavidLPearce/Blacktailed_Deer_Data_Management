# Author: David L. Pearce
# Description:
#       Data wrangling for Columbia black-tailed deer in South Slough in 
#         the Sixes WMU in 2024. Samples were collected by dogs.
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

# Load Dataframe Format
source("./Scripts/YearWMU_processing/DatabaseFormat.R")

# Set seed, scientific notation
set.seed(123)
options(scipen = 9999)

# ------------------------------------------------------------------------------
#
#                                 Load Data
#
# ------------------------------------------------------------------------------

# Path to Excel file
path <- "./Data/0_Raw/2024EppsLab_OSU_DeerGenotypes_SouthSlough_29Aug24.xlsx"

# Each sheet in Excel File
sheets <- excel_sheets(path)

# Read all sheets into a list of data frames
df_list <- lapply(sheets, function(x) readxl::read_excel(path, sheet = x))

# Name each df according to sheet name
names(df_list) <- sheets

# Take a look
print(df_list)

# # Assigment file is separate
# assn_path <- "./Data/0_Raw/2012 Deer Matching.xlsx"
# assn_sheets <- excel_sheets(assn_path)
# assn_df_list <- lapply(assn_sheets, function(x) readxl::read_excel(assn_path, sheet = x))
# names(assn_df_list) <- assn_sheets
# print(assn_df_list)

# ------------------------------------------------------------------------------
#
#                               Data Wrangling
#
# ------------------------------------------------------------------------------

# -----------------------
# Separating
# -----------------------

# Extract Genetic, and Assignment into individual df
data_geo <- as.data.frame(df_list$`All South Slough sample Info`)
data_gen <- as.data.frame(df_list$`2024 South Slough Genotypes`)
data_assn <- as.data.frame(df_list$`South Slough Deer Assignment`)

# Inspect each df
str(data_geo) 
str(data_gen)
str(data_assn)

# head(data_geo) 
head(data_gen)
head(data_assn)

# -----------------------
# Cleaning
# -----------------------


# Columns were not named correctly due to notes above each sheet, column names
# are actually row 1
colnames(data_gen) <- as.character(unlist(data_gen[1, ])) # row 1 as column name
data_gen <- data_gen[-1, ]# Drop the first row
head(data_gen)

# The genetic data has identical column names for each allele call. Adding a 
# .1 and .2 to the allele to fix this
names(data_gen) <- fix_alleles(names(data_gen))
head(data_gen)

# data_geo's headers are okay
head(data_geo)

# data_assn headers
colnames(data_assn) <- as.character(unlist(data_assn[1, ])) # row 1 as column name
data_assn <- data_assn[-1, ]
head(data_assn)

# Removing NAs from coords
# First sandardizing how NA could have been entered
# Then converting to numeric
# Lastly removing NAs
names(data_geo) # Check column naming

data_geo <- data_geo %>%
  mutate(
    # To character and trim whitespace
    `Latitude` = as.character(`Latitude`) %>% trimws(),
    `Longitude`            = as.character(`Longitude`) %>% trimws()
  ) %>%
  mutate(
    # Standardize NA
    `Latitude` = ifelse(`Latitude` %in% c("", "NA", "na", "Na", "NULL"), NA, `Latitude`),
    `Longitude`            = ifelse(`Longitude` %in% c("", "NA", "na", "Na", "NULL"), NA, `Longitude`)
  ) %>%
  mutate(
    # To numeric
    `Latitude` = as.numeric(`Latitude`),
    `Longitude`            = as.numeric(`Longitude`)
  ) %>%
  # Remove NAs
  filter(!is.na(`Latitude`), !is.na(`Longitude`))

# If there is an error by as.numeric it is because there are other entries
# for NA or missing data that the standardize pipe did not catch
# Checking for any NAs
data_geo %>%
  summarise(
    Latitude_NAs  = sum(is.na(`Latitude`)),
    Longitude_NAs = sum(is.na(`Longitude`))
  )


# -----------------------
# Merging Together
# -----------------------

# theres a empty column name in data_gen

# In case there is a sample with replicate OSU IDs 
# In the data, this will determine how many times it appears
# Should be zero
names(data_gen)
data_gen %>% 
  count(`OSU Label`) %>% 
  filter(n > 1)


# Merging Deer Assignment Number from data_assn to data_gen 
names(data_gen)# Check column naming
names(data_assn)
names(data_geo) 

data_merge <- data_gen %>%
  left_join(
    # Deer Assignment
    data_assn %>% select(`OSU Label`, `Deer Assignment Number`),
    by = c("OSU Label" = "OSU Label")
  ) %>%
  # Merge Latitude and Longitude from data_geo
  # Geo data - all columns retained
  left_join(
    data_geo,
    by = c("OSU Label" = "OSU Sample Number")
  )



# Take a look
View(data_merge)


# -----------------------
# Reorganizing and Renaming
# -----------------------

# Add in a column for WMU for later on when all years/WMUs are compiled together
data_merge$WMU <- "Sixes"

# A column for additional location identifier
data_merge$MgmtArea <- "South Slough"

# Add in a year column
data_merge$Year <- 2024

# Add in categorical of who collected the sample, Human or Dog
data_merge$Collection_method <- "Dog"

# Species
data_merge$Species <- "CBTD"

# Naming Scheme and columns to retain 
print(names(data_merge))

# Manual changes
# Format follows db
data_merge <- data_merge %>% 
  rename(
    # Metadata
    "ODFW_ID" = "South Slough Sample #.x", 
    "OSU_ID" = "OSU Label",
    # "Tray_ID" =  ,
    # "Year" = , 
    # "WMU" = ,
    # "MgmtArea" = , 
    # "Latitude" = , 
    # "Longitude" = ,
    # "Species" = ,  
    # "Sex" = ,
    "DAN" = "Deer Assignment Number",
    # "Collection_method" = ,
    "Sample_Quality" = "Quality",
    # "Pellet_Length_inches" =  ,
    # "Pellet_Width_inches" =  ,
    # "Processor" = ,
    # "Extractor" = ,
    "Processing_Date" = "Processing Date",
    "Extraction_Date" = "Extraction Date", 
    "Extraction_Method" = "Extraction Method",
    "Weather" = "Weather",
    "Working_Dog"  = "Working Dog",
    "Dog_Handler"  = "Dog Handler",
    "Site_type" =  "Site type",
    
    
    # Notes
    "Collection_Notes" = "Dog Handler Collection Notes",
    "OSU_Notes" = "OSU Notes",
    "Condition_Notes" = "Condition Notes",
    "Extraction_Notes" = "Extraction Notes",
    "Processing_Notes" = "Processing Notes.x",
    # "Marker_Notes" = , 
    # "Location_Notes" = ,
    # "Other_Notes" = ,
    
    # Markers
    "Nmarkers" = "# loci typed (original 7 markers)"
  )

# Convert all marker columns and Nmarkers to numeric
marker_cols <- c("Nmarkers", 
                 "C273.1", "C273.2", "C89.1", "C89.2", 
                 "OdhE.1", "OdhE.2", "SBTD05.1", "SBTD05.2",
                 "SBTD06.1", "SBTD06.2", "T159s.1", "T159s.2",
                 "T7.1", "T7.2", "SBTD04.1", "SBTD04.2",
                 "SBTD07.1", "SBTD07.2", "B.1", "B.2",
                 "C.1", "C.2", "H.1", "H.2", "N.1", "N.2",
                 "R.1", "R.2", "V.1", "V.2")

data_merge <- data_merge %>%
  mutate(across(any_of(marker_cols), as.numeric))


# Combine with data with database format
cbtd_data <- dplyr::bind_rows(cbtd_data, data_merge %>% select(any_of(names(cbtd_data))))

# Inspect
print(names(cbtd_data)) 
View(cbtd_data)


# -----------------------
# Exporting
# -----------------------

saveRDS(cbtd_data, file = "./Data/1_YearWMU_processed/2024SouthSlough.rds")

# ----------------------------- End of Script -----------------------------
