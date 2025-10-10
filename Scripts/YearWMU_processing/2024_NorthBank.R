# Author: David L. Pearce
# Description:
#       Data wrangling for Columbian black-tailed and white-tailed deer 
#       in the North Bank WMU in 2024
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

# Set working directory
setwd("E:/Projects/Current_Projects/Blacktailed_Deer_Genetics/Msat_Genetic_Data_Management/R")

# Load Functions
source("./Scripts/Functions/AlleleID_Suffix_Function.R")

# Load Dataframe Format
source("./Scripts/YearWMU_processing/DatabaseFormat.R")

# Set seed, scientific notation, and workplace
set.seed(123)
options(scipen = 9999)

# ------------------------------------------------------------------------------
#
#                                 Load Data
#
# ------------------------------------------------------------------------------

# Path to Excel file
path <- "./Data/0_Raw/2024-North_Bank_BTD_21July25.xlsx"

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

# Extract Genetic, Geospatial, and Assignment into individual df
data_gen <- df_list$`2024 All North Bank Genotypes`
data_geo <- df_list$`2024 North Bank Dog Samples`
BTdata_assn <- df_list$`2024 NoB BTD Assignment`
WTdata_assn <- df_list$`2024 NoB CWTD Assignment`

# Inspect each df
# View(data_gen)
# View(data_geo)
# View(BTdata_assn)
# View(WTdata_assn)

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

# data_geo header is good

# BTdata_assn is not 
colnames(BTdata_assn) <- as.character(unlist(BTdata_assn[1, ])) # row 1 as column name
BTdata_assn <- BTdata_assn[-1, ]
print(BTdata_assn)

# # WTdata_assn is not
colnames(WTdata_assn) <- as.character(unlist(WTdata_assn[1, ])) # row 1 as column name
WTdata_assn <- WTdata_assn[-1, ]
print(WTdata_assn)

# -----------------------
# Merging Together
# -----------------------

# Merging Deer Assignment Number from BTdata_assn to data_gen but nothing else 
data_merge <- data_gen %>%
  # Blacktail assignment
  left_join(
    BTdata_assn %>% select(`ODFW Sample #`, `Deer Assignment Number`),
    by =  "ODFW Sample #"
  )%>%
  # Whitetail assignment
  left_join(
    WTdata_assn %>% select(`ODFW Sample #`, `Deer Assignment Number`),
    by = "ODFW Sample #"
  )%>%
  # Coalesce to one column
  mutate(
    DAN = coalesce(`Deer Assignment Number.x`, `Deer Assignment Number.y`)
  ) %>%
  select(-`Deer Assignment Number.x`, -`Deer Assignment Number.y`
  )%>%
  # Geo data - all columns retained
  left_join(
    data_geo,
    by = "ODFW Sample #"
  )


# Take a look
View(data_merge)

# -----------------------
# Reorganizing and Renaming
# -----------------------

# Add in a column for WMU for later on when all years/WMUs are compiled together
data_merge$WMU <- "Melrose"

# A column for additional location identifier
data_merge$MgmtArea <- "North Bank"

# Add in a year column
data_merge$Year <- 2024

# Add in categorical of who collected the sample, Human or Dog
data_merge$Collection_method <- "Dog"

# Fix species names
data_merge$Species
data_merge <- data_merge %>%
  mutate(Species = case_when(
    Species == "BTD" ~ "CBTD",
    Species == "CWTD" ~ "CWTD",
    is.na(Species) ~ "Unknown",
    TRUE ~ Species
  ))
data_merge$Species # Check



# Naming Scheme and columns to retain 
print(names(data_merge))

# Manual changes
# Format follows db
data_merge <- data_merge %>% 
  rename(
    # Metadata
    "ODFW_ID" = "ODFW Sample #", 
    "OSU_ID" = "OSU Label",
    "Tray_ID" = "Tray #",
    # "Year" = , 
    # "WMU" = ,
    # "MgmtArea" = , 
    # "Latitude" = , 
    # "Longitude" = ,
    # "Species" = ,  
    # "Sex" = ,
    # "DAN" = ,
    # "Collection_method" = ,
    "Sample_Quality" = "Quality (3, 2, 1)",
    "Pellet_Length_inches" = "Pellet Length (inches)",
    "Pellet_Width_inches" = "Pellet Width (inches)",
    # "Processor" = ,
    # "Extractor" = ,
    "Processing_Date" = "Processing Date",
    "Extraction_Date" = "Extraction Date", 
    "Extraction_Method" = "Extraction Method",
    
    # Notes
    # "Collection_Notes" = ,
    "OSU_Notes" = "OSU Notes",
    "Extraction_Notes" = "Extraction Notes",
    "Processing_Notes" = "Processing Notes",
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

saveRDS(cbtd_data, file = "./Data/1_YearWMU_processed/2024NorthBank.rds")

# ----------------------------- End of Script -----------------------------