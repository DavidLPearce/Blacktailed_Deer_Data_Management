# Author: David L. Pearce
# Description:
#       Data wrangling for Columbian black-tailed deer in the Dixon WMU in 2024
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

# Set working directory
setwd("E:/Projects/Current_Projects/Blacktailed_Deer_Genetics/Msat_Genetic_Data_Management/R")

# Load Functions
source("./Scripts/Functions/AlleleID_Suffix_Function.R")

# Load Dataframe Format
source("./Scripts/YearWMU_processing/DatabaseFormat.R")

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
path <- "./Data/0_Raw/2024-Dixon_BTD_14July25.xlsx"

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

# ---- Note ----
# The genetic data already has Lat and Long, don't need to add that in, 
# and there is no other information that is needed `2024 Dixon Dog Samples` 

# Extract Genetic, and Assignment into individual df
data_geo <- as.data.frame(df_list$`2024 Dixon Dog Samples`)
data_gen <- as.data.frame(df_list$`2024 All Dixon Genotypes`)
data_assn <- as.data.frame(df_list$`2024 DxD Deer Assignment`)

# Inspect each df
str(data_geo) 
str(data_gen)
str(data_assn)

head(data_geo) 
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

# data_assn is not - repeat
colnames(data_assn) <- as.character(unlist(data_assn[1, ])) # row 1 as column name
data_assn <- data_assn[-1, ]
head(data_assn)

# Data_geo
head(data_geo)

# Quality, Latitude, and Longitude are switched up, but data_gen has them so dropping
data_geo <- data_geo[,-c(4:6)]
head(data_geo)

# Coords as numeric
data_gen <- data_gen %>%
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  )
# -----------------------
# Merging Together
# -----------------------

# Merging Deer Assignment Number from data_assn to data_gen 
names(data_gen)# Check column naming
names(data_assn)
names(data_geo) 

data_merge <- data_gen %>%
  left_join(
    data_assn %>% select(`OSU Label`, `Deer Assignment Number`),
    by = c("OSU ID" = "OSU Label")
  )%>%
  # Geo data - all columns retained
  left_join(
    data_geo,
    by = c("OSU ID" = "OSU Sample Number")
  )

# Take a look
View(data_merge)

# -----------------------
# Reorganizing and Renaming
# -----------------------

# Add in a column for WMU for later on when all years/WMUs are compiled together
data_merge$WMU <- "Dixon"

# Add in a year column
data_merge$Year <- 2024

# Add in categorical of who collected the sample, Human or Dog
data_merge$Collection_method <- "Dog"

# A column for additional location identifier
data_merge$MgmtArea <- NA

# Species
data_merge$Species <- "CBTD"

# Naming Scheme and columns to retain 
print(names(data_merge))

# Manual changes
# Format follows db
data_merge <- data_merge %>% 
  rename(
    # Metadata
    "ODFW_ID" = "ODFW ID", 
    "OSU_ID" = "OSU ID",
    "Tray_ID" =  "OSU Tray",
 
    "Sex" = "sex",
    "Fawn" = "Fawn?.x",
    "DAN" = "Deer Assignment Number",
    # "Collection_method" = ,
    "Sample_Quality" = "Quality",
    # "Pellet_Length_inches" = "Pellet length (inches)",
    # "Pellet_Width_inches" = "Pellet width (inches)",
    # "Processor" = ,
    # "Extractor" = ,
    "Processing_Date" = "Processing Date",
    "Extraction_Date" = "Extraction Date", 
    "Extraction_Method" = "Extraction Method",
    # "Weather" = ,
    # "Working_Dog"  = ,
    # "Dog_Handler"  = ,
    # "Site_type" =  ,
    
    
    # Notes
    # "Collection_Notes" = ,
    "OSU_Notes" = "OSU Notes",
    # "Condition_Notes" = ,
    "Extraction_Notes" = "Extraction Notes",
    "Processing_Notes" = "Processing Notes",
    # "Marker_Notes" = , 
    # "Location_Notes" = ,
    # "Other_Notes" = ,
    
    # Markers
    "Nmarkers" = "# of loci" 
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

# -----------------------
# Exporting
# -----------------------

saveRDS(cbtd_data, file = "./Data/1_YearWMU_processed/2024Dixon.rds")

# ----------------------------- End of Script -----------------------------