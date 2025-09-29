# Author: David L. Pearce
# Description:
#       Data wrangling for Columbia black-tailed and white-tailed deer 
#       in the North Bank WMU in 2024
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
path <- "./Data/Raw/2024-North_Bank_BTD_21July25.xlsx"

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

# ---------------------------------------
# 
# Black-tailed deer first
# 
# ---------------------------------------

# -----------------------
# Separating
# -----------------------

# Extract Genetic, Geospatial, and Assignment into individual df
data_gen <- df_list$`2024 All North Bank Genotypes`
data_geo <- df_list$`2024 North Bank Dog Samples`
BTdata_assn <- df_list$`2024 NoB BTD Assignment`

# Inspect each df
View(data_gen)
View(data_geo)
View(BTdata_assn)

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

# BTdata_assn is not - repeat
colnames(BTdata_assn) <- as.character(unlist(BTdata_assn[1, ])) # row 1 as column name
BTdata_assn <- BTdata_assn[-1, ]
print(BTdata_assn)

# -----------------------
# Merging Together
# -----------------------

# Merging Deer Assignment Number from BTdata_assn to data_gen but nothing else 
data_merge <- data_gen %>%
  left_join(
    BTdata_assn %>% select(`ODFW Sample #`, `Deer Assignment Number`),
    by =  "ODFW Sample #"
  )

# Take a look
View(data_merge)

# ---------------------------------------
# 
# White-tailed deer now
# 
# ---------------------------------------

# Extract white-tail assignment
WTdata_assn <- df_list$`2024 NoB CWTD Assignment`
View(WTdata_assn)

# Fix header
colnames(WTdata_assn) <- as.character(unlist(WTdata_assn[1, ])) # row 1 as column name
WTdata_assn <- WTdata_assn[-1, ]
print(WTdata_assn)

# Merging Deer Assignment Number from WTdata_assn to data_gen but nothing else 
data_merge2 <- data_merge %>%
  left_join(
    WTdata_assn %>% select(`ODFW Sample #`, `Deer Assignment Number`),
    by = "ODFW Sample #"
  ) %>%
  mutate(
    Deer_Assignment_Number = coalesce(`Deer Assignment Number.x`, `Deer Assignment Number.y`)
  ) %>%
  select(-`Deer Assignment Number.x`, -`Deer Assignment Number.y`)

# Take a look
View(data_merge2)

# ---------------------------------------
# 
#  All Together
# 
# ---------------------------------------

# -----------------------
# Reorganizing and Renaming
# -----------------------


data_merge3 <- data_merge2 %>%
  left_join(
    data_geo %>% select(`ODFW Sample #`, `Tray #`, Latitude, Longitude),
    by =  "ODFW Sample #"
  )

# Take a look
View(data_merge3)


# -----------------------
# Reorganizing and Renaming
# -----------------------

# Add in a column for WMU for later on when all years/WMUs are compiled together
data_merge3$WMU <- "NorthBank"

# Add in a year column
data_merge3$Year <- 2024

# Renaming column names for consistency across years. 
names(data_merge3) <- gsub(" ", "_", names(data_merge3)) # spaces to underscores
data_merge3 <- data_merge3 %>% # Manual changes
  rename(
    "ODFW_ID" = "ODFW_Sample_#",
    "OSU_ID" = "OSU_Label", 
    "Fawn" = "Fawn?",
    "Nloci" = "#_loci_typed_(original_7_markers)", 
    "DAN" = "Deer_Assignment_Number",
    "OSU_Tray" = "Tray_#"
    
  )
print(names(data_merge3)) # Take a look




# ---------------------------------------
# 
# Subset Data to Species
# 
# ---------------------------------------

btd_data <- data_merge3[which(data_merge3$Species == "BTD"),]
wtd_data <- data_merge3[which(data_merge3$Species == "CWTD"),]

# -----------------------
# Exporting
# -----------------------

# All deer data, even species unknown
write.csv(data_merge3, file = "./Data/Cleaned/csv/2024NorthBankAllspp.csv")

# Black-tailed deer
saveRDS(btd_data, file = "./Data/Cleaned/rds/2024NorthBank.rds")
write.csv(btd_data, file = "./Data/Cleaned/csv/2024NorthBank.csv")

# White-tailed deer
saveRDS(wtd_data, file = "./Data/Cleaned/WTD/2024NorthBank_WTD.rds")


# ----------------------------- End of Script -----------------------------