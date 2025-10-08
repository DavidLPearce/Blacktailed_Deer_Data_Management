# ------------------------------------------------------------------------------
#
#                                 Clean Data
#
# ------------------------------------------------------------------------------
library(dplyr)

# Read in sample data
cbtd_data <- readRDS(file = "./Data/Sample/Compiled_YearWMU.rds")
nrow(cbtd_data)

# Remove samples that have NA for Latitude and Longitude
cbtd_data_coords <- cbtd_data %>% 
  filter(!is.na(Latitude) & !is.na(Longitude))


# Inspect
head(cbtd_data_coords)
str(cbtd_data_coords)

any(is.na(cbtd_data_coords$Latitude))
any(is.na(cbtd_data_coords$Longitude))

# Some samples are outside of Oregon due to incorrect Lat/Long
# Filter to Oregon boundaries
cbtd_data_coords <- cbtd_data_coords %>%
  filter(Latitude >= 42 & Latitude <= 46) %>%
  filter(Longitude >= -124.5 & Longitude <= -116.5)

# Check how many points remain
nrow(cbtd_data)
nrow(cbtd_data_coords)

saveRDS(cbtd_data_coords, file = "./Data/Sample/Compiled_YearWMU_Coords.rds")
