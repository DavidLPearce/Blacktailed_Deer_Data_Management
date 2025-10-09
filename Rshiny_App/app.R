#
# Author: David L. Pearce
# Description:
#       This script is to create a Rshiny app for sharing GPS locations of genetic 
#       samples of Columbian black-tailed deer sampled in western Oregon.
#              
#              
#                           

# ------------------------------------------------------------------------------
#
#                               Load Packages
#
# ------------------------------------------------------------------------------

# Install packages
# install.packages("shiny")
# install.packages("leaflet")
# install.packages("dplyr") 
# install.packages("sf")
# install.packages("rsconnect")


# Load packages
library(shiny)
library(rsconnect)
library(leaflet)
library(dplyr)
library(sf)

# Set working directory --- only for local run
# setwd("E:/Projects/Current_Projects/Blacktailed_Deer_Genetics/Msat_Genetic_Data_Management/R/Rshiny_App")


# ------------------------------------------------------------------------------
#
#                                 Load Data
#
# ------------------------------------------------------------------------------

# Read in sample data
cbtd_data <- readRDS(file = "./Data/Sample/Compiled_YearWMU.rds")

# Read in WMU polygons
wmu_shp <- st_read("./Data/GIS/OregonWMUs/ODFW_wildlife_mgmt_units.shp")

# ------------------------------------------------------------------------------
#
#                            Format Data
#
# ------------------------------------------------------------------------------

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


# Fix shapefile projection
wmu_shp <- st_transform(wmu_shp, crs = 4326) 

# ------------------------------------------------------------------------------
#
#                            Create Server Page
#
# ------------------------------------------------------------------------------


# -----------------------
# Interface and Server
# -----------------------

# UI
ui <- fluidPage(
  titlePanel("Sample Locations"),
  leafletOutput("map", height = "800px")
)


# Server
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap, group = "Street Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topographic") %>%
      
      # WMU polygons
      addPolygons(
        data = wmu_shp,
        weight = 2,
        color = "yellow",
        fillOpacity = 0.1,
        fillColor = "white",
        label = ~UNIT_NAME,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "white",
          fillOpacity = 0.3,
          bringToFront = FALSE
        )
      ) %>%
      
      # Sample points
      addCircleMarkers(
        data = cbtd_data_coords,
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 3,
        stroke = TRUE,
        color = "black",
        weight = 1,
        fillOpacity = 1,
        fillColor = "purple",
        popup = ~paste("WMU:", WMU)
      ) %>%
      
      addLayersControl(
        baseGroups = c("Street Map", "Satellite", "Topographic"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
}
# Run the app
shinyApp(ui = ui, server = server)

# -----------------------
# Publish
# -----------------------

##  ---- To Publish run the following code in console ---- 
# rsconnect::setAccountInfo(name='davidlpearce',
#                           token='836F07366DE04EC094C0ECADAA61F884',
#                           secret='Q6HJKssRCwjbHSuWC4eZ9oaR76V/Dh6QO3hHAFyS')
# 
# rsconnect::deployApp()
# 
# ----------------------------- End of Script -----------------------------