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

# Set working directory
# setwd("E:/Projects/Current_Projects/Blacktailed_Deer_Genetics/Msat_Genetic_Data_Management/R/Rshiny_App")

# # Connect to online server
# rsconnect::setAccountInfo(name='davidlpearce',
#                           token='836F07366DE04EC094C0ECADAA61F884',
#                           secret='Q6HJKssRCwjbHSuWC4eZ9oaR76V/Dh6QO3hHAFyS')
# 


# ------------------------------------------------------------------------------
#
#                                 Load Data
#
# ------------------------------------------------------------------------------

# Read in sample data
cbtd_data <- readRDS(file = "./Data/Sample/Compiled_YearWMU_Coords.rds")

# Read in WMU polygons
wmu_shp <- st_read("./Data/GIS/OregonWMUs/ODFW_wildlife_mgmt_units.shp")
# Fix projection
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
        data = cbtd_data,
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

# rsconnect::deployApp()
# 
# ----------------------------- End of Script -----------------------------