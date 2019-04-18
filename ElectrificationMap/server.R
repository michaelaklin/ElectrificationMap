
#################################################################################
# File Name:  	server.R
# Project:			Global Electrification
# Purpose:      Create interactive maps with Shiny
# Data input:   Electrification_Database.dta
# Output File:  na
# Author: 			Michaël Aklin
# Date:         09/07/2018
# Notes:      
#################################################################################

#################################################################################
# Packages
#################################################################################

#rm(list=ls())

library("shiny")
library("leaflet")
library("tmap")
library("tmaptools")
library("sf")
library("foreign")
library("shinydashboard")
#library("MASS")
#library("dplyr")
#library("doBy")

#################################################################################
# Data
#################################################################################

# CHANGE PATH HERE
#setwd("~/Dropbox/AklinKennedyShiny/ElectrificationMap")

# Raw data
data = read.dta("./Data/Electrification_Database.dta")
tempdata = data

# Create a shapefile for R
worldshapefile = "./MapShapeFileLarge/tmw.shp"
worldshapefile_shape = read_shape(file=worldshapefile, as.sf = TRUE)

# Remove Antarctica (actually will not work; depends on base map)
worldshapefile_shape = filter(worldshapefile_shape, worldshapefile_shape$NAME != "Antarctica")

# Modify the shape file for merger with data. Shapefile names are not the same as the 
# names in the data
worldshapefile_shape$NAME = as.character(worldshapefile_shape$NAME) # The country ID in the elec dataset are strings
worldshapefile_shape$NAME[worldshapefile_shape$NAME == "Bahamas"] = "Bahamas, The"
worldshapefile_shape$NAME[worldshapefile_shape$NAME == "Democratic Republic of the Congo"] = "Congo, Dem. Rep."
worldshapefile_shape$NAME[worldshapefile_shape$NAME == "Egypt"] = "Egypt, Arab Rep."
worldshapefile_shape$NAME[worldshapefile_shape$NAME == "Lao People's Democratic Republic"] = "Lao PDR"
worldshapefile_shape$NAME[worldshapefile_shape$NAME == "Viet Nam"] = "Vietnam"
worldshapefile_shape$NAME[worldshapefile_shape$NAME == "Iran (Islamic Republic of)"] = "Iran, Islamic Rep."
worldshapefile_shape$NAME[worldshapefile_shape$NAME == "United Republic of Tanzania"] = "Tanzania"
worldshapefile_shape$NAME[worldshapefile_shape$NAME == "Venezuela"] = "Venezuela, RB"
worldshapefile_shape$NAME[worldshapefile_shape$NAME == "Libyan Arab Jamahiriya"] = "Libya"
worldshapefile_shape$NAME[worldshapefile_shape$NAME == "Gambia"] = "Gambia, The"
worldshapefile_shape$NAME[worldshapefile_shape$NAME == "Congo"] = "Congo, Rep."
worldshapefile_shape$NAME[worldshapefile_shape$NAME == "Burma"] = "Myanmar"
worldshapefile_shape$NAME[worldshapefile_shape$NAME == "Yemen"] = "Yemen, Rep."
worldshapefile_shape$NAME[worldshapefile_shape$NAME == "The former Yugoslav Republic of Macedonia"] = "Macedonia, FYR"


# Tests to see what doesn't merge. Need to do it year-by-year. 
# tempdata = filter(data, year==2005) # Temporary data to learn
# worldmap = append_data(worldshapefile_shape, tempdata, key.shp = "NAME", key.data="countryname",
#                         ignore.duplicates=TRUE)
# under_coverage()


# Additional material for the map
elec_total_palette = colorNumeric(palette = "Blues", domain=c(0,100))
elec_rural_palette = colorNumeric(palette = "Reds", domain=c(0,100))
elec_urban_palette = colorNumeric(palette = "Greens", domain=c(0,100))


#################################################################################
# Server
#################################################################################

server <- function(input, output, session) {
 
# Subset the data based on input 
  worldmap_filter = reactive({
    a = subset(tempdata, (year >= input$slider[1] & year <= input$slider[2])) # Subset the data
    # Collapsing between the upper and lower limit of the slider
    collapse = a %>% group_by(countryname) %>% 
      summarise_at(c("elecrate_total", "elecrate_rural", "elecrate_urban"), mean, na.rm = TRUE)
    
    worldmap = append_data(worldshapefile_shape, collapse, 
                           key.shp = "NAME", key.data="countryname",
                           ignore.duplicates=TRUE) # Merge the shape file with the subsetted data
    return(worldmap)
  })


# Map
  output$mymap <- renderLeaflet({
    leaf = leaflet(worldmap_filter()) %>%
      addProviderTiles("CartoDB.Positron",
                       options = providerTileOptions(noWrap = TRUE)) %>% # Options: CartoDB.Positron; see https://leaflet-extras.github.io/leaflet-providers/preview/
      addPolygons(stroke=FALSE, 
                  smoothFactor = 0.2,
                  fillOpacity = .7, 
                  popup=paste("<b>County: </b>", worldmap_filter()$NAME,
                                                 "<br /><b>Total electrification: </b>", round(worldmap_filter()$elecrate_total,1), "%",
                                                 "<br /><b>Rural electrification: </b>", round(worldmap_filter()$elecrate_rural,1), "%",
                                                 "<br /><b>Urban electrification: </b>", round(worldmap_filter()$elecrate_urban,1), "%"
                         ),
                  color= ~elec_total_palette(worldmap_filter()$elecrate_total),
                  group="Total"
      ) %>%
      addPolygons(stroke=TRUE,
                  weight=1,
                  smoothFactor = 0.2, 
                  fillOpacity = .75, 
                  popup=paste("<b>County: </b>", worldmap_filter()$NAME,
                              "<br /><b>Total electrification: </b>", round(worldmap_filter()$elecrate_total,1), "%",
                              "<br /><b>Rural electrification: </b>", round(worldmap_filter()$elecrate_rural,1), "%",
                              "<br /><b>Urban electrification: </b>", round(worldmap_filter()$elecrate_urban,1), "%"
                  ),
                  color= ~elec_rural_palette(worldmap_filter()$elecrate_rural),
                  group="Rural"
      )  %>%
      addPolygons(stroke=TRUE,
                  weight=1,
                  smoothFactor = 0.2, 
                  fillOpacity = .75, 
                  popup=paste("<b>County: </b>", worldmap_filter()$NAME,
                              "<br /><b>Total electrification: </b>", round(worldmap_filter()$elecrate_total,1), "%",
                              "<br /><b>Rural electrification: </b>", round(worldmap_filter()$elecrate_rural,1), "%",
                              "<br /><b>Urban electrification: </b>", round(worldmap_filter()$elecrate_urban,1), "%"
                  ),
                  color= ~elec_urban_palette(worldmap_filter()$elecrate_urban),
                  group="Urban"
      )  %>%
      addLayersControl(
        baseGroups=c("Total", "Rural", "Urban"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      setView(20,30.68, zoom = 1.5) # Where the map is centered
    # leaf
    
  })
  leafletOutput('mymap', height=900)
}






