
#################################################################################
# File Name:  	ShinyMapInterval.R
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
library("foreign")
library("dplyr")
library("shinydashboard")
#library("MASS")
#library("sf")
#library("doBy")

#################################################################################
# Data
#################################################################################

# CHANGE PATH HERE
#setwd("~/Dropbox/AklinKennedyShiny/ElectrificationMap")

# Raw data
#data = read.dta("./Data/Electrification_Database.dta")
#tempdata = data

# Create a shapefile for R
#worldshapefile = "./MapShapeFileLarge/tmw.shp"
#worldshapefile_shape = read_shape(file=worldshapefile, as.sf = TRUE)

# Remove Antarctica (actually will not work; depends on base map)
#worldshapefile_shape = filter(worldshapefile_shape, worldshapefile_shape$NAME != "Antarctica")

# Modify the shape file for merger with data. Shapefile names are not the same as the 
# names in the data
#worldshapefile_shape$NAME = as.character(worldshapefile_shape$NAME) # The country ID in the elec dataset are strings
#worldshapefile_shape$NAME[worldshapefile_shape$NAME == "Bahamas"] = "Bahamas, The"
#worldshapefile_shape$NAME[worldshapefile_shape$NAME == "Democratic Republic of the Congo"] = "Congo, Dem. Rep."
#worldshapefile_shape$NAME[worldshapefile_shape$NAME == "Egypt"] = "Egypt, Arab Rep."
#worldshapefile_shape$NAME[worldshapefile_shape$NAME == "Lao People's Democratic Republic"] = "Lao PDR"
#worldshapefile_shape$NAME[worldshapefile_shape$NAME == "Viet Nam"] = "Vietnam"
#worldshapefile_shape$NAME[worldshapefile_shape$NAME == "Iran (Islamic Republic of)"] = "Iran, Islamic Rep."
#worldshapefile_shape$NAME[worldshapefile_shape$NAME == "United Republic of Tanzania"] = "Tanzania"
#worldshapefile_shape$NAME[worldshapefile_shape$NAME == "Venezuela"] = "Venezuela, RB"
#worldshapefile_shape$NAME[worldshapefile_shape$NAME == "Libyan Arab Jamahiriya"] = "Libya"
#worldshapefile_shape$NAME[worldshapefile_shape$NAME == "Gambia"] = "Gambia, The"
#worldshapefile_shape$NAME[worldshapefile_shape$NAME == "Congo"] = "Congo, Rep."

## Tests to see what doesn't merge. Need to do it year-by-year. 
## tempdata = filter(data, year==2005) # Temporary data to learn
## worldmap = append_data(worldshapefile_shape, tempdata, key.shp = "NAME", key.data="countryname",
##                         ignore.duplicates=TRUE)
## under_coverage()


# Additional material for the map
#elec_total_palette = colorNumeric(palette = "Blues", domain=c(0,100))
#elec_rural_palette = colorNumeric(palette = "Reds", domain=c(0,100))
#elec_urban_palette = colorNumeric(palette = "Greens", domain=c(0,100))



#################################################################################
# User interface
#################################################################################

ui <- fluidPage(
  # Details
  dashboardHeader(tags$li(a(href = 'https://sais-isep.org/',
                            img(src = 'isep_logo.png',
                                title = "Company Home", height = "30px"),
                            style = "padding-top:10px; padding-bottom:10px;"),
                          class = "dropdown")),
  title = "ISEP Global Electrification Map",
  titlePanel("ISEP Global Electrification Map"),
  p("Welcome to the interactive maps of the global electrification dataset."),
  p("Source: Aklin, Michaël, S.P. Harish, and Johannes Urpelainen. 2018. A Global Analysis of Progress in Household Electrification.", tags$em("Energy Policy."), "122: 421-428.", 
    tags$a(href="https://www.sciencedirect.com/science/article/pii/S030142151830466X", "[link]")),
  # Output
  leafletOutput("mymap",height="600px", width="900px"),
  # Input 1
  sliderInput(inputId="slider", 
              label="Time (average electrification rate over this time period)", 
              min=1949,
              max=2015,
              value=c(2000, 2010),
              round=TRUE,
              sep = "",
              width='50%'
              ),
  mainPanel(
    p("Full dataset available at", a(href="https://dataverse.harvard.edu/dataverse/electrification","https://dataverse.harvard.edu/dataverse/electrification")),
    p("More information available at", a(href="https://sais-isep.org/","https://sais-isep.org/"))
  )
  )








