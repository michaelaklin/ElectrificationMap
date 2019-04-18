# ElectrificationMap
Code to create an interactive map (via Shiny apps) of new household electrification data. The map is online [here](https://aklin.shinyapps.io/ElectrificationMap/). This repo contains the code to recreate this app. 

## Background
For background on the data, see Aklin, Michaël, S.P. Harish and Johannes Urpelainen. 2018. "A Global Analysis of Progress in Household Electrification." _Energy Policy_ 122: 421-428. [[link to the article](https://www.sciencedirect.com/science/article/pii/S030142151830466X)], [[ungated link](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3220460)], [[link to the data](https://dataverse.harvard.edu/dataverse/electrification)].

## Code

The Shiny code can be run in R. See the code for a list of packages. To run the app, download the files in the following structure:

_ElectrificationMap_

--- server.R

--- ui.R

--- Data

|   --- Electrification_Database.dta

|   --- post.html

--- MapShapeFileLarge

|   --- TM_WORLD_BORDERS-0.3.shp

--- www

|   --- isep_logo.png

_Electrification_Database.dta_ is available [here](https://dataverse.harvard.edu/dataverse/electrification). _TM_WORLD_BORDERS-0.3.shp_ is available [here](http://thematicmapping.org/downloads/world_borders.php). 

## Contact

Let me know if you spot errors or have questions ([aklin@pitt.edu](aklin@pitt.edu)). 
