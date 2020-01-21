library(shiny)
library(shinydashboard)
library(data.table)
library(dplyr)
library(tidyr)
library(googleVis)
library(leaflet)

#Import databases
terror_db = fread(file = "./data/terror_db.csv", stringsAsFactors = F)

