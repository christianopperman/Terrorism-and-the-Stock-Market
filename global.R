library(shiny)
library(shinydashboard)
library(data.table)
library(dplyr)
library(tidyr)
library(googleVis)
library(leaflet)
library(ggplot2)
library(lubridate)

#Import databases
terror_db = fread(file = "./data/terror_db.csv", stringsAsFactors = F) %>% mutate(., date = as.Date(date))
vix_sandp_db = fread(file = "./data/vix_sandp_db.csv", stringsAsFactors = F) %>% mutate(., date = as.Date(date))

