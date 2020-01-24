library(shiny)
library(shinydashboard)
library(data.table)
library(dplyr)
library(tidyr)
library(googleVis)
library(leaflet)
library(ggplot2)
library(lubridate)
library(DT)

#Import databases
terror_db = fread(file = "./data/terror_db.csv", stringsAsFactors = F) %>% mutate(., date = as.Date(date)) %>% filter(., year(date)>1989)
vix_sandp_db = fread(file = "./data/vix_sandp_db.csv", stringsAsFactors = F) %>% mutate(., date = as.Date(date))

