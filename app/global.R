library(shiny)
library(tidyverse)
library(shinyjs)
library(leaflet)
library(ggiraph)
library(sf)
library(writexl)
library(ggtext)
library(lubridate)

source("R/mini_func_gis.R")

options(dplyr.summarise.inform = FALSE)

# ---- 1. Läs in data ----
brottsdata <- readRDS("data/brott_polisen.rds") %>% 
  select(Kommunkod, Kommunnamn, desokod, regsonamn, Brottskod,  
         `Inskr år`, `Inskr årmånad`, Inskrivningsdatum, antal_brott) %>% 
  mutate(across(starts_with("Inskr "), as.character))
brott_niva_nyckel <- readRDS("data/brott_niva_nyckel.rds")
kommun_sf <- readRDS("data/kommun_sf.rds")
deso_sf <- readRDS("data/deso_sf.rds")
bef_df <- readRDS("data/bef_deso.rds")
bra_kommunindikatorer <- readRDS("data/bra_kommunindikatorer.rds") %>% 
  filter(variabel != "Antal svarande i NTU.")

# färgvektor
rus_tre_fokus <- c("#93cec1", "#178571", "#000000")
