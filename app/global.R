library(shiny)
library(tidyverse)
library(shinyjs)
library(leaflet)
library(ggiraph)
library(sf)
library(writexl)
library(ggtext)
library(lubridate)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R", encoding = "utf-8", echo = FALSE)

options(dplyr.summarise.inform = FALSE)

# ---- 1. Läs in data ----

# läs in från vår postgres-databas som heter oppna_data
brottsdata <- tbl(uppkoppling_db("oppna_data"), dbplyr::in_schema("polisen", "brott")) %>%
  select(kommunkod, kommunnamn, desokod, regsonamn, brottskod,
         `inskr år`, `inskr årmånad`, inskrivningsdatum, antal_brott) %>%
  mutate(across(starts_with("inskr "), as.character)) %>%
  collect()

# läs in från vår postgres-databas som heter oppna_data
brott_niva_nyckel <- tbl(uppkoppling_db("oppna_data"), dbplyr::in_schema("polisen", "brott_niva_nyckel")) %>%
  collect()

# läs in från vår geodatabas i postgis/postgres
kommun_sf <- hamta_karta("kommuner", regionkoder = "20") %>% 
  rename(kommunkod = knkod, kommunnamn = knnamn) %>%
  st_transform(crs = 4326)

# läs in från vår geodatabas i postgis/postgres
deso_sf <- hamta_karta("deso", regionkoder = "20") %>% 
  rename(kommunkod = kommunkod, kommunnamn = kommunnamn) %>%
  st_transform(crs = 4326)

# läs in från vår postgres-databas som heter oppna_data - där vi skapat denna särskilt för att vi imputerar år som inte finns 
# hos scb på deso och befolking men som finns i polisens data
bef_df <- tbl(uppkoppling_db("oppna_data"), dbplyr::in_schema("brottsforebyggande", "bef_deso_med_imputering")) %>%
  collect()

# läs in från vår postgres-databas som heter oppna_data
bra_kommunindikatorer <- tbl(uppkoppling_db("oppna_data"), dbplyr::in_schema("bra", "kommunindikatorer")) %>%
  collect() %>% 
  filter(variabel != "Antal svarande i NTU.")

# färgvektor
rus_tre_fokus <- c("#93cec1", "#178571", "#000000")
