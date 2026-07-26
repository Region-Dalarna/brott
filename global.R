library(shiny)
library(tidyverse)
library(shinyjs)
library(leaflet)
library(ggiraph)
library(sf)
library(writexl)
library(ggtext)
library(lubridate)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_shinyappar.R", encoding = "utf-8", echo = FALSE)

options(dplyr.summarise.inform = FALSE)

# ---- 1. Läs in data ----

kommun_sf <- tbl(shiny_uppkoppling_las("geodata"), dbplyr::in_schema("karta", "kommun_scb")) %>%                # använd dbplyr för att hämta delar av tabellen
  filter(str_sub(knkod, 1, 2) == "20") %>%                                               # filtrera ut Dalarnas kommuner
  collect() %>%                                                                          # först här görs uttaget ur databasen
  df_till_sf() %>%
  rename(kommunkod = knkod, kommunnamn = knnamn) %>%
  st_transform(crs = 4326)

# läs in från vår geodatabas i postgis/postgres
deso_sf <- tbl(shiny_uppkoppling_las("geodata"), dbplyr::in_schema("karta", "deso")) %>%
  filter(lanskod == "20") %>%                                               # filtrera ut Dalarnas kommuner
  collect() %>%                                                                          # först här görs uttaget ur databasen
  df_till_sf() %>%
  rename(kommunkod = kommunkod, kommunnamn = kommunnamn) %>%
  st_transform(crs = 4326)

# läs in från vår postgres-databas som heter oppna_data - där vi skapat denna särskilt för att vi imputerar år som inte finns
# hos scb på deso och befolking men som finns i polisens data
bef_df <- tbl(shiny_uppkoppling_las("oppna_data"), dbplyr::in_schema("brottsforebyggande", "bef_deso_med_imputering")) %>%
  collect()

# # läs in från vår postgres-databas som heter oppna_data
# bra_kommunindikatorer <- tbl(shiny_uppkoppling_las("oppna_data"), dbplyr::in_schema("bra", "kommunindikatorer")) %>%
#   filter(variabel != "Antal svarande i NTU.") %>%
#   collect()


# färgvektor
rus_tre_fokus <- c("#93cec1", "#178571", "#000000")
