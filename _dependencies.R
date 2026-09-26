# _dependencies.R – läses av renv::dependencies(), körs aldrig
library(DBI)
library(sf)
library(RPostgres)
library(shiny.telemetry)
library(htmlwidgets)
# ... lägg till alla paket appen använder, även de som laddas via source()
