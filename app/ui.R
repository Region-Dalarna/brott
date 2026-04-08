library(shiny)
library(tidyverse)
library(leaflet)
library(ggiraph)
library(shinyjs)

shinyUI(fluidPage(
  tags$head(
    tags$link(rel = 'icon', type = 'image/x-icon', href = 'favicon.ico'),
    tags$style(HTML("
      .leaflet-tooltip {
      z-index: 1000 !important;
    }
    .leaflet-control {
      z-index: 900 !important;
    }
  "))
  ),
  useShinyjs(),
  titlePanel("Brottsförebyggande arbete i Dalarna"),

  tabsetPanel(

    # ----------------------------
    # Flik 1: Karta & Diagram
    # ----------------------------
    tabPanel("Polisstatistik",

  br(),

  # Ladda ner-knapp
  fluidRow(
    column(
      width = 4,
      downloadButton("export_excel", "Ladda ner hela datasetet", icon = icon("download"))
    ),
    column(
      width = 4,
      selectInput(
        inputId = "val_indelning",
        label = "Brottsindelning",
        choices = names(brott_niva_nyckel) %>%
          .[str_detect(., "_namn")] %>%
          str_remove("_namn"),
        selected = 1
      )),
  column(                    # Listruta för år
    width = 4,
    selectInput(
      inputId = "val_ar",
      label = "Välj tidsperiod:",
      choices = c("Senaste 12 månaderna", rev(sort(unique(format(brottsdata$inskrivningsdatum, "%Y"))))),
      selected = "Senaste 12 månaderna"
    ))
  ),
  br(),

  # Karta + diagram
  fluidRow(
    style = "display: flex; justify-content: center; align-items: flex-start;",
    column(
      width = 7,
      leafletOutput("karta_brott", height = "600px")
      ),


    column(
      width = 7,
      # Uppåtpilen högerställd
      div(style = "text-align: right;",
          actionButton("diagram_back", label = NULL, icon = icon("level-up-alt"),
                       class = "btn btn-light", title = "Gå tillbaka till högre nivå i diagrammet nedan")
      ),
      div(style = "display: flex; flex-direction: column; align-items: stretch;",  # stretch = full bredd
          girafeOutput("diagram_brottsomrade", width = "100%"),        #,height = "600px"),
          girafeOutput("diagram_geografi", width = "100%")    # Lilla diagrammet direkt under
      )
    ),
    column(
      width = 8,
      fluidRow(
        girafeOutput("diagram_manader", height = "295px", width = "100%")
        ),
      fluidRow(
        girafeOutput("diagram_veckodag", height = "295px", width = "100%")
        )
      ) # column
    ) # fluidRow
  ),

  # ----------------------------
  # Flik 2: Annat innehåll
  # ----------------------------
  tabPanel("BRÅ kommunindikatorer",
           fluidRow(
             div(style = "display: flex; gap: 20px; align-items: flex-start;",
                 div(style = "flex: 0 0 250px; margin-left: 10px;",  # vänsterspalten fast bredd
                     br(),
                     downloadButton("export_excel_bra", "Ladda ner hela datasetet", icon = icon("download")),
                     br(), br(),
                     selectInput("kommun", "Välj kommun:", choices = "Dalarnas län"),
                     selectInput("variabel_ntu", "Välj NTU-variabel:", choices = NULL),
                     selectInput("variabel_anm", "Välj variabel för Anmälda brott:", choices = NULL)
                 ),
                 div(style = "flex: 1; margin-right: 10px;",        # diagramdelen tar resten
                     fluidRow(
                       column(width = 6, girafeOutput("diagram_ntu",height = "100%", width = "100%")),
                       column(width = 6,
                              girafeOutput("diagram_anm_brott", height = "100%", width = "100%"),
                              girafeOutput("diagram_anm_brott_antal", height = "100%", width = "100%")
                              )
                     )
                 )
             )
           )
        ),
  tabPanel("Instruktioner",
           br(),
           div(
             style = "margin-bottom: 20px; color: #666; font-size: 14px;",
             HTML("
          <h3>Polisstatistik</h3>
          <p>
          Här kan du analysera brottsstatistik för Dalarna. Detta verktyg är en prototyp och det färdiga verktyget kommer att publiceras på annan plats. I och med det kommer denna sida att stängas ned.
          <ul>
          <li>Klicka på staplarna i det stora diagrammet för att se underkategorier av brott.</li>
          <li>Du går tillbaka med uppåt-pilen under diagrammet.</li>
          <li>Klicka på en kommun i kartan för att se brott i den kommunen och hur de fördelar sig per Demografiskt Statistikområde (DeSO).</li>
          <li>Klicka på hus-ikonen upp till vänster i kartan för att se alla kommuner igen.</li>
          <li>För att ladda ner hela datasetet för alla kommuner i Dalarna i en Excelfil, klicka på <i>Ladda ner hela datasetet</i>.</li>
          <li>För att spara ett diagram, klicka på den blå ikonen högst uppe till höger i diagrammet. Den dyker upp när du har muspekaren över diagrammet.</li>
          </ul>
          </p>
          <h3>Brottsförebyggande rådets kommunindikatorer</h3>
          Välj kommun, indikator för anmälda brott respektive för Nationella Trygghetsundersökningen (NTU).<br>
          <br>
          2025 års NTU visar 2024 års anmälda brott och självrapporterade utsatthet. Däremot gällande frågorna om självrapporterad otrygghet, oro <br>
          och upplevda problem, redovisas resultaten för insamlingsåret och handlar om hur respondenterna upplever sin situation vid undersökningstillfället.<br><br>
          <ul>
          <li>För att ladda ner hela datasetet för alla kommuner i Dalarna i en Excelfil, klicka på <i>Ladda ner hela datasetet</i>.</li>
          <li>För att spara ett diagram, klicka på den blå ikonen högst uppe till höger i diagrammet. Den dyker upp när du har muspekaren över diagrammet.</li>
          </ul>
          "))
  ),
  tabPanel("Om rapporten",
  br(),
  div(
    style = "margin-bottom: 20px; color: #666; max-width: 800px; font-size: 14px;",
    HTML("Rapporten är skapad av Samhällsanalys, Region Dalarna. Syftet är att
         avlasta brottsförebyggande tjänstepersoner i kommunerna och på Länststyrelsen genom att
         underlätta visualisering och analys av brottsstatistik från Polisen och från
         Brottsförebyggande rådet (BRÅ). Samhällsanalys har god kompetens att bearbeta och visualisera
         data medan ovan nämnda tjänstepersoner har god kompetens att bedriva brottsförebyggande
         arbete. Därmed frigörs tid för de brottsförebyggande aktörerna åt brottsförebyggande arbete vilket
         därmed kan bidra till länets utveckling.<br><br>
         <b>Kontaktuppgifter</b><br>
         <a href='mailto:samhallsanalys@regiondalarna.se?subject=Webbrapport Brottsförebyggande arbete i Dalarna'>Samhällsanalys, Region Dalarna</a> för frågor om webbrapporten.<br>
         <a href='mailto:malin.kapla@lansstyrelsen.se?subject=Webbrapport Brottsförebyggande arbete i Dalarna'>Brottsförebyggande samordnare, Länsstyrelsen Dalarna</a> för frågor om det brottsförebyggande arbetet i Dalarna.
         ")
  )
  )
  ) # tabsetpanel
)) # shinyUI och fluidPage
