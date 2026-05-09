library(shiny)
library(tidyverse)
library(leaflet)
library(ggiraph)
library(shinyjs)

shinyUI(fluidPage(
  tags$head(
    tags$link(rel = 'icon', type = 'image/x-icon', href = 'favicon.ico'),
    tags$style(HTML("
      /* Tooltip på polygoner ska ligga ovanför teckenförklaringen.
         Vi nollar map-panens stacking-context så att tooltip-pane kan
         lyfta sig ovanför kontroll-lagret (teckenförklaringen). */
      .leaflet-pane.leaflet-map-pane { z-index: auto !important; }
      .leaflet-pane.leaflet-tooltip-pane { z-index: 1100 !important; }
      .leaflet-tooltip { z-index: 1100 !important; }
      .leaflet-bottom.leaflet-left { z-index: 600 !important; }
      /* Hus-knappen ska ligga ovanför tooltipen */
      .leaflet-top.leaflet-left { z-index: 1200 !important; }

      /* Kompakt och halvt genomskinlig teckenförklaring i nedre vänstra hörnet */
      .leaflet-control.kompakt-legend {
        font-size: 10px !important;
        line-height: 1.25 !important;
        padding: 5px 7px !important;
        background-color: rgba(255, 255, 255, 0.85) !important;
        box-shadow: 0 0 6px rgba(0, 0, 0, 0.15) !important;
        pointer-events: none !important;  /* mus släpps igenom så tooltip visas */
      }
      .leaflet-control.kompakt-legend i {
        width: 12px !important;
        height: 12px !important;
        margin-right: 3px !important;
        opacity: 0.9;
      }
      .leaflet-control.kompakt-legend strong {
        font-size: 11px !important;
      }

      /* ---- Layout för Polisstatistik-fliken ---- */
      .polisstat-layout #karta_brott { height: 70vh !important; }
      .polisstat-layout .diagram-cell {
        height: 35vh;
        min-height: 240px;
      }
      /* Karta lite smalare så diagrammen får mer plats */
      @media (min-width: 992px) {
        .polisstat-layout > .row > .col-sm-4 {
          flex: 0 0 28%;
          max-width: 28%;
        }
        .polisstat-layout > .row > .col-sm-8 {
          flex: 0 0 72%;
          max-width: 72%;
        }
      }
      /* Lite vertikal luft ovanför översta diagramraden */
      .polisstat-layout .col-sm-8 > .row:first-child {
        margin-top: 10px;
      }
      /* Vertikal luft mellan översta och understa diagramraden */
      .polisstat-layout .col-sm-8 > .row + .row {
        margin-top: 18px;
      }
      .polisstat-layout .diagram-cell .girafe_container_std,
      .polisstat-layout .diagram-cell .html-widget {
        height: 100% !important;
        width: 100% !important;
      }
      /* Tightare gutters mellan diagram-cellerna i 2x2-rutnätet */
      .polisstat-layout .col-sm-6 {
        padding-left: 4px !important;
        padding-right: 4px !important;
      }
      .polisstat-layout .fluidRow,
      .polisstat-layout .row {
        margin-left: -4px;
        margin-right: -4px;
      }
      .polisstat-layout .diagram-toolbar {
        display: flex;
        align-items: flex-end;
        gap: 8px;
        margin-bottom: 12px;
        min-height: 50px;
      }
      .polisstat-layout .diagram-toolbar > div { flex: 1; min-width: 0; }
      .polisstat-layout .diagram-toolbar .form-group { margin-bottom: 0; }
      .polisstat-layout .diagram-toolbar .btn { flex: 0 0 auto; }

      /* Anpassad period - kompakt rad med två små listrutor */
      .polisstat-layout .anpassad-period {
        display: flex;
        gap: 6px;
        margin-top: 4px;
      }
      .polisstat-layout .anpassad-period .anpassad-cell {
        flex: 1; min-width: 0;
      }
      .polisstat-layout .anpassad-period label {
        font-size: 11px;
        font-weight: 400;
        margin-bottom: 2px;
        color: #555;
      }
      .polisstat-layout .anpassad-period .form-group { margin-bottom: 0; }
      .polisstat-layout .anpassad-period select.form-control {
        height: 30px;
        padding: 2px 6px;
        font-size: 12px;
      }

      /* Snyggare nedladdningsknappar - sida vid sida */
      .polisstat-layout .karta-knapp {
        margin-top: 10px;
        display: flex;
        gap: 6px;
      }
      .polisstat-layout .karta-knapp .btn {
        flex: 1;
        width: auto;
        background-color: #2c5aa0;
        color: #fff;
        border-color: #1e3f70;
        font-weight: 500;
        padding: 8px 10px;
        font-size: 13px;
        white-space: normal;
        line-height: 1.2;
      }
      .polisstat-layout .karta-knapp .btn:hover {
        background-color: #1e3f70;
        color: #fff;
      }

      /* ---- Responsivt: stacka allt under 992 px ---- */
      @media (max-width: 991px) {
        .polisstat-layout .col-sm-4,
        .polisstat-layout .col-sm-6,
        .polisstat-layout .col-sm-8 {
          width: 100% !important;
          max-width: 100% !important;
          flex: 0 0 100% !important;
        }
        .polisstat-layout #karta_brott { height: 50vh !important; }
        .polisstat-layout .diagram-cell {
          height: 40vh;
          margin-bottom: 12px;
        }
        /* När allt stackas behövs inte den tomma toolbar-platshållaren */
        .polisstat-layout .diagram-toolbar.spacer { display: none; }
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

             # Karta + diagram (responsiv layout via .polisstat-layout)
             div(class = "polisstat-layout",
                 fluidRow(
                   # Vänster: kartan + nedladdningsknapp
                   column(
                     width = 4,
                     leafletOutput("karta_brott", height = "70vh"),
                     div(class = "karta-knapp",
                         downloadButton("export_excel", "Hela datasetet", icon = icon("download")),
                         downloadButton("export_excel_urval", "Aktuellt urval", icon = icon("download"))
                     )
                   ),
                   # Höger: 2x2-rutnät med fyra diagram av samma storlek
                   column(
                     width = 8,
                     fluidRow(
                       column(
                         width = 6,
                         # Toolbar med brottsindelning + uppåtpil
                         div(class = "diagram-toolbar",
                             div(
                               selectInput(
                                 inputId = "val_indelning",
                                 label = "Brottsindelning",
                                 choices = NULL
                               )
                             ),
                             actionButton("diagram_back", label = NULL, icon = icon("level-up-alt"),
                                          class = "btn btn-light", title = "Gå tillbaka till högre nivå i diagrammet nedan")
                         ),
                         div(class = "diagram-cell",
                             girafeOutput("diagram_brottsomrade", width = "100%", height = "100%")
                         )
                       ),
                       column(
                         width = 6,
                         # Toolbar med tidsperiod
                         div(class = "diagram-toolbar",
                             div(
                               selectInput(
                                 inputId = "val_ar",
                                 label = "Välj tidsperiod:",
                                 choices = NULL
                               ),
                               conditionalPanel(
                                 condition = "input.val_ar == 'Anpassad period…'",
                                 div(class = "anpassad-period",
                                     div(class = "anpassad-cell",
                                         tags$label("Från:", `for` = "anpassad_fran"),
                                         selectInput("anpassad_fran", label = NULL, choices = NULL, width = "100%")
                                     ),
                                     div(class = "anpassad-cell",
                                         tags$label("Till:", `for` = "anpassad_till"),
                                         selectInput("anpassad_till", label = NULL, choices = NULL, width = "100%")
                                     )
                                 )
                               )
                             )
                         ),
                         div(class = "diagram-cell",
                             girafeOutput("diagram_manader", width = "100%", height = "100%")
                         )
                       )
                     ),
                     fluidRow(
                       column(
                         width = 6,
                         div(class = "diagram-cell",
                             girafeOutput("diagram_geografi", width = "100%", height = "100%")
                         )
                       ),
                       column(
                         width = 6,
                         div(class = "diagram-cell",
                             girafeOutput("diagram_veckodag", width = "100%", height = "100%")
                         )
                       )
                     )
                   )
                 )
             )
    ), # tabPanel Polisstatistik

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
