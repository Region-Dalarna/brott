library(shiny)
library(tidyverse)
library(leaflet)
library(ggiraph)
library(shinyjs)

# Liten alltid-synlig badge som markerar klickbara figurer
klick_hint <- function(text = "Klickbar – borra ner") {
  div(class = "klick-hint",
      icon("hand-pointer"),
      span(text))
}

shinyUI(tagList(

  tags$head(
    tags$title("Brottsförebyggande arbete i Dalarna"),
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),

    # Delad identitet för Regionala utvecklingsförvaltningen (petrolblå)
    # Poppins (Region Dalarnas primärtypsnitt) laddas via @font-face i css-filen
    tags$link(rel = "stylesheet", type = "text/css", href = "regiondalarna_ruf.css"),

    # App-specifik styling (laddas EFTER den delade så lokala regler kan
    # överstyra utan !important där så behövs)
    tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),

    # JS: sätt title-attribut på selectize-fältet så hela texten visas vid hover
    # även när vi har klippt av den med ellipsis
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('set_select_title', function(msg){",
      "  var item = document.querySelector('#' + msg.id + ' + .selectize-control .selectize-input .item');",
      "  if (item) { item.setAttribute('title', msg.title); }",
      "  var sel = document.getElementById(msg.id);",
      "  if (sel) { sel.setAttribute('title', msg.title); }",
      "});"
    )),
    # fada in klick-hint-badgen först när respektive diagram renderats
    tags$script(HTML(
      "$(document).on('shiny:value', function(event) {",
      "  if (event.name === 'diagram_brottsomrade' || event.name === 'diagram_geografi') {",
      "    $(event.target).closest('.diagram-cell--klickbar')",
      "      .find('.klick-hint').addClass('klick-hint--synlig');",
      "  }",
      "});"
    ))
  ),

  useShinyjs(),

  # ---- Header (ligger utanför fluidPage så den spänner hela bredden) ----
  tags$header(
    class = "rd-header",
    div(
      class = "rd-header__title",
      "Brottsförebyggande arbete i Dalarna"
    ),
    tags$a(
      href = "https://www.regiondalarna.se/verksamhet/regional-utveckling/statistik-och-rapporter/",
      target = "_blank",
      class = "rd-header__right",
      tags$img(src = "logo_liggande_fri_vit.png", alt = "Region Dalarna"),
      tags$span("Samhällsanalys")
    )
  ),

  # ---- Huvudinnehåll ----
  fluidPage(

    tabsetPanel(

      # ----------------------------
      # Flik 1: Karta & Diagram
      # ----------------------------
      tabPanel("Polisstatistik",

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
                           div(class = "diagram-cell diagram-cell--klickbar",
                               klick_hint("Klicka på en stapel för underkategorier"),
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
                           div(class = "diagram-cell diagram-cell--klickbar",
                               div(class = "klick-hint",
                                   icon("hand-pointer"),
                                   textOutput("geografi_klick_text", inline = TRUE)),
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
      # Flik 2: BRÅ kommunindikatorer
      # ----------------------------
      tabPanel("BRÅ kommunindikatorer",
               div(class = "bra-layout",
                   fluidRow(
                     # Vänster: kommunval + två nedladdningsknappar
                     column(
                       width = 2,
                       class = "bra-col-vanster",
                       selectInput("kommun", "Välj kommun:",
                                   choices = NULL,
                                   selected = "Dalarnas län"),
                       div(class = "bra-downloads",
                           downloadButton("export_excel_bra",
                                          "Ladda ner hela datasetet",
                                          icon = icon("download")),
                           downloadButton("export_excel_bra_kommun",
                                          "Ladda ner data för vald kommun",
                                          icon = icon("download"))
                       )
                     ),
                     # Mitten: NTU-filter + NTU-diagram
                     column(
                       width = 5,
                       div(class = "diagram-toolbar",
                           selectInput("variabel_ntu", "Välj NTU-variabel:",
                                       choices = NULL, width = "100%")
                       ),
                       div(class = "bra-diagram-cell",
                           girafeOutput("diagram_ntu", height = "100%", width = "100%")
                       )
                     ),
                     # Höger: Anmält brott-filter + två staplade diagram
                     column(
                       width = 5,
                       div(class = "diagram-toolbar",
                           selectInput("variabel_anm", "Välj variabel för anmälda brott:",
                                       choices = NULL, width = "100%")
                       ),
                       div(class = "bra-diagram-cell bra-diagram-cell--half",
                           girafeOutput("diagram_anm_brott", height = "100%", width = "100%")
                       ),
                       div(class = "bra-diagram-cell bra-diagram-cell--half",
                           girafeOutput("diagram_anm_brott_antal", height = "100%", width = "100%")
                       )
                     )
                   )
               )
      ),

      tabPanel("Instruktioner",
               div(
                 style = "margin-bottom: 20px; color: #666; font-size: 14px;",
                 HTML("
            <h3>Polisstatistik</h3>
            <p>
            Här kan du analysera brottsstatistik för Dalarna. Detta verktyg är en prototyp och det färdiga verktyget kommer att publiceras på annan plats. I och med det kommer denna sida att stängas ned.
            <ul>
            <li>Figurer markerade med handikonen <i>(Klickbar)</i> går att klicka i för att borra ner i statistiken.</li>
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

    ) # tabsetPanel

  ), # fluidPage

  # ---- Footer (ligger utanför fluidPage så den spänner hela bredden) ----
  tags$footer(
    class = "rd-footer",
    HTML(
      "Samhällsanalys, Region Dalarna &middot; ",
      "<a href='mailto:samhallsanalys@regiondalarna.se'>samhallsanalys@regiondalarna.se</a>"
    )
  )

)) # shinyUI och tagList
