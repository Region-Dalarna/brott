library(shiny)
library(tidyverse)
library(leaflet)
library(ggiraph)
library(shinyjs)

APP_BESKRIVNING <- "Polisstatistik, BRÅ:s kommunindikatorer och avloppsmätningar som stöd för det brottsförebyggande arbetet i Dalarna."

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

    # använd telemetry-paketet om det finns konfigurerat på maskinen (kommer att fungerar på servrarna men ignoreras om man kör appen lokalt)
    telemetri_ui(telemetry),

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
      tags$img(src = "logo_liggande_fri_vit.png", alt = "Region Dalarna")
    )
  ),

  # ---- Huvudinnehåll ----
  fluidPage(

    tabsetPanel(
      id = "flikval",

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

      # ----------------------------
      # Flik 3: Avloppsmätningar (RISE)
      # ----------------------------
      tabPanel("Avloppsmätningar",
               # Dyker upp som en notis nere till höger när man öppnar fliken, ligger kvar en
               # lång stund så att alla hinner läsa den, och går även att stänga direkt med
               # krysset - samma mönster som appens Utbildning och Branscher, men begränsat till
               # just den här fliken (inte hela appen).
               tags$div(
                 id = "avlopp-dev-notice", class = "rd-dev-notice", role = "status",
                 tags$button(
                   type = "button", class = "rd-dev-notice__stang",
                   onclick = "document.getElementById('avlopp-dev-notice').classList.add('rd-dev-notice--dold')",
                   `aria-label` = "Stäng meddelandet", HTML("&times;")
                 ),
                 h3("Fliken är under utveckling"),
                 p("Avloppsmätningar byggs och utvecklas löpande, så innehåll och utseende kan komma att ändras."),
                 p("Har du frågor eller funderingar? Hör gärna av dig till ",
                   tags$a(href = "mailto:samhallsanalys@regiondalarna.se", "samhallsanalys@regiondalarna.se"), "."),
                 tags$script(HTML(
                   "setTimeout(function() {",
                   "  var el = document.getElementById('avlopp-dev-notice');",
                   "  if (el) el.classList.add('rd-dev-notice--dold');",
                   "}, 25000);"
                 ))
               ),
               div(class = "avlopp-layout",
                   fluidRow(
                     # Vänster: karta, exakt samma storlek/form som i Polisstatistik-fliken
                     column(
                       width = 4,
                       leafletOutput("karta_avlopp", height = "70vh")
                     ),
                     # Höger: ämnesfilter överst, sedan de två diagrammen staplade
                     column(
                       width = 8,
                       div(class = "avlopp-filterrad",
                           div(class = "avlopp-filter",
                               selectInput("avlopp_kategori", "Kategori:",
                                           choices = c("Narkotika", "Doping"))
                           ),
                           div(class = "avlopp-filter",
                               selectInput("avlopp_substans", "Välj ämne:",
                                           choices = NULL)
                           )
                       ),
                       tags$details(class = "avlopp-info",
                           tags$summary(icon("info-circle"), "Så ska resultaten tolkas"),
                           tags$ul(
                             tags$li("Varje mätpunkt avser ett enskilt mättillfälle under angiven säsong - inte en period eller ett snitt."),
                             tags$li("Resultaten är ögonblicksbilder. Dra inte för stora slutsatser utan lokal kännedom om förhållandena i kommunen."),
                             tags$li("Jämförelser mellan kommuner bör göras med försiktighet, eftersom lokala faktorer (t.ex. befolkning, avloppsflöde, nederbörd) påverkar resultatet."),
                             tags$li("Värden går inte att jämföra mellan olika ämnen. En “dos” skiljer sig i mängd mellan preparat, t.ex. cannabis 125 mg, amfetamin 30 mg och kokain 100 mg (enligt EMCDDA)."),
                             tags$li("En lucka i diagrammen betyder att ämnet inte gick att detektera eller kvantifiera i just det provet - inte att halten var noll.")
                           )
                       ),
                       # Tidpunkt för stapeldiagrammet - "Senaste mätning per kommun" är
                       # standardvalet (se AVLOPP_TIDPUNKT_SENASTE i global.R), men man kan
                       # även välja en specifik säsong för att jämföra kommunerna vid exakt
                       # samma tidpunkt.
                       div(class = "avlopp-filter avlopp-stapel-toolbar",
                           selectInput("avlopp_tidpunkt", "Tidpunkt:", choices = NULL)
                       ),
                       # Höjden sätts dynamiskt i servern efter antal kommuner med data, så att
                       # diagrammen inte har onödigt tomrum idag men växer av sig själva i takt
                       # med att fler kommuner får mätningar.
                       uiOutput("avlopp_diagram_senaste_ui"),
                       div(class = "avlopp-tid-toolbar",
                           conditionalPanel(
                             condition = "input.avlopp_vy == 'linje'",
                             div(class = "avlopp-fokus-grupp",
                                 div(class = "avlopp-fokuskommun",
                                     tags$label("Fokusera på kommun:", `for` = "avlopp_fokuskommun",
                                                class = "avlopp-fokuskommun-label"),
                                     # "Jämför alla kommuner" (AVLOPP_FOKUS_ALLA, se global.R) nålas
                                     # fast överst i dropdown-panelen via CSS (se
                                     # .option[data-value="__alla_kommuner__"] i app.css) - annars
                                     # scrollar selectize ned till den redan valda kommunen när
                                     # listan öppnas och gömmer alternativet.
                                     selectInput("avlopp_fokuskommun", NULL, choices = NULL, width = "100%")
                                 ),
                                 # Bara meningsfull (och synlig) när en enskild kommun är vald -
                                 # annars finns ingen "egen kommun" att visa bara den.
                                 conditionalPanel(
                                   condition = sprintf("input.avlopp_fokuskommun != '%s'", AVLOPP_FOKUS_ALLA),
                                   div(class = "avlopp-bara-fokus",
                                       checkboxInput("avlopp_bara_fokus", "Visa bara min kommun", value = FALSE)
                                   )
                                 )
                             )
                           ),
                           radioButtons("avlopp_vy", NULL,
                                        choices = c("Linjediagram" = "linje", "Heatmap" = "heatmap"),
                                        selected = "linje", inline = TRUE)
                       ),
                       uiOutput("avlopp_diagram_tid_ui"),
                       # Liten referensgraf för kontrollvärdet (Kotinin/Epitestosteron) - visas
                       # bara i linjevyn, och bara när man inte redan tittar på kontrollvärdet.
                       uiOutput("avlopp_diagram_kontroll_ui")
                     )
                   )
               )
      ),

      tabPanel(
        "Om rapporten",
        div(
          style = paste(
            "margin-bottom: 20px;",
            "color: #666;",
            "max-width: 800px;",
            "font-size: 14px;"
          ),
          HTML("
      <p>
        Rapporten är skapad av Samhällsanalys, Region Dalarna. Syftet är att
        avlasta brottsförebyggande tjänstepersoner i kommunerna och på Länsstyrelsen
        genom att underlätta visualisering och analys av brottsstatistik från
        Polisen och från Brottsförebyggande rådet (BRÅ). Samhällsanalys har god
        kompetens att bearbeta och visualisera data medan ovan nämnda tjänstepersoner
        har god kompetens att bedriva brottsförebyggande arbete. Därmed frigörs tid
        för de brottsförebyggande aktörerna åt brottsförebyggande arbete, vilket
        därmed kan bidra till länets utveckling.
      </p>

      <p>
        <b>Kontaktuppgifter</b><br>
        <a href='mailto:samhallsanalys@regiondalarna.se?subject=Webbrapport Brottsförebyggande arbete i Dalarna'>
          Samhällsanalys, Region Dalarna
        </a>
        för frågor om webbrapporten.<br>
        <a href='mailto:malin.kapla@lansstyrelsen.se?subject=Webbrapport Brottsförebyggande arbete i Dalarna'>
          Brottsförebyggande samordnare, Länsstyrelsen Dalarna
        </a>
        för frågor om det brottsförebyggande arbetet i Dalarna.
      </p>
      <br>
      <h4>Polisstatistik</h4>
      <p>
        Här kan du analysera brottsstatistik för Dalarna. Detta verktyg är en
        prototyp och det färdiga verktyget kommer att publiceras på annan plats.
        I och med det kommer denna sida att stängas ned.
      </p>

      <ul>
        <li>Figurer markerade med handikonen <i>(Klickbar)</i> går att klicka i för att borra ner i statistiken.</li>
        <li>Klicka på staplarna i det stora diagrammet för att se underkategorier av brott.</li>
        <li>Du går tillbaka med uppåtpilen under diagrammet.</li>
        <li>Klicka på en kommun i kartan för att se brott i kommunen och hur de fördelar sig per demografiskt statistikområde (DeSO).</li>
        <li>Klicka på husikonen uppe till vänster i kartan för att se alla kommuner igen.</li>
        <li>För att ladda ner hela datasetet för alla kommuner i Dalarna som en Excelfil, klicka på <i>Hela datasetet</i>.</li>
        <li>För att spara ett diagram, för muspekaren över diagrammet och klicka på den blå ikonen högst upp till höger.</li>
      </ul>

      <h4>Brottsförebyggande rådets kommunindikatorer</h4>
      <p>
        Välj kommun samt indikator för anmälda brott respektive Nationella
        trygghetsundersökningen (NTU).
      </p>

      <p>
        2025 års NTU visar 2024 års anmälda brott och självrapporterade utsatthet.
        För frågor om självrapporterad otrygghet, oro och upplevda problem redovisas
        däremot resultaten för insamlingsåret och avser hur respondenterna upplever
        sin situation vid undersökningstillfället.
      </p>

      <ul>
        <li>För att ladda ner hela datasetet för alla kommuner i Dalarna som en Excelfil, klicka på <i>Ladda ner hela datasetet</i>.</li>
        <li>För att spara ett diagram, för muspekaren över diagrammet och klicka på den blå ikonen högst upp till höger.</li>
      </ul>
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
