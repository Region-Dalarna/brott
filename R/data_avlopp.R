# ---- Avloppsmätningar (RISE): narkotika och doping i avloppsvatten ----
#
# Tabellen rise.avloppsmatningar i sekretessdatabasen ligger i långt format med en rad per
# kommun (ev. plats inom kommunen), provtillfälle, ämne och måttenhet:
#
#   kommunkod      chr   4 siffror (SCB:s kommunkod)
#   kommun         chr   kommunnamn
#   plats          chr   mätplats inom kommunen, NA för kommuner som bara mäter på en plats.
#                        Tillkom när en kommun började mäta på två platser samtidigt - de
#                        flesta kommuner har bara en plats (eller NA).
#   rapportnummer  chr   id för labbrapporten raden kommer från (en rapport per kommun/plats/
#                        provtillfälle, en för narkotika och en för doping)
#   provdatum      date  faktiskt provtagningsdatum
#   provsasong     chr   "vinter", "vår", "sommar" eller "höst" - satt av RISE, inte av oss
#   substans       chr   ämnesnamn, t.ex. "Cannabis (THCA-metabolit)" eller "Testosteron"
#   enhet          chr   "Halt (μg/m3)", "Total halt (mg) per 1000 inv. & 24 h",
#                        "Doser totalt" eller "Doser per 1000 inv. & 24 h"
#   varde          dbl   mätvärde, NA om ämnet inte gick att detektera/kvantifiera
#   flagga         chr   detektionsstatus, t.ex. "under_LLOQ" eller "ej_detekterad"
#
# "Doser"-måtten beräknas bara för narkotikasubstanser (där ett dos-ekvivalent värde går att
# definiera), aldrig för dopingpreparat. Det utnyttjas i forbered_avloppsmatningar() för att
# dela upp ämnena i appens kategorival utan att hårdkoda vilka ämnen som hör till vilken grupp.

hamta_avloppsmatningar <- function(tabell) {
  # Uppkopplingen görs uttryckligen via paketet rdshinyappar (till skillnad från resten av appens
  # data, som läses via det äldre, URL-sourcade func_shinyappar.R).
  con <- rdshinyappar::shiny_uppkoppling_las(db_name = tabell$databas, db_user = tabell$anvandare)
  df <- tbl(con, dbplyr::in_schema(tabell$schema, tabell$tabell)) %>%
    collect()
  DBI::dbDisconnect(con)

  forbered_avloppsmatningar(df)
}

# Måttet som visas som standard i karta och stapeldiagram - befolkningsnormaliserat så att
# kommuner av olika storlek går att jämföra rättvist, och finns beräknat för alla ämnen.
avlopp_standardenhet <- "Total halt (mg) per 1000 inv. & 24 h"

# Säsongernas ordning inom ett kalenderår - används för sortering och heatmapens x-axel.
# Samtliga mätningar i datasetet har "vinter" daterad i december samma kalenderår som vår/
# sommar/höst, dvs. vintermätningen räknas inte till det kommande kalenderåret.
avlopp_sasong_ordning <- c(vår = 1L, sommar = 2L, höst = 3L, vinter = 4L)

forbered_avloppsmatningar <- function(df) {
  amnesgrupp_df <- df %>%
    mutate(ar_dos_matt = enhet %in% c("Doser totalt", "Doser per 1000 inv. & 24 h")) %>%
    group_by(substans) %>%
    summarise(amnesgrupp = if (any(ar_dos_matt)) "Narkotika" else "Doping", .groups = "drop")

  # Antal unika mätplatser per kommun, baserat på HELA datasetet (inte bara vald
  # period/ämne) - en kommun ska inte byta mellan att visas med och utan plats
  # beroende på vilket ämne som råkar vara valt. NA-plats räknas inte som en egen
  # plats, så kommuner som bara mäter på en plats (eller saknar platsuppgift helt)
  # räknas som en plats och visas utan platsnamn.
  antal_platser_per_kommun <- df %>%
    dplyr::filter(!is.na(plats)) %>%
    dplyr::distinct(kommunkod, plats) %>%
    dplyr::count(kommunkod, name = "n_platser")

  df %>%
    mutate(
      kommunkod = as.character(kommunkod),
      provdatum = as.Date(provdatum),
      ar = as.integer(format(provdatum, "%Y")),
      sasong_ordning = avlopp_sasong_ordning[provsasong],
      sasongsnyckel = ar * 10L + sasong_ordning,
      period = paste0(str_to_upper(str_sub(provsasong, 1, 1)), str_sub(provsasong, 2), " ", ar)
    ) %>%
    left_join(amnesgrupp_df, by = "substans") %>%
    left_join(antal_platser_per_kommun, by = "kommunkod") %>%
    mutate(
      n_platser = tidyr::replace_na(n_platser, 0L),
      # Visningsetikett som används genomgående i appen i stället för "kommun" -
      # "Kommun, Plats" för kommuner med fler än en plats, annars bara kommunnamnet.
      # Rader utan platsuppgift (plats = NA) faller tillbaka till kommunnamnet även
      # om kommunen i övrigt har flera platser, eftersom vi då inte vet vilken.
      kommun_etikett = ifelse(n_platser > 1 & !is.na(plats), paste0(kommun, ", ", plats), kommun)
    )
}

# Senaste mätningen per kommun (och plats, för de kommuner som mäter på flera) för ett valt
# ämne/enhet - oberoende kommun/plats för kommun/plats, eftersom de inte nödvändigtvis provtar
# samma säsonger. Om flera mätningar råkar finnas för samma kommun/plats och säsong tas
# medelvärdet. Används av stapeldiagram/heatmap/linjediagram (via kommun_etikett) - för kartan,
# som bara har en polygon per kommun, se avlopp_senaste_per_kommun_karta().
avlopp_senaste_per_kommun <- function(df, substans_vald, enhet_vald) {
  df %>%
    filter(substans == substans_vald, enhet == enhet_vald) %>%
    group_by(kommunkod, kommun, kommun_etikett, plats) %>%
    filter(sasongsnyckel == max(sasongsnyckel)) %>%
    summarise(
      period         = dplyr::first(period),
      sasongsnyckel  = dplyr::first(sasongsnyckel),
      varde          = mean(varde, na.rm = TRUE),
      flaggor        = paste(unique(stats::na.omit(flagga)), collapse = ", "),
      .groups = "drop"
    ) %>%
    mutate(varde = ifelse(is.nan(varde), NA_real_, varde))
}

# Mätvärden för EN specifik tidpunkt (säsong), en rad per kommun(/plats) - till skillnad från
# avlopp_senaste_per_kommun() som alltid tar den senaste tillgängliga säsongen oavsett om den
# skiljer sig åt mellan kommuner. Används av stapeldiagrammet när användaren valt en annan
# tidpunkt än standardvalet "senaste" - kommuner som inte provtogs just den säsongen saknas
# (eller får varde = NA om de har en rad för perioden men inget detekterbart värde).
avlopp_vid_tidpunkt <- function(df, substans_vald, enhet_vald, period_vald) {
  df %>%
    filter(substans == substans_vald, enhet == enhet_vald, period == period_vald) %>%
    group_by(kommunkod, kommun, kommun_etikett, plats) %>%
    summarise(
      period         = dplyr::first(period),
      sasongsnyckel  = dplyr::first(sasongsnyckel),
      varde          = mean(varde, na.rm = TRUE),
      flaggor        = paste(unique(stats::na.omit(flagga)), collapse = ", "),
      .groups = "drop"
    ) %>%
    mutate(varde = ifelse(is.nan(varde), NA_real_, varde))
}

# avlopp_senaste_per_kommun(), aggregerad till en rad per kommun(kod) - för kartan, som har en
# polygon per kommun och inte kan visa flera platser separat. Kommuner med flera platser
# färgsätts efter den plats som mättes senast (högst sasongsnyckel); om platserna råkar ha
# mätningar från samma säsong används istället den högsta av dem. Tooltipen är opåverkad av
# det här - den byggs separat direkt ur avlopp_senaste_per_kommun() och listar alla platser.
avlopp_senaste_per_kommun_karta <- function(df_senaste) {
  df_senaste %>%
    group_by(kommunkod, kommun) %>%
    filter(sasongsnyckel == max(sasongsnyckel)) %>%
    summarise(
      period  = dplyr::first(period),
      varde   = if (all(is.na(varde))) NA_real_ else max(varde, na.rm = TRUE),
      flaggor = paste(unique(stats::na.omit(unlist(strsplit(flaggor, ", ")))), collapse = ", "),
      .groups = "drop"
    )
}

# All data för ett ämne/enhet, en rad per kommun(/plats) och säsong - underlag för heatmapen
# och linjediagrammet över tid.
avlopp_over_tid <- function(df, substans_vald, enhet_vald) {
  df %>%
    filter(substans == substans_vald, enhet == enhet_vald) %>%
    group_by(kommunkod, kommun, kommun_etikett, plats, period, sasongsnyckel) %>%
    summarise(
      varde   = mean(varde, na.rm = TRUE),
      flaggor = paste(unique(stats::na.omit(flagga)), collapse = ", "),
      .groups = "drop"
    ) %>%
    mutate(varde = ifelse(is.nan(varde), NA_real_, varde),
           period = forcats::fct_reorder(period, sasongsnyckel))
}

# Dopingpreparaten grupperas enligt underlag från Hanna/RISE, för att underlätta tolkningen av
# ämnen som annars är svåra att skilja åt (se mejlet med gruppindelningen, okt 2026).
avlopp_doping_grupper <- list(
  "Grupp 1: Helt syntetiska substanser (saknar annan användning än doping)" = c(
    "Stanozolol", "3'-Hydroxystanozolol", "Oxandrolon", "Oxymesteron",
    "Oxymetolon", "Mesterolon", "Trenbolon", "Klordehydrometyltestosteron"
  ),
  "Grupp 2: Forskningen inte entydig, kan i vissa fall förekomma naturligt" = c(
    "19-Norandrosteron", "Boldenon", "Boldion", "4-Dihydroboldenon"
  ),
  "Grupp 3: Naturliga testosteronvarianter (sannolikt detekterbara i alla prov)" = c(
    "Testosteron", "Epitestosteron", "Androstanolon", "Androstenedion"
  )
)

# Ämnen som fungerar som kontrollvärden - relativt stabila över tid, så stora avvikelser mellan
# mätningar kan tyda på ett mätfel snarare än en verklig förändring i befolkningens konsumtion.
avlopp_kontrollvarden <- c(
  Kotinin = paste(
    "Kotinin är en nikotinmetabolit och ett relativt stabilt kontrollvärde.",
    "Stora skillnader mellan mätningar kan tyda på att något blivit fel i mätningen,",
    "snarare än en verklig förändring."
  ),
  Epitestosteron = paste(
    "Epitestosteron bildas naturligt i kroppen och är ett bra kontrollvärde.",
    "Nivån bör vara ungefär densamma över tid."
  )
)

# Text som visas som underrubrik i diagrammen när ett kontrollvärde är valt, annars NULL.
avlopp_kontrollvarde_notis <- function(substans) {
  if (substans %in% names(avlopp_kontrollvarden)) unname(avlopp_kontrollvarden[substans]) else NULL
}

# Vilket ämne som fungerar som kontrollvärde-referens för respektive kategori - används för att
# visa en liten referensgraf vid sidan av huvudvyn (se avlopp_kontroll_amne() i server.R).
avlopp_kontrollamne_for_kategori <- c(Narkotika = "Kotinin", Doping = "Epitestosteron")

# Bygger choices till ämnesväljaren. Narkotika blir en platt, alfabetisk lista; Doping grupperas
# i optgroups enligt avlopp_doping_grupper. Kontrollvärden (Kotinin, Epitestosteron) märks ut i
# den synliga etiketten, men det underliggande värdet (det som når input$avlopp_substans och
# används för att filtrera data) är alltid det rena ämnesnamnet.
avlopp_amnesval <- function(amnen_i_data, kategori) {
  etikett <- function(x) {
    ifelse(x %in% names(avlopp_kontrollvarden), paste0(x, " (kontrollvärde)"), x)
  }

  if (identical(kategori, "Doping")) {
    grupper <- purrr::map(avlopp_doping_grupper, ~ intersect(.x, amnen_i_data))
    grupper <- purrr::keep(grupper, ~ length(.x) > 0)
    purrr::map(grupper, ~ setNames(.x, etikett(.x)))
  } else {
    amnen <- sort(amnen_i_data)
    setNames(amnen, etikett(amnen))
  }
}
