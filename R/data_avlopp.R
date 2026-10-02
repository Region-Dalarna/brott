# ---- Avloppsmätningar (RISE): narkotika och doping i avloppsvatten ----
#
# Tabellen rise.avloppsmatningar i sekretessdatabasen ligger i långt format med en rad per
# kommun, provtillfälle, ämne och måttenhet:
#
#   kommunkod      chr   4 siffror (SCB:s kommunkod)
#   kommun         chr   kommunnamn
#   rapportnummer  chr   id för labbrapporten raden kommer från (en rapport per kommun/
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

  df %>%
    mutate(
      kommunkod = as.character(kommunkod),
      provdatum = as.Date(provdatum),
      ar = as.integer(format(provdatum, "%Y")),
      sasong_ordning = avlopp_sasong_ordning[provsasong],
      sasongsnyckel = ar * 10L + sasong_ordning,
      period = paste0(str_to_upper(str_sub(provsasong, 1, 1)), str_sub(provsasong, 2), " ", ar)
    ) %>%
    left_join(amnesgrupp_df, by = "substans")
}

# Senaste mätningen per kommun för ett valt ämne/enhet - oberoende kommun för kommun, eftersom
# kommunerna inte nödvändigtvis provtar samma säsonger. Om flera mätningar råkar finnas för
# samma kommun och säsong tas medelvärdet.
avlopp_senaste_per_kommun <- function(df, substans_vald, enhet_vald) {
  df %>%
    filter(substans == substans_vald, enhet == enhet_vald) %>%
    group_by(kommunkod, kommun) %>%
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

# All data för ett ämne/enhet, en rad per kommun och säsong - underlag för heatmapen över tid.
avlopp_over_tid <- function(df, substans_vald, enhet_vald) {
  df %>%
    filter(substans == substans_vald, enhet == enhet_vald) %>%
    group_by(kommunkod, kommun, period, sasongsnyckel) %>%
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
