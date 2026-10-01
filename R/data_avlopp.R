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
