
# ---- Hjälpfunktioner ----

filtrera_data <- function(df, kommun, indelning, niva, brott_niva_nyckel_df) {
  if (kommun != "Alla") df <- df %>% filter(kommunnamn == kommun)

  # Kolumnnamn för vald indelning
  niva_col <- "brott_niva" # indelning
  namn_col <- paste0(tolower(indelning), "_namn")

  # Slå ihop och filtrera
  df <- df %>%
    left_join(
      brott_niva_nyckel_df %>%
        select(brottskod, !!niva_col, !!namn_col),
      by = "brottskod",
      relationship = "many-to-many"
    ) %>%
    filter(.data[[niva_col]] == !!niva)

  return(df)
}

format_armanad <- function(armanad) {
  # "202203" -> "mar 2022"
  if (is.null(armanad) || is.na(armanad) || armanad == "") return("")
  d <- as.Date(paste0(armanad, "01"), "%Y%m%d")
  if (is.na(d)) return(as.character(armanad))
  paste0(format(d, "%b"), " ", format(d, "%Y"))
}

filtrera_tidsperiod <- function(df, period, fran = NULL, till = NULL) {
  if (period == "Hela perioden") {
    # Ingen filtrering - returnera all data
    return(df)
  }

  if (period == "Anpassad period…") {
    if (is.null(fran) || is.null(till) || fran == "" || till == "") return(df)
    rng <- sort(c(fran, till))
    df <- df %>% filter(`inskr årmånad` >= rng[1], `inskr årmånad` <= rng[2])
    return(df)
  }

  if (period == "Senaste 12 månaderna") {

    # Hämta senaste årmånad i datan
    senaste_armanad <- max(df$`inskr årmånad`, na.rm = TRUE)

    # Om vi fortfarande inte hittar något → avbryt tyst
    if (is.na(senaste_armanad)) {
      return(df)
    }


    # Skapa en sekvens med de senaste 12 månaderna
    senaste_armanad_datum <- as.Date(paste0(senaste_armanad, "01"), "%Y%m%d")
    senaste_seq <- format(seq(from = senaste_armanad_datum %m-% months(11),
                              to = senaste_armanad_datum,
                              by = "month"), "%Y%m")

    # Filtrera på dessa 12 månader
    df <- df %>% filter(`inskr årmånad` %in% senaste_seq)

  } else {
    df <- df %>% filter(`inskr år` == period)
  }
  return(df)
}

berakna_brott_per_100k <- function(df, bef_df, kommun, period, namn_col) {
  # Om "Alla" -> summera hela länet
  if (kommun == "Alla") {
    befolkning <- bef_df %>%
      filter(år == period) %>%
      summarise(bef = sum(bef, na.rm = TRUE)) %>%
      dplyr::pull(bef)
  } else {
    befolkning <- bef_df %>%
      filter(år == period, kommunnamn == kommun) %>%
      summarise(bef = sum(bef, na.rm = TRUE)) %>%
      dplyr::pull(bef)
  }

  df_sum <- df %>%
    group_by(across(all_of(namn_col))) %>%
    summarise(antal_brott = sum(antal_brott, na.rm = TRUE),
              befolkning = befolkning, .groups = "drop") %>%
    mutate(brott_per_100k = ifelse(befolkning > 0, (antal_brott / befolkning) * 100000, NA))

  return(df_sum)
}


summering_med_bef <- function(df, join_var, bef_df, period, extra_grp_var = NULL, brott_niva_nyckel_df) {

  tabort_kols <- names(brott_niva_nyckel_df) %>%
    .[str_detect(., "_namn")]

  # Ta bort Brottskategorier_namn från join_var om den finns
  join_geo <- join_var[!join_var %in% tabort_kols]

  # Lägg till extra_grp_var i grupperingen om den finns
  all_group_vars <- if (!is.null(extra_grp_var)) {
    c(join_var, extra_grp_var)
  } else {
    join_var
  }

  # Summera antal brott per grupp
  df_sum <- df %>%
    group_by(across(any_of(all_group_vars))) %>%
    summarise(antal_brott = sum(antal_brott, na.rm = TRUE), .groups = "drop")

  if (period == "Senaste 12 månaderna") {
    # Beräkna viktad befolkning baserat på antal månader per år
    # Använd den färdiga "inskr årmånad"-kolumnen istället för att parsa datum på nytt
    senaste_armanad <- max(df$`inskr årmånad`, na.rm = TRUE)
    senaste_datum <- as.Date(paste0(senaste_armanad, "01"), "%Y%m%d")
    start_armanad <- format(senaste_datum %m-% months(11), "%Y%m")

    manader_per_ar <- df %>%
      filter(`inskr årmånad` >= start_armanad) %>%
      mutate(år = str_sub(`inskr årmånad`, 1, 4),
             månad = str_sub(`inskr årmånad`, 5, 6)) %>%
      group_by(år, across(all_of(join_geo))) %>%
      summarise(antal_manader = n_distinct(månad), .groups = "drop") %>%
      left_join(bef_df, by = c("år", join_geo)) %>%
      mutate(viktad_bef = (bef / 12) * antal_manader)

    # Summera viktad befolkning per geografi (inte per extra grupperingsvariabler)
    viktad_bef_df <- manader_per_ar %>%
      group_by(across(all_of(join_geo))) %>%
      summarise(bef = sum(viktad_bef, na.rm = TRUE), .groups = "drop")

    # Join tillbaka endast på geografi
    df_sum <- df_sum %>%
      left_join(viktad_bef_df, by = join_geo) %>%
      mutate(brott_per_100k = ifelse(bef > 0, round((antal_brott / bef) * 100000), NA))

  } else if (period == "Hela perioden" || period == "Anpassad period…") {
    # Hela perioden / anpassad period: använd summan av befolkningen för alla år som finns i datan
    # (dvs. person-år) som nämnare, så får vi ett genomsnittligt årligt antal per 100k
    # Optimerat: använd färdig "inskr år"-kolumn istället för att re-parsa datum per rad
    ar_i_data <- unique(as.character(df$`inskr år`))

    bef_for_join <- bef_df %>%
      filter(år %in% ar_i_data) %>%
      group_by(across(all_of(join_geo))) %>%
      summarise(bef = sum(bef, na.rm = TRUE), .groups = "drop")

    df_sum <- df_sum %>%
      left_join(bef_for_join, by = join_geo) %>%
      mutate(brott_per_100k = ifelse(bef > 0, round((antal_brott / bef) * 100000), NA))

  } else {
    # För ett helt år: använd befolkning för det året
    bef_for_join <- bef_df %>%
      filter(år == period) %>%
      group_by(across(all_of(join_geo))) %>%
      summarise(bef = sum(bef, na.rm = TRUE), .groups = "drop")

    df_sum <- df_sum %>%
      left_join(bef_for_join, by = join_geo) %>%
      mutate(brott_per_100k = ifelse(bef > 0, round((antal_brott / bef) * 100000), NA))
  }

  return(df_sum)
}

# ---- Källhänvisningar ----
KALLA_POLISEN <- "Källa: Polisen, bearbetning av Samhällsanalys, Region Dalarna"
KALLA_BRA     <- "Källa: Brottsförebyggande rådet (BRÅ), bearbetning av Samhällsanalys, Region Dalarna"

# ---- Server ----
shinyServer(function(input, output, session) {

  # Visa laddningsmeddelande direkt när appen startar
  showNotification("Data laddas, vänta lite…",
                   id = "loading_msg",
                   duration = NULL,
                   closeButton = FALSE,
                   type = "message")

  kartniva <- reactiveVal("kommun")                             # kartnivå i kartan kommun och deso, ska vi ha en till där vi tittar på enskilda deso?
  vald_kommun <- reactiveVal("Alla")                            # vald kommun i kartan
  vald_deso <- reactiveVal(NULL)                                # hantera deso i geografi-diagrammet
  brott_niva <- reactiveVal(1)                                  # Nivå för hierarkin vi är i
  rubrik_brott_niva <- reactiveVal(list())                      # Här bygger vi listor med nivårubriker för att kunna borra ner och tillbaka upp
  vald_indelning <- reactiveVal("brottskategorier")             # typ av brottshierarki (polisens egna = Brottskategorier), eller egna
  vald_max_niva_kategori <- reactiveVal(NULL)  # Håller reda på vald kategori på max-nivå


  meta_polisen <- reactive({
    tbl(
      shiny_uppkoppling_las("oppna_data"),
      dbplyr::in_schema("metadata", "polisen")
    ) %>%
      summarise(senast = max(version, na.rm = TRUE)) %>%
      collect()
  })

  brottsdata <- reactive({
    meta_polisen()   # ← trigger

    tbl(
      shiny_uppkoppling_las("oppna_data"),
      dbplyr::in_schema("polisen", "brott")
    ) %>%
      collect()
  })

  brott_niva_nyckel <- reactive({
    meta_polisen()   # ← trigger

    tbl(
      shiny_uppkoppling_las("oppna_data"),
      dbplyr::in_schema("polisen", "brott_niva_nyckel")
    ) %>%
      collect()
  })


  # Lazy load för BRÅ (flik 2)
  bra_kommunindikatorer <- reactiveVal(NULL)

  session$onFlushed(function() {
    bra_kommunindikatorer(
      tbl(
        shiny_uppkoppling_las("oppna_data"),
        dbplyr::in_schema("bra", "kommunindikatorer")
      ) %>%
        filter(variabel != "Antal svarande i NTU.") %>%
        collect()
    )
  }, once = TRUE)


  # uppdatera inputkontroll i ui för brottsnivå
  observe({
    val <- names(brott_niva_nyckel()) %>%
      str_subset("_namn$") %>%
      str_remove("_namn$")

    updateSelectInput(
      session,
      "val_indelning",
      choices = val,
      selected = val[1]
    )
  })

  # updatera inputkontroll i ui för tidsperiod
  observe({
    ar <- sort(unique(format(brottsdata()$inskrivningsdatum, "%Y")), decreasing = TRUE)

    updateSelectInput(
      session,
      "val_ar",
      choices = c("Senaste 12 månaderna", "Hela perioden", "Anpassad period…", ar),
      selected = "Senaste 12 månaderna"
    )

    # YYYY-MM-valen för "Anpassad period…"
    armanader <- sort(unique(format(brottsdata()$inskrivningsdatum, "%Y%m")))
    if (length(armanader) > 0) {
      etiketter <- vapply(armanader, format_armanad, character(1))
      val_vec <- setNames(armanader, etiketter)
      updateSelectInput(session, "anpassad_fran",
                        choices = val_vec,
                        selected = armanader[1])
      updateSelectInput(session, "anpassad_till",
                        choices = val_vec,
                        selected = armanader[length(armanader)])
    }
  })

  # för att använda i kart- och diagramrubriker, Dalarna eller vald kommun
  geografi_text <- reactive({
    if (vald_kommun() == "Alla") {
      "i Dalarna"
    } else {
      paste0("i ", vald_kommun())
    }
  })

  vald_deso_namn <- reactive({
    # Bara aktuellt när vi är på DeSO-nivå och något är valt
    if (kartniva() != "deso" || is.null(vald_deso())) return(NULL)

    geo_data <- data_beredd()
    df_map   <- geo_data$data %>% sf::st_drop_geometry()
    join_var <- geo_data$join_var   # "desokod"
    join_namn <- geo_data$join_namn # "regsonamn"

    nm <- df_map %>%
      dplyr::filter(.data[[join_var]] == vald_deso()) %>%
      dplyr::pull(.data[[join_namn]]) %>%
      unique()

    if (length(nm) == 0) return(NULL)
    nm[1]
  })


  # för att använda i kart- och diagramrubriker, år eller de senaste 12 månaderna
  tidsperiod_text <- reactive({
    if (input$val_ar == "Senaste 12 månaderna") {
      "de senaste 12 månaderna"
    } else if (input$val_ar == "Hela perioden") {
      "hela perioden"
    } else if (input$val_ar == "Anpassad period…") {
      req(input$anpassad_fran, input$anpassad_till)
      paste0(format_armanad(input$anpassad_fran), "–", format_armanad(input$anpassad_till))
    } else {
      paste0("år ", input$val_ar)
    }
  })

  tidsperiod_text_karta <- reactive({
    if (input$val_ar == "Senaste 12 månaderna") {
      "sen 12 mån"
    } else if (input$val_ar == "Hela perioden") {
      "hela perioden"
    } else if (input$val_ar == "Anpassad period…") {
      req(input$anpassad_fran, input$anpassad_till)
      paste0(format_armanad(input$anpassad_fran), "–", format_armanad(input$anpassad_till))
    } else {
      paste0("år ", input$val_ar)
    }
  })

  # skapa tooltip till karta
  etiketter_karta <- reactive({
    df_map <- data_beredd()$data
    join_namn <- data_beredd()$join_namn

    # Brottsbenämning (finaste nivån)

    # Brottsbenämning (finaste nivån) + ev. max-nivå-filter
    brottsbenamning <- if (length(rubrik_brott_niva()) > 0) {
      rubrik_brott_niva()[length(rubrik_brott_niva())]
    } else {
      "Alla brott"
    }

    kat_max <- vald_max_niva_kategori()
    if (!is.null(kat_max)) {
      # Lägg till max-nivån i texten
      brottsbenamning <- paste0(brottsbenamning, " – ", kat_max)
    }


    # Skapa etiketter
    map(seq_len(nrow(df_map)), function(i) {
      htmltools::HTML(paste0(
        df_map[[join_namn]][i], "<br>",
        "<b>", brottsbenamning, "</b><br>",
        format(round(df_map$brott_per_100k[i]), big.mark = " "), " per 100.000 inv<br>",
        format(df_map$antal_brott[i], big.mark = " "), " brott totalt<br>",
        "<i>", str_to_sentence(tidsperiod_text()), "</i>"
      ))
    })
  })


  # --- Text till kartans övre högra hörn ---
  label_for_map <- function() {
    hierarki <- rubrik_brott_niva()
    kat_max <- vald_max_niva_kategori()

    # Om listan är tom, visa "Alla brott"
    if (length(hierarki) == 0) {
      brott_text <- "Alla brott"
    } else {
      # Bygg hierarkin med radbrytningar mellan nivåerna
      brott_text <- paste(hierarki, collapse = "<br>→ ")
    }


    # Lägg till info om max-nivå-filter om något är valt
    if (!is.null(kat_max)) {
      brott_text <- paste0(
        brott_text,
        "<br>", kat_max
      )
    }


    # Lägg till kommun
    kommun_text <- if (vald_kommun() == "Alla") {
      "i Dalarna"
    } else {
      paste0("i ", vald_kommun())
    }

    paste0(brott_text, "<br>", kommun_text, "<br>", tidsperiod_text_karta())
  }

  data_beredd <- reactive({
    req(input$val_ar)
    req(vald_indelning())

    niva <- brott_niva()
    hierarki <- rubrik_brott_niva()
    indelning <- vald_indelning()

    # Samma filtrering som i diagrammet
    brottskoder_filtrering <- brott_niva_nyckel() %>%
      {
        if (length(hierarki) > 0) {
          filter(., .data[[paste0(indelning, "_namn")]] == hierarki[length(hierarki)])
        } else {
          .
        }
      } %>% dplyr::pull(brottskod)

    # Lägg till filtrering för vald kategori på max-nivå
    if (!is.null(vald_max_niva_kategori())) {
      brottskoder_maxniva <- brott_niva_nyckel() %>%
        filter(.data[[paste0(indelning, "_namn")]] == vald_max_niva_kategori()) %>%
        dplyr::pull(brottskod)

      brottskoder_filtrering <- intersect(brottskoder_filtrering, brottskoder_maxniva)
    }

    df_filtrering <- brottsdata() %>%
      filter(brottskod %in% brottskoder_filtrering)

    df <- filtrera_data(df_filtrering, vald_kommun(), indelning, niva, brott_niva_nyckel())

    if (kartniva() == "kommun") {
      geo      <- kommun_sf
      join_var <- "kommunkod"
      join_namn <- "kommunnamn"
    } else {
      geo      <- deso_sf %>%
        filter(kommunnamn == vald_kommun())
      join_var <- "desokod"
      join_namn <- "regsonamn"

      # Lägg på regsonamn från brottsdata() så kolumnen alltid finns
      regsonamn_lookup <- brottsdata() %>%
        filter(kommunnamn == vald_kommun()) %>%
        select(desokod, regsonamn) %>%
        distinct()

      geo <- geo %>%
        left_join(regsonamn_lookup, by = "desokod")
    }

    if (nrow(df) == 0) {
      return(list(
        data      = geo %>% mutate(antal_brott = 0, brott_per_100k = NA),
        join_var  = join_var,
        join_namn = join_namn
      ))
    }

    df <- filtrera_tidsperiod(df, input$val_ar, input$anpassad_fran, input$anpassad_till)                                 # filtrera på tidsperiod
    df_sum <- summering_med_bef(df, join_var, bef_df, input$val_ar, brott_niva_nyckel_df = brott_niva_nyckel())             # summera ihop antal brott och beräkna brott per 100k invånare

    # Slå ihop och behåll join_namn kolumnen
    geo_joined <- left_join(geo, df_sum, by = join_var) %>%
      mutate(antal_brott = replace_na(antal_brott, 0))

    list(
      data = geo_joined,
      join_var = join_var,
      join_namn = join_namn
    )
  })


  # Hjälpfunktion som ritar/uppdaterar kartan
  rendera_karta <- function(df_map, join_var, join_namn,
                            ar_uppdatering = FALSE,
                            legend_titel = NULL) {

    # 1. Hantera fallet inga data

    if (nrow(df_map) == 0 || all(is.na(df_map$brott_per_100k))) {

      if (!ar_uppdatering) {
        return(
          #leaflet(df_map) |>
          leaflet() |>
            addProviderTiles("CartoDB.Positron") |>
            addControl("Inga data för vald nivå/kombination",
                       position = "topright",
                       className = "map-filter-text")
        )
      } else {
        return(
          #leafletProxy("karta_brott", data = df_map) |>
          leafletProxy("karta_brott") |>
            clearShapes() |>
            clearControls() |>
            addControl("Inga data för vald nivå/kombination",
                       position = "topright",
                       className = "map-filter-text")
        )
      }
    }

    # Skapa karta för normalfallet, dvs. att det finns data

    # 2. Färgskala
    pal <- colorNumeric("YlOrRd", df_map$brott_per_100k,
                        na.color = "transparent")

    # 3. Markering av valt DeSO (TRUE/FALSE per rad)
    df_map <- df_map %>%
      dplyr::mutate(
        vald_geom = if (kartniva() == "deso" && !is.null(vald_deso())) {
          .data[[join_var]] == vald_deso()
        } else {
          FALSE
        }
      )

    # 4. Tooltip-texter & bbox
    etiketter <- etiketter_karta()
    bbox <- sf::st_bbox(df_map)

    # 5. Legendtitel
    if (is.null(legend_titel)) {
      legend_titel <- if (kartniva() == "kommun") {
        "Brott per 100k inv"
      } else {
        paste0("Brott per 100k inv – ", vald_kommun())
      }
    }

    # 6. Välj bas-objekt: ny karta eller uppdatering
    bas <- if (!ar_uppdatering) {
      leaflet(df_map) %>%
        addProviderTiles("CartoDB.Positron")
    } else {
      leafletProxy("karta_brott", data = df_map) %>%
        clearShapes() %>%
        clearControls()
    }

    # 7. Bygg upp kartan
    kartobj <- bas %>%
      addPolygons(
        layerId    = df_map[[join_namn]],
        fillOpacity = ~ifelse(vald_geom, 0.95, ifelse(antal_brott == 0, 0, 0.6)),
        color       = ~ifelse(vald_geom, "#ff0000", "#555555"),   # röd kant för vald DeSO
        weight      = ~ifelse(vald_geom, 3, 0.7),                 # mycket tjockare ram
        fillColor   = ~ifelse(vald_geom, "#ff6666", pal(brott_per_100k)),  # ljusröd fyllning
        #fillColor  = ~pal(brott_per_100k),
        label  = etiketter,
        highlightOptions = highlightOptions(
          weight = 3, color = "#000000", bringToFront = TRUE
        )
      ) %>%
      addLegend(
        "bottomleft", pal = pal, values = ~brott_per_100k,
        title = legend_titel,
        labFormat = labelFormat(big.mark = " ", digits = 0),
        className = "info legend kompakt-legend"
      ) %>%
      addControl(label_for_map(),
                 position = "topright",
                 className = "map-filter-text")

    # Styr zoom-nivån.
    # Asymmetrisk padding: mer luft till vänster så att polygonerna förskjuts
    # åt höger (bort från teckenförklaringen). Vi krymper också bbox något så
    # att polygonerna fyller en större del av kartan.
    bbox_w <- bbox[["xmax"]] - bbox[["xmin"]]
    bbox_h <- bbox[["ymax"]] - bbox[["ymin"]]

    kartobj <- kartobj %>%
      fitBounds(
        lng1 = bbox[["xmin"]] + bbox_w * 0.04,
        lat1 = bbox[["ymin"]] + bbox_h * 0.04,
        lng2 = bbox[["xmax"]] - bbox_w * 0.04,
        lat2 = bbox[["ymax"]] - bbox_h * 0.04,
        options = list(
          paddingTopLeft     = c(80, 5),
          paddingBottomRight = c(5, 5)
        )
      )

    # hantering av hus-ikonen, dvs. gå tillbaka till kommunnivå
    if (!ar_uppdatering) {
      kartobj <- kartobj %>%
        addEasyButton(
          easyButton(
            icon = "fa-home",
            title = "Visa alla kommuner igen",
            onClick = JS("function(btn, map){ Shiny.setInputValue('reset_map', true); }")
          )
        )
    }

    return(kartobj)
  }

  # --- Startkarta vid laddning ---
  output$karta_brott <- renderLeaflet({

    data_karta <- data_beredd()
    rendera_karta(
      df_map       = data_karta$data,
      join_var     = data_karta$join_var,
      join_namn    = data_karta$join_namn,
      ar_uppdatering = FALSE
    )

  })

  # --- Klick på kommun i kartan ---
  observeEvent(input$karta_brott_shape_click, {
    click <- input$karta_brott_shape_click
    req(click$id)

    if (kartniva() == "kommun") {
      # klick på kommun
      #if (is.null(click$id)) return()

      vald_kommun(click$id)
      vald_deso(NULL)
      kartniva("deso")

      geo_data <- data_beredd()

      rendera_karta(
        df_map        = geo_data$data,
        join_var      = geo_data$join_var,
        join_namn     = geo_data$join_namn,
        ar_uppdatering = TRUE
      )
    } else {

      # ----- Klick på DESO -----
      geo_data  <- data_beredd()
      df_map    <- geo_data$data
      join_var  <- geo_data$join_var   # "desokod"
      join_namn <- geo_data$join_namn  # "regsonamn"

      # Matcha klickat regsonamn -> desokod
      desokod_klickad <- df_map |>
        sf::st_drop_geometry() |>
        dplyr::filter(.data[[join_namn]] == click$id) |>
        dplyr::pull(.data[[join_var]]) |>
        unique()

      if (length(desokod_klickad) == 1) {
        # Toggla vald_deso (samma logik som i geografi-diagrammet)
        if (!is.null(vald_deso()) && vald_deso() == desokod_klickad) {
          vald_deso(NULL)
        } else {
          vald_deso(desokod_klickad)
        }

        # Uppdatera kartan: samma data, men ny markering
        rendera_karta(
          df_map         = df_map,
          join_var       = join_var,
          join_namn      = join_namn,
          ar_uppdatering = TRUE
        )
      }

    }
  })

  # --- Uppdatera karta när brottsnivå ändras ---
  observeEvent(brott_niva(), {

    geo_data <- data_beredd()
    rendera_karta(
      df_map        = geo_data$data,
      join_var      = geo_data$join_var,
      join_namn     = geo_data$join_namn,
      ar_uppdatering = TRUE
    )

  })

  observeEvent(input$val_indelning, {

    vald_indelning(input$val_indelning)
    brott_niva(1)
    rubrik_brott_niva(list())
    vald_max_niva_kategori(NULL)                     # nollställ vald kategori på max-nivå när indelning ändras

  }, ignoreInit = TRUE)

  # --- Återställning till kommunnivå ---
  observeEvent(input$reset_map, {

    kartniva("kommun")
    vald_kommun("Alla")
    vald_deso(NULL)

    df_karta <- data_beredd()

    rendera_karta(
      df_map        = df_karta$data,
      join_var      = df_karta$join_var,
      join_namn     = df_karta$join_namn,
      ar_uppdatering = TRUE
    )

    # Nollställ reset-flaggan så att nästa klick fungerar
    shinyjs::runjs("Shiny.setInputValue('reset_map', null);")
  }, ignoreInit = TRUE)


  # ========================= diagrammen ==========================================
  output$diagram_brottsomrade <- renderGirafe({

    indelning <- vald_indelning()
    niva <- brott_niva()
    kommun <- vald_kommun()

    # Kolumnnamn för vald indelning
    niva_col <- "brott_niva"  # indelning
    namn_col <- paste0(indelning, "_namn")

    # Hämta max-nivå för aktuell indelning
    max_niva <- brott_niva_nyckel() %>%
      filter(!is.na(!!sym(namn_col))) %>%
      dplyr::pull(!!niva_col) %>%
      max(na.rm = TRUE)

    # Bestäm vilken nivå vi ska visa i diagrammet
    if (niva >= max_niva) {
      visa_niva <- niva
    } else {
      visa_niva <- niva
    }

    # skapa en vektor med alla brottskoder i
    brottskoder_filtrering <- brott_niva_nyckel() %>%
      {
        if (length(rubrik_brott_niva()) > 0) {
          filter(., .data[[paste0(indelning, "_namn")]] == rubrik_brott_niva()[length(rubrik_brott_niva())])
        } else {
          .
        }
      } %>% dplyr::pull(brottskod)

    df_filtrering <- brottsdata() %>%
      filter(brottskod %in% brottskoder_filtrering)

    df_visa <- filtrera_data(df_filtrering, kommun, indelning, visa_niva, brott_niva_nyckel())
    df_visa <- filtrera_tidsperiod(df_visa, input$val_ar, input$anpassad_fran, input$anpassad_till)

    # Filtrera på vald DeSO om vi är på DeSO-nivå
    if (kartniva() == "deso" && !is.null(vald_deso())) {
      df_visa <- df_visa %>% filter(desokod == vald_deso())
    }

    if (nrow(df_visa) == 0) {
      return(girafe(ggobj = ggplot() + theme_void()))
    }

    # Ta bort laddningsmeddelandet så fort vi har data att rendera
    removeNotification("loading_msg")

    if (vald_kommun() == "Alla") {
      df_visa <- df_visa %>% mutate(kommunkod = str_sub(kommunkod, 1, 2))
      bef_sum <- bef_df %>% mutate(kommunkod = str_sub(kommunkod, 1, 2))
    } else bef_sum <- bef_df

    df_sum <- summering_med_bef(df_visa, c("kommunkod", namn_col), bef_sum, input$val_ar, brott_niva_nyckel_df = brott_niva_nyckel())

    vald_kat <- vald_max_niva_kategori()

    df_sum <- df_sum %>%
      arrange(desc(antal_brott)) %>%
      mutate(
        kategori_namn = .data[[namn_col]],
        kategori_kort = paste0(str_sub(kategori_namn, 1, 20), "..."),
        etikett_antal = paste0(kategori_namn, " ", format(antal_brott, big.mark = " ")),
        etikett = paste0(kategori_namn, "\n", format(round(brott_per_100k), big.mark = " "), " per 100 000 inv\n",
                         paste0(format(antal_brott, big.mark = " "), " brott totalt")),
        grupp_kort = str_wrap(kategori_namn, width = 35),
        farg = if (is.null(vald_kat)) "#3182bd" else {
          ifelse(kategori_namn == vald_kat, "#e31a1c", "#3182bd")
        }
      )

    if (nrow(df_sum) == 0) {
      return(girafe(ggobj = ggplot() + theme_void()))
    }

    # Skapa titel
    # om det finns ett vald_deso_namn() så används det, annars kommunnamn eller hela länet
    geo_txt <- if (is.null(vald_deso_namn())) geografi_text() else vald_deso_namn()

    titel <- if (length(rubrik_brott_niva()) > 0) rubrik_brott_niva()[length(rubrik_brott_niva())] else "Alla brott"
    titel <- paste0(titel, " ", geo_txt, " ", tidsperiod_text())
    titel <- str_wrap(titel, width = 60)
    undertitel <- paste0(indelning, " - Nivå ", visa_niva)


    # Skapa diagram
    p <- ggplot(df_sum, aes(x = reorder(grupp_kort, brott_per_100k), y = brott_per_100k)) +
      geom_col_interactive(
        aes(tooltip = etikett,
            data_id = kategori_namn,
            fill = farg),     # kategori_namn
        color = NA
      ) +
      scale_fill_identity() +           # Använd färgerna direkt från data
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 5),
        labels = function(x) format(x, big.mark = " ", scientific = FALSE)
      ) +
      scale_x_discrete(labels = function(x) str_trunc(x, 25)) +
      labs(x = NULL, y = "Antal brott per 100.000 inv",
           title = titel, subtitle = undertitel,
           caption = KALLA_POLISEN) +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(
          size = 12, lineheight = 1.1, face = "bold",
          margin = margin(b = 4)
        ),
        plot.subtitle = element_text(
          size = 10, margin = margin(b = 6)
        ),
        plot.caption = element_text(
          size = 8, color = "#666", hjust = 0,
          margin = margin(t = 6)
        ),
        plot.margin = margin(t = 4, r = 6, b = 2, l = 6),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "none"
      )

    girafe(
      ggobj = p,
      width_svg = 8,
      height_svg = 5,
      options = list(
        opts_sizing(rescale = TRUE, width = 1),
        opts_hover(css = "stroke-width:2;stroke:black;cursor:pointer;"),  # ändra bara kant + pekare
        opts_selection(type = "single", only_shiny = TRUE)
      )
    )
  })

  # --- Klick i diagram (samma som innan) ---
  observeEvent(input$diagram_brottsomrade_selected, ignoreInit = TRUE, ignoreNULL = TRUE, {
    req(input$diagram_brottsomrade_selected)

    # Rensa selection direkt så Shiny kan registrera nästa klick
    session$sendCustomMessage(type = 'diagram_brottsomrade_set', message = character(0))

    nuvarande_niva <- isolate(brott_niva())
    indelning <- isolate(vald_indelning())
    niva_col <- "brott_niva" # vald_indelning()
    namn_col <- paste0(indelning, "_namn")

    # Hämta brottskoder för den klickade kategorin
    brottskoder_klickad <- brott_niva_nyckel() %>%
      filter(.data[[namn_col]] == input$diagram_brottsomrade_selected,
             .data[[niva_col]] == nuvarande_niva) %>%
      dplyr::pull(brottskod)

    # Kontrollera om dessa brottskoder har poster på nästa nivå
    har_underniva <- brott_niva_nyckel() %>%
      filter(brottskod %in% brottskoder_klickad,
             .data[[niva_col]] == (nuvarande_niva + 1),
             !is.na(.data[[namn_col]])) %>%
      nrow() > 0

    # Öka nivån OCH uppdatera rubrik endast om det finns fler nivåer för denna kategori
    if (har_underniva) {
      # Om det finns undernivåer, gå djupare
      brott_niva(nuvarande_niva + 1)
      rubrik_brott_niva(append(rubrik_brott_niva(), input$diagram_brottsomrade_selected))
      vald_max_niva_kategori(NULL)  # Nollställ eventuell markering
    } else {
      # På max-nivå: Toggla filtrering
      if (!is.null(vald_max_niva_kategori()) && vald_max_niva_kategori() == input$diagram_brottsomrade_selected) {
        # Klick på samma kategori igen = avmarkera och gå upp en nivå
        vald_max_niva_kategori(NULL)
      } else {
        # Ny kategori vald på max-nivå = markera (men lägg INTE till i rubrik_brott_niva!)
        vald_max_niva_kategori(input$diagram_brottsomrade_selected)
      }
    }

  })

  # --- Tillbaka-knapp (samma som innan) ---
  observeEvent(input$diagram_back, {
    nuvarande_niva <- brott_niva()
    if (nuvarande_niva > 1) {
      brott_niva(nuvarande_niva - 1)

      # ta bort ett element i rubrik_brott_niva()
      nuvarande_lista <- rubrik_brott_niva()
      if (length(nuvarande_lista) > 0) rubrik_brott_niva(nuvarande_lista[-length(nuvarande_lista)])
      #print(paste0("Ny lista: ", rubrik_brott_niva()))
    }
  })

  # diagram för vald geografi
  output$diagram_geografi <- renderGirafe({

    # Hämta data från data_beredd()
    geo_data <- data_beredd()
    join_var <- geo_data$join_var
    join_namn <- geo_data$join_namn

    vald_geo <- vald_deso() # if (kartniva() == "kommun") vald_kommun() else vald_deso()


    # Skapa geografisk nivå-text (kommun eller deso)
    geo_niva_text <- if (kartniva() == "kommun") "kommun" else "DeSO-område"

    # Skapa brottkategori-text baserat på hierarkin
    hierarki <- rubrik_brott_niva()
    kat_max <- vald_max_niva_kategori()

    # Välj brottskategori till diagramrubriken
    if (!is.null(kat_max)) {
      brottkategori <- kat_max                            # Om max-nivå är vald: använd den
    } else if (length(hierarki) == 0) {
      brottkategori <- "Alla brott"                       # Ingen hierarki vald, dvs. alla brott visas
    } else {
      brottkategori <- hierarki[length(hierarki)]         # Annars senaste nivån i hierarkin
    }

    # Skapa titel
    titel <- paste0(brottkategori, " per ", geo_niva_text, " ", geografi_text(), " ", tidsperiod_text())
    titel <- str_wrap(titel, width = 60)

    # Förbered data för diagram
    df_diag <- geo_data$data %>%
      st_drop_geometry() %>%  # Ta bort geometrin för snabbare rendering
      mutate(etikett = paste0(
        .data[[join_namn]], "<br>",
        format(round(brott_per_100k), big.mark = " "), " per 100 000 inv", "<br>",
        paste0(format(antal_brott, big.mark = " "), " brott totalt")
      )) %>%
      filter(!is.na(brott_per_100k)) %>%
      arrange(desc(brott_per_100k))

    # hantera att vi kan ha att göra med deso som vi kan välja, annars kommuner


    df_diag <- df_diag %>%
      mutate(
        farg = if (is.null(vald_geo)) "#3182bd"
        else ifelse(.data[[join_var]] == vald_geo, "#e31a1c", "#3182bd")
      )

    # Skapa diagrammet
    p <- ggplot(df_diag, aes(x = reorder(.data[[join_namn]], brott_per_100k), y = brott_per_100k)) +
      geom_col_interactive(
        aes(tooltip = etikett,
            data_id = .data[[join_var]],
            fill = farg),
        color = NA
      ) +
      scale_fill_identity() +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 5),
        labels = function(x) format(x, big.mark = " ", scientific = FALSE)
      ) +
      scale_x_discrete(labels = function(x) str_trunc(x, 20)) +
      labs(x = NULL, y = "Antal brott per 100.000 inv", title = titel,
           caption = KALLA_POLISEN) +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(
          size = 12,
          lineheight = 1.1,
          face = "bold",
          margin = margin(b = 6)
        ),
        plot.caption = element_text(
          size = 8, color = "#666", hjust = 0,
          margin = margin(t = 6)
        ),
        plot.margin = margin(t = 4, r = 6, b = 2, l = 6),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "none"
      )

    girafe(
      ggobj = p,
      width_svg = 8,
      height_svg = 5,
      options = list(
        opts_sizing(rescale = TRUE, width = 1),
        opts_hover(css = "stroke-width:2;stroke:black;cursor:pointer;"),  # ändra bara kant + pekare
        opts_selection(type = "single", only_shiny = TRUE)
      ))
  })



  observeEvent(input$diagram_geografi_selected, {
    req(input$diagram_geografi_selected)

    # Rensa selection visuellt så nästa klick alltid registreras
    session$sendCustomMessage(type    = "diagram_geografi_set", message = character(0))

    klickad_kod <- input$diagram_geografi_selected  # kommunkod/desokod

    # --- Klick på kommunnivå ---
    if (kartniva() == "kommun") {

      vald_kommunkod <- klickad_kod                        # Sätt vald kommunkod

      ny_kommun <- kommun_sf %>%
        dplyr::filter(kommunkod == vald_kommunkod) %>%
        dplyr::pull(kommunnamn) %>%
        unique()

      if (length(ny_kommun) == 1) {
        vald_kommun(ny_kommun)
      }

      vald_deso(NULL)                       # Nollställ vald DeSO
      kartniva("deso")                      # Gå ned till DeSO-nivå

      # rita om kartan
      geo_data <- data_beredd()


      rendera_karta(
        df_map        = geo_data$data,
        join_var      = geo_data$join_var,
        join_namn     = geo_data$join_namn,
        ar_uppdatering = TRUE
      )


    } else {

      if (!is.null(vald_deso()) && vald_deso() == klickad_kod) {          # --- Klick på DeSO-nivå ---
        vald_deso(NULL)                                               # Toggla av
      } else {
        vald_deso(klickad_kod)                                            # Sätt ny vald DeSO
      }
    }
  })

  # --- Linjediagram per månad ---
  output$diagram_manader <- renderGirafe({

    # Hämta data från data_beredd()
    geo_data <- data_beredd()
    df <- geo_data$data %>% st_drop_geometry()

    # Skapa brottkategori-text baserat på hierarkin
    hierarki <- rubrik_brott_niva()
    kat_max <- vald_max_niva_kategori()

    # Välj brottskategori till diagramrubriken
    if (!is.null(kat_max)) {
      brottkategori <- kat_max                            # Om max-nivå är vald: använd den
    } else if (length(hierarki) == 0) {
      brottkategori <- "Alla brott"                       # Ingen hierarki vald, dvs. alla brott visas
    } else {
      brottkategori <- hierarki[length(hierarki)]         # Annars senaste nivån i hierarkin
    }

    # Bearbeta för månadsvyn - behöver gå tillbaka till brottsdata() för att få månadsinfo
    niva <- brott_niva()
    indelning <- vald_indelning()
    indelning_namnkol <- paste0(indelning, "_namn")

    # Samma filtrering som i data_beredd()
    brottskoder_filtrering <- brott_niva_nyckel() %>%
      {
        if (length(hierarki) > 0) {
          filter(., .data[[paste0(indelning, "_namn")]] == hierarki[length(hierarki)])
        } else {
          .
        }
      } %>% dplyr::pull(brottskod)


    # Lägg till filtrering för vald kategori på max-nivå om sådan finns
    kat_max <- vald_max_niva_kategori()
    if (!is.null(kat_max)) {
      brottskoder_maxniva <- brott_niva_nyckel() %>%
        filter(.data[[paste0(indelning, "_namn")]] == kat_max) %>%
        dplyr::pull(brottskod)

      brottskoder_filtrering <- intersect(brottskoder_filtrering, brottskoder_maxniva)
    }

    df_filtrering <- brottsdata() %>%
      filter(brottskod %in% brottskoder_filtrering)

    df_manad_data <- filtrera_data(df_filtrering, vald_kommun(), indelning, niva, brott_niva_nyckel())
    df_manad_data <- filtrera_tidsperiod(df_manad_data, input$val_ar, input$anpassad_fran, input$anpassad_till)

    # Filtrera på vald DeSO om det finns
    if (kartniva() == "deso" && !is.null(vald_deso())) {
      df_manad_data <- df_manad_data %>% filter(desokod == vald_deso())
    }

    # Skapa månadsdata med månadsvariabel
    # Optimerat: parsa datum bara på unika årmånad-värden, inte per rad
    manad_lookup <- df_manad_data %>%
      distinct(`inskr årmånad`) %>%
      mutate(
        datum_lookup = as.Date(paste0(`inskr årmånad`, "01"), "%Y%m%d"),
        manad_namn   = format(datum_lookup, "%B"),
        manad_nr     = as.numeric(format(datum_lookup, "%m")),
        ar           = format(datum_lookup, "%Y"),
        armanad      = `inskr årmånad`
      ) %>%
      select(-datum_lookup)

    df_manad_data <- df_manad_data %>%
      left_join(manad_lookup, by = "inskr årmånad")

    # Använd summering_med_bef med extra grupperingsvariabel
    # Bestäm geografisk join-variabel beroende på kartnivå
    geo_join_var <- if (kartniva() == "deso") "desokod" else "kommunkod"

    bef_for_manad <- if (vald_kommun() == "Alla") {
      bef_df %>%
        group_by(år, kommunkod) %>%
        summarise(bef = sum(bef, na.rm = TRUE), .groups = "drop")
    } else {
      bef_df %>% filter(kommunkod == unique(df_manad_data$kommunkod))
    }

    df_manad <- summering_med_bef(
      df_manad_data,
      c(geo_join_var, indelning_namnkol),
      bef_for_manad,
      input$val_ar,
      extra_grp_var = c("manad_namn", "manad_nr", "ar", "armanad"),
      brott_niva_nyckel_df = brott_niva_nyckel()
    )

    df_manad <- df_manad %>%
      group_by(manad_namn, manad_nr, ar, armanad) %>%
      summarise(
        antal_brott = sum(antal_brott, na.rm = TRUE),
        bef = sum(bef, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      #mutate(brott_per_100k = ifelse(bef > 0, round((antal_brott / bef) * 12 * 100000), NA)) %>%          # årstakt
      mutate(brott_per_100k = ifelse(bef > 0, round((antal_brott / bef) * 100000), NA)) %>%
      arrange(armanad) %>%
      mutate(
        # Använd armanad (YYYYMM) som faktor så varje månad får en unik x-position
        # även när samma månadsnamn förekommer flera år i rad
        x_etikett = factor(armanad, levels = unique(armanad)),
        etikett   = paste0(
          str_to_sentence(manad_namn), " ", ar, "<br>",
          format(antal_brott, big.mark = " "), " brott"
        )
      )

    # Bestäm hur tätt etiketter ska visas beroende på antal månader
    n_manader <- nrow(df_manad)
    if (n_manader <= 12) {
      # Upp till 12 månader: visa varje månad, lägg till år på första och vid januari
      df_manad <- df_manad %>%
        mutate(x_label = ifelse(manad_nr == 1 | row_number() == 1,
                                paste0(str_to_sentence(manad_namn), "\n", ar),
                                str_to_sentence(manad_namn)))
    } else if (n_manader <= 24) {
      # 13–24 månader: visa varannan månad samt alltid januari
      df_manad <- df_manad %>%
        mutate(
          visa = manad_nr == 1 | row_number() == 1 | (row_number() %% 2 == 0),
          x_label = ifelse(manad_nr == 1 | row_number() == 1,
                           paste0(str_to_sentence(manad_namn), "\n", ar),
                           ifelse(visa, str_to_sentence(manad_namn), ""))
        )
    } else {
      # Fler än 24 månader: visa bara året vid januari, övriga tomma
      df_manad <- df_manad %>%
        mutate(x_label = ifelse(manad_nr == 1 | row_number() == 1,
                                ar,
                                ""))
    }

    # Bygg etikettvektor: armanad -> x_label
    x_labels_vec <- setNames(df_manad$x_label, df_manad$armanad)

    # Skydd mot tom data
    if (nrow(df_manad) == 0) {
      return(girafe(ggobj = ggplot() + theme_void()))
    }



    # Skapa diagramtitel
    # om det finns ett vald_deso_namn() så används det, annars kommunnamn eller hela länet
    geo_txt <- if (is.null(vald_deso_namn())) geografi_text() else vald_deso_namn()
    titel <- paste0(brottkategori, " per månad ", geo_txt, " ", tidsperiod_text())
    titel <- str_wrap(titel, width = 60)

    p <- ggplot(df_manad, aes(x = x_etikett, y = antal_brott, group = 1)) +
      geom_line(color = "#3182bd", linewidth = 1) +
      geom_point_interactive(
        aes(tooltip = etikett, data_id = x_etikett),
        color = "#3182bd",
        size = 2
      ) +
      scale_x_discrete(labels = x_labels_vec) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 5),
        labels = function(x) format(x, big.mark = " ", scientific = FALSE)
      ) +
      #labs(x = NULL, y = "Antal brott per 100.000 inv", title = titel) +
      labs(x = NULL, y = "Antal brott", title = titel,
           caption = KALLA_POLISEN) +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(
          size = 12,
          lineheight = 1.1,
          face = "bold",
          margin = margin(b = 6)
        ),
        plot.caption = element_text(
          size = 8, color = "#666", hjust = 0,
          margin = margin(t = 6)
        ),
        plot.margin = margin(t = 4, r = 6, b = 2, l = 6),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "none"
      )

    girafe(
      ggobj = p,
      width_svg = 8,
      height_svg = 5,
      options = list(
        opts_sizing(rescale = TRUE, width = 1),
        opts_hover_inv(css = "opacity:1;"),
        opts_hover(css = "stroke-width:0;fill-opacity:1;cursor:default;"),
        opts_selection(type = "none")
      )
    )
  })

  # --- Stapeldiagram per veckodag ---
  output$diagram_veckodag <- renderGirafe({

    # Hämta data från data_beredd()
    geo_data <- data_beredd()
    df <- geo_data$data %>% st_drop_geometry()

    # Skapa brottkategori-text baserat på hierarkin
    hierarki <- rubrik_brott_niva()
    kat_max <- vald_max_niva_kategori()

    # Välj brottskategori till diagramrubriken
    if (!is.null(kat_max)) {
      brottkategori <- kat_max                            # Om max-nivå är vald: använd den
    } else if (length(hierarki) == 0) {
      brottkategori <- "Alla brott"                       # Ingen hierarki vald, dvs. alla brott visas
    } else {
      brottkategori <- hierarki[length(hierarki)]         # Annars senaste nivån i hierarkin
    }

    # Bearbeta för veckodagsvyn - behöver gå tillbaka till brottsdata() för att få datuminfo
    niva <- brott_niva()
    indelning <- vald_indelning()
    indelning_namnkol <- paste0(indelning, "_namn")

    # Samma filtrering som i data_beredd()
    brottskoder_filtrering <- brott_niva_nyckel() %>%
      {
        if (length(hierarki) > 0) {
          filter(., .data[[paste0(indelning, "_namn")]] == hierarki[length(hierarki)])
        } else {
          .
        }
      } %>% dplyr::pull(brottskod)


    # Lägg till filtrering för vald kategori på max-nivå om sådan finns
    kat_max <- vald_max_niva_kategori()
    if (!is.null(kat_max)) {
      brottskoder_maxniva <- brott_niva_nyckel() %>%
        filter(.data[[paste0(indelning, "_namn")]] == kat_max) %>%
        dplyr::pull(brottskod)

      brottskoder_filtrering <- intersect(brottskoder_filtrering, brottskoder_maxniva)
    }

    df_filtrering <- brottsdata() %>%
      filter(brottskod %in% brottskoder_filtrering)

    df_veckodag_data <- filtrera_data(df_filtrering, vald_kommun(), indelning, niva, brott_niva_nyckel())
    df_veckodag_data <- filtrera_tidsperiod(df_veckodag_data, input$val_ar, input$anpassad_fran, input$anpassad_till)

    # Filtrera på vald DeSO om det finns
    if (kartniva() == "deso" && !is.null(vald_deso())) {
      df_veckodag_data <- df_veckodag_data %>% filter(desokod == vald_deso())
    }

    # Skapa veckodagsdata med veckodagsvariabel
    # Optimerat: parsa datum bara på unika datum, inte per rad
    veckodag_lookup <- df_veckodag_data %>%
      distinct(inskrivningsdatum) %>%
      mutate(
        datum_lookup = as.Date(inskrivningsdatum),
        veckodag     = format(datum_lookup, "%A"),
        veckodag_nr  = as.numeric(format(datum_lookup, "%u"))
      ) %>%
      select(-datum_lookup)

    df_veckodag_data <- df_veckodag_data %>%
      left_join(veckodag_lookup, by = "inskrivningsdatum")

    # Använd summering_med_bef med extra grupperingsvariabel
    geo_join_var <- if (kartniva() == "deso") "desokod" else "kommunkod"

    bef_for_veckodag <- if (vald_kommun() == "Alla") {
      bef_df %>%
        group_by(år, kommunkod) %>%
        summarise(bef = sum(bef, na.rm = TRUE), .groups = "drop")
    } else {
      bef_df %>% filter(kommunkod == unique(df_veckodag_data$kommunkod))
    }

    df_veckodag <- summering_med_bef(
      df_veckodag_data,
      c(geo_join_var, indelning_namnkol),
      bef_for_veckodag,
      input$val_ar,
      extra_grp_var = c("veckodag", "veckodag_nr"),
      brott_niva_nyckel_df = brott_niva_nyckel()
    ) %>%
      group_by(veckodag, veckodag_nr) %>%
      summarise(
        antal_brott = sum(antal_brott, na.rm = TRUE),
        bef = sum(bef, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      #mutate(brott_per_100k = ifelse(bef > 0, round((antal_brott / bef) * (365/7) * 100000), NA)) %>%         # årstakt
      mutate(brott_per_100k = ifelse(bef > 0, round((antal_brott / bef) * 100000), NA)) %>%
      arrange(veckodag_nr) %>%
      mutate(
        veckodag = factor(veckodag, levels = unique(veckodag)),
        etikett = paste0(
          str_to_sentence(veckodag), "<br>",
          #format(round(brott_per_100k), big.mark = " "), " per 100 000 inv", "<br>",
          #paste0(format(antal_brott, big.mark = " "), " brott totalt")
          paste0(format(antal_brott, big.mark = " "), " brott")
        )
      )

    # Skydd mot tom data
    if (nrow(df_veckodag) == 0) {
      return(girafe(ggobj = ggplot() + theme_void()))
    }

    # Skapa diagramtitel
    # om det finns ett vald_deso_namn() så används det, annars kommunnamn eller hela länet
    geo_txt <- if (is.null(vald_deso_namn())) geografi_text() else vald_deso_namn()
    titel <- paste0(brottkategori, " per veckodag", " ", geo_txt, " ", tidsperiod_text())
    titel <- str_wrap(titel, width = 60)

    p <- ggplot(df_veckodag, aes(x = veckodag, y = antal_brott)) +
      geom_col_interactive(
        aes(tooltip = etikett, data_id = veckodag),
        fill = "#3182bd"
      ) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 5),
        labels = function(x) format(x, big.mark = " ", scientific = FALSE)
      ) +
      #labs(x = NULL, y = "Antal brott per 100.000 inv", title = titel) +
      labs(x = NULL, y = "Antal brott", title = titel,
           caption = KALLA_POLISEN) +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(
          size = 12,
          lineheight = 1.1,
          face = "bold",
          margin = margin(b = 6)
        ),
        plot.caption = element_text(
          size = 8, color = "#666", hjust = 0,
          margin = margin(t = 6)
        ),
        plot.margin = margin(t = 4, r = 6, b = 2, l = 6),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "none"
      )

    girafe(
      ggobj = p,
      width_svg = 8,
      height_svg = 5,
      options = list(
        opts_sizing(rescale = TRUE, width = 1),
        opts_hover_inv(css = "opacity:1;"),
        opts_hover(css = "stroke-width:0;fill-opacity:1;cursor:default;"),
        opts_selection(type = "none")
      )
    )
  })

  # spara excelfil
  output$export_excel <- downloadHandler(
    filename = function() {
      paste0("brottsdata_", Sys.Date(), ".xlsx")
    },
    content = function(file) {

      # Skriv till Excel
      write_xlsx(brottsdata(), file)
    }
  )

  # spara excelfil med aktuellt urval, aggregerat på brott (aktuell nivå),
  # geografi (aktuell nivå), månad och veckodag
  output$export_excel_urval <- downloadHandler(
    filename = function() {
      # Hjälpfunktion: gör om text till filnamnssäker form (åäö -> aao,
      # mellanslag/specialtecken -> _, max 30 tecken så filnamnet inte blir för långt)
      slugify <- function(x, maxlen = 30) {
        if (is.null(x) || length(x) == 0 || is.na(x) || x == "") return(NA_character_)
        s <- tolower(as.character(x))
        s <- chartr("åäöéèêü", "aaoeeeu", s)
        s <- str_replace_all(s, "[^a-z0-9]+", "_")
        s <- str_replace_all(s, "^_+|_+$", "")
        if (nchar(s) > maxlen) s <- str_sub(s, 1, maxlen)
        if (nchar(s) == 0) NA_character_ else s
      }

      # Geografi: kommun (+ ev. DeSO-namn)
      geo_del <- if (vald_kommun() == "Alla") {
        "dalarna"
      } else if (kartniva() == "deso" && !is.null(vald_deso()) && !is.null(vald_deso_namn())) {
        paste0(slugify(vald_kommun(), 15), "_", slugify(vald_deso_namn(), 15))
      } else {
        slugify(vald_kommun())
      }

      # Brottstyp: senaste nivån i hierarkin + ev. vald max-nivå-kategori
      brott_del <- if (!is.null(vald_max_niva_kategori())) {
        slugify(vald_max_niva_kategori())
      } else if (length(rubrik_brott_niva()) > 0) {
        slugify(rubrik_brott_niva()[length(rubrik_brott_niva())])
      } else {
        "alla_brott"
      }

      # Tidsperiod
      tid_del <- if (input$val_ar == "Senaste 12 månaderna") {
        "sen_12_man"
      } else if (input$val_ar == "Hela perioden") {
        "hela_perioden"
      } else if (input$val_ar == "Anpassad period…") {
        if (!is.null(input$anpassad_fran) && !is.null(input$anpassad_till)) {
          paste0(input$anpassad_fran, "-", input$anpassad_till)
        } else {
          "anpassad"
        }
      } else {
        slugify(input$val_ar)
      }

      delar <- c("brott", geo_del, brott_del, tid_del, as.character(Sys.Date()))
      delar <- delar[!is.na(delar) & nchar(delar) > 0]
      paste0(paste(delar, collapse = "_"), ".xlsx")
    },
    content = function(file) {

      niva <- brott_niva()
      hierarki <- rubrik_brott_niva()
      indelning <- vald_indelning()
      indelning_namnkol <- paste0(indelning, "_namn")

      # Samma filtrering som i diagrammen
      brottskoder_filtrering <- brott_niva_nyckel() %>%
        {
          if (length(hierarki) > 0) {
            filter(., .data[[indelning_namnkol]] == hierarki[length(hierarki)])
          } else {
            .
          }
        } %>% dplyr::pull(brottskod)

      if (!is.null(vald_max_niva_kategori())) {
        brottskoder_maxniva <- brott_niva_nyckel() %>%
          filter(.data[[indelning_namnkol]] == vald_max_niva_kategori()) %>%
          dplyr::pull(brottskod)

        brottskoder_filtrering <- intersect(brottskoder_filtrering, brottskoder_maxniva)
      }

      df_filt <- brottsdata() %>%
        filter(brottskod %in% brottskoder_filtrering)

      df_filt <- filtrera_data(df_filt, vald_kommun(), indelning, niva, brott_niva_nyckel())
      df_filt <- filtrera_tidsperiod(df_filt, input$val_ar, input$anpassad_fran, input$anpassad_till)

      # Filtrera på vald DeSO om kartan är på deso-nivå
      if (kartniva() == "deso" && !is.null(vald_deso())) {
        df_filt <- df_filt %>% filter(desokod == vald_deso())
      }

      if (nrow(df_filt) == 0) {
        write_xlsx(tibble(meddelande = "Inga brott i aktuellt urval"), file)
        return(invisible())
      }

      # Lägg till månad och veckodag från datum
      df_filt <- df_filt %>%
        mutate(
          datum = as.Date(inskrivningsdatum),
          ar = as.integer(substr(`inskr årmånad`, 1, 4)),
          manad_nr = as.integer(substr(`inskr årmånad`, 5, 6)),
          manad = format(datum, "%B"),
          veckodag_nr = as.integer(format(datum, "%u")),
          veckodag = format(datum, "%A")
        )

      # Geografikolumner enligt aktuell nivå (kommun eller DeSO)
      if (kartniva() == "deso") {
        geo_kols <- c("kommunnamn", "desokod", "regsonamn")
      } else {
        geo_kols <- c("kommunnamn")
      }

      grp_kols <- c(indelning_namnkol, geo_kols,
                    "ar", "manad_nr", "manad", "veckodag_nr", "veckodag")

      df_agg <- df_filt %>%
        group_by(across(all_of(grp_kols))) %>%
        summarise(antal_brott = sum(antal_brott, na.rm = TRUE), .groups = "drop") %>%
        arrange(across(all_of(c(geo_kols, indelning_namnkol, "ar", "manad_nr", "veckodag_nr"))))

      write_xlsx(df_agg, file)
    }
  )
  # ==================================== kod för andra fliken ===================================

  # spara excelfil med brådataset
  output$export_excel_bra <- downloadHandler(
    filename = function() {
      paste0("bra_kommunindikatorer_", Sys.Date(), ".xlsx")
    },
    content = function(file) {

      # Skriv till Excel
      write_xlsx(bra_kommunindikatorer(), file)
    }
  )

  # Uppdatera kommun-listrutan med alla unika kommuner
  observeEvent(bra_kommunindikatorer(), {
    updateSelectInput(session, "kommun",
                      choices = sort(unique(bra_kommunindikatorer()$geografi)),
                      selected = "Dalarnas län")
  }, ignoreInit = TRUE)

  # Filtrera data baserat på vald kommun
  data_filtered <- reactive({
    bra_kommunindikatorer() %>%
      filter(geografi %in% c(input$kommun, "Dalarnas län", "Hela landet")) %>%
      mutate(enhet = ifelse(str_sub(enhet, nchar(enhet), nchar(enhet)) == "\\.",
                            str_sub(enhet, 1, nchar(enhet)-1), enhet))
  })


  observeEvent(bra_kommunindikatorer(), {
    df <- bra_kommunindikatorer()

    ntu_vars <- df %>%
      dplyr::filter(kalla == "Nationella trygghetsundersökningen (NTU)") %>%
      dplyr::pull(variabel) %>%
      unique() %>%
      sort()

    anm_vars <- df %>%
      dplyr::filter(kalla == "Anmälda brott") %>%
      dplyr::pull(variabel) %>%
      unique() %>%
      sort()

    updateSelectInput(session, "variabel_ntu", choices = ntu_vars)
    updateSelectInput(session, "variabel_anm", choices = anm_vars)
  }, ignoreInit = TRUE)

  # # Uppdatera variabel-listrutor för respektive diagram
  # #observe({
  # ntu_vars <- bra_kommunindikatorer %>%
  #   filter(kalla == "Nationella trygghetsundersökningen (NTU)") %>%
  #   dplyr::pull(variabel) %>%
  #   unique()
  #
  # anm_vars <- bra_kommunindikatorer %>%
  #   filter(kalla == "Anmälda brott") %>%
  #   dplyr::pull(variabel) %>%
  #   unique()
  #
  # updateSelectInput(session, "variabel_ntu", choices = ntu_vars)
  # updateSelectInput(session, "variabel_anm", choices = anm_vars)
  # #})

  # Diagram för NTU

  output$diagram_ntu <- renderGirafe({
    req(input$variabel_ntu)

    plot_data <- data_filtered() %>%
      filter(kalla == "Nationella trygghetsundersökningen (NTU)",
             !str_detect(enhet, "KI "),                 # vi tar bort konfidensintervallen ur datasetet, men borde lägga till dem vid tillfälle
             variabel == input$variabel_ntu) %>%
      mutate(etikett = paste0(enhet, " år ", ar, " i ", ifelse(geografi == "Hela landet", tolower(geografi), geografi), ": \n", format(round(varde,1), big.mark = " ")))

    # Skydd mot tom data
    if (nrow(plot_data) == 0) {
      return(girafe(ggobj = ggplot() + theme_void()))
    }

    # Hämta alla geografier som finns i plot_data
    alla_geografier <- unique(plot_data$geografi)

    # Skapa färgvektor med namn som matchar exakt
    farger <- setNames(
      c(rus_tre_fokus[3], rus_tre_fokus[1:(length(alla_geografier)-1)]),
      c(input$kommun, setdiff(alla_geografier, input$kommun))
    )

    p <- ggplot(plot_data, aes(x = ar, y = varde, group = geografi, color = geografi)) +
      geom_line(linewidth = 1) +
      geom_point_interactive(
        aes(tooltip = etikett, data_id = ar),
        size = 2
      ) +
      scale_color_manual(values = farger,
                         breaks = c(input$kommun, "Dalarnas län", "Hela landet")) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 5),
                         labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
      labs(x = "År",
           y = unique(plot_data$enhet),
           title = input$variabel_ntu,
           color = NULL,
           caption = KALLA_BRA) +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_textbox_simple(
          size = 12, lineheight = 1.1, face = "bold",
          margin = margin(b = 8),
          width = unit(1, "npc")),
        plot.caption = element_text(
          size = 8, color = "#666", hjust = 0,
          margin = margin(t = 6)
        ),
        plot.margin = margin(t = 12, r = 10, b = 8, l = 10),
        axis.title.y = element_textbox_simple(
          #size = diagram_titel_storlek,
          orientation = "left-rotated",
          width = unit(0.9, "npc"),  # Bredd som proportion av plottens område
          halign = 0.5,  # Centrera texten
          margin = margin(7, 0, 7, 0)),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 10)
      )

    girafe(
      ggobj = p,
      width_svg = 6,
      height_svg = 4,
      options = list(
        opts_sizing(rescale = TRUE, width = 1),
        opts_hover_inv(css = "opacity:1;"),
        opts_hover(css = "stroke-width:0;fill-opacity:1;cursor:default;"),
        opts_selection(type = "none")
      )
    )
  })

  # Diagram för Anmälda brott per 100 000 inv
  output$diagram_anm_brott <- renderGirafe({
    req(input$variabel_anm)

    plot_data <- data_filtered() %>%
      filter(kalla == "Anmälda brott",
             variabel == input$variabel_anm,
             enhet == "Antal per 100 000 inv") %>%
      mutate(etikett = paste0(enhet, " år ", ar, " i ", ifelse(geografi == "Hela landet", tolower(geografi), geografi), ": \n", format(round(varde,1), big.mark = " ")))
    if (nrow(plot_data) == 0) {
      return(girafe(ggobj = ggplot() + theme_void()))
    }

    # Hämta alla geografier som finns i plot_data
    alla_geografier <- unique(plot_data$geografi)

    # Skapa färgvektor med namn som matchar exakt
    farger <- setNames(
      c(rus_tre_fokus[3], rus_tre_fokus[1:(length(alla_geografier)-1)]),
      c(input$kommun, setdiff(alla_geografier, input$kommun))
    )

    p <- ggplot(plot_data, aes(x = ar, y = varde, group = geografi, color = geografi)) +
      geom_line(linewidth = 1) +
      geom_point_interactive(
        aes(tooltip = etikett, data_id = paste(geografi, ar)),
        size = 2
      ) +
      scale_color_manual(values = farger,
                         breaks = c(input$kommun, "Dalarnas län", "Hela landet")) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 5),
                         labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
      labs(x = "År",
           y = unique(plot_data$enhet),
           color = NULL,
           title = paste0(input$variabel_anm, " i ", input$kommun, " (Antal per 100 000 inv)"),
           caption = KALLA_BRA) +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_textbox_simple(
          size = 12, lineheight = 1.1, face = "bold",
          margin = margin(b = 8),
          width = unit(1, "npc")),
        plot.caption = element_text(
          size = 8, color = "#666", hjust = 0,
          margin = margin(t = 6)
        ),
        plot.margin = margin(t = 12, r = 10, b = 8, l = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 10)
      )

    girafe(
      ggobj = p,
      width_svg = 6,
      height_svg = 4,
      options = list(
        opts_sizing(rescale = TRUE, width = 1),
        opts_hover_inv(css = "opacity:1;"),
        opts_hover(css = "stroke-width:0;fill-opacity:1;cursor:default;"),
        opts_selection(type = "none")
      )
    )
  })


  # Diagram för Anmälda brott (Antal)
  output$diagram_anm_brott_antal <- renderGirafe({
    req(input$variabel_anm)

    plot_data <- data_filtered() %>%
      filter(kalla == "Anmälda brott",
             variabel == input$variabel_anm,
             enhet == "Antal",
             !geografi %in% c("Dalarnas län", "Hela landet")) %>%
      group_by(ar, geografi) %>%
      reframe(varde = sum(varde, na.rm = TRUE), enhet = first(enhet)) %>%
      mutate(etikett = paste0(enhet, " år ", ar, " i ", geografi, ": \n", format(round(varde,1), big.mark = " ")))
    if (nrow(plot_data) == 0) return(girafe(ggobj = ggplot() + theme_void()))

    p <- ggplot(plot_data, aes(x = ar, y = varde, group = 1)) +
      geom_line(color = rus_tre_fokus[3], linewidth = 1) +
      geom_point_interactive(aes(tooltip = etikett, data_id = ar),
                             color = rus_tre_fokus[3], size = 2) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 5),
                         labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
      labs(x = "År", y = "Antal",
           title = paste0(input$variabel_anm, " i ", input$kommun, " (Antal)"),
           caption = KALLA_BRA) +
      theme_minimal(base_size = 10) +
      theme(plot.title = element_textbox_simple(
        size = 12, face = "bold",
        margin = margin(b = 8),
        width = unit(1, "npc")),
        plot.caption = element_text(
          size = 8, color = "#666", hjust = 0,
          margin = margin(t = 6)
        ),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 10))

    girafe(ggobj = p, width_svg = 6, height_svg = 4,
           options = list(opts_sizing(rescale = TRUE, width = 1),
                          opts_hover_inv(css = "opacity:1;"),
                          opts_hover(css = "stroke-width:0;fill-opacity:1;cursor:default;"),
                          opts_selection(type = "none")))
  })
})  # slut shinyServer()

