# --- Ladda paket ---

library(tidyverse)
library(sf)
library(RPostgres)

hamta_karta <- function(karttyp = "kommuner", regionkoder = NA, tabellnamn = NA) {

    tabell_df <- hamta_karttabell()
    df_rad <- suppressWarnings(str_which(tabell_df$sokord, karttyp))
    if (length(df_rad) > 0) 
        df_rad <- df_rad[map_lgl(df_rad, ~karttyp %in% tabell_df$sokord[[.x]])]
    if (length(df_rad) == 0) 
        pg_tabell <- "finns ej"
    else pg_tabell <- tabell_df$namn[df_rad]
    if (pg_tabell != "finns ej") {
        if (all(!is.na(regionkoder)) & all(regionkoder != "00")) {
            kommunkoder <- regionkoder[nchar(regionkoder) == 
                4]
            if (karttyp == "nuts2") 
                lanskoder <- regionkoder[nchar(regionkoder) == 
                  2]
            lanskoder <- regionkoder[nchar(regionkoder) == 2 & 
                regionkoder != "00"]
            if (karttyp == "nuts2") 
                kommunkoder <- regionkoder[nchar(regionkoder) == 
                  4]
        }
        else {
            kommunkoder <- NULL
            lanskoder <- NULL
        }
        grundquery <- paste0("SELECT * FROM karta.", pg_tabell)
        if ((length(kommunkoder) == 0) & (length(lanskoder) == 
            0)) 
            skickad_query <- NA
        else {
            skickad_query <- " WHERE "
            if (length(lanskoder) != 0 & !is.na(tabell_df$lankol[df_rad])) {
                skickad_query <- paste0(skickad_query, tabell_df$lankol[df_rad], 
                  " IN (", paste0("'", lanskoder, "'", collapse = ", "), 
                  ")")
            }
            if ((length(lanskoder) != 0 & !is.na(tabell_df$lankol[df_rad])) & 
                (length(kommunkoder) != 0 & !is.na(tabell_df$kommunkol[df_rad]))) 
                mellanquery <- " OR "
            else mellanquery <- ""
            if (length(kommunkoder) != 0 & !is.na(tabell_df$kommunkol[df_rad])) {
                skickad_query <- paste0(skickad_query, mellanquery, 
                  tabell_df$kommunkol[df_rad], " IN (", paste0("'", 
                    kommunkoder, "'", collapse = ", "), ");")
            }
            else {
                skickad_query <- paste0(skickad_query, ";")
            }
        }
        retur_sf <- suppress_specific_warning(postgis_postgistabell_till_sf(schema = "karta", 
            tabell = pg_tabell, query = skickad_query), "Invalid time zone 'UTC', falling back to local time.")
        return(retur_sf)
    }
    else {
        warning(paste0("Karttypen ", karttyp, " finns inte i databasen."))
    }
}

hamta_karttabell <- function() {

    kolumn_namn <- c("namn", "id_kol", "lankol", "kommunkol", 
        "sokord")
    antal_kol <- length(kolumn_namn)
    karttabell_df <- as.data.frame(matrix(nrow = 0, ncol = antal_kol)) %>% 
        setNames(kolumn_namn) %>% mutate(across(1:(antal_kol - 
        1), as.character), sokord = sokord %>% as.list())
    karttabell_df <- karttabell_df %>% add_row(namn = "kommun_scb", 
        id_kol = "knkod", lankol = "lanskod_tx", kommunkol = "knkod", 
        sokord = list(c("kommun", "kommuner", "kommunpolygoner"))) %>% 
        add_row(namn = "kommun_lm", id_kol = "kommunkod", lankol = "lankod", 
            kommunkol = "kommunkod", sokord = list(c("kommun_lm", 
                "kommuner_lm", "kommunpolygoner_lm"))) %>% add_row(namn = "lan_scb", 
        id_kol = "lnkod", lankol = "lnkod", kommunkol = NA, sokord = list(c("lan", 
            "lanspolygoner"))) %>% add_row(namn = "lan_lm", id_kol = "lankod", 
        lankol = "lankod", kommunkol = NA, sokord = list(c("lan_lm", 
            "lanspolygoner_lm"))) %>% add_row(namn = "tatorter", 
        id_kol = "tatortskod", lankol = "lan", kommunkol = "kommun", 
        sokord = list(c("tatort", "tätort", "tatorter", "tätorter", 
            "tatortspolygoner", "tätortspolygoner"))) %>% add_row(namn = "tatortspunkter", 
        id_kol = "tatortskod", lankol = "lan", kommunkol = "kommun", 
        sokord = list(c("tatortspunkter", "tätortspunkter"))) %>% 
        add_row(namn = "regso", id_kol = "regsokod", lankol = "lanskod", 
            kommunkol = "kommunkod", sokord = list(c("regso", 
                "regsopolygoner"))) %>% add_row(namn = "deso", 
        id_kol = "deso", lankol = "lanskod", kommunkol = "kommunkod", 
        sokord = list(c("deso", "desopolygoner"))) %>% add_row(namn = "distrikt", 
        id_kol = "distriktskod", lankol = "lankod", kommunkol = "kommunkod", 
        sokord = list(c("distrikt"))) %>% add_row(namn = "nuts2", 
        id_kol = "nuts_id", lankol = "cntr_code", kommunkol = "geo", 
        sokord = list(c("nuts2", "nuts2-områden"))) %>% add_row(namn = "nuts3", 
        id_kol = "nuts_id", lankol = "cntr_code", kommunkol = "geo", 
        sokord = list(c("nuts3", "nuts3-områden"))) %>% add_row(namn = "laregion_scb", 
        id_kol = "lakod", lankol = "lan", kommunkol = "kommun", 
        sokord = list(c("la", "laomraden", "la-omraden", "la-områden", 
            "la-omraden"))) %>% add_row(namn = "varlden", id_kol = "Landskod", 
        lankol = NA, kommunkol = NA, sokord = list(c("varlden", 
            "varldskarta", "världen", "världskarta"))) %>% 
        add_row(namn = "varldsdelar", id_kol = "Landskod", lankol = NA, 
            kommunkol = NA, sokord = list(c("varldsdelskarta", 
                "varldsdelar", "världsdelskarta", "världsdelar")))
    return(karttabell_df)
}

kartifiera <- function(skickad_df, geom_nyckel, tatortspunkter = TRUE) {

    kartifiera_regionkoder <- unique(skickad_df[[geom_nyckel]])
    geom_nyckel_langd <- nchar(kartifiera_regionkoder) %>% unique()
    if (length(geom_nyckel_langd) > 1) {
        print("Skickad df:s geom_nyckel kan bara innehålla värden av samma typ. Kontrollera att så är fallet och försök igen.")
    }
    else {
        if (geom_nyckel_langd == 2) 
            kartifiera_karttyp <- "lan"
        if (geom_nyckel_langd == 4 & all(str_detect(kartifiera_regionkoder, 
            "^[:digit:]+$"))) 
            kartifiera_karttyp <- "kommun"
        if (geom_nyckel_langd == 4 & !all(str_detect(kartifiera_regionkoder, 
            "^[:digit:]+$"))) 
            kartifiera_karttyp <- "nuts2"
        if (geom_nyckel_langd == 9 & all(str_sub(kartifiera_regionkoder, 
            5, 5) %in% c("A", "B", "C"))) 
            kartifiera_karttyp <- "deso"
        if (geom_nyckel_langd == 8 & all(str_sub(kartifiera_regionkoder, 
            5, 5) %in% c("R"))) 
            kartifiera_karttyp <- "regso"
        if (geom_nyckel_langd == 9 & all(str_sub(kartifiera_regionkoder, 
            5, 5) %in% c("T"))) 
            kartifiera_karttyp <- "tatorter"
        if (kartifiera_karttyp == "tatorter" & tatortspunkter) 
            kartifiera_karttyp <- "tatortspunkter"
        gis_lager <- hamta_karta(karttyp = kartifiera_karttyp, 
            regionkoder = kartifiera_regionkoder)
        tabell_df <- hamta_karttabell()
        vald_karta <- tabell_df %>% mutate(ord_match = map_lgl(sokord, 
            ~kartifiera_karttyp %in% .)) %>% filter(ord_match)
        if (nrow(vald_karta) == 0) 
            pg_tab_idkol <- "finns ej"
        else pg_tab_idkol <- vald_karta$id_kol
        join_sf <- skickad_df %>% left_join(gis_lager, by = setNames(pg_tab_idkol, 
            geom_nyckel)) %>% st_as_sf()
        return(join_sf)
    }
}

postgis_postgistabell_till_sf <- function(con = "default", schema, tabell, query = NA, meddelande_tid = FALSE) {

    starttid <- Sys.time()
    if (is.character(con) && con == "default") {
        con <- uppkoppling_db()
        default_flagga = TRUE
    }
    else default_flagga = FALSE
    if (is.na(query)) {
        query <- paste0("SELECT * FROM ", schema, ".", tabell)
    }
    else {
        query <- paste0("SELECT * FROM ", schema, ".", tabell, 
            " ", query)
    }
    retur_sf <- st_read(con, query = query)
    if (default_flagga) 
        dbDisconnect(con)
    berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% 
        round(1)
    if (meddelande_tid) 
        cat(glue("Processen tog {berakningstid} sekunder att köra"))
    return(retur_sf)
}

postgis_sf_till_postgistabell <- function(con = "default", inlas_sf, schema = "karta", tabell, 
    postgistabell_id_kol = NA, postgistabell_geo_kol = NA, skapa_spatialt_index = TRUE, 
    nytt_schema_oppet_for_geodata_las = TRUE, meddelande_tid = FALSE) {

    if (all(is.na(postgistabell_geo_kol)) & skapa_spatialt_index) 
        stop("postgistabell_geo_kol måste skickas med om skapa_spatialt_index är satt till TRUE. Om du vill skapa en tabell utan geografi måste skapa_spatialt_index sättas till FALSE.")
    starttid <- Sys.time()
    if (is.character(con) && con == "default") {
        con <- uppkoppling_db()
        default_flagga = TRUE
    }
    else default_flagga = FALSE
    names(inlas_sf) <- tolower(names(inlas_sf))
    tabell <- tabell %>% tolower()
    schema_finns <- postgres_schema_finns(con, schema)
    if (!schema_finns) {
        dbExecute(con, paste0("create schema if not exists ", 
            schema, ";"))
        if (nytt_schema_oppet_for_geodata_las) {
            sql_command <- sprintf("                                          # skapa sql-kommando för att öppna schemat och framtida tabeller för geodata_las\n        ALTER DEFAULT PRIVILEGES IN SCHEMA %s\n        GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO %s;\n        ", 
                schema, "geodata_las")
            dbExecute(con, sql_command)
        }
    }
    tabell_finns <- DBI::dbExistsTable(con, DBI::Id(schema = schema, 
        table = tabell))
    if (tabell_finns) {
        dbExecute(con, paste0("TRUNCATE TABLE ", schema, ".", 
            tabell, ";"))
        system.time({
            st_write(obj = inlas_sf, dsn = con, layer = DBI::Id(schema = schema, 
                table = tabell), append = FALSE)
        })
    }
    else {
        system.time({
            st_write(obj = inlas_sf, dsn = con, layer = DBI::Id(schema = schema, 
                table = tabell), append = FALSE)
        })
    }
    if (skapa_spatialt_index) {
        for (geokol in 1:length(postgistabell_geo_kol)) {
            dbExecute(con, paste0("DROP INDEX IF EXISTS ", schema, 
                ".", postgistabell_geo_kol[geokol], "_idx;"))
            dbExecute(con, paste0("CREATE INDEX ", postgistabell_geo_kol[geokol], 
                "_idx ON ", schema, ".", tabell, " USING GIST (", 
                postgistabell_geo_kol[geokol], ");"))
        }
    }
    if (!is.na(postgistabell_id_kol)) {
        dbExecute(con, paste0("ALTER TABLE ", schema, ".", tabell, 
            " ADD PRIMARY KEY (", postgistabell_id_kol, ");"))
    }
    if (default_flagga) 
        dbDisconnect(con)
    berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% 
        round(1)
    if (meddelande_tid) 
        cat(glue("Processen tog {berakningstid} sekunder att köra"))
}

postgres_hamta_oppnadata <- function(schema = NA, tabell = NA, query = NA, meddelande_info = FALSE, 
    meddelande_tid = FALSE) {

    if (is.na(schema) | is.na(tabell)) {
        postgres_lista_scheman_tabeller(con = uppkoppling_db(db_name = "oppna_data"))
    }
    else {
        retur_df <- postgres_tabell_till_df(con = uppkoppling_db(db_name = "oppna_data"), 
            schema = schema, tabell = tabell, query = query, 
            meddelande_info = meddelande_info, meddelande_tid = meddelande_tid)
        return(retur_df)
    }
}

postgres_tabell_till_df <- function(con = "default", schema, tabell, query = NA, meddelande_info = FALSE, 
    meddelande_tid = FALSE) {

    starttid <- Sys.time()
    if (is.character(con) && con == "default") {
        con <- uppkoppling_db()
        default_flagga <- TRUE
    }
    else {
        default_flagga <- FALSE
    }
    if (is.na(query)) {
        sql_query <- paste0("SELECT * FROM ", schema, ".", tabell)
    }
    else {
        sql_query <- paste0("SELECT * FROM ", schema, ".", tabell, 
            " ", query)
    }
    tryCatch({
        retur_df <- dbGetQuery(con, sql_query)
        if (meddelande_info) 
            message(paste("Tabellen", tabell, "från schemat", 
                schema, "har lästs in."))
    }, error = function(e) {
        message(paste("Kunde inte läsa tabellen", tabell, "från schemat", 
            schema, ":", e$message))
        retur_df <- NULL
    })
    if (default_flagga) 
        dbDisconnect(con)
    berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% 
        round(1)
    if (meddelande_tid) 
        cat(glue("Processen tog {berakningstid} sekunder att köra"))
    return(retur_df)
}

uppkoppling_adm <- function(databas = "geodata") {

    uppkoppling_db(service_name = "databas_adm", db_name = databas)
}

uppkoppling_db <- function(db_name = "geodata", service_name = NA, db_host = "WFALMITVS526.ltdalarna.se", 
    db_port = 5432, db_options = "-c search_path=public", db_user = NA, 
    db_password = NA) {

    if (!is.na(service_name)) {
        if (is.na(db_user)) 
            db_user <- key_list(service = service_name)$username
        if (is.na(db_password)) 
            db_password <- key_get(service_name, key_list(service = service_name)$username)
    }
    else {
        if (is.na(db_user)) 
            db_user <- "geodata_las"
        if (is.na(db_password)) 
            db_password <- "geodata_las"
    }
    current_hostname <- Sys.info()[["nodename"]]
    if (str_detect(toupper(db_host), toupper(current_hostname))) {
        db_host <- "localhost"
    }
    else {
        db_host <- if (is.na(db_host)) 
            "WFALMITVS526.ltdalarna.se"
        else db_host
    }
    tryCatch({
        con <- suppress_specific_warning(dbConnect(RPostgres::Postgres(), 
            bigint = "integer", user = db_user, password = db_password, 
            host = db_host, port = db_port, dbname = db_name, 
            options = db_options), "Invalid time zone 'UTC', falling back to local time.")
        return(con)
    }, error = function(e) {
        print(paste("Ett fel inträffade vid anslutning till databasen:", 
            e$message))
        return(NULL)
    })
}
