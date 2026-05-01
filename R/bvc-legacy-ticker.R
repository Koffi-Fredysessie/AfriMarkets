

#' Internal cache for build ID
#'
#' This internal function creates and returns a cache environment that stores
#' the build ID and its timestamp. It is used internally by `get_build_id`
#' to reduce API calls.
#'
#' @return Environment with cached build data.
#' @keywords internal
#' @rdname get_build_cache
get_build_cache <- local({
    cache <- new.env(parent = emptyenv())
    cache$build_id <- NULL
    cache$timestamp <- 0
    cache$cache_duration <- 3600

    function() cache
})


#' Retrieve build ID from Casablanca Stock Exchange
#'
#' @description
#' This function retrieves a unique build ID from the homepage of the Casablanca Stock Exchange (CSE). The build ID is extracted from a JSON script embedded in the page. The result is cached internally for efficiency, so it is reused for subsequent calls within a one-hour window unless a refresh is forced.
#'
#' @param force_refresh Logical. If TRUE, the function bypasses the cache and fetches a fresh build ID from the website.
#'
#' @return A character string representing the build ID. Returns NULL if retrieval fails.
#'
#' @examples
#' \dontrun{
#' build_id <- get_build_id()
#' }
#'
#' @note
#' This function is intended for internal use only. It is not exported and should not be called directly by users.
#'
#' @family internal
#' @importFrom httr GET content add_headers status_code timeout
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_match
#' @rdname get_build_id
get_build_id <- function(force_refresh = FALSE) {
    cache <- get_build_cache()

    current_time <- as.numeric(Sys.time())

    # Use cached build ID if valid and no force refresh
    if (!force_refresh &&
        !is.null(cache$build_id) &&
        (current_time - cache$timestamp) < cache$cache_duration) {
        #message("Using cached build ID")
        return(cache$build_id)
    }

    # Set up request headers
    headers <- httr::add_headers(
        "Upgrade-Insecure-Requests" = "1",
        "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
        "sec-ch-ua" = '"Chromium";v="142", "Google Chrome";v="142", "Not_A Brand";v="99"',
        "sec-ch-ua-mobile" = "?0",
        "sec-ch-ua-platform" = '"Windows"'
    )

    tryCatch({
        message("Fetching build ID from Casablanca Stock Exchange homepage...")
        res <- httr::GET(
            "https://www.casablanca-bourse.com/fr",
            headers,
            httr::timeout(30)
        )

        if (httr::status_code(res) != 200) {
            message("Failed to access homepage. Status code: ", httr::status_code(res))
            return(NULL)
        }

        html <- httr::content(res, as = "text", encoding = "UTF-8")

        # Extract JSON embedded in the page
        match <- stringr::str_match(
            html,
            '<script id="__NEXT_DATA__" type="application/json">(.*?)</script>'
        )

        if (is.na(match[1, 2])) {
            #message("Could not find the __NEXT_DATA__ script in the page")
            return(NULL)
        }

        # Safely parse the JSON
        next_data <- tryCatch(
            jsonlite::fromJSON(match[1, 2]),
            error = function(e) NULL
        )

        if (is.null(next_data) || is.null(next_data$buildId)) {
            #message("build ID not found in extracted JSON")
            return(NULL)
        }

        build_id <- next_data$buildId

        # Save build ID to cache
        cache$build_id <- build_id
        cache$timestamp <- current_time

        message("Build ID successfully retrieved: ", build_id)
        return(build_id)

    }, error = function(e) {
        message("Error while retrieving build ID: ", e$message)
        return(NULL)
    })
}



#' Retrieve Cached Build ID (Wrapper)
#'
#' @description
#' Wrapper function that ensures a build ID is always returned by leveraging
#' the caching mechanism. If a fresh value cannot be retrieved, it falls back
#' to the last cached value when available.
#'
#' @param force_refresh Logical. If TRUE, forces a refresh of the build ID.
#'
#' @return A character string containing the build ID, or NULL if unavailable.
#'
#' @keywords internal
get_build_id_cached <- function(force_refresh = FALSE) {

    cache <- get_build_cache()
    current_time <- as.numeric(Sys.time())

    # -----------------------------
    # Step 1: Check valid cache
    # -----------------------------
    if (!force_refresh &&
        !is.null(cache$build_id) &&
        (current_time - cache$timestamp) < cache$cache_duration) {

        # message("Using cached build ID: ", cache$build_id)
        return(cache$build_id)
    }

    # -----------------------------
    # Step 2: Try to fetch new value
    # -----------------------------
    new_build_id <- get_build_id(force_refresh = TRUE)

    # -----------------------------
    # Step 3: If success → return it
    # -----------------------------
    if (!is.null(new_build_id)) {
        return(new_build_id)
    }

    # -----------------------------
    # Step 4: Fallback to old cache
    # -----------------------------
    if (!is.null(cache$build_id)) {
        return(cache$build_id)
    }

    # -----------------------------
    # Step 5: No value available
    # -----------------------------
    # message("No build ID available")
    return(NULL)
}




#' Extract ticker widget values from API response
#'
#' Searches for the "bourse_dynamic_field:ticker" widget inside both
#' internal blocks and paragraph components of a JSON API response.
#' If found, extracts and returns the `mw_values` object contained
#' in the widget payload.
#'
#' @param res A parsed JSON response object (typically from httr::content)
#'        containing `pageProps$node` with nested blocks and paragraphs.
#'
#' @return A list containing ticker `mw_values` if found, otherwise `NULL`.
#'
#' @details
#' The function inspects:
#' \itemize{
#'   \item `node$internal_blocks`
#'   \item `node$field_vactory_paragraphs`
#' }
#'
#' It parses the `widget_data` JSON field and extracts:
#' `components[[1]]$bande$mw_values`
#'
#' The first valid match is returned.
#'
#' @importFrom jsonlite fromJSON
find_ticker_widget <- function(res) {

    # Fonction qui parse le widget_data d'un bloc/paragraph
    extract_from_widget_data <- function(widget_data_str) {
        tryCatch({
            wd <- fromJSON(widget_data_str, simplifyVector = FALSE)
            # Vérifier que c'est bien un ticker avec mw_values
            mw <- wd$components[[1]]$bande$mw_values
            if (!is.null(mw)) return(mw)
        }, error = function(e) NULL)
        return(NULL)
    }

    node <- res$pageProps$node

    # 1. Chercher dans internal_blocks
    for (block in node$internal_blocks) {
        if (!is.null(block$content$widget_id) &&
            block$content$widget_id == "bourse_dynamic_field:ticker") {
            mw <- extract_from_widget_data(block$content$widget_data)
            if (!is.null(mw)) return(mw)
        }
    }

    # 2. Chercher dans field_vactory_paragraphs
    for (para in node$field_vactory_paragraphs) {
        comp <- para$field_vactory_component
        if (!is.null(comp$widget_id) &&
            comp$widget_id == "bourse_dynamic_field:ticker") {
            mw <- extract_from_widget_data(comp$widget_data)
            if (!is.null(mw)) return(mw)
        }
    }

    return(NULL)
}



#' Retrieve market classes and market type from Casablanca Stock Exchange API
#'
#' Downloads the live market listing page from the Casablanca Stock Exchange
#' Next.js API, extracts the ticker widget, and returns active market classes
#' along with the market type identifier.
#'
#' @return A list with two elements:
#' \itemize{
#'   \item `class`: A tibble containing only active classes (non-zero values)
#'   \item `marche`: A numeric identifier of the market type
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Retrieves a dynamic build ID using `get_build_id_cached()`
#'   \item Builds request headers required by the Casablanca Stock Exchange API
#'   \item Sends an HTTP GET request using `httr::GET()`
#'   \item Parses the response using `httr::content()`
#'   \item Extracts ticker data using `find_ticker_widget()`
#'   \item Filters active classes where values are non-zero
#' }
#'
#' If no ticker widget is found, the function returns `NULL`.
#'
#' @importFrom httr GET content add_headers
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#'
#' @examples
#' \dontrun{
#' result <- .get_class_marche()
#' result$class
#' result$marche
#' }
.get_class_marche = function() {


    buildID = get_build_id_cached()

    headers = c(
        `sec-ch-ua-platform` = '"Windows"',
        `x-vactory-data-loader` = paste0("/_next/data/",buildID,"/en/live-market/marche-actions-listing.json?slug=live-market&slug=marche-actions-listing"),
        Referer = "https://www.casablanca-bourse.com/en/live-market/marche-actions-listing",
        `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/147.0.0.0 Safari/537.36",
        `sec-ch-ua` = '"Google Chrome";v="147", "Not.A/Brand";v="8", "Chromium";v="147"',
        `sec-ch-ua-mobile` = "?0"
    )

    params = list(
        slug = "live-market",
        slug = "marche-actions-listing"
    )


    url = paste0(
        "https://www.casablanca-bourse.com/_next/data/",buildID,"/en/live-market/marche-actions-listing.json"
    )
    req <- httr::GET(url = url, httr::add_headers(.headers=headers), query = params)
    res <- content(req, as = "parsed")


    mw_values <- find_ticker_widget(res)

    if(is.null(mw_values)) {
        return(NULL)
    }

    # Extraire marché et classes
    type_marche <- mw_values$type_marche  # ex: "59"
    classes     <- mw_values$classes       # liste nommée

    # Garder uniquement les classes actives (valeur != 0)
    classes = as_tibble(classes)
    classes_actives = classes[, which(classes[1, ] != 0)]

    return(list(
        class = as.numeric(classes_actives),
        marche = as.numeric(type_marche)
    ))

}



#' Mettre a jour l'objet Market avec les tickers de la BVC
#'
#' @description
#' Cette fonction interne recupere les donnees de la Bourse de Casablanca (BVC) via
#' \code{bvc_share_index_info()}, trie les actifs par ordre alphabetique et
#' structure les informations dans un objet de classe MARKET.
#'
#' @details
#' Le processus se deroule en trois etapes cles :
#' \enumerate{
#'   \item Appel de la fonction de base pour obtenir les actions et indices marocains.
#'   \item Separation des donnees selon leur type ("Share" ou "Index") pour alimenter les slots S4.
#'   \item Compilation des listes de noms et de tickers complets pour les recherches ulterieures.
#' }
#'
#' @return Un objet de classe \code{MARKET} (BVC_MARKET) avec les slots \code{Indexes},
#' \code{Shares}, \code{List}, et \code{Ticker_full_name} mis a jour. Retourne un message
#' d'erreur en cas de probleme de connexion.
#'
#' @note
#' Comme son nom l'indique par le prefixe ".", cette fonction est privee et
#' destinee a etre utilisee a l'interieur du package.
#'
#' @importFrom methods new
#'
#' @keywords internal
.GET_tickers_BVC = function() {
    tryCatch(
        {


            ticker_data = bvc_share_index_info()
            ticker_data = ticker_data[order(ticker_data$Ticker),]


            # MISE EN FORMAT SELON L'AFFICHAGE STANDARD

            BVC_market = CREATE_ALL_MARKETS()$BVC_MARKET

            # Indexes
            BVC_market@Indexes = ticker_data[ticker_data$Type == "Index",2:3]
            rownames(BVC_market@Indexes) = NULL

            # Shares
            BVC_market@Shares = ticker_data[ticker_data$Type == "Share",2:4]
            rownames(BVC_market@Shares) = NULL

            # Name List
            BVC_market@ListIndexes = BVC_market@Indexes[,1]
            BVC_market@ListShares = BVC_market@Shares[,1]
            BVC_market@List = c(BVC_market@ListIndexes,BVC_market@ListShares) #Nom de tous les actifs
            BVC_market@Ticker_full_name = ticker_data[!(ticker_data$Ticker_fullname == ""),5]

            return(BVC_market)
        },
        error = function(e) {
            print(e)
            message("Make sure you have an active internet connection. ")
        },
        warning = function(w) {
            message("Make sure you have an active internet connection. ")
        }
    )
}





#' Recuperer les informations consolidees des titres et indices de la BVC
#'
#' @description
#' Cette fonction agrege les donnees des actions et des indices boursiers de la
#' Bourse de Casablanca (BVC). Elle harmonise les colonnes pour correspondre
#' au format standard des donnees de marche.
#'
#' @details
#' La fonction appelle en interne \code{bvc_market_share_info()} pour les actions
#' et \code{bvc_market_index_info()} pour les indices. Le code pays est fixe
#' par defaut a "ma" (Maroc).
#'
#' @return Un data.frame contenant les colonnes suivantes :
#' \itemize{
#'   \item \strong{Type}: Nature de la valeur ("Share" pour les actions ou "Index" pour les indices).
#'   \item \strong{Ticker}: Code mnemonique ou code de l'indice.
#'   \item \strong{Description}: Libelle complet de la valeur ou de l'indice.
#'   \item \strong{Country.Code}: Code ISO du pays ("ma").
#'   \item \strong{Ticker_fullname}: Identifiant complet (ici identique au Ticker).
#' }
#'
#'
#' @examples
#' \dontrun{
#'  # Necessite que les fonctions sources soient definies
#'  all_bvc_data <- bvc_share_index_info()
#'  print(head(all_bvc_data))
#' }
bvc_share_index_info = function() {

    bvc_market_shares_data = bvc_market_share_info()

    bvc_market_shares = data.frame(
        Type = "Share",
        Ticker = bvc_market_shares_data$Symbole,
        Description = bvc_market_shares_data$Nom,
        `Country.Code` = "ma",
        Ticker_fullname = bvc_market_shares_data$Symbole
    )

    bvc_market_indexes_data = bvc_market_index_info()

    bvc_market_indexes = data.frame(
        Type = "Index",
        Ticker = bvc_market_indexes_data$Code_Index,
        Description = bvc_market_indexes_data$Indice,
        `Country.Code` = "MA",
        Ticker_fullname = bvc_market_indexes_data$Code_Index
    )

    ticker_data <- rbind(
        bvc_market_shares,
        bvc_market_indexes
    )

    return(ticker_data)
}



#' Recuperer les donnees de marche en temps reel (Ticker) de la Bourse de Casablanca
#'
#' Cette fonction interroge l'API de la Bourse de Casablanca pour obtenir les
#' statistiques de trading (cours, volumes, transactions, carnet d'ordres simplifie)
#' pour un marche et des classes d'actifs specifiques.
#'
#' @param marche Integer. L'identifiant du marche (ex: 59 pour le marche central). Par defaut `59`.
#' @param classes Integer vector. Un vecteur d'identifiants de classes d'actifs (ex: 50). Par defaut `c(50)`.
#'
#' @return Un \code{data.frame} contenant les colonnes renommees en francais (Nom, Symbole, Cours, Volume, etc.).
#' Retourne \code{NULL} en cas d'erreur de connexion ou si aucune donnee n'est trouvee.
#'
#' @importFrom httr GET status_code content config timeout
#' @importFrom jsonlite fromJSON
#'
#'
#' @examples
#' \dontrun{
#' df_market <- bvc_market_share_info(marche = NULL, classes = NULL)
#' head(df_market)
#' }
bvc_market_share_info <- function(marche = NULL, classes = NULL) {

    class_marche_value = .get_class_marche()

    if(is.null(marche)) marche = class_marche_value$marche
    if(is.null(classes)) classes = class_marche_value$class

    url <- "https://www.casablanca-bourse.com/api/proxy/fr/api/bourse/dashboard/ticker"

    params <- list(
        "marche" = marche,
        "class[]" = classes
    )

    headers = c(
        `sec-ch-ua-platform` = '"Windows"',
        Referer = "https://www.casablanca-bourse.com/en/live-market/marche-actions-listing",
        `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/147.0.0.0 Safari/537.36",
        Accept = "application/vnd.api+json",
        `sec-ch-ua` = '"Google Chrome";v="147", "Not.A/Brand";v="8", "Chromium";v="147"',
        `Content-Type` = "application/vnd.api+json",
        `sec-ch-ua-mobile` = "?0"
    )

    # Appel API
    res <- try(httr::GET(url,
                         query = params,
                         httr::add_headers(.headers=headers),
                         httr::config(ssl_verifypeer = FALSE),
                         httr::timeout(60)),
               silent = TRUE)

    if (inherits(res, "try-error")) {
        message("Erreur lors de l'appel API")
        return(NULL)
    }

    if (httr::status_code(res) != 200) {
        message(" Erreur ", httr::status_code(res))
        return(NULL)
    }


    data <- content(res, as = "text", encoding = "UTF-8")
    data <- fromJSON(data)

    # Extraire les valeurs
    values <- data$data$values

    if (is.null(values)) {
        message(" Aucune donnee recue")
        return(NULL)
    }

    # Transformation en DataFrame
    df <- as.data.frame(values)

    # Renommer les colonnes
    column_mapping <- c(
        field_best_ask_price          = "Meilleur prix vente",
        field_best_ask_size           = "Quantite meilleur prix vente",
        field_best_bid_price          = "Meilleur prix achat",
        field_best_bid_size           = "Quantite meilleur prix achat",
        field_capitalisation          = "Capitalisation",
        field_closing_price           = "Prix cloture",
        field_cours_ajuste            = "Cours ajuste",
        field_cours_courant           = "Cours courant",
        field_cumul_titres_echanges   = "Quantite echangee",
        field_cumul_volume_echange    = "Volume echange",
        field_difference              = "Difference",
        field_etat_cot_val            = "Statut",
        field_high_price              = "Plus haut",
        field_low_price               = "Plus bas",
        field_opening_price           = "Ouverture",
        field_static_reference_price  = "Prix reference",
        field_total_trades            = "Nombre transactions",
        field_var_veille              = "Variation %",
        label                         = "Nom",
        ticker                        = "Symbole",
        sous_secteur                  = "Secteur"
    )

    # Appliquer le renommage si les colonnes existent
    existing <- intersect(names(column_mapping), names(df))
    names(df)[match(existing, names(df))] <- column_mapping[existing]

    return(df)
}



#' Recuperer les donnees detaillees de tous les indices de la Bourse de Casablanca
#'
#' Version etendue de la recuperation des indices retournant l'integralite des
#' colonnes disponibles (valeur, plus haut, plus bas, diviseur, etc.), triee
#' par capitalisation decroissante.
#'
#' @param formatted Logical. Indique si les colonnes de texte doivent etre formatees.
#'
#' @return Un \code{data.frame} exhaustif de l'etat des indices.
#'
bvc_market_index_info = function(formatted = TRUE) {
    indices_data <- get_all_indices_overview(formatted)
    if (is.null(indices_data)) return(NULL)

    df <- format_indices_to_dataframe(indices_data, formatted)
    if (is.null(df) || nrow(df) == 0) return(NULL)

    if (!"Capitalisation_num" %in% colnames(df)) {
        message(" Colonne 'Capitalisation_num' non trouvee dans le DataFrame")
        return(NULL)
    }

    result_df <- df
    # sort descending
    result_df <- result_df[order(-as.numeric(result_df$Capitalisation_num)), , drop = FALSE]
    # remove numeric helper column
    result_df$Capitalisation_num <- NULL

    return(result_df)
}



#' Recuperer un apercu global de tous les indices de la Bourse de Casablanca
#'
#' Cette fonction interroge l API publique de la Bourse de Casablanca afin de
#' recuperer un apercu regroupe des indices par categorie (indices generaux,
#' sectoriels, thematiques, etc.).
#'
#' @param formatted Logical. Indique s il faut formater les nombres avec des
#' espaces pour les milliers dans l'apercu. Par defaut \code{TRUE}.
#'
#' @return Une liste contenant les categories et leurs indices associes.
#' Retourne \code{NULL} en cas d erreur ou si aucune donnee n est disponible.
#'
#' @details
#' La fonction realise un appel HTTP vers l endpoint :
#' \code{/api/bourse/dashboard/grouped_index_watch}. Elle inclut des headers
#' specifiques pour simuler un navigateur et eviter les blocages.
#'
#'
#' @examples
#' \dontrun{
#' indices <- get_all_indices_overview()
#' }
get_all_indices_overview <- function(formatted = TRUE) {
    headers <- httr::add_headers(
        `sec-ch-ua-platform` = '"Windows"',
        Referer = 'https://www.casablanca-bourse.com/fr/live-market/marche-cash/indices',
        `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64)',
        Accept = 'application/vnd.api+json',
        `sec-ch-ua` = '"Chromium";v="142", "Google Chrome";v="142", "Not_A Brand";v="99"',
        `Content-Type` = 'application/vnd.api+json',
        `sec-ch-ua-mobile` = '?0'
    )

    endpoint <- "https://www.casablanca-bourse.com/api/proxy/fr/api/bourse/dashboard/grouped_index_watch"


    res <- try(
        httr::GET(endpoint, headers, httr::config(ssl_verifypeer = FALSE), httr::timeout(30)),
        silent = TRUE
    )

    Sys.sleep(2)

    if (inherits(res, "try-error")) {
        message(" Erreur lors de l'appel API : ", conditionMessage(attr(res, "condition")))
        return(NULL)
    }

    if (httr::status_code(res) != 200) {
        message(" Erreur API: ", httr::status_code(res))
        return(NULL)
    }

    parsed <- content(res, as = "parsed", encoding = "UTF-8")
    #data <- fromJSON(parsed)

    if (is.null(parsed$data)) {
        cat(" Aucune donnee trouvee\n")
        return(NULL)
    }
    values = parsed$data
    #values = as.data.frame(parsed$data)

    return(values)
}



#' Extraire le code d'un indice depuis son URL
#'
#' Analyse une chaine de caracteres correspondant a l URL relative d un indice
#' pour en extraire le dernier segment (generalement le code de l indice).
#'
#' @param index_url Character. URL de l indice (par ex. \code{"/fr/live-market/indices/MASI"}).
#'
#' @return Le code de l indice en majuscules (ex. \code{"MASI"}) ou une chaine vide \code{""} si l URL est invalide.
#'
#' @examples
#' # extract_index_code("/fr/live-market/indices/MASI")
extract_index_code <- function(index_url) {
    if (is.null(index_url) || index_url == "") return("")
    parts <- strsplit(index_url, "/", fixed = TRUE)[[1]]
    if (length(parts) == 0) return("")
    tail(parts, 1)
}


#' Convertir les donnees des indices en data.frame structure
#'
#' @param indices_data Liste. Donnees des indices (retournees par \code{get_all_indices_overview}).
#' @param formatted Logical. Si TRUE, formate les nombres en notation francaise (espaces milliers).
#'
#' @return Un \code{data.frame} contenant les indices. Retourne \code{NULL} si \code{indices_data} vide.
#' @examples
#' \dontrun{
#' raw <- get_all_indices_overview()
#' df <- format_indices_to_dataframe(raw)
#' }
#' @importFrom jsonlite fromJSON
format_indices_to_dataframe <- function(indices_data, formatted = TRUE) {
    if (is.null(indices_data) || length(indices_data) == 0) return(NULL)

    all_indices <- list()

    for (category in indices_data) {
        category_name <- if (!is.null(category$title)) category$title else "Non categorise"
        items <- category$items
        if (is.null(items) || length(items) == 0) next

        for (item in items) {
            index_url <- if (!is.null(item$index_url)) item$index_url else ""
            index_code <- extract_index_code(index_url)
            full_url <- if (index_url != "") paste0("https://www.casablanca-bourse.com", index_url) else ""

            # robust numeric extraction
            as_num_safe <- function(x) {
                if (is.null(x) || x == "") return(0)
                v <- suppressWarnings(as.numeric(x))
                if (is.na(v)) return(0)
                v
            }

            capitalisation <- as_num_safe(item$field_market_capitalisation)
            valeur_indice <- as_num_safe(item$field_index_value)
            plus_bas <- as_num_safe(item$field_index_low_value)
            plus_haut <- as_num_safe(item$field_index_high_value)
            cours_veille <- as_num_safe(item$veille)
            diviseur <- as_num_safe(item$field_divisor)

            # format helper (format francais : espace pour milliers, virgule pour decimales)
            format_number_french <- function(number) {
                if (is.null(number)) return(NA_character_)
                if (!is.numeric(number)) number <- suppressWarnings(as.numeric(number))
                if (is.na(number)) return(as.character(number))
                # use format with big.mark = " "
                formatted <- format(round(number, 2), big.mark = " ", decimal.mark = ".", nsmall = 2, scientific = FALSE)
                # replace decimal point by comma for French style on "display" if wanted
                formatted <- gsub("\\.00$", "", formatted) # keep as in original behavior if needed
                formatted
            }

            row <- list(
                Categorie = category_name,
                Indice = if (!is.null(item$index)) item$index else "",
                Code_Index = index_code,
                URL_Complete = full_url,
                URL_Relative = index_url,
                `Capitalisation (MAD)` = if (formatted) format_number_french(capitalisation) else capitalisation,
                `Valeur indice` = if (formatted) format_number_french(valeur_indice) else valeur_indice,
                `Plus bas` = if (formatted) format_number_french(plus_bas) else plus_bas,
                `Plus haut` = if (formatted) format_number_french(plus_haut) else plus_haut,
                `Cours veille` = if (formatted) format_number_french(cours_veille) else cours_veille,
                `Variation annuelle (%)` = as_num_safe(item$field_var_year),
                `Variation veille (%)` = as_num_safe(item$field_var_veille),
                Diviseur = if (formatted) format_number_french(diviseur) else diviseur,
                `Heure transaction` = if (!is.null(item$field_transact_time)) item$field_transact_time else "",
                Capitalisation_num = capitalisation,
                Valeur_indice_num = valeur_indice
            )

            all_indices[[length(all_indices) + 1]] <- row
        }
    }

    df <- do.call(rbind, lapply(all_indices, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
    rownames(df) <- NULL

    return(df)
}





#' Liste des indices avec code et capitalisation triee par capitalisation decroissante
#'
#' @param formatted Logical. Indique si les valeurs numeriques doivent etre formatees. Par defaut TRUE.
#'
#' @return Un \code{data.frame} avec les colonnes \code{Indice, Code_Index, Capitalisation (MAD), URL_Complete, Categorie}.
#' @examples
#' \dontrun{
#' indices <- get_indices_list_with_capitalization()
#' }
get_indices_list_with_capitalization <- function(formatted = TRUE) {
    indices_data <- get_all_indices_overview(formatted)
    if (is.null(indices_data)) return(NULL)

    df <- format_indices_to_dataframe(indices_data, formatted)
    if (is.null(df) || nrow(df) == 0) return(NULL)

    if (!"Capitalisation_num" %in% colnames(df)) {
        message(" Colonne 'Capitalisation_num' non trouvee dans le DataFrame")
        return(NULL)
    }

    result_df <- df[, c("Indice", "Code_Index", "Capitalisation (MAD)", "URL_Complete", "Categorie", "Capitalisation_num")]
    # sort descending
    result_df <- result_df[order(-as.numeric(result_df$Capitalisation_num)), , drop = FALSE]
    # remove numeric helper column
    result_df$Capitalisation_num <- NULL

    return(result_df)
}





#' Récupérer les indices de la Bourse de Casablanca
#'
#' Cette fonction interroge l'API de la Bourse de Casablanca afin de récupérer
#' l'ensemble des indices (principaux, sectoriels, devises, etc.) sous forme
#' de dataframe structuré.
#'
#' Les données sont organisées par catégorie (title) et contiennent des
#' informations telles que la valeur de l'indice, la capitalisation,
#' les variations et les bornes (haut/bas).
#'
#' @return Un dataframe contenant :
#' \itemize{
#'   \item `title` : catégorie de l'indice (Main indices, Sector Indices, etc.)
#'   \item `index` : nom de l'indice
#'   \item `index_code` : code extrait de l'URL (ex: MASI, FCSELIQ)
#'   \item `field_index_value` : valeur actuelle de l'indice
#'   \item `field_var_year` : variation annuelle (%)
#'   \item `field_var_veille` : variation journalière (%)
#'   \item `field_market_capitalisation` : capitalisation
#'   \item `field_transact_time` : date/heure de la dernière transaction
#'   \item autres variables associées aux indices
#' }
#'
#' @details
#' La fonction :
#' \enumerate{
#'   \item Effectue une requête HTTP vers l'API BVC
#'   \item Parse le JSON retourné
#'   \item Aplati la structure imbriquée (`data -> items`)
#'   \item Ajoute la catégorie (`title`)
#'   \item Extrait le code de l'indice depuis `index_url`
#'   \item Remplace les valeurs NULL par NA
#'   \item Convertit les colonnes numériques et dates
#' }
#'
#' @examples
#' \dontrun{
#' df_indices <- get_bvc_index()
#' head(df_indices)
#' }
#'
#' @importFrom httr GET add_headers config timeout status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate across
#' @importFrom purrr map map_dfr
#' @importFrom stringr str_extract
#' @importFrom tibble as_tibble
#'
get_bvc_index = function(){

    url_grouped_index = "https://www.casablanca-bourse.com/api/proxy/en/api/bourse/dashboard/grouped_index_watch?"

    headers <- c(
        "Upgrade-Insecure-Requests" = "1",
        "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/142.0.0.0 Safari/537.36",
        "sec-ch-ua" = "\"Chromium\";v=\"142\", \"Google Chrome\";v=\"142\", \"Not_A Brand\";v=\"99\"",
        "sec-ch-ua-mobile" = "?0",
        "sec-ch-ua-platform" = "\"Windows\""
    )

    # Requête HTTP
    res <- try(GET(url_grouped_index,
                   add_headers(.headers=headers),
                   config(ssl_verifypeer = TRUE),
                   timeout(30)), silent = TRUE)

    if (inherits(res, "try-error") || status_code(res) != 200) {
        message(paste("Can't recover the hidden index"))
        return(NULL)
    }

    txt <- content(res, as = "text", encoding = "UTF-8")
    json_data <- fromJSON(txt, simplifyVector = FALSE)

    df <- map_dfr(json_data$data, function(block){

        title <- block$title

        if (length(block$items) == 0) return(NULL)

        map_dfr(block$items, function(item) {

            item_clean <- map(item, ~ if (is.null(.x)) NA else .x)

            as_tibble(item_clean) %>%
                mutate(
                    title = title,
                    index_code = str_extract(index_url, "[^/]+$")
                )
        })
    })


    # Conversion des types
    df <- df %>%
        mutate(
            field_transact_time = as.POSIXct(field_transact_time),
            across(starts_with("field_"), as.numeric)
        )

    return(df)
}


