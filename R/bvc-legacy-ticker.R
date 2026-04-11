
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

            # General extraction

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
        `Country.Code` = "ma",
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
#' df_market <- bvc_market_share_info(marche = 59, classes = c(50))
#' head(df_market)
#' }
bvc_market_share_info <- function(marche = 59, classes = c(50)) {

    url <- "https://www.casablanca-bourse.com/api/proxy/fr/api/bourse/dashboard/ticker"

    params <- list(
        "marche" = marche,
        "class[]" = classes
    )

    # Appel API
    res <- try(httr::GET(url,
                         query = params,
                         httr::config(ssl_verifypeer = FALSE),
                         httr::timeout(10)),
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


