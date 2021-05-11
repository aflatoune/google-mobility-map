`%>%` <- magrittr::`%>%`


download_google_data <- function(url) {
    temp <- tempfile(tmpdir = 'data/')
    tryCatch({
        utils::download.file(url, temp)
        utils::unzip(
            temp,
            files = c(
                '2020_FR_Region_Mobility_Report.csv',
                '2021_FR_Region_Mobility_Report.csv'
            ),
            exdir = 'data'
        )
    },
    error = function(cnd) {
        message("URL does not seem to exist: ", url)
    },
    finally = {
        unlink(temp)
    })
}


read_data <- function(file_name,
                      geo = F,
                      quiet = T) {
    if (geo) {
        geo_data <- sf::st_read(file.path('data', file_name), quiet = quiet)
        return(geo_data)
    }
    if (length(file_name) == 2) {
        google_data_20 <- readr::read_csv(file.path('data', file_name[1]),
                                          col_types = readr::cols())
        google_data_21 <-
            readr::read_csv(file.path('data', file_name[2]),
                            col_types = readr::cols())
        google_data <- rbind(google_data_20, google_data_21)
        return(google_data)
    } else {
        google_data <- readr::read_csv(file.path('data', file_name),
                                       col_types = readr::cols())
        return(google_data)
    }
    
}


prepare_data <- function(google_data, level = 'DEP') {
    colnames(google_data) <- gsub('_percent_change_from_baseline',
                                  '',
                                  colnames(google_data))
    if (level == 'DEP') {
        mycols <- c(
            'DEP',
            'date',
            'retail_and_recreation',
            'grocery_and_pharmacy',
            'transit_stations',
            'workplaces',
            'residential'
        )
        google_data <- google_data %>%
            dplyr::filter(!is.na(google_data$sub_region_2))
        google_data$DEP <- sapply(google_data$iso_3166_2_code,
                                  function(x)
                                      gsub('FR-', '', x))
    } else if (level == 'FR') {
        mycols <- c(
            'country_region_code',
            'date',
            'retail_and_recreation',
            'grocery_and_pharmacy',
            'transit_stations',
            'workplaces',
            'residential'
        )
        google_data <- google_data %>%
            dplyr::filter(is.na(google_data$sub_region_1))
    }
    google_data <- google_data %>%
        dplyr::select(dplyr::all_of(mycols))
    google_data$total <- (
        google_data$retail_and_recreation +
            google_data$grocery_and_pharmacy + google_data$transit_stations +
            google_data$workplaces - google_data$residential
    ) / 5
    dep_data <- google_data
    return(dep_data)
}


.impute_NA <- function(X, level) {
    X <- X %>%
        dplyr::group_by(rlang::.data[[level]]) %>%
        dplyr::mutate_if(is.numeric,
                         function(x)
                             ifelse(is.na(x), mean(x, na.rm = TRUE), x))
    return(X)
}


filter_date <- function(X, date_vect, level) {
    if ((length(date_vect) != 2)) {
        stop('You must indicate two dates: c(starting_date, end_date).')
    } else {
        upper <- as.Date(date_vect[2])
        lower <- as.Date(date_vect[1])
    }
    X <- X %>%
        dplyr::filter((upper >= date) & (date >= lower))
    if (sum(colSums(is.na(dplyr::select_if(X, is.numeric)))) > 0) {
        X <- .impute_NA(X, level = level)
    }
    return(X)
}


aggregate_data <- function(X, level = 'DEP', FUN = mean) {
    X <- stats::aggregate(dplyr::select_if(X, is.numeric),
                          by = X[, level],
                          FUN = FUN)
    return(X)
}


merge_with_geo <- function(X, geo_data, level) {
    merged_data <- merge(X, geo_data, by = level, all.x = T)
    return(merged_data)
}


compute_moving_average <- function(X, level, n = 7) {
    # TO DO
    return(X)
}


.translate_col_names <- function(X) {
    colnames(X)[3:8] <- c(
        'Loisirs et commerces',
        'Alimentation et pharmacies',
        'Transports',
        'Lieu de travail',
        'Lieu de residence',
        'Total'
    )
    return(X)
}


prepare_map_data <- function(date_vect, level = 'DEP') {
    google_data <- read_data('2021_FR_Region_Mobility_Report.csv')
    if (level == 'DEP') {
        geo_data <- read_data('departement_2020.gpkg', geo = T)
        dep_data <- prepare_data(google_data)
    }
    X <-
        filter_date(dep_data, date_vect = date_vect, level = level) %>%
        .translate_col_names() %>%
        aggregate_data(level = level) %>%
        merge_with_geo(geo_data, level = level)
    return(X)
}


prepare_plot_data <- function(date_vect, level) {
    google_data <- read_data(
        c(
            '2020_FR_Region_Mobility_Report.csv',
            '2021_FR_Region_Mobility_Report.csv'
        )
    )
    X <- prepare_data(google_data, level)
    X <- filter_date(X, date_vect, level) %>%
        .translate_col_names()
    return(X)
}
