source('src/prepare_data.R', encoding = 'utf-8')
source('src/prepare_plot.R', encoding = 'utf-8')


# Télécharger les dernières données disponibles et les enregistrer si besoin :
#-----------------------------------------------------------------------------

download = T
if (download) {
    url = paste0(
        'https://www.gstatic.com/covid19/mobility/',
        'Region_Mobility_Report_CSVs.zip'
    )
    download_google_data(url)
}


# Dans ce qui suit, lorsque SAVE = TRUE,  l'objet est enregistré dans
# le répertoire output/


# Réaliser une carte des départements de France pour une période donnée :
#------------------------------------------------------------------------

# Renseigner la période à étudier et créer le dataframe

date_start <- as.Date('2021-05-31')
date_end <- as.Date('2021-06-06')
X_map <- prepare_map_data(date_vect = c(date_start, date_end))

##### En "statique"
# Faire appel à la fonction map_dep en lui passant le dataframe X_map un
# indicateur Google mobility à afficher et un vecteur de dates qui sera
# uniquement utilisé pour titrer la carte et lui donner un nom par défaut
# en cas d'enregistrement.
#
# Pour mettre en évidence certains départements, renseigner le paramètre
# dep_to_higlight sous forme de vecteur

dep_to_highlight <-
    c(
        '02',
        '06',
        '27',
        '58',
        '69'
    )

map_dep(
    X = X_map,
    col = 'Total',
    date_vect = c(date_start, date_end),
    dep_to_highlight = dep_to_highlight,
    save = T
)

##### En "dynamique"
# Pour obtenir la variation des indicateur entre la dernière semaine
# observée et celle qui la précède, choisir un délai égal à 7, puis créer un
# dataframe par période afin de calculer la variation des indicateurs entre les
# deux dates

diff_period <- 7
X1_map <- prepare_map_data(date_vect = c(date_start - diff_period,
                                         date_end - diff_period))
X2_map <- prepare_map_data(date_vect = c(date_start, date_end))
X_diff_map <- X2_map %>%
    dplyr::mutate(
        dplyr::select_if(X2_map, is.numeric) - 
        dplyr::select_if(X1_map, is.numeric)
        )


##### Faire une grille statique/dynamique par indicateur
# Passer la condition à TRUE pour enregistrer les grilles

for (col in colnames(X_map)[2:7]) {
    map_grid_1 <- map_dep(
        X = X_map,
        col = col,
        date_vect = c(date_start, date_end),
        save = F
    )
    map_grid_2 <- map_dep(
        X = X_diff_map,
        col = col,
        date_vect = c(date_start - diff_period, date_start),
        plot_diff = T,
        save = F,
        palette = 'PRGn'
    )
    grid <- tmap::tmap_arrange(map_grid_1, map_grid_2)
    if (TRUE) {
        file_name <- glue::glue(paste0('grid_{col}_',
                                      '{date_start}',
                                      '.png'))
        file_name <- gsub(' ', '_', file_name)
        tmap::tmap_save(
            grid,
            filename = file.path('output', file_name),
            width = 10,
            height = 6
        )
    }
}
