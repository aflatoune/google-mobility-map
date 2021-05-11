# Ce script propose deux options pour créer le fichier .gpkg nécessaire à la
# réalisation des cartes au niveau départemental. Ledit fichier étant déjà
# enregistré (data/departement_2020.gpkg), il n'y pas de besoin de ré-exécuter
# le présent script. Ce dernier sert seulement à garantir une reproductibilité
# intégrale du travail mené.
#
# Option 1 : télécharger le fichier (CONTOURS-IRIS.shp), le charger puis le
# modifier. Le fichier peut être téléchargé ici :
# ftp://Contours_IRIS_ext:ao6Phu5ohJ4jaeji@ftp3.ign.fr/CONTOURS-IRIS_2-1__SHP__FRA_2020-01-01.7z
#
# Options 2 :  passer par le package R CARTElette


# Option 1 :
#-----------

`%>%` <- magrittr::`%>%`

commune <- sf::st_read(
    paste0(
        'data/geo_data_input/',
        'CONTOURS-IRIS_2-1_SHP_LAMB93_FXX-2020/',
        'CONTOURS-IRIS.shp'
    )
)
commune <- commune %>%
    dplyr::mutate(DEP = substr(INSEE_COM, 1, 2)) %>%
    dplyr::select(DEP, geometry)

departement <- stats::aggregate(commune,
                                list(commune$DEP),
                                function(x)
                                    x[1]) %>%
    dplyr::select(-Group.1)

sf::st_write(departement, 'data/departement_2020.gpkg')


# Option 2 :
#-----------

# TO DO