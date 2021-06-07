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


# Dans ce qui suit, lorsque SAVE = TRUE,  l'objet est alors enregistré dans
# le répertoire output/


# Réaliser un graphique pour la France sur une période donnée :
#--------------------------------------------------------------

# Renseigner la chronologie à indiquer (s'il y en a une) sous la forme :
# c(date début, date fin, nom de l'évènement). Une date doit être au format :
# yyyy-mm-dd
# Si l'événement est ponctuel, renseigner 2 fois la même date

timeline <- list(c('2020-10-28', '2020-10-28', ''),
                 c('2021-04-11', '2021-04-11', ''))

# Faire appel à la fonction prepare_plot_data en passant le parametre level à
# 'FR' et en renseignant un vecteur de dates sous la forme :
# c(date début, date fin).

X_graph <- prepare_plot_data(date_vect = c('2021-03-01', '2021-05-21'),
                      level = 'FR')
rolling_mean <- tibbletime::rollify(mean, 7)
X_graph <- X_graph %>%
    dplyr::mutate_if(is.numeric, rolling_mean)
X_graph <- X_graph[-c(1:7), ]

# Puis faire appel à la fonction plot_dep

plot_dep(
    X = X_graph,
    department = 'FR',
    dep_name = 'France',
    save = T,
    timeline = timeline
)

# Réaliser un graphique pour un département sur une période donnée :
#-------------------------------------------------------------------

X_graph <- prepare_plot_data(date_vect = c('2021-03-01', '2021-03-21'),
                      level = 'DEP')

# Exemple pour le département des Alpes-Maritimes :

timeline <- list(
    c('2021-02-26', '2021-02-28', 'Confinement localisé le WE'),
    c('2021-03-05', '2021-03-07', 'Confinement localisé le WE'),
    c('2021-03-12', '2021-03-14', 'Confinement localisé le WE'),
    c('2021-02-20', '2021-02-20', 'Début des vacances d\'hiver'),
    c('2021-03-07', '2021-03-07', 'Fin des vacances d\'hiver'),
    c('2021-03-19', '2021-03-19', 'Renforcement localisé des mesures')
)
plot_dep(
    X = X_graph,
    department = '06',
    dep_name = 'Alpes-Maritimes',
    save = T,
    timeline = timeline
)

# Réaliser un graphique pour plusieurs départements sur une période donnée :
#---------------------------------------------------------------------------

timeline <- list(c('2021-03-19', '2021-03-19',
                   'Renforcement localisé des mesures'))

X_graph <- prepare_plot_data(date_vect = c('2021-03-01', '2021-03-21'),
                      level = 'DEP')
X_graph <- X_graph %>%
    dplyr::mutate(DEP = dplyr::case_when(DEP %in% dep_confinement ~ 'confines',
                                         T ~ 'non_confines')) %>%
    dplyr::group_by(date, DEP) %>%
    dplyr::summarise_all(mean) %>%
    dplyr::ungroup()

plot_dep(
    X = X_graph,
    department = 'confines',
    dep_name = 'dép. concernés par un renforcement des mesures',
    save = T,
    timeline = timeline
)

plot_dep(
    X = X_graph,
    department = 'non_confines',
    dep_name = 'dép. non concernés par un renforcement des mesures',
    save = T
)
