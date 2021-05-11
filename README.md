# google-mobility-map

Depuis le début de la pandémie, Google publie en libre accès les [données de mobilité](https://www.google.com/covid19/mobility/) issues de ses produits tels que Google Maps. Ce répertoire propose de quoi exploiter ces données afin de créer des cartes et des graphiques pour la France.

* Le dossier `data\` contient les données Google ainsi qu'un fichier de données géographiques nécessaire pour produire des cartes.

* Le dossier `src\` regroupe les scripts de fonctions `R` nécessaires pour produire des cartes et des graphiques à partir des données.

* Le dossier `output\` présente des exemples de cartes et de graphiques réalisés à partir des scripts. C'est ici que sont enregistrées les sorties.

Le script `run.R` explicite l'usage des fonctions du dossier `src\` pour produire les objets souhaités.
