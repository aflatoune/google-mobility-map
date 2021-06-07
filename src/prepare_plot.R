library(ggplot2)


map_dep <- function(X,
                    col,
                    date_vect,
                    plot_diff = F,
                    dep_to_highlight = NULL,
                    title = T,
                    save = F,
                    file_name = NULL,
                    palette = 'BrBG',
                    title.size = 1.2,
                    title.fontface = 1,
                    title.position = c('center', 'TOP'),
                    title.fontfamily = 'serif',
                    legend.position = c('left', 'bottom'),
                    legend.title.fontfamily = 3,
                    source.position = 'right',
                    highlight.color = 'darkred',
                    inner.margin = 0.1,
                    frame = F,
                    fig_width = 6,
                    fig_height = 6) {
    upper <- format(as.Date(date_vect[2]), "%d/%m")
    lower <- format(as.Date(date_vect[1]), "%d/%m")
    if (title & !plot_diff) {
        titre <-
            glue::glue(paste(
                'Indicateur {col} entre le {lower} et le \n',
                '{upper} (moyenne)',
                sep = ' '
            ))
    } else if (title & plot_diff) {
        titre <-
            glue::glue(
                paste(
                    'Variation moyenne de l\'indicateur {col} \n',
                    'entre la semaine du {lower} et celle du',
                    '{upper}',
                    sep = ' '
                )
            )
    } else {
        titre = ''
    }
    X <- sf::st_as_sf(X)
    tm <- tmap::tm_shape(X) +
        tmap::tm_polygons(
            col = col,
            palette = palette,
            midpoint = 0,
            title = ''
        ) +
        tmap::tm_layout(
            title = titre,
            frame = frame,
            title.size = title.size,
            title.position = title.position,
            title.fontface = title.fontface,
            title.fontfamily = title.fontfamily,
            legend.position =  legend.position,
            legend.title.fontfamily = legend.title.fontfamily,
            inner.margin = inner.margin
        ) +
        tmap::tm_credits('Source : Google Mobility. Calculs : DG Trésor',
                         align = source.position)
    if (!is.null(dep_to_highlight)) {
        X_h <- X
        X_h$highlight <-
            ifelse(X_h$DEP %in% dep_to_highlight, 'Yes', 'No')
        X_h <- X_h %>%
            dplyr::group_by(highlight) %>%
            dplyr::summarise() %>%
            dplyr::ungroup()
        tm <- tm +
            tmap::tm_shape(X_h[2, ]) +
            tmap::tm_borders(col = highlight.color, lwd = 1.3) +
            tmap::tm_add_legend(
                type = 'line',
                labels = 'Départements confinés',
                col = highlight.color,
                lwd = 1.5
            )
    }
    if (save) {
        if (!is.null(file_name)) {
            tmap::tmap_save(
                tm,
                filename = file.path('output', file_name),
                width = fig_width,
                height = fig_height
            )
        } else if (is.null(file_name) & !plot_diff) {
            file_name <- glue::glue('map_{col}_{date_vect[1]}.png')
            file_name <- gsub(' ', '_', file_name)
            tmap::tmap_save(
                tm,
                filename = file.path('output', file_name),
                width = fig_width,
                height = fig_height
            )
        } else if (is.null(file_name) & plot_diff) {
            file_name = glue::glue(paste0('map_{col}_',
                                          'diff_',
                                          '{date_vect[2]}',
                                          '.png'))
            file_name <- gsub(' ', '_', file_name)
            tmap::tmap_save(
                tm,
                filename = file.path('output', file_name),
                width = fig_width,
                height = fig_height
            )
        }
    }
    return(tm)
}


plot_dep <- function(X,
                     department,
                     dep_name,
                     save = F,
                     file_name = NULL,
                     timeline = NULL,
                     legend_text_size = 12,
                     axis_text_size = 11,
                     annotation_size = 3,
                     title_style = 'bold',
                     fig_width = 9.5,
                     fig_height = 5.5) {
    upper <- max(X$date)
    lower <- min(X$date)
    if ((upper - lower) <= 30) {
        date_breaks <- '1 day'
    } else if ((upper - lower) <= 180) {
        date_breaks <- '1 week'
    } else {
        date_breaks <- '1 month'
    }
    if (department == 'FR') {
        X[['Lieu de residence']] <- X[['Lieu de residence']] * -1
        Y <- reshape2::melt(
            dplyr::select(X, -country_region_code),
            id.vars = 'date',
            variable.name = 'indicateur'
        )
    } else {
        X <- X %>%
            dplyr::filter(DEP %in% department)
        X[['Lieu de residence']] <- X[['Lieu de residence']] * -1
        Y <- reshape2::melt(dplyr::select(X, -DEP),
                            id.vars = 'date',
                            variable.name = 'indicateur')
    }
    titre <-
        glue::glue(
            paste0(
                'Variations des indicateurs Google Mobility',
                ' par rapport à la normale ({dep_name})'
            )
        )
    last_obs_x <- upper
    last_obs_y <- min(Y$value)
    last_obs_text <- paste0('Dernier point : ', upper)
    source_x <- lower
    source_y <- last_obs_y
    source_text <- 'Source : Google Mobility. Calculs : DG Trésor'
    graph <- ggplot(Y, aes(x = date,
                           y = value,
                           colour = indicateur))
    graph <- graph + geom_line(size = 1) +
        scale_color_manual(
            labels = c(
                'Loisirs et commerces',
                'Alimentation et pharmacies',
                'Transports',
                'Lieu de travail',
                'Lieu de résidence (inversé)',
                'Total'
            ),
            values = c('yellow',
                       'blue',
                       'red',
                       'orange',
                       'green',
                       'purple')
        ) +
        geom_hline(yintercept = 0, size = .5) +
        theme_minimal() +
        labs(
            title = titre,
            x = "",
            y = "",
            caption = c(
                glue::glue("Dernier point : {upper} \n",
                           "Source : Google Mobility. Calculs : DG Trésor")
            )
        ) +
        theme(
            legend.position = 'bottom',
            legend.title = element_blank(),
            legend.text = element_text(size = legend_text_size),
            plot.title = element_text(
                color = '#000080',
                hjust = 0.5,
                face = title_style
            ),
            axis.title.x = element_blank(),
            axis.text.x = element_text(
                color = 'black',
                angle = 90,
                size = axis_text_size,
                vjust = 0
            ),
            axis.title.y = element_blank(),
            axis.text.y = element_text(color = 'black',
                                       size = axis_text_size),
            panel.border = element_rect(
                colour = 'black',
                fill = NA,
                size = 1.5
            ),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank()
        ) +
        scale_x_date(
            date_labels = '%d-%b',
            date_breaks = date_breaks,
            expand = c(0, 0)
        ) +
        scale_y_continuous(n.breaks = 10)
    
    if (!is.null(timeline)) {
        for (event in timeline) {
            graph <- graph + annotate(
                'text',
                x = as.Date(event[[1]]),
                y = source_y + 5,
                label = event[[3]],
                hjust = 0,
                vjust = -1,
                angle = 90,
                col = 'gray53'
            ) +
                annotate(
                    'rect',
                    xmin = as.Date(event[[1]]),
                    xmax = as.Date(event[[2]]),
                    ymin = -Inf,
                    ymax = Inf,
                    fill = 'blue',
                    alpha = .17
                )
            if (as.Date(event[[1]]) == as.Date(event[[2]])) {
                graph <- graph +
                    geom_vline(
                        xintercept = as.Date(event[[1]]),
                        color = 'gray52',
                        lwd = 0.5
                    )
            }
        }
    }
    
    if (save) {
        if (!is.null(file_name)) {
            ggsave(
                graph,
                filename = file.path('output', file_name),
                width = fig_width,
                height = fig_height
            )
        } else {
            file_name <- glue::glue('plot_{department}_{upper}.png')
            ggsave(
                graph,
                filename = file.path('output', file_name),
                width = fig_width,
                height = fig_height
            )
        }
    }
    return(graph)
}
