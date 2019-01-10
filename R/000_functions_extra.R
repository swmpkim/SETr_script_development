




plot_cumu_set_interactive <- function(columns = 4){
    change_cumu_set <- change_cumu_set %>%
        mutate(labels = paste0(date, ':  ', round(mean_cumu, 2)))
    ggplot(change_cumu_set, aes(x = date, y = mean_cumu)) +
        geom_line(col = 'lightsteelblue4') +
        geom_smooth(se = FALSE, method = 'lm', 
                    col = 'steelblue4', lty = 5, size = 1) +
        geom_point_interactive(aes(tooltip = labels), shape = 21, 
                   fill = 'lightsteelblue1', col = 'steelblue3', 
                   size = 3.5, alpha = 0.9) +
        facet_wrap(~set_id, ncol = columns, scales = 'free_y') +
        labs(title = 'Cumulative Change since first reading', 
             subtitle = 'dashed line is linear regression',
             x = 'Date',
             y = 'Change since first reading (mm)') +
        theme_classic()
}