plot_incr_arm2 <- function(columns = 4, set = NULL){
    if(is.null(set)){
        ggplot(change_incr_arm, aes(x = date, y = mean_incr, col = as.factor(arm_position))) +
            geom_point(size = 2) +
            geom_hline(yintercept = 25, col = "red", size = 1) +
            geom_hline(yintercept = -25, col = "red", size = 1) +
            facet_wrap(~set_id, ncol = columns) +
            labs(title = 'Incremental Change', 
                 subtitle = 'red lines at +/- 25 mm',
                 x = 'Date',
                 y = 'Change since previous reading (mm)') +
            theme_bw() +
            scale_color_discrete(name = 'Arm Position') +
            theme(legend.position = 'bottom')
    }
    else{
        change_incr_arm %>%
            filter(set_id == !!set) %>%
            ggplot(., aes(x = date, y = mean_incr, col = as.factor(arm_position))) +
            geom_point(size = 2) +
            geom_hline(yintercept = 25, col = "red", size = 1) +
            geom_hline(yintercept = -25, col = "red", size = 1) +
            facet_wrap(~set_id, ncol = columns) +
            labs(title = 'Incremental Change', 
                 subtitle = 'red lines at +/- 25 mm',
                 x = 'Date',
                 y = 'Change since previous reading (mm)') +
            theme_bw() +
            scale_color_discrete(name = 'Arm Position') +
            theme(legend.position = 'bottom')
    }
}

plot_incr_pin2 <- function(set, columns = 2, pointsize = 2){
    change_incr_pin %>%
        filter(set_id == !!set) %>%
        ggplot(., aes(x = date, y = incr, col = as.factor(pin_number))) +
        geom_point(size = pointsize) +
        geom_hline(yintercept = 25, col = "red", size = 1) +
        geom_hline(yintercept = -25, col = "red", size = 1) +
        facet_wrap(~arm_position, ncol = columns) +
        labs(title = paste0('Incremental Change at ', set),
             subtitle = 'red lines at +/- 25 mm',
             x = 'Date',
             y = 'Change since previous reading (mm)') +
        theme_bw() +
        scale_color_discrete(name = 'Pin') +
        theme(legend.position = 'bottom')
}


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