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

plot_incr_pin2 <- function(set, columns = 2){
    change_incr_pin %>%
        filter(set_id == !!set) %>%
        ggplot(., aes(x = date, y = incr, col = as.factor(pin_number))) +
        geom_point(size = 2) +
        geom_hline(yintercept = 25, col = "red", size = 1) +
        geom_hline(yintercept = -25, col = "red", size = 1) +
        facet_wrap(~arm_position, ncol = columns) +
        labs(title = 'Incremental Change', 
             subtitle = 'red lines at +/- 25 mm',
             x = 'Date',
             y = 'Change since previous reading (mm)') +
        theme_bw() +
        scale_color_discrete(name = 'Pin') +
        theme(legend.position = 'bottom')
}