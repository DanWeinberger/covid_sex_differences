
death_plot_fun <- function(...){
  plot.ds1 <- pneumo.deaths %>% 
    group_by( ... ) %>%
    summarize(N_deaths=n()) %>%
    ungroup() %>%
    mutate(year=as.factor(year),month=as.numeric(as.character(month))) %>%
    tidyr::complete( ..., fill=list(N_deaths=0) ) 
    
  
  plot.cols <- c(rep('gray',6), 'red')
  
  p1 <- ggplot(plot.ds1, aes(x=month, y=N_deaths, group=year, col=year)) +
    geom_line() +
    theme_classic() +
    scale_color_manual(values=plot.cols)+
    ylim(0, NA)
  return(p1)
}
