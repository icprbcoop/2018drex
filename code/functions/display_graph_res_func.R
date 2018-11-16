display_graph_res_func <- function(name, res.ts.df){
  res.graph <- res.ts.df %>%
    select(date_time, storage, outflow) %>%
    gather(key = "Legend", 
           value = "MG", -date_time) %>%
    filter(date_time >= input$plot_range[1],
           date_time <= input$plot_range[2])
  res_plot <- ggplot(data = res.graph,
         aes(x = date_time, y = MG, group = Legend)) +
    geom_line(aes(color = Legend, size = Legend)) +
    scale_color_manual(values = c("lightblue",
                                  "blue")) +
    scale_size_manual(values = c(0.5, 1)) +
    ggtitle(name) +
    theme(plot.title = element_text(size = 18,
                                    face = "bold")) +
    theme(axis.title.x = element_blank()) +
    theme(legend.position = "none")
  return(res_plot)
}

