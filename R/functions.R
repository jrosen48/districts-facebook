#d <- tar_read(raw_data)
prep_data <- function(data) {
  data %>% 
    clean_names() %>% 
    filter(overperforming_score != min(overperforming_score, na.rm = TRUE))
}

create_hist_perform <- function(data) {
  p <- ggplot(data, aes(overperforming_score)) +
    geom_histogram(fill = "cornflowerblue",
                   alpha = 0.7) +
    theme_minimal(15) +
    labs(title = "Preliminary plot",
         y = "Count") +
    theme(plot.title.position = "plot")
  
  ggsave(here::here("plots", "hist_perform.png"), 
         p,
         width = 6.5,
         height = 8)
  
  here::here("plots", "hist_perform.png")
}
