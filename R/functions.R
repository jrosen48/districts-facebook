#d <- tar_read(raw_data)

prep_data <- function(data) {
  data %>% 
    clean_names() %>% 
    mutate(created = lubridate::as_datetime(created))
}

compute_covid_mentions <- function(data) {
  data %>% 
    mutate(message = tolower(message),
           description = tolower(description),
           link_text = tolower(link_text),
           covid_mentioned = grepl("covid|coronavirus", message) |
             grepl("covid|coronavirus", description) |
             grepl("covid|coronavirus", link_text),
           month = month(created),
           day = day(created)) %>% 
    group_by(month, day) %>% 
    summarize(covid_mentions = sum(covid_mentioned)) %>% 
    mutate(date = make_date(year = 2020, month = month, day = day)) %>% 
    ungroup() %>% 
    mutate(weekday = weekdays(date)) %>% 
    filter(!weekday %in% c("Saturday", "Sunday")) %>% 
    select(date, covid_mentions)
}

create_covid_mention_plot <- function(covid_mention_data) {
  p <- ggplot(covid_mention_data, aes(date, covid_mentions)) +
    geom_line(color = "cornflowerblue",
              size = 1.2) +
    labs(title = "Mentions of COVID or Coronavirus",
         subtitle = "Data from weekends removed",
         y = "COVID Mention Count") +
    theme_minimal() +
    theme(plot.title.position = "plot")
  
  ggsave(here("plots", "covid-mentions.png"), 
         p,
         width = 6.5,
         height = 8)
  
  here("plots", "covid-mentions.png")
}
