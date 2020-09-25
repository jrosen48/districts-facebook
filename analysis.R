library(tidyverse)

d <- list.files("data", full.names = T) %>% 
  purrr::map_df(read_csv)

d %>% 
  summarize_if(is.numeric, funs(sum)) %>% View()
