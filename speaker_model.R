library(rwebppl)
library(tidyverse)

outcomes <- webppl(program_file = "speaker.wppl") %>%
  as_data_frame

outcomes %>% 
  group_by(value) %>%
  summarise(n = n()) %>%
  mutate(n = n/sum(n))
