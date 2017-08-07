library(rwebppl)
library(tidyverse)

outcomes <- webppl(program_file = "speaker.wppl") %>%
  as_data_frame

outcomes %>% 
  group_by(value) %>%
  summarise(n = n()) %>%
  mutate(n = n/sum(n))

recall <- function(exposure) inv.logit(-.4252 + 1.1487*log2(exposure))


recall2 <-(1-recall(1))*recall(1) + recall(1)

(1-(recall2))*recall(1)+recall2
