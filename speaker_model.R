library(rwebppl)
library(tidyverse)




learn <- function(state, prob, count) {
  
  if(count == 0)
    state
  else {
      
    if(state)
      new_state = state
    else
      new_state = as.logical(rbinom(1,1,prob))
    
    learn(new_state, prob, count -1)
  
  }
}

replicate(1000, learn(FALSE, .45, 4)) %>% mean

outcomes <- webppl(program_file = "speaker.wppl")

outcome_vals <- unique(outcomes$value) %>%
  bind_rows(.id = "sample") %>%
  group_by(exposures, label) %>%
  summarise(known = mean(known))

outcome_vals

  spread(label, known) %>%
  select(blicket, wug, dax)

  unite(stringform, blicket:wug) %>%
  distinct(stringform)

outcomes <- webppl(program_file = "agent.wppl")

outcomes <- webppl(program_file = "speaker.wppl") %>%
  select(((ncol(.)/2)+1):ncol(.)) %>%
  t() %>%
  as_tibble() %>%
  rename(point = V1, speak = V2) %>%
  rowid_to_column(var = "sample") 

hist(outcomes$speak)


%>%
  as_data_frame

outcomes %>% 
  group_by(value) %>%
  summarise(n = n()) %>%
  mutate(n = n/sum(n))

recall <- function(exposure) inv.logit(-.4252 + 1.1487*log2(exposure))


recall2 <-(1-recall(1))*recall(1) + recall(1)

(1-(recall2))*recall(1)+recall2
