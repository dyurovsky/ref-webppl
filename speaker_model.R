library(rwebppl)
library(tidyverse)
library(ggjoy)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

empiricalVocabsConds<- read.csv("empiricalVocabsFull.csv") %>% 
  select(-X) 

t1<- proc.time()
outcomes<-webppl(program_file = "speaker.wppl", data=empiricalVocabsConds, data_var = "empiricalVocabs")
proc.time-t1


# gamePredictions <- outcomes %>%
#   unnest(predictions, gameTrials) %>%
#   cbind(appearance= rep.int(c(1,2,3), nrow(.)/3))
# 
# full_empirical_points <- read.csv('empirical_data/5.12_data_anon.csv') %>% select(-X)
# 
# all_data_points <- full_empirical_points %>%
#   filter(ldf_num %in% gamePredictions$me) %>%
#   select(-partnersExposure) %>%
#   filter(toBeDropped != 1) %>%
#   group_by(ldf_num, exposureRate, targetObjectName) %>%
#   mutate(appearance = if_else(trialnum == min(trialnum), 1,
#                               if_else(trialnum == max(trialnum), 3, 2))) %>%
#   left_join(gamePredictions, by=c('ldf_num'='me', 'realLabel'='gameTrials', 'appearance'))


tmp <- read.csv("5.12_with_theoretical.csv") %>% select(-X)







# smooth_prob = function(x){(0+x*65)/((0+x*65) + (30+x*65))}
# sapply(0:10, smooth_prob, simplify = TRUE)





## LearnPs analysis
accs <- empiricalVocabs %>%
  group_by(ldf_num) %>%
  summarise(known = mean(known)) %>%
  select(known)


empirical_learn <-map(1:nrow(outcomes), function(row) as_tibble(outcomes[row,])) %>%
  bind_rows() %>%
  mutate(person = 1:nrow(.)) %>%
  unnest() 

empirical_mean <- empirical_learn %>%
  group_by(person) %>%
  summarise(p = weighted.mean(learnPs.support, learnPs.probs)) %>%
  arrange(p)
  
ordered_learn <- empirical_learn %>%
  mutate(person = factor(person, levels = empirical_mean$person))




ggplot(ordered_learn, aes(x = learnPs.support, y = as.factor(person), 
                              weight = learnPs.probs)) + 
  geom_joy() + 
  theme_bw()


data_frame(value = rnorm(1000, 0, 40)) %>%
  ggplot(aes(x = value)) + 
  geom_histogram()


quartz()
hist(outcomes)






# ------------------------------------------------------------------
tmp <- outcomes %>%
  select(((ncol(.)/2)+1):ncol(.)) %>%
  t() %>%
  as_tibble() %>%
  rename(point = V1, speak = V2) %>%
  rowid_to_column(var = "sample") %>% head()


outcomes %>% group_by(ldf_num) %>% summarize(mean(value))
#vs
empiricalVocabs %>% group_by(ldf_num) %>% summarize(known=mean(known)) %>% filter(known<.3)





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

test <- webppl(program_file = "binomial_test.webppl")


outcomes <- webppl(program_file = "speaker.wppl")

outcome_vals <- outcomes$value %>%
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


