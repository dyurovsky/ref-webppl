library(rwebppl)
library(tidyverse)
library(ggjoy)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

empiricalVocabsConds<- read.csv("empiricalVocabsFull.csv") %>% 
  select(-X) 

# this line runs webppl model
# outcomes_perfect_learner<-webppl(program_file = "speaker.wppl", data=empiricalVocabsConds, data_var = "empiricalVocabs")

# these 
# gamePredictions_perfect_learner <- outcomes_perfect_learner %>%
#   unnest(predictions, gameTrials) %>%
#   cbind(appearance= rep.int(c(1,2,3), nrow(.)/3))
#  
# full_empirical_points_init_perfect <- read.csv('empirical_data/5.12_data_anon.csv') %>% select(-X)
# 
# all_data_init_perfect <- full_empirical_points_init_perfect %>%
#   filter(ldf_num %in% gamePredictions_perfect_learner$me) %>%
#   select(-partnersExposure) %>%
#   filter(toBeDropped != 1) %>%
#   group_by(ldf_num, exposureRate, targetObjectName) %>%
#   mutate(appearance = if_else(trialnum == min(trialnum), 1,
#                               if_else(trialnum == max(trialnum), 3, 2))) %>%
#   left_join(gamePredictions_perfect_learner, by=c('ldf_num'='me', 'realLabel'='gameTrials', 'appearance')) %>%
#   rename(predictions_perfect_learner=predictions)


# hm <- cbind(all_data_coinflip, all_data_perfect_learner$predictions_perfect_learner)


# tmp <- read.csv("5.12_with_theoretical.csv") %>% select(-X)
tmp <- all_data_points
# all_data_coinflip <- all_data_points
# outcomes_coinflip<-outcomes

seprop <- function(props) {
  mean_prop = mean(props)
  sqrt( (mean_prop*(1 - mean_prop)) / length(props))
}

##getting empirical data for plot 1
quartz()
prop_methods <- all_data_init_perfect %>%
  filter(toBeDropped != 1) %>%
  filter(partnersExposure == 0) %>%
  # group_by(condition, ldf_num, method) %>%
  mutate(predictions_perfect_learner=ifelse(predictions_perfect_learner=='point', "click", 
                            ifelse(predictions_perfect_learner=="speak","label", "label_click")))  %>%
  ungroup() %>%
  select(condition,ldf_num,appearance,method,predictions_perfect_learner) %>%
  gather(isEmpirical, method, -condition, -ldf_num, -appearance) %>%
  group_by(condition, ldf_num, appearance,isEmpirical, method) %>%
  summarise(n = n()) %>%
  group_by(condition, ldf_num, isEmpirical, appearance) %>%
  mutate(n = n/sum(n)) %>%
  ungroup() %>%
  mutate(method = as.factor(method)) %>%
  complete(nesting(condition, ldf_num), isEmpirical, appearance, method, fill = list(n = 0)) %>%
  group_by(condition, method, isEmpirical, appearance) %>%
  # group_by(condition, method, appearance) %>%
  summarise(mean = mean(n), se = seprop(n), n = n())
  # ungroup() %>%
  # mutate(method = factor(method, levels = c("click", "label", "label_click")))



#plot of method choice as a function of appearance, split across point conditions
prop_methods %>% filter(isEmpirical=="method") %>%
  ggplot(aes(x=appearance, y=mean)) +
  geom_line(aes(x=appearance, y=mean, group=method, color=method), position=position_dodge(.25)) +
  geom_pointrange(aes(ymax = mean + se, 
                     ymin = mean - se, color=method), position=position_dodge(.25)) +
  # facet_grid(partnersExposure ~ condition) +
  labs(y="Proportion of Trials", x="Point Scheme Condition") +
  coord_cartesian(ylim=c(0,1)) +
  # geom_ribbon(data=theo_methods_perfect, aes(x=condition, y=mean, group=method), position=position_dodge(.5), linetype='dashed') +
  geom_ribbon(data=(prop_methods %>% filter(isEmpirical=="predictions_perfect_learner")),
              aes(x=as.numeric(appearance),ymax = mean + se, 
                      ymin = mean - se, fill=method, alpha=.4), position=position_dodge(.25)) +
  facet_wrap(~condition)



#direct plot of model fit
#
#
#



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


#getting theoretical data for plot 1
# theo_methods_perfect <- tmp %>%
#   filter(toBeDropped != 1) %>%
#   filter(partnersExposure == 0) %>%
#   filter(appearance == 1) %>%
#     group_by(condition, ldf_num, method) %>%
#   summarise(mean_prob_speech = mean(speak),
#             mean_prob_point = mean(point),
#             mean_prob_teach = mean(teach)) %>%
#   group_by(condition, ldf_num) %>%
#   # mutate(n = n/sum(n)) %>%
#   group_by(condition) %>%
#   # group_by(condition, method, appearance) %>%
#   summarise(mean_speech = mean(mean_prob_speech),
#             se_speech = seprop(mean_prob_speech),
#             mean_point = mean(mean_prob_point),
#             se_point = seprop(mean_prob_point),
#             mean_teach = mean(mean_prob_teach),
#             se_teach = seprop(mean_prob_teach)) %>%
#   ungroup() %>%
#   gather(method, mean, -condition, -se_point, -se_speech, -se_teach) %>%
#   mutate(se=ifelse(grepl('speech', method), se_speech, 
#                    ifelse(grepl('point', method), se_point, se_teach)))


