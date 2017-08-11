library(rwebppl)
library(tidyverse)
library(ggjoy)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


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

empiricalTrials<- read.csv("empiricalTrials.csv")

# trialsAsList = array(dim=length(unique((empiricalTrials$ldf))))
# count=0
# for (i in 1:max(empiricalTrials$ldf)+1) {
#   trials = (empiricalTrials %>% filter(ldf_num==i) %>% select(realLabel))
#   if(length(trials[[1]])==0) {
#    print('skiiiiiiip meeeeeeeee') 
#   }
#   if(length(trials[[1]]) != 0) {
#     trialsAsList[count] = trials
#     count= count+1
#   }
#   if(count>198) {print(trials)}
# }
# 
# outcomes<- NULL
# for(i in 1:2){
#   meIndex = (21 * (i-1))+1
#   tmp=empiricalTrials[meIndex:(meIndex+20),]
#   outcomes <- rbind(outcomes, cbind(i, webppl(program_file = "speaker.wppl", data=tmp, data_var="empiricalTrials")))
# }
    
empiricalVocabs<- read.csv("empiricalVocabs.csv") %>% 
  select(-X, -targetObjectName) %>%
  rename(exposures = exposureRate,
         label = realLabel,
         known = performance)

# outcomes<- data_frame(run = replicate(5,NULL))
ptm <- proc.time()
outcomes<- NULL
for(i in 1:1){
  meIndex = (9 * (i-1))+1
  tmp=empiricalVocabs[meIndex:(meIndex+8),]
  outcomes <- bind_rows(outcomes, cbind(ldf_num=unique(tmp$ldf_num), webppl(program_file = "speaker2.wppl", data=tmp, data_var="empiricalVocabs")))
}
proc.time() - ptm


# learnProbs_500 <- outcomes
learnProbs_500 

# learnProbs_tmp <- outcomes
learnProbs_tmp
ggplot(learnProbs_tmp, aes(x = value, y = as.factor(ldf_num))) + geom_joy()

hypothetical_vocabs_fep <- outcomes


outcomes <- webppl(program_file = "speaker.wppl")

ptm <- proc.time()
outcomes<-webppl(program_file = "speaker.wppl", data=empiricalVocabs, data_var="empiricalVocabs")
proc.time() - ptm

outcomes %>%
  select(((ncol(.)/2)+1):ncol(.)) %>%
  t() %>%
  as_tibble() %>%
  rename(point = V1, speak = V2) %>%
  rowid_to_column(var = "sample") 


outcomes %>% group_by(ldf_num) %>% summarize(mean(value))
#vs
empiricalVocabs %>% group_by(ldf_num) %>% summarize(known=mean(known)) %>% filter(known<.3)





joesTrials <- empiricalTrials %>%
  filter(ldf_num==1)
joesVocab <- empiricalVocabs %>%
  filter(ldf_num==1)

outcomes %>% 
  group_by(value) %>%
  summarise(n = n()) %>%
  mutate(n = n/sum(n))

recall <- function(exposure) inv.logit(-.4252 + 1.1487*log2(exposure))


recall2 <-(1-recall(1))*recall(1) + recall(1)

(1-(recall2))*recall(1)+recall2
