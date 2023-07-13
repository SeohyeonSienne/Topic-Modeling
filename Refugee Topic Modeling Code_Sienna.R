
library(dplyr)
library(readr)
library(stringr)
install.packages("ggplot2")
library(ggplot2)
library(reshape2)
library(multilinguer)
install_jdk()

library(KoNLP)
useNIADic()

#Set path
setwd("C:/")

#Import data
raw_refugee <- read_csv("Twitter_working_final.csv") %>%
  mutate(id = row_number ())

#Tag Packages
library(stringr)
library(textclean)

#Preprocess text data
refugee <- raw_refugee %>%
  mutate(message = str_replace_all(message, "[가??-?힣]", " "),
         message = str_squish(message)) 

library(tidytext)

#extract noun
n_refugee <- refugee %>%
  unnest_tokens(input = message,
                output = word,
                token = extractNoun,
                drop = F) %>%
  filter(str_count(word) > 1) %>%
  group_by(id) %>%
  distinct(word) %>%
  ungroup() %>%
  select(id, word)
  
#remove words with low frequency
count_word <- n_refugee %>%
  add_count(word) %>%
  filter(n >= 5) %>%
  select(-n)

count_word %>%
  count(word, sort = T) %>%
  print(n = 5)

write.csv(count_word, "C:/refugee_wordabove5.csv")

#Add Dictionary
buildDictionary(user_dic = refugee_defined, replace_usr_dic = T)

#Thesaurus
count_word <- count_word %>%
  mutate(word = recode(word, " " = ""))at%
  count(id, word, sort = T)

count_word_doc

library(tm)
dtm_refugee <- count_word_doc %>%
  cast_dtm(document = id, term = word, value = n)

as.matrix(dtm_refugee[1:8, 1:8])

library(topicmodels)
lda_model <- LDA(dtm_refugee,
                 k = 20,
                 method = "Gibbs",
                 control = list(seed = 1234))

lda_model

glimpse(lda_model)

#Main Words of Each Topic
term_topic <- tidy(lda_model, matrix = "beta")
term_topic

term_topic %>%
  filter(topic == 6) %>%
  arrange(-beta)

terms(lda_model, 20) %>%
  data.frame()

#Optimal Topic Number
install.packages("ldatuning")
library(ldatuning)

models <- FindTopicsNumber(dtm = dtm_refugee,
                           topics = 2:25,
                           return_models = T,
                           control = list(seed = 1234))

models %>%
  select(topics, Griffiths2004)

FindTopicsNumber_plot(models)

#Main Word List of Each Topic
top_term <-  term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n = 300, with_ties = F) %>%
  summarise(term = paste(term, collapse = ", "))

top_term

write.csv(top_term, "C:/221116_topic modeling_refugee.csv")
