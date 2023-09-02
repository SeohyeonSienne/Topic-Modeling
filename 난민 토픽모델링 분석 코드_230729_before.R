#Tip for Korean error: tools>code>saving>euc-kr, file>save with encoding>euc-kr
#Re-spaced all the sentences using python w PyKoSpacing

install.packages("ggplot2")
install_jdk()

#library packages
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(reshape2)
library(multilinguer)
library(KoNLP)
library(textclean)
library(tidytext)
useNIADic()

setwd("C:/")

#Import data
raw_refugee_b <- Twitter_refugee_before %>%
  mutate(id = row_number ())

#Basic pre-processing; leave only Korean words
refugee_b <- raw_refugee_b %>%
  mutate(message = str_replace_all(message, "[^°¡-ÆR]", " "),
         message = str_squish(message))

#Defining specific words(ignore mergeUserdic is now deprecated)
data <- refugee_defined
defined <- data[[1]]
mergeUserDic(data.frame(defined, "ncn"))

#Extract noun
n_refugee_b <- refugee_b %>%
  unnest_tokens(input = message,
                output = word,
                token = extractNoun,
                drop = F) %>%
  filter(str_count(word) > 1) %>%
  group_by(id) %>%
  distinct(word) %>%
  ungroup() %>%
  select(id, word)

#Eliminate words w low frequency
#Mostly words w high frequency are eliminated but the researchers thought those words are valuable in this case
count_word_b <- n_refugee_b %>%
  add_count(word) %>%
  filter(n >= 5) %>%
  select(-n)

#Stop words
stopword <- refugee_stop
count_word_b <- count_word_b %>%
  anti_join(stopword, by = "word")

#Thesaurus(Divided words to prevent getting stopped. words here are defined after comparing the original text and the noun outcome)
count_word_b <- count_word_b %>%
  filter(!word %in% stopword) %>%
  mutate(word = recode(word,
                       "³­¼¾" = "³­¹Î¼¾ÅÍ",
                       "³²¼ºÀÌ" = "³²¼º",
                       "ÀÚÇÑ´ç" = "ÀÚÀ¯ÇÑ±¹´ç",
                       "´õ¹ÎÁÖ" = "´õºÒ¾î¹ÎÁÖ´ç",
                       "´ëÅë·ÉÀÇ" = "´ëÅë·É",
                       "µô·¹" = "µô·¹¸¶",
                       "¸»·¹ÀÌ" = "¸»·¹ÀÌ½Ã¾Æ",
                       "»ç¿ìµð" = "»ç¿ìµð¾Æ¶óºñ¾Æ",
                       "¾Ö½º´õ±âµµ¿îµ¿" = "¿¡½º´õ±âµµ¿îµ¿",
                       "¿¹¸Ç" = "¿¹¸à",
                       "¿Ã¸°" = "¿Ã¸²",
                       "¿Í¸£´Ù¿Í" = "¿Í¸£´Ù",
                       "¿ì¸°" = "¿ì¸®",
                       "ºí·ç¿À¼ÇÀÎ" = "ºí·ç¿À¼Ç",
                       "¿¹¸à¿¡¼­" = "¿¹¸à"))

count_word_b <- count_word_b %>%
  filter(!word %in% stopword) %>%
  mutate(word = recode(word, 
                       "¿ì¼±½Ã" = "¿ì¼±",
                       "¿øÄ¢Àû" = "¿øÄ¢",
                       "À¯¿£ÀÌ" = "À¯¿£",
                       "ÀÌÁ¨" = "ÀÌÁ¦",
                       "ÀÏº»µµ" = "ÀÏº»",
                       "ÀÏº»ÀÇ" = "ÀÏº»",
                       "ÀÏÇÒ" = "ÀÏ",
                       "ÀÏÇÏ" = "ÀÏ",
                       "Á¦ÁÖÆ¯º°ÀÚÄ¡µµ" = "Á¦ÁÖµµ",
                       "Á¦ÁÖÀÎ" = "Á¦ÁÖµµ¹Î",
                       "ÁÁ°Ú" = "ÁÁÀ½",
                       "ÁÁ°Ú½À´Ï" = "ÁÁÀ½",
                       "Áö±¸»ó" = "Áö±¸",
                       "µµÁö»ç" = "¿øÈñ·æ",
                       "ÃÖ¼ÒÇÑ" = "ÃÖ¼Ò",
                       "Åë·É" = "´ëÅë·É",
                       "ÆäºÏ" = "ÆäÀÌ½ººÏ",
                       "ÆäÁö" = "ÆóÁö",
                       "ÇÕ¹ýÀû" = "ÇÕ¹ý",
                       "È®Á¤½Ã" = "È®Á¤",
                       "±×¶©" = "±×¶§",
                       "±âµ¶" = "±âµ¶±³",
                       "±â»çµµ" = "±â»ç",
                       "°­Á¤" = "°­Á¤¸¶À»",
                       "±³±¸Àå" = "°­¿ìÀÏ",
                       "°Çµµ" = "»ç°Ç",
                       "°ÇÀÌ" = "»ç°Ç",
                       "°æ±â" = "°æ±âµµ",
                       "°­Á¶Çß" = "°­Á¶",
                       "°ü·ÃÇÏ" = "°ü·Ã",
                       "±¤È­¹®½Ã¹Î¿­¸°¸¶´ç" = "±¤È­¹®",
                       "±Çº¸" = "ÀÎ±Çº¸È£",
                       "±ÔÁ¤ÇÏ" = "±ÔÁ¤",
                       "±×°É" = "±×°Í",
                       "±×°É·Î" = "±×°Í",
                       "±×°Å" = "±×°Í",
                       "±âµ¶" = "±âµ¶±³"))

count_word_b <- count_word_b %>%
  filter(!word %in% stopword) %>%
  mutate(word = recode(word,
                       "³²¼ºÀÌ" = "³²¼º",
                       "´ëÅë·ÉÀÇ" = "´ëÅë·É",
                       "µô·¹" = "µô·¹¸¶",
                       "¸»·¹ÀÌ" = "¸»·¹ÀÌ½Ã¾Æ",
                       "¹Ì±¹Àº" = "¹Ì±¹",
                       "¹ßÀÇÇÏ¿´½À´Ï" = "¹ßÀÇ",
                       "¹ý¹«Àå°ü" = "¹ý¹«ºÎÀå°ü",
                       "ºÏ¿¡" = "ºÏÇÑ",
                       "ºñ°ø" = "ºñ°ø°³",
                       "¼³¸íÇÏ°Ú" = "¼³¸í",
                       "¼³¸íÇß" = "¼³¸í",
                       "¿¹¸Ç" = "¿¹¸à",
                       "¿ì¸°" = "¿ì¸®",
                       "¿ì¹°¿¡" = "¿ì¹°",
                       "¿ì¼±½Ã" = "¿ì¼±",
                       "ÀÌ»óÇÑ" = "ÀÌ»óÇÔ",
                       "ÀÌ½½" = "ÀÌ½½¶÷",
                       "½½¸²" = "¹«½½¸²",
                       "ÀÏº»ÀÇ" = "ÀÏº»",
                       "Àß¸øµÈ" = "Àß¸ø",
                       "¹®ÁËÀÎ" = "¹®ÀçÀÎ",
                       "µµ¹Î" = "Á¦ÁÖµµ¹Î",
                       "¹®´ëÅë·É"="¹®ÀçÀÎ",
                       "ÀÌ¸í¹Ú±ÙÇý" = "ÀÌ¸í¹Ú ¹Ú±ÙÇý",
                       "Á¦ÁÖµµ"="Á¦ÁÖ",
                       "´Ïµé" = "³ÊÈñµé",
                       "ÀÏºÎ°³Á¤¹ý·ü¾È" = "°³Á¤¾È",
                       "´ëÅë·É´Ô" = "´ëÅë·É",
                       "¿ø³ªÀÕÇÏÀÚ" = "¿ø³ªÀÕ"))
  
#Check words (you can change the number)
count_word_b %>%
  count(word, sort = T) %>%
  print(n = 500)

write.csv(count_word_b, "C:/refugee_before_above5.csv")

#Make LDA Model
#Word frequency for each document 
count_word_doc_b <- count_word_b %>%
  count(id, word, sort = T)

count_word_doc_b

library(tm)
dtm_refugee_b <- count_word_doc_b %>%
  cast_dtm(document = id, term = word, value = n)

dtm_refugee_b #doc: 2083, term:547

as.matrix(dtm_refugee_b[1:8, 1:8]) #check the content of DTM

library(topicmodels)
lda_model_b <- LDA(dtm_refugee_b,
                 k = 15,
                 method = "Gibbs",
                 control = list(seed = 1234))

lda_model_b

glimpse(lda_model_b) #check the content of LDA Model: beta for the possibility of words to appear in each topic, gamma for the possibility of docs to appear in each topic

#Extract beta
term_topic_b <- tidy(lda_model_b, matrix = "beta")
term_topic_b

#Check the words w the highest beta value in the topic
term_topic_b %>%
  filter(topic == 6) %>%
  arrange(-beta)

#Check the words w the highest beta value in every topic
terms(lda_model_b, 20) %>%
  data.frame()

#Extract gamma
doc_topic_b <- tidy(lda_model_b, matrix = "gamma")
doc_topic_b

doc_topic_b %>%
  count(topic)

#Divide each doc into the highest possible topic
doc_class_b <- doc_topic_b %>%
  group_by(document) %>%
  slice_max(gamma, n = 1)

doc_class_b

doc_class_b$document <- as.integer(doc_class_b$document)

refugee_topic_b <- raw_refugee_b %>%
  left_join(doc_class_b, by = c("id" = "document"))

refugee_topic_b %>%
  select(id, topic)

refugee_topic_b %>%
  count(topic)

refugee_topic_b <- refugee_topic_b %>%
  na.omit()

#Visualize the outcome - size of topics
top_terms <- term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = F) %>%
  summarise(term = paste(term, collapse = ", "))

top_terms

count_topic <- refugee_topic %>%
  count(topic)

count_topic

count_topic_word <- count_topic %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic_name = paste("Topic", topic))

count_topic_word

ggplot(count_topic_word,
       aes(x = reorder(topic_name, n),
           y = n,
           fill = topic_name)) +
  geom_col(show.legend = F) +
  coord_flip() +
  
  geom_text(aes(label = n),
            hjust = -0.2) +
  geom_text(aes(label = term),
            hjust = 1.03,
            col = "white",
            fontface = "bold",
            family = "nanumgothic") +
  
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1000)) +
  labs(x = NULL)

#Visualize - each topic and terms
top_term_topic <- term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n = 5)

top_term_topic

name_topic <- tibble(topic = 1:20,
                     name = c("1", "2", "3", "4", "5", 
                              "6", "7", "8", "9", "10", "11", "12", "13", "14",
                              "15", "16", "17", "18", "19", "20"))

top_term_topic_name <- top_term_topic %>%
  left_join(name_topic, name_topic, by = "topic")

top_term_topic_name

ggplot(top_term_topic_name,
       aes(x = reorder_within(term, beta, name),
           y = beta,
           fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ name, scales = "free", ncol = 4) +
  coord_flip() +
  scale_x_reordered() +
  
  labs(title = "2018³â Á¦ÁÖ ¿¹¸à³­¹Î °ü·Ã Æ®À­ ÅäÇÈ",
       subtitle = "ÅäÇÈº° ÁÖ¿ä ´Ü¾î TOP 10",
       x = NULL, y = NULL) +
  
  theme_minimal() +
  theme(title = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


#Download top 300 terms
top_term_b <-  term_topic_b %>%
  group_by(topic) %>%
  slice_max(beta, n = 300, with_ties = F) %>%
  summarise(term = paste(term, collapse = ", "))

top_term_b

write.csv(top_term_b, "C:/topic modeling_refugee_before_15.csv")


#Use hyperparameter tuning to find out optimal topic number
install.packages("ldatuning")
library(ldatuning)

models_b <- FindTopicsNumber(dtm = dtm_refugee_b,
                           topics = 2:20,
                           return_models = T,
                           control = list(seed = 1234))

models_b %>%
  select(topics, Griffiths2004) #shows perplextity

FindTopicsNumber_plot(models_b)
