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
raw_refugee_a <- Twitter_refugee_after %>%
  mutate(id = row_number ())

#Basic pre-processing; leave only Korean words
refugee_a <- raw_refugee_a %>%
  mutate(message = str_replace_all(message, "[^가-R]", " "),
         message = str_squish(message))

#Defining specific words(ignore mergeUserdic is now deprecated)
data <- refugee_defined
defined <- data[[1]]
mergeUserDic(data.frame(defined, "ncn"))

#Extract noun
n_refugee_a <- refugee_a %>%
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
count_word_a <- n_refugee_a %>%
  add_count(word) %>%
  filter(n >= 5) %>%
  select(-n)

#Stop words
stopword <- refugee_stop
count_word_a <- count_word_a %>%
  anti_join(stopword, by = "word")

#Thesaurus(Divided words to prevent getting stopped. words here are defined after comparing the original text and the noun outcome)
count_word_a <- count_word_a %>%
  filter(!word %in% stopword) %>%
  mutate(word = recode(word,
                       "난센" = "난민센터",
                       "남성이" = "남성",
                       "자한당" = "자유한국당",
                       "더민주" = "더불어민주당",
                       "대통령의" = "대통령",
                       "딜레" = "딜레마",
                       "말레이" = "말레이시아",
                       "사우디" = "사우디아라비아",
                       "애스더기도운동" = "에스더기도운동",
                       "예맨" = "예멘",
                       "올린" = "올림",
                       "와르다와" = "와르다",
                       "우린" = "우리",
                       "블루오션인" = "블루오션",
                       "예멘에서" = "예멘"))

count_word_a <- count_word_a %>%
  filter(!word %in% stopword) %>%
  mutate(word = recode(word, 
                       "우선시" = "우선",
                       "원칙적" = "원칙",
                       "유엔이" = "유엔",
                       "이젠" = "이제",
                       "일본도" = "일본",
                       "일본의" = "일본",
                       "일할" = "일",
                       "일하" = "일",
                       "제주특별자치도" = "제주도",
                       "제주인" = "제주도민",
                       "좋겠" = "좋음",
                       "좋겠습니" = "좋음",
                       "지구상" = "지구",
                       "도지사" = "원희룡",
                       "최소한" = "최소",
                       "통령" = "대통령",
                       "페북" = "페이스북",
                       "페지" = "폐지",
                       "합법적" = "합법",
                       "확정시" = "확정",
                       "그땐" = "그때",
                       "기독" = "기독교",
                       "기사도" = "기사",
                       "강정" = "강정마을",
                       "교구장" = "강우일",
                       "건도" = "사건",
                       "건이" = "사건",
                       "경기" = "경기도",
                       "강조했" = "강조",
                       "관련하" = "관련",
                       "광화문시민열린마당" = "광화문",
                       "권보" = "인권보호",
                       "규정하" = "규정",
                       "그걸" = "그것",
                       "그걸로" = "그것",
                       "그거" = "그것",
                       "기독" = "기독교"))

count_word_a <- count_word_a %>%
  filter(!word %in% stopword) %>%
  mutate(word = recode(word,
                       "남성이" = "남성",
                       "대통령의" = "대통령",
                       "딜레" = "딜레마",
                       "말레이" = "말레이시아",
                       "미국은" = "미국",
                       "발의하였습니" = "발의",
                       "법무장관" = "법무부장관",
                       "북에" = "북한",
                       "비공" = "비공개",
                       "설명하겠" = "설명",
                       "설명했" = "설명",
                       "예맨" = "예멘",
                       "우린" = "우리",
                       "우물에" = "우물",
                       "우선시" = "우선",
                       "이상한" = "이상함",
                       "이슬" = "이슬람",
                       "슬림" = "무슬림",
                       "일본의" = "일본",
                       "잘못된" = "잘못",
                       "문죄인" = "문재인",
                       "도민" = "제주도민",
                       "문대통령"="문재인",
                       "이명박근혜" = "이명박 박근혜",
                       "제주도"="제주",
                       "니들" = "너희들",
                       "일부개정법률안" = "개정안",
                       "대통령님" = "대통령",
                       "원나잇하자" = "원나잇"))
  
#Check words (you can change the number)
count_word_a %>%
  count(word, sort = T) %>%
  print(n = 500)

write.csv(count_word_a, "C:/refugee_after_above5.csv")

#Make LDA Model
#Word frequency for each document 
count_word_doc_a <- count_word_a %>%
  count(id, word, sort = T)

count_word_doc_a

library(tm)
dtm_refugee_a <- count_word_doc_a %>%
  cast_dtm(document = id, term = word, value = n)

dtm_refugee_a #doc: 7358, term:2122

as.matrix(dtm_refugee_a[1:8, 1:8]) #check the content of DTM

library(topicmodels)
lda_model_a <- LDA(dtm_refugee_a,
                 k = 13,
                 method = "Gibbs",
                 control = list(seed = 1234))

lda_model_a

glimpse(lda_model_a) #check the content of LDA Model: beta for the possibility of words to appear in each topic, gamma for the possibility of docs to appear in each topic

#Extract beta
term_topic_a <- tidy(lda_model_a, matrix = "beta")
term_topic_a

#Check the words w the highest beta value in the topic
term_topic_a %>%
  filter(topic == 6) %>%
  arrange(-beta)

#Check the words w the highest beta value in every topic
terms(lda_model_a, 20) %>%
  data.frame()

#Extract gamma
doc_topic_a <- tidy(lda_model_a, matrix = "gamma")
doc_topic_a

doc_topic_a %>%
  count(topic)

#Divide each doc into the highest possible topic
doc_class_a <- doc_topic_a %>%
  group_by(document) %>%
  slice_max(gamma, n = 1)

doc_class_a

doc_class_a$document <- as.integer(doc_class_a$document)

refugee_topic_a <- raw_refugee_a %>%
  left_join(doc_class_a, by = c("id" = "document"))

refugee_topic_a %>%
  select(id, topic)

refugee_topic_a %>%
  count(topic)

refugee_topic_a <- refugee_topic_a %>%
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
  
  labs(title = "2018년 제주 예멘난민 관련 트윗 토픽",
       subtitle = "토픽별 주요 단어 TOP 10",
       x = NULL, y = NULL) +
  
  theme_minimal() +
  theme(title = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


#Download top 300 terms
top_term_a <-  term_topic_a %>%
  group_by(topic) %>%
  slice_max(beta, n = 300, with_ties = F) %>%
  summarise(term = paste(term, collapse = ", "))

top_term_a

write.csv(top_term_a, "C:/topic modeling_refugee_after_13.csv")


#Use hyperparameter tuning to find out optimal topic number
install.packages("ldatuning")
library(ldatuning)

models <- FindTopicsNumber(dtm = dtm_refugee_a,
                           topics = 2:20,
                           return_models = T,
                           control = list(seed = 1234))

models %>%
  select(topics, Griffiths2004) #shows perplextity

FindTopicsNumber_plot(models)
