setwd("c:\\rawdata")

library(stringr)
library(KoNLP)
library(dplyr)
library(tidytext)

#텍스트 전처리 및 토큰화(단어)

raw_p_chock <- readLines("petition_chock.txt")
head(raw_p_chock)

p_chock <- raw_p_chock %>%
  str_replace_all("[^가-?R]", " ") %>%
  str_squish() %>%
  as_tibble()

p_chock_noun <- p_chock %>%
  unnest_tokens(input = value,
                output = word,
                token = "words")


raw_p_minors <- readLines("petition_minors.txt")
head(raw_p_minors)

p_minors <- raw_p_minors %>%
  str_replace_all("[^가-?R]", " ") %>%
  str_squish() %>%
  as_tibble()

p_minors_noun <- p_minors %>%
  unnest_tokens(input = value,
                output = word,
                token = "words")

raw_p_so <- readLines("petition_so.txt")
head(raw_p_so)

p_so <- raw_p_so %>%
  str_replace_all("[^가-?R]", " ") %>%
  str_squish() %>%
  as_tibble()

p_so_noun <- p_so %>%
  unnest_tokens(input = value,
                output = word,
                token = "words")


#형태소 분석기 통해 명사 추출하기

library(multilinguer)
install_jdk()
install.packages(c("stringr", "hash", "tau", "Sejong", "RSQLite",
                   "devtools"), type = "binary")
library(KoNLP)
useNIADic()

library(tidytext)

p_chock_n <- p_chock %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)

p_chock_n

p_minors_n <- p_minors %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)

p_minors_n

p_so_n <- p_so %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)

p_so_n

# 명사빈도분석하기

p_chock_n <- p_chock_n %>%
  count(word, sort = T) %>%
  filter(str_count(word) > 1)

p_chock_n

p_minors_n <- p_minors_n %>%
  count(word, sort = T) %>%
  filter(str_count(word) > 1)

p_minors_n

p_so_n <- p_so_n %>%
  count(word, sort = T) %>%
  filter(str_count(word) > 1)

p_so_n

#word2vec / R 버전 바꿔야함
install.packages(wordVectors)

install.packages("devtools")
library(devtools)

install.packages("wordVectors")

library(wordVectors)


library(tm)


dir("c:\\rawdata")[1:2]
plans <- VCorpus(DirSource("c:\\rawdata", patter="txt"))
plans
str(plans[[2]])

content(plans[[2]])
meta(plans[[2]])
meta(plans[[2]], tag="etc") <- "추가 정보입니다"
meta(plans[[2]])

for(i in seq_along(plans)){
  plans[[i]]$content <- paste(plans[[i]]$content, collapse = " ")
}

plans[[2]]$content
plans <- tm_map(plans, removePunctuation)
plans = tm_map(plans, removeNumbers)
plans = tm_map(plans, stripWhitespace)
plans = tm_map(plans, removeWords, stopwords('english'))

library(SnowballC)

plans <- tm_map(plans, stemDocument)
plans[[1]]$content

library(NIADic)
useNIADic()

for(i in seq_along(plans)){
  plans[[i]]$content <- gsub
}


for(i in seq_along(plans)){
  nouns <- extractNoun(plans[[i]]$content)
  nouns <- nouns[nchar(nouns) > 20]
  plans[[i]]$content <- paste(nouns, collapse = " ")
}



