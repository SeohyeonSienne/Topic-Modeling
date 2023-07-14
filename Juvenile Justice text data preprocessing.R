raw_minute_chock <- petition_chock
raw_minute_sonyun <- petition_sonyun
raw_minute_minors <- petition_minors

library(stringr)
library(dplyr)

petition_chock_p <- petition_chock %>%
  str_replace_all("[^°¡-?R]", " ") %>%
  str_squish() %>%
  as_tibble()

write.csv(petition_chock_p,"petition_chock_p.csv")

word_space_minute_chock <- minute_chock %>%
  unnest_tokens(input = value,
                output = word,
                token = "words")

install.packages("multilinguer")
library(multilinguer)
install_jdk()
2

install.packages("remotes")
remotes::install_github("haven-jeon/KoNLP",
                        upgrade = "never",
                        INSTALL_opts = c("--no-multiarch"))
library(KoNLP)
useNIADic()


library(KoNLP)
library(dplyr)

write.csv(minute_chock,"minute_chock.csv")

extractNoun(minute_chock)
