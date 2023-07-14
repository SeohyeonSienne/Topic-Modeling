#R 3.6.3 버전 설치

# [KoNLP 다운 전 필수 다운로드 파일]에서 Javasetup~ 파일과 Rtools35 파일 실행

#KoNLP 다운로드
install.packages("remotes")
remotes::install_github("mrchypark/multilinguer")
library(multilinguer)
multilinguer::install_jdk()
2
install.packages(c("hash", "tau", "Sejong", "RSQLite", "devtools", "bit", "rex", "lazyeval", "htmlwidgets", "crosstalk", "promises", "later", "sessioninfo", "xopen", "bit64", "blob", "DBI", "memoise", "plogr", "covr", "DT", "rcmdcheck", "rversions"), type = "binary")

remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))

library(KoNLP)
useNIADic()
extractNoun('이 영화 정말 재미있다')
