#Naver blog 텍스트 전처리 방법2

#http://bigdata.dongguk.ac.kr/lectures/bigdata/_book
/%ED%85%8D%EC%8A%A4%ED%8A%B8%EB%A7%88%EC%9D%B4%EB%8B%9D%EA%B8%B0%EC%B4%88.html
#%EB%8D%B0%EC%9D%B4%ED%84%B0-%EB%B6%84%EC%84%9D-1---%EC%A7%84%EB%A1%9C-%EA%B3%84
%ED%9A%8D%EC%84%9C  참고

contents <- nblog2012$contents
contents <- stringr::str_replace_all(contents, "[[:punct:]]", " ")
contents

contents <- stringr::str_replace_all(contents, "[[:digit:]]", " ")
contents

contents <- stringr::str_replace_all(contents, "\\s+", " ")
contents <- stringr::str_trim(contents) # 앞뒤 공백제거

library(KoNLP)
#library(NIADic) # not used
useNIADic()
out1 <- lapply(contents, extractNoun)
out1[1:2]

#out2 : list type ==> vector
out_v <- do.call(c, out1)
wt <- table(out_v)
wt <- wt[nchar(names(wt)) > 1]
sort(wt, decreasing = T)[1:30]

getwd()
path_cwd <- getwd() 
path_cwd

dir('nblog10y')[1:10]


library(tm)
plans <- VCorpus(DirSource('nblog10y', pattern="txt"))
plans

str(plans[[1]])

content(plans[[1]]) # lapply(plans, content)

meta(plans[[1]]) 

for(i in seq_along(plans)){
  plans[[i]]$content <- paste(plans[[i]]$content, collapse=" ")
}
plans[[1]]$content

plans = tm_map(plans, removeNumbers)

plans = tm_map(plans, stripWhitespace)



