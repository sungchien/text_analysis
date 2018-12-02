library(readr)
library(tidyverse)
library(jiebaR)
library(tidytext)

news <- data.frame()

# 讀入要處理的新聞資料
for (i in 15:24) {
  date <- sprintf("2018_11_%02d", i)
  
  news.df <- read_csv(file=paste0("udn_", date,".csv"))
  news <- rbind(news, news.df) #將讀入的新聞資料與原先的資料合併
}

# 設定斷詞系統
mp.seg <- worker(type="mp", symbol=TRUE)

# 進行斷詞
news <- news %>%
  filter(!is.na(text)) %>%  # 過濾空的新聞資料
  mutate(id=row_number()) %>%
  rowwise() %>%
  mutate(text.sg=paste0(segment(paste(title, text, sep="。"), mp.seg), collapse=" ")) %>%
  ungroup()

###################
# 統計輸入的文本內每一個可能的NGram出現的次數，計算候選詞語出現的次數與左右複雜度
NGramGenerator <- function(df, n) {
  # df為輸入之文本, n是NGram的N
  this_ngram <- df %>%
    select(id, text.sg) %>%
    unnest_tokens(ngram, text.sg, token="ngrams", n=n) %>% # 利用tidytext套件提供的功能從文本產生NGram
    count(ngram, sort=TRUE) %>% # 統計每一個NGram出現次數
    rename(c=n)
  
  return(this_ngram)
}

###########
# 根據頻率及字串的樣式(pattern)判斷是否為可能的候選詞語
CandidateSelector <- function(charstr, n) {
  # charstr為可能的候選詞(即NGram)，n為NGram的N
  count.th <- 30
  
  lstr <- "^\\p{L}+"  # 全部為數字或文字組成的字串，若不是這種字串便需要排除
  estr <- "^[a-zA-Z]+" # 全部由英文字母組成的字串，須排除
  for (i in 1:(n-1)) {
    lstr <- paste0(lstr, "\\s\\p{L}+")
    estr <- paste0(estr, "\\s[a-zA-Z]+")
  }
  lstr <- paste0(lstr, "$")
  estr <- paste0(estr, "$")
  
  stopwords <- c("的", "在", "是", "都", "了", "也", "很", "會", "有", "呢", "嗎", "就", "但", "所", "我", "不", "到", "要", "於")
  
  charstr <- charstr %>%
    filter(c>count.th) %>% # 取出總頻次大於count.th的Ngram
    filter(grepl(lstr, ngram, perl=TRUE)) %>% #保留內容為數字和文字的NGram(也就是去除包含符號的NGram)
    filter(!grepl(estr, ngram, perl=TRUE)) %>% # 排除全英文字母的Trigram
    filter(!(substr(ngram, 1, 1) %in% stopwords)) %>% # 去除第一字為停用字的NGram
    filter(!(substr(ngram, nchar(ngram), nchar(ngram)) %in% stopwords)) # 去除最後一字為停用字的NGram
  
  return(charstr)
}

########
# 取出NGram前面的(N-1)個相連詞
headString <- function (ngram, n) {
  sapply(ngram, function (x, n) {
    last_space = gregexpr(" ", x)[[1]][n-1]
    return(substr(x, 1, last_space-1))
  }, n)
}

# 取出NGram後面的(N-1)個相連詞
tailString <- function (ngram) {
  sapply(ngram, function (x) {
    first_space = gregexpr(" ", x)[[1]][1]
    return(substr(x, first_space+1, nchar(x)))
  })
}

# 依據左右複雜度判斷是否為可能的候選詞語，複雜度太小比較不可能是詞語
LRJointEstimator <- function(this_ngram, nextone, n) {
  # this_ngram目前處理的NGram，nextone為(N+1)Gram，n為NGram的N
  complex.th <- 1
  
  # 計算右接的複雜度
  ngram_at_head <- nextone %>%
    mutate(headstr=as.character(headString(ngram, n+1))) %>% # 找出所有(N+1)Gram的前N個相連詞
    inner_join(this_ngram, by=c("headstr"="ngram")) %>% # 比對(N+1)Gram的前N個相連詞與要判斷的NGram
    group_by(headstr) %>% # 利用熵(entropy)公式計算右接複雜度
    mutate(ratio=c.x/sum(c.x)) %>%
    mutate(ent=-log(ratio)*ratio) %>%
    summarise(rjoint=sum(ent))
  
  # 計算左接的複雜度
  ngram_at_tail <- nextone %>%
    mutate(tailstr=as.character(tailString(ngram))) %>% # 找出所有(N+1)Gram的後N個相連詞
    inner_join(this_ngram, by=c("tailstr"="ngram")) %>% # 比對(N+1)Gram的後N個相連詞與要判斷的NGram
    group_by(tailstr) %>% # 利用熵(entropy)公式計算左接複雜度
    mutate(ratio=c.x/sum(c.x)) %>%
    mutate(ent=-log(ratio)*ratio) %>%
    summarise(ljoint=sum(ent))
  
  this_ngram <- this_ngram %>%
    left_join(ngram_at_head, by=c("ngram"="headstr")) %>% # 加上右接複雜度
    left_join(ngram_at_tail, by=c("ngram"="tailstr")) %>% # 加上左接複雜度
    filter(ljoint>=complex.th) %>% # 過濾左右複雜度大小的字串
    filter(rjoint>=complex.th)
  
  return(this_ngram)
}
####

bigram <- NGramGenerator(news, 2) # 產生文本中所有Bigram及其出現次數

##
bigram <- CandidateSelector(bigram, 2) # 根據出現次數及字串的樣式(pattern)判斷Bigram是否為可能的候選詞語

trigram <- NGramGenerator(news, 3) # 產生文本中所有Trigram及其出現次數

bigram <- LRJointEstimator(bigram, trigram, 2) # 依據左右複雜度判斷Bigram是否為可能的候選詞語

##
trigram <- CandidateSelector(trigram, 3) # 根據出現次數及字串的樣式(pattern)判斷Trigram是否為可能的候選詞語

fourgram <- NGramGenerator(news, 4) # 產生文本中所有Fourgram及其出現次數

trigram <- LRJointEstimator(trigram, fourgram, 3) # 依據左右複雜度判斷Trigram是否為可能的候選詞語

##
fourgram <- CandidateSelector(fourgram, 4) # 根據出現次數及字串的樣式(pattern)判斷Fourgram是否為可能的候選詞語

fivegram <- NGramGenerator(news, 5) # 產生文本中所有Fivegram及其出現次數

fourgram <- LRJointEstimator(fourgram, fivegram, 4) # 依據左右複雜度判斷Fourgram是否為可能的候選詞語

#################
# 合併所有候選詞語篩選結果
dict <- data.frame() %>%
  bind_rows(bigram) %>%
  bind_rows(trigram) %>%
  bind_rows(fourgram) %>%
  select(ngram)

## 去除字串中的空白
dict$ngram <- gsub("\\s", "", dict$ngram, perl=TRUE)

## 寫入檔案
write.table(dict, file="udn_2018_09.dict", quote=FALSE,
            row.names=FALSE, col.names=FALSE, fileEncoding="UTF-8",
            append=TRUE)
