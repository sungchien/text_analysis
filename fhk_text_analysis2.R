library(tidyverse)
library(readr)
library(magrittr)
library(jiebaR)
library(tm)
library(slam)
library(stm)
library(xlsx)
library(readxl)

news.df <- read_csv(file="parade_text.csv")

# 設定jieba斷詞器
mp.seg <- worker(type="mp", user="./parade.dict", bylines=TRUE)

# 製作過濾去不包含中文字或只有一個中文字之候選詞語的函數
filterChineseTerms <- function (str_text) {
  str_text <- unlist(str_text)
  str_text <- str_text[grepl("\\p{Han}+", str_text, perl=TRUE)]
  str_text <- str_text[nchar(str_text)>1]
  paste(str_text, collapse=" ")
}

# 將新聞內容斷詞
news.df <- news.df %>%
  filter(!is.na(text)) %>%                # 保留有內容的新聞資料
  mutate(id=row_number()) %>%             # 每則新聞加上編號(id)
  rowwise() %>%                           # 逐行運算
  mutate(words=segment(text, mp.seg)) %>% # 斷詞
  mutate(words=filterChineseTerms(words)) %>% # 過濾去不包含中文字的候選詞語
  ungroup() %>%
  select(id, Source, date, title, words)  # 選擇新聞編號和斷詞結果

#建立語料庫
vc = VCorpus(VectorSource(news.df$words))

#將形式為character string的句子依據詞語之間的空白轉為vector，vector上的單位為詞語
strsplit_space_tokenizer <- function(x)
  unlist(strsplit(as.character(x), "[[:space:]]+"))

#建立文件-詞語矩陣，每個元素為每一種詞語出現在一筆文件上的次數
dtm = DocumentTermMatrix(vc,
                         control=list(tokenize=strsplit_space_tokenizer, wordLengths=c(1, Inf)))

#統計詞語出現的總頻次
term.count = col_sums(dtm)
#統計詞語出現的文件數
term.df = tapply(dtm$v, dtm$j, length)

#統計出現總頻次大於20
cr1 <- term.count > 5
length(which(cr1))
#統計出現系所數不大於1/4系所總數
cr2 <- term.df < 100
length(which(cr2))

stopwords <- c("的", "在", "是", "都", "了", "也", "很", "會", "有", "呢", "嗎", "就", "但", "所", "我", "不", "到", "要", "於")
cr3 <- !(dtm$dimnames$Terms %in% stopwords)
length(which(cr3))

#統計出現總頻次大於20且出現系所數不大於1/4系所總數的詞語數
length(which(cr1 & cr2 & cr3))

#刪除詞語，保留文件-詞語矩陣上出現總頻次大於20且出現系所數不大於1/4系所總數的詞語
dtm1 <- dtm[, cr1&cr2&cr3]

#統計每一筆文件上出現的詞語數
doc.termno = row_sums(dtm1)
#是否有沒有詞語的文件
which(doc.termno<=10)
dtm1 <- dtm1[doc.termno>10, ]

out <- list()
out$documents <- lapply(1:dtm1$nrow, function (x) matrix(as.integer(c(dtm1$j[dtm1$i==x], dtm1$v[dtm1$i==x])), nrow=2, byrow=TRUE))
out$vocab <- dtm1$dimnames$Terms
news.df1 <- news.df[dtm1$dimnames$Docs, ]
out$meta <- news.df1

stora <- searchK(documents=out$documents, vocab=out$vocab,
                 K=seq(2, 30, 2), prevalence=~Source,
                 data=out$meta, init.type = "LDA")

plot.searchK(stora)
