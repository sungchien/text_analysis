library(readxl)
library(tidyverse)
library(lubridate)
library(jiebaR)
#文字處理套件
library(tm)
#疏鬆矩陣運算套件
library(slam)
#主題模型套件
library(topicmodels)
library(xlsx)
library(tidytext)

news.df <- data.frame()
for (sh in excel_sheets("歷屆總統講稿分析.xlsx")) {
  news <- read_excel(path="歷屆總統講稿分析.xlsx", sheet=sh)
  news$Speech <- sh
  news.df = rbind(news.df, news)
}

news.df <- news.df %>%
  filter(!is.na(Content)) %>%
  unnest(Content=strsplit(Content, "\\r\\n")) %>%
  mutate(Content=trimws(Content)) %>%
  filter(nchar(Content)>0) %>% # 過濾空的新聞資料
  mutate(id=row_number())
  
# 設定斷詞系統
mp.seg <- worker(type="mp", symbol=TRUE)

# 進行斷詞
news <- news.df %>%
  rowwise() %>%
  mutate(text.sg=paste0(segment(Content, mp.seg), collapse=" ")) %>%
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
  count.th <- 5
  
  lstr <- "^\\p{L}+"  # 全部為數字或文字組成的字串，若不是這種字串便需要排除
  estr <- "^[a-zA-Z]+" # 全部由英文字母組成的字串，須排除
  for (i in 1:(n-1)) {
    lstr <- paste0(lstr, "\\s\\p{L}+")
    estr <- paste0(estr, "\\s[a-zA-Z]+")
  }
  lstr <- paste0(lstr, "$")
  estr <- paste0(estr, "$")
  
  stopwords <- c("的", "在", "是", "都", "了", "也", "很",
                 "會", "有", "呢", "嗎", "就", "但", "所",
                 "我", "不", "到", "要", "於", "讓", "與",
                 "為", "上", "每", "來", "再", "之", "更",
                 "第", "這", "並", "從", "項", "對", "及",
                 "中", "億", "以", "將", "向", "使", "著",
                 "而", "下", "和", "日", "出", "成", "此")
  
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
write.table(dict, file="national_day.dict", quote=FALSE,
            row.names=FALSE, col.names=FALSE, fileEncoding="UTF-8",
            append=TRUE)

###############################################################
# 設定jieba斷詞器
mp.seg <- worker(type="mp", user="./national_day.dict", bylines=TRUE)

# 製作過濾去不包含中文字或只有一個中文字之候選詞語的函數
filterChineseTerms <- function (str_text) {
  str_text <- unlist(str_text)
  str_text <- str_text[grepl("\\p{Han}+", str_text, perl=TRUE)]
  str_text <- str_text[nchar(str_text)>1]
  paste(str_text, collapse=" ")
}

# 將新聞內容斷詞
news.df <- news.df %>%
  rowwise() %>%                           # 逐行運算
  mutate(words=segment(Content, mp.seg)) %>% # 斷詞
  mutate(words=filterChineseTerms(words)) %>% # 過濾去不包含中文字的候選詞語
  ungroup() %>%
  select(id, Year, President, Speech, Title, words)  # 選擇新聞編號和斷詞結果

#建立語料庫
vc = VCorpus(VectorSource(news.df$words))

#將形式為character string的句子依據詞語之間的空白轉為vector，vector上的單位為詞語
strsplit_space_tokenizer <- function(x)
  unlist(strsplit(as.character(x), "[[:space:]]+"))

#建立文件-詞語矩陣，每個元素為每一種詞語出現在一筆文件上的次數
dtm = DocumentTermMatrix(vc,
                         control=list(tokenize=strsplit_space_tokenizer, wordLengths=c(1, Inf)))
dim(dtm)

#統計詞語出現的總頻次
term.count = col_sums(dtm)
#統計詞語出現的文件數
term.df = tapply(dtm$v, dtm$j, length)

#統計出現總頻次大於20
cr1 <- term.count > 5
length(which(cr1))
#統計出現系所數不大於1/20段落
cr2 <- term.df < dtm$nrow/20
length(which(cr2))

stopwords <- c("的", "在", "是", "都", "了", "之下", "不會",
               "也", "很", "會", "有", "呢", "第二", "如何",
               "嗎", "就", "但", "所", "我", "各項", "年來",
               "不", "到", "要", "於", "表示", "增加", "而是",
               "公里", "一名", "可能", "認為", "更是", "逐步",
               "一次", "分享", "攝影", "繼續", "很多", "百年",
               "這次", "近年", "特派記者", "隨著", "日益",
               "就是", "沒有", "他們", "指出", "更加", "方面",
               "整個", "任何", "不同", "本人", "充滿", "帶來",
               "更好", "正在", "一天", "超過", "一直", "之外",
               "再次", "對於", "曾經", "做出", "國人同胞",
               "這塊", "儘管", "歷經", "如今", "各位伙伴",
               "或者", "方式", "一向", "這樣", "各位鄉親",
               "唯有", "來自", "英九", "一百年", "並且",
               "登輝", "全體", "七年", "億元", "達到", "此外",
               "絕對", "阿扁", "個人", "不管", "一切", "看到",
               "親愛的國人同胞", "各位鄉親父老", "父老鄉親",
               "什麼", "受到", "只有", "做到", "全體國人",
               "起來", "我國", "民國", "年前", "所以", "謝謝",
               "台灣人民", "願意", "相同", "依據", "院長",
               "一方面", "同胞", "全國同胞", "決定", "如此",
               "之間", "女士", "主席", "完全", "充分", "目前",
               "只是", "始終", "通過", "大幅", "所謂", "之前",
               "提高", "心中", "一部分", "不論是", "進一步",
               "之際", "有關", "走過", "還有", "一塊", "三大",
               "副總統", "相關", "各種", "甚至", "最近",
               "打造一個", "以上", "然而", "兩千", "降到",
               "因應", "需要", "由於", "當前", "最高", "一條",
               "減少", "第一", "一位", "四年", "這種", "一代",
               "全體國人同胞", "各位先進", "三年多", "非常",
               "第三", "而且", "在於", "許多", "一起", "無法",
               "包括", "因此", "各位貴賓", "大家好", "所有",
               "今天是中華民國", "能夠", "不是", "不但",
               "最大", "最後", "終於", "先生", "只要", "除了",
               "一年", "當年", "之後", "一樣", "以來", "如果",
               "才能", "還是", "現在", "不要", "失去", "感到",
               "不僅", "透過", "開始", "提出", "只要我們")
cr3 <- !(dtm$dimnames$Terms %in% stopwords)
length(which(cr3))

#統計出現總頻次大於20且出現系所數不大於1/4系所總數的詞語數
length(which(cr1 & cr2 & cr3))

#刪除詞語，保留文件-詞語矩陣上出現總頻次大於20且出現系所數不大於1/4系所總數的詞語
dtm1 <- dtm[, cr1&cr2&cr3]

#統計每一筆文件上出現的詞語數
doc.termno = row_sums(dtm1)
#是否有沒有詞語的文件
which(doc.termno<=5)
length(which(doc.termno<=5))
dtm1 <- dtm1[doc.termno>5, ]
news.df1 <- news.df[doc.termno>5, ]

#產生亂數
SEED = as.integer(Sys.time())%%10000

#根據亂數值，切分文件為十等分
set.seed(SEED)
fold = 10
folding = sample(rep(seq_len(fold), ceiling(nrow(dtm1)))[seq_len(nrow(dtm1))])
chain.list = seq_len(fold)

#設定主題模型的主題數量
topics = seq(4, 30, 2)

#以下利用10-fold cross validation計算不同主題數量下，每份語料在主題模型下的perplexity
#perpDF用來儲存每種主題數量、每份語料的perplexity
perpDF = data.frame(num_topic=integer(), chain=integer(), perplexity=double())
for (ntopic in topics) {
  for (chain in chain.list) {
    #LDA主題模型的參數
    control_list = list(alpha=0.01, seed=SEED, burnin=1000, thin=100, iter=1000, best=FALSE)
    #保留一份語料，以其餘的語料訓練LDA主題模型
    training = LDA(dtm1[folding != chain,], k = ntopic, control = control_list, method = "Gibbs")
    #選擇目前模型內最佳模型
    best_training = training@fitted[[which.max(logLik(training))]]
    #以最佳模型測試保留的語料
    testing = LDA(dtm1[folding == chain,], model = best_training, control = list(estimate.beta = FALSE, seed = SEED, burnin = 1000, thin = 100, iter = 1000, best = FALSE))
    #計算perplexity
    perp = perplexity(testing, dtm1[folding == chain,], use_theta = FALSE)
    print(paste0("topics: ", ntopic, ", folds: ", chain, ", perplexity: ", perp))
    perpDF = rbind(perpDF, data.frame(num_topic=ntopic, chain=chain, perplexity=perp))
  }
}

perpDF %>%
  ggplot() +
  geom_line(aes(x=num_topic, y=perplexity, colour=factor(chain), group=factor(chain)))

cohDF = data.frame(num_topic=integer(), coherence=double())
doc_no <- nrow(dtm1)
for (ntopic  in topics) {
  #LDA主題模型的參數
  control_list = list(alpha=0.01, seed=SEED, burnin=1000, thin=100, iter=1000, best=FALSE)
  #訓練LDA主題模型
  model = LDA(dtm1, k = ntopic, control = control_list, method = "Gibbs")
  #選擇目前模型內最佳模型
  best_model = model@fitted[[which.max(logLik(model))]]
  
  #主題內的詞語分布和文件內的主題分布
  post_prob = posterior(best_model)
  
  #將每個主題前10個最重要的詞語
  term_no = 10
  topicterm = terms(best_model, term_no)
  
  coh = double(length=ntopic)
  for (i in seq(1, ntopic)) {
    t_idx <- sapply(topicterm[, i], function(x) which(dtm1$dimnames$Terms==x))
    t_freq <- sapply(t_idx, function (x) length(which(dtm1$j==x))) / doc_no
    t_docidx <- lapply(t_idx, function (x) dtm1$i[dtm1$j==x])
    sum_pmi = 0
    for (j in seq(1, term_no-1)) {
      for (k in seq(j+1, term_no)) {
        co_doc <- length(intersect(t_docidx[[j]], t_docidx[[k]])) / doc_no
        pmi <- log(ifelse(co_doc>0,
                          co_doc/(t_freq[j]*t_freq[k]),
                          1e-12/(t_freq[j]*t_freq[k])))
        sum_pmi = sum_pmi + pmi
      }
    }
    coh[i] = sum_pmi*2 / (term_no*(term_no-1))
  }
  print(paste("ntopic:", ntopic, "Coherence:", mean(coh)))
  cohDF = rbind(cohDF, data.frame(num_topic=ntopic, coherence=mean(coh)))
}

###
cohDF %>%
  ggplot(aes(x=num_topic, y=coherence)) +
  geom_line()

ntopic = 20
control_list = list(alpha=0.01, seed=SEED, burnin=1000, thin=100, iter=1000, best=FALSE)
model = LDA(dtm1, k = ntopic, control = control_list, method = "Gibbs")
best_model = model@fitted[[which.max(logLik(model))]]
## 各主題上的關鍵詞
term_no <- 10
topicterm = terms(best_model, term_no) %>%
  t() %>%
  as.data.frame(stringsAsFactors=FALSE) %>%
  mutate(TopicName = rownames(.))

write.xlsx(topicterm, "president_talk_lda.xlsx", "term", row.names = FALSE)

#主題內的詞語分布和文件內的主題分布
post_prob = posterior(best_model)
doc_topic_distr = post_prob$topics

doc_topic_distr.df = as.data.frame(doc_topic_distr)

colnames(doc_topic_distr.df) <- sprintf("Topic%02d", 1:20)

news.df1 <- news.df1 %>%
  cbind(doc_topic_distr.df)

write.xlsx(news.df1, "president_talk_lda.xlsx", "speeches", row.names = FALSE, append=TRUE)

news.df2 <- news.df1 %>%
  select(c(2, 3, 4, 7:26)) %>%
  gather(key="Topics", value="Probability", -Year, -President, -Speech)

news.df2 %>%
  group_by(President, Topics) %>%
  summarise(Prob=mean(Probability)) %>%
  ggplot() +
  geom_col(aes(x=President, y=Prob, fill=President)) +
  facet_wrap(~Topics, nrow=4)

news.df2 %>%
  group_by(Speech, Topics) %>%
  summarise(Prob=mean(Probability)) %>%
  ggplot() +
  geom_col(aes(x=Speech, y=Prob, fill=Speech)) +
  facet_wrap(~Topics, nrow=4)
