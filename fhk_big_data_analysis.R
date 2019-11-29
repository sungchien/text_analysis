library(readr)
library(tidyverse)
library(lubridate)
library(jiebaR)
#文字處理套件
library(tm)
#疏鬆矩陣運算套件
library(slam)
#主題模型套件
library(topicmodels)

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
dim(dtm)

#統計詞語出現的總頻次
term.count = col_sums(dtm)
#統計詞語出現的文件數
term.df = tapply(dtm$v, dtm$j, length)

#統計出現總頻次大於5且出現文件數不大於100的詞語數
length(which(term.count>5 & term.df<=100))

#刪除詞語，保留文件-詞語矩陣上出現總頻次大於5且出現文件數不大於100的詞語
dtm1 <- dtm[, term.count>5 & term.df<=100]

#統計每一筆文件上出現的詞語數
doc.termno = row_sums(dtm1)
#是否有沒有詞語的文件
which(doc.termno==0)
dtm1 <- dtm1[doc.termno!=0, ]

#產生亂數
SEED = as.integer(Sys.time())%%10000

#根據亂數值，切分文件為十等分
set.seed(SEED)
fold = 10
folding = sample(rep(seq_len(fold), ceiling(nrow(dtm)))[seq_len(nrow(dtm))])
chain.list = seq_len(fold)

#設定主題模型的主題數量
topics = c(1:10)*3

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
