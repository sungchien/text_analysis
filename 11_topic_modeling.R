library(readr)
library(tidyverse)
library(lubridate)
library(jiebaR)
#文字處理套件
library(tm)
#tokenizers套件
library(tokenizers)
#疏鬆矩陣運算套件
library(slam)
#主題模型套件
library(topicmodels)

# 讀入聯合報九月一日到十日的要聞新聞資料
news <- data.frame()

for (i in 1:15) {
  date <- sprintf("2018_09_%02d", i)
  
  news.df <- read_csv(file=paste0("udn_", date,".csv")) %>%
    mutate(date=ymd(date))
  news <- rbind(news, news.df)
}

# 設定jieba斷詞器
mp.seg <- worker(type="mp", user="./udn_2018_09.dict", bylines=TRUE)

# 製作過濾去不包含中文字或只有一個中文字之候選詞語的函數
filterChineseTerms <- function (str_text) {
  str_text <- unlist(str_text)
  str_text <- str_text[grepl("\\p{Han}+", str_text, perl=TRUE)]
  str_text <- str_text[nchar(str_text)>1]
  paste(str_text, collapse=" ")
}

# 將新聞內容斷詞
news <- news %>%
  filter(!is.na(text)) %>%                # 保留有內容的新聞資料
  mutate(id=row_number()) %>%             # 每則新聞加上編號(id)
  rowwise() %>%                           # 逐行運算
  mutate(words=segment(text, mp.seg)) %>% # 斷詞
  mutate(words=filterChineseTerms(words)) %>% # 過濾去不包含中文字的候選詞語
  ungroup() %>%
  select(id, date, title, words)          # 選擇新聞編號和斷詞結果

#建立語料庫
vc = VCorpus(VectorSource(news$words))

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

#統計出現總頻次大於20且出現文件數不大於100的詞語數
length(which(term.count>20 & term.df<=100))

#刪除詞語，保留文件-詞語矩陣上出現總頻次大於20且出現文件數不大於100的詞語
dtm1 <- dtm[, term.count>20 & term.df<=100]

#統計每一筆文件上出現的詞語數
doc.termno = row_sums(dtm1)
#是否有沒有詞語的文件
which(doc.termno==0)
dtm1 <- dtm1[doc.termno!=0, ]

#產生亂數
SEED = as.integer(Sys.time())%%10000

#根據亂數值，切分文件為五等分
set.seed(SEED)
fold = 10
folding = sample(rep(seq_len(fold), ceiling(nrow(dtm)))[seq_len(nrow(dtm))])
chain.list = seq_len(fold)

#設定主題模型的主題數量
topics = c(1:5)*5

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

######################################################
# 根據主題在整個文件集合上的分布，找出重要的主題
#
# 統計主題分布機率 colSums(doc_topic_distr)
#
# 將主題依據其分布機率由大到小排序 order(colSums(doc_topic_distr), decreasing = TRUE)
topics.order <- order(colSums(doc_topic_distr), decreasing = TRUE)

# 查看分布機率最大的十個主題之前十個詞語
lda_model$get_top_words(n = 10, lambda = 1.0)[, topics.order[1:10]]


######################################################
# 根據文件上的最主要(最大)主題，找出重要的主題
#
# 找出每篇文件最主要(最大)的主題
doc_max_topic <- apply(doc_topic_distr, 1, which.max)
#
# 統計各主題為最主要者的文件數目 table(doc_max_topic)
#
#  將主題依據其為文件最主要主題數由大到小排序
topics.order <- order(table(doc_max_topic), decreasing = TRUE)

# 查看各文件最主要主題的十個主題之前十個詞語
lda_model$get_top_words(n = 10, lambda = 0.5)[, topics.order[1:10]]

####################################################
# 驗證主題模型在新進資料上的有效性(validation)

# 設定隨機取樣方式
SEED = as.integer(Sys.time())%%10000

set.seed(SEED)
# 隨機將所有文件分為10等分(folds)
fold = 10
doc_no <- nrow(news)
folding = sample(rep(seq_len(fold), ceiling(doc_no/fold))[seq_len(doc_no)])

# 將資料分為訓練與測試資料
training_set <- news[folding!=1, ]
testing_set <- news[folding==1, ]

# 將訓練資料轉換成text2vec所需的文字處理格式
it <- itoken(training_set$words, progressbar = FALSE)

# 抽取所有出現在文件中的詞語，並刪除不合預設條件的詞語
vocab <- create_vocabulary(it, stopwords=stwd) %>%
  prune_vocabulary(term_count_min = 20, doc_proportion_min = 0.02, doc_proportion_max = 0.1)

# 設定詞語向量化
vectorizer = vocab_vectorizer(vocab)

# 計算詞語出現在文件上的次數(DTM)
dtm = create_dtm(it, vectorizer)

# 建立測試文件的DTM
new_dtm = itoken(testing_set$words, progressbar = FALSE) %>% 
  create_dtm(vectorizer)

topics_seq = seq(10, 200, 10)

for (topic_no in topics_seq) {
  # 設定LDA主題模型的參數
  lda_model = LDA$new(n_topics = topic_no, doc_topic_prior = 0.1, topic_word_prior = 0.01)

  # 訓練主題模型
  doc_topic_distr = 
    lda_model$fit_transform(x = dtm, n_iter = 1000, 
                            convergence_tol = 0.001, n_check_convergence = 25,
                            progressbar = FALSE)
  
  new_doc_topic_distr = lda_model$transform(new_dtm)
  
  pp <- perplexity(new_dtm, topic_word_distribution = lda_model$topic_word_distribution, doc_topic_distribution = new_doc_topic_distr)
  print(paste0("Topic No. ", topic_no, "   Perplexity: ", pp))
}
