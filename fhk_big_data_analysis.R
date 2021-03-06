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
library(xlsx)

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

#統計出現總頻次大於20
cr1 <- term.count > 5
length(which(cr1))
#統計出現系所數不大於1/4系所總數
cr2 <- term.df < 100
length(which(cr2))

stopwords <- c("的", "在", "是", "都", "了",
               "也", "很", "會", "有", "呢",
               "嗎", "就", "但", "所", "我",
               "不", "到", "要", "於", "表示",
               "公里", "一名", "可能", "認為",
               "一次", "分享", "攝影", "繼續",
               "這次", "近年", "特派記者",
               "就是", "沒有", "他們", "指出")
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
news.df1 <- news.df[doc.termno>10, ]

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

ntopic = 8
control_list = list(alpha=0.01, seed=SEED, burnin=1000, thin=100, iter=1000, best=FALSE)
model = LDA(dtm1, k = ntopic, control = control_list, method = "Gibbs")
best_model = model@fitted[[which.max(logLik(model))]]
## 各主題上的關鍵詞
topicterm = terms(best_model, term_no) %>%
  t() %>%
  as.data.frame(stringsAsFactors=FALSE) %>%
  mutate(TopicName = rownames(.))
  
write.xlsx(topicterm, "parade_lda_8.xlsx", "term", row.names = FALSE)

saveRDS(best_model, "parade_lda_8.rds")

topicterm = topicterm %>%
  gather(key="rank", value="term", -TopicName) %>%
  mutate(rank = as.numeric(substr(rank, 2, nchar(rank))))

#主題內的詞語分布和文件內的主題分布
post_prob = posterior(best_model)
doc_topic_distr = post_prob$topics

doc_topic_distr.df = as.data.frame(doc_topic_distr)

colnames(doc_topic_distr.df) <- paste0("Topic", 1:8)

news.df1 <- news.df1 %>%
  cbind(doc_topic_distr.df)
  
write.xlsx(news.df1, "parade_lda_8.xlsx", "news", row.names = FALSE, append=TRUE)

news.df2 <- news.df1 %>%
  gather(key="Topics", value="Probability", Topic1, Topic2, Topic3, Topic4, Topic5, Topic6, Topic7, Topic8)

######################################################
# 根據主題在整個文件集合上的分布，找出重要的主題
#
# 統計主題分布機率
#
# 將主題依據其分布機率由大到小排序

news.df2 %>%
  group_by(Topics) %>%
  summarise(Prob=mean(Probability)) %>%
  arrange(desc(Prob))

news.df2 %>%
  group_by(Topics, Source) %>%
  summarise(Prob=mean(Probability)) %>%
  arrange(Source, desc(Prob)) %>%
  ggplot() +
  geom_col(aes(x=Topics, y=Prob, fill=Source), position="dodge")

udn <- doc_topic_distr[news.df1$Source=="udn", ]
udn.topics.order <- order(colSums(udn), decreasing = TRUE)

ltn <- doc_topic_distr[news.df1$Source=="ltn", ]
ltn.topics.order <- order(colSums(ltn), decreasing = TRUE)

######################################################
# 根據文件上的最主要(最大)主題，找出重要的主題
#
# 找出每篇文件最主要(最大)的主題
doc_max_topic <- apply(doc_topic_distr, 1, which.max)
#
# 統計各主題為最主要者的文件數目 table(doc_max_topic)
#
#  將主題依據其為文件最主要主題數由大到小排序
udn.topics.order2 <- order(table(doc_max_topic[news.df1$Source=="udn"]), decreasing = TRUE)
ltn.topics.order2 <- order(table(doc_max_topic[news.df1$Source=="ltn"]), decreasing = TRUE)
