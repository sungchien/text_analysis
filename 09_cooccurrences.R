library(readr)
library(tidyverse)
library(jiebaR)
library(igraph)

# 讀入聯合報九月一日到十五日的生活新聞資料
news <- data.frame()

for (i in 15:24) {
  date <- sprintf("2018_11_%02d", i)
  
  news.df <- read_csv(file=paste0("udn_", date,".csv")) %>%
    mutate(date=date)
  news <- rbind(news, news.df)
}

#########
# 找出新聞中重要的詞語及其共同出現的新聞數量
# 利用tfidf找出重要詞語

# 設定jieba斷詞器
mp.seg <- worker(type="mp", user="./udn_2018_09.dict", bylines=TRUE)

# 將新聞內容斷詞
news <- news %>%
  filter(!is.na(text)) %>%                # 保留有內容的新聞資料
  mutate(id=row_number()) %>%             # 每則新聞加上編號(id)
  mutate(word=segment(text, mp.seg)) %>% # 斷詞
  select(id, word) %>%                    # 選擇新聞編號和斷詞結果
  unnest(word) %>%                        # 將斷詞結果展開
  filter(grepl("\\p{Han}+", word, perl=TRUE)) %>% # 保留至少一個中文字的詞語
  filter(nchar(word)>1)

# 計算每個詞語idf(inverse document frequency)
word.idf <- news %>%
  mutate(total.doc = n_distinct(id)) %>%  # 統計文件總數
  group_by(word, total.doc) %>%           # 統計詞語出現文件數
  summarise(doc=n_distinct(id)) %>%
  mutate(idf=log(total.doc/doc)) %>%      # 計算idf
  ungroup()

# 計算每個詞語在各個新聞中的出現次數與頻率(tf)
word.news <- news %>%
  group_by(word, id) %>%    # 計算每個詞語在各個新聞中的出現次數
  summarise(c=n()) %>%
  ungroup() %>%
  group_by(id) %>%          # 計算各新聞的詞語出現次數總和(sum(c))
  mutate(tf=c/sum(c)) %>%   # 計算詞語的出現頻率(tf)
  ungroup()

# 計算tf*idf
word.news <- word.news %>%
  left_join(word.idf, by="word") %>%      # 連結idf資料
  mutate(tfidf=tf*idf)                    # 計算

# 計算詞語在所有新聞的tfidf總和，做為詞語的重要性，並且選出前100個重要詞語
keyword <- word.news %>%
  filter(tfidf>0.04) %>%               # 去除新聞中tfidf過低的詞語
  group_by(word) %>%                   # 計算詞語在所有新聞的tfidf總和
  summarise(sum.tfidf=sum(tfidf)) %>%
  top_n(100, sum.tfidf) %>%            # 選出前100個重要詞語
  arrange(desc(sum.tfidf))

# 找出每則新聞中出現的重要詞語
keyword.news <- word.news %>%
  filter(tfidf>0.04) %>%             # 去除新聞中tfidf過低的詞語
  select(id, word) %>%
  semi_join(keyword, by="word") %>%  # 以重要詞語比對每個新聞內容
  arrange(id)

# 統計重要詞語出現的新聞數量
kw_docs <- keyword.news %>%
  group_by(word) %>%              # 統計重要詞語出現的文件數
  summarise(c=n_distinct(id)) %>%
  ungroup() %>%
  arrange(desc(c))

# 計算重要詞語共同出現的新聞數量
kw_codocs <- keyword.news %>%
  inner_join(keyword.news, by=c("id")) %>% # 找出每一則新聞共同出現的重要詞語
  group_by(word.x, word.y) %>%             # 統計每一對重要詞語共同出現的新聞數量
  summarise(dxy=n()) %>%
  arrange(desc(dxy)) %>%                   # 按照共同出現的新聞數排序
  ungroup() %>%
  filter(word.x != word.y)                 # 刪除相同的詞語

# 加上重要詞語出現的新聞數量
kw_codocs <- kw_codocs %>%
  left_join(kw_docs, by=c("word.x"="word")) %>%
  rename(dx=c) %>%
  left_join(kw_docs, by=c("word.y"="word")) %>%
  rename(dy=c)

#########
# 計算詞語共同出現的相關性
# 本次課程以Jaccard similarity和Correlation Coefficient兩種方式計算

# Jaccard similarity
jaccardSimilarity <- function(dx, dy, dxy) {
  dx <- as.numeric(dx)
  dy <- as.numeric(dy)
  dxy <- as.numeric(dxy)
  return(dxy/(dx+dy-dxy))
}

# 計算每對重要詞語的Jaccard Similarity
word_net.js <- kw_codocs %>%
  rowwise() %>%
  mutate(js=jaccardSimilarity(dx, dy, dxy)) %>%
  ungroup() %>%
  arrange(desc(js))

# 刪減較不重要共現資訊
word_net.js <- word_net.js %>%
  mutate(pr=percent_rank(js)) %>%           # 根據jc進行百分比排序(由小到大)
  filter(pr>0.75) %>%                       # 保留後1/4
  select(from=word.x, to=word.y, weight=js)

########
# 將重要詞語與其共現資訊表示成網路圖

# 將資料轉成網路
wg.js <- graph_from_data_frame(word_net.js, directed=FALSE)

# 將節點之間的線合併
wg.js <- simplify(wg.js, edge.attr.comb = list("mean"))

# 根據節點分群的結果為各節點設定顏色
cl.js <- cluster_louvain(wg.js)
cl.js.mem <-  membership(cl.js)

# 計算各節點在圖形上的座標
coords.js <- layout_(wg.js, with_graphopt())

# 畫圖
png(file="graph_js.png", width=800, height=600)
plot(x=cl.js, y=wg.js, vertex.shape="none",
     vertex.label.cex=0.8, edge.lty="blank", layout=coords.js)
dev.off()

# 查看各分群(主題)內的詞語
cl.js.mem <- membership(cl.js)
for (i in seq(max(cl.js.mem))) {
  print(paste("Cluster", i))
  print(V(wg.js)$name[cl.js.mem==i])
}

# correlation coefficient
phiCoefficient <- function(d, dx, dy, dxy) {
  d <- as.numeric(d)
  dx <- as.numeric(dx)
  dy <- as.numeric(dy)
  dxy <- as.numeric(dxy)
  d.not.x <- d - dx
  d.not.y <- d - dy
  dx.not.y <- dx - dxy
  dy.not.x <- dy - dxy
  d.not.x.not.y <- d.not.y - dx.not.y
  return((dxy*d.not.x.not.y-dx.not.y*dy.not.x)/sqrt(dx*dy*d.not.x*d.not.y))
}

# 所有的關鍵詞語共出現在多少則新聞(d)
d <- keyword.news %>%
  distinct(id) %>%
  nrow()

# 計算每個詞語的Correlation Coefficient
word_net.cc <- kw_codocs %>%
  rowwise() %>%
  mutate(cc=phiCoefficient(d, dx, dy, dxy)) %>%
  ungroup()

# 刪減較不重要共現資訊的網路圖
word_net.cc <- word_net.cc %>%
  mutate(pr=percent_rank(cc)) %>%
  filter(pr>0.75) %>%
  select(from=word.x, to=word.y, weight=cc)

# 將資料轉成網路
wg.cc <- graph_from_data_frame(word_net.cc, directed=FALSE)

# 將節點之間的線合併
wg.cc <- simplify(wg.cc, edge.attr.comb = list("mean"))

# 對節點分群
cl.cc <- cluster_louvain(wg.cc)

# 計算各節點在圖形上的座標
coords.cc <- layout_(wg.cc, with_graphopt())

# 畫圖
png(file="graph_cc.png", width=800, height=600)
plot(x=cl.cc, y=wg.cc, vertex.shape="none",
     vertex.label.cex=0.8, edge.lty="blank", layout=coords.cc)
dev.off()

cl.cc.mem <- membership(cl.cc)
for (i in seq(max(cl.cc.mem))) {
  print(paste("Cluster", i))
  print(V(wg.cc)$name[cl.cc.mem==i])
}
