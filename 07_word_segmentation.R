library(readr)
library(tidyverse)
library(jiebaR)
library(wordcloud)

# 讀取歷史新聞資料
tdf <- data.frame()
for (i in 1:15) {
  file <- sprintf("udn_2018_09_%02d.csv", i)
  df <- read_csv(file)
  df$date <- sprintf("2018/09/%02d", i)
  tdf <- rbind(tdf, df)
}

# 將每則新聞加上編號(id)
tdf <- tdf %>%
  mutate(id = row_number())

# 設定斷詞器
word.seg <- worker(type="mix", symbol=TRUE, bylines=TRUE)

ws <- tdf %>%
  mutate(word=segment(paste(title, text, sep="。"), word.seg)) %>% # 斷詞
  select(id, word) %>%
  unnest(word)  # 展開每則新聞的詞語

# 過濾至少包含一個中文字的詞語
ws <- ws %>%
  filter(grepl("(\\p{Han})+", word, perl=TRUE))

# 統計每個詞語在新聞中出現的總次數與頻率
word.df <- ws %>%
  count(word, sort=TRUE)  %>% #統計詞語在新聞中的出現總次數，並依據次數高低排序
  mutate(frequency=n/sum(n)) %>% #計算詞語出現頻率
  mutate(rank=row_number()) #依照紀錄順序設定流水編號

# Zipf's Law的驗證
word.df %>%
  ggplot(aes(x=rank, y=frequency)) + # 將詞語排序與出現頻率的關係
  geom_line() + # 畫成線圖
  scale_x_log10(breaks=c(1, 10, 100, 1000)) + # 將x軸與y軸改為對數
  scale_y_log10(breaks=seq(0, 0.1, 0.01))

# 取出排名10到1000名詞語的部分
rank_subset <- word.df %>%
  filter(rank>10, rank<1000)

# 以線性迴歸的方式計算截距與斜率
lm(log10(frequency) ~ log10(rank), data = rank_subset)

word.df %>%
  ggplot(aes(x=rank, y=frequency)) + # 將詞語排序與出現頻率的關係
  geom_line() + # 畫成線圖
  scale_x_log10(breaks=c(1, 10, 100, 1000)) + # 將x軸與y軸改為對數
  scale_y_log10(breaks=seq(0, 0.1, 0.01)) +
  geom_abline(intercept = -1.57, slope = -0.74, color = "gray50", linetype = 2) +
  labs(x="將詞語依照出現次數大小的排序", y="出現頻率") # 加上標題

# 新聞中出現頻率最高的詞語
word.df %>%
  top_n(20, frequency) %>% #取出前二十名
  ggplot(aes(x=reorder(word, frequency), y=frequency)) + # 依據頻率大小將詞語排序
  geom_col(fill = alpha("purple", 0.7)) +
  coord_flip() + #將詞語置於y軸，出現次數置於x軸
  labs(title="依照出現次數排列的前二十個詞語", x="詞語", y="出現頻率") # 加上標題

# 計算idf
word.idf <- ws %>%
  mutate(total.doc=n_distinct(id)) %>% #新聞總數
  group_by(word, total.doc) %>% #依據每個詞語分群
  summarise(doc.freq=n_distinct(id)) %>% #統計各詞語出現的新聞數
  mutate(idf=log(total.doc/doc.freq)) %>%
  select(word, idf)

# 詞語在各則新聞的出現次數與頻率
word.msg <- ws %>%
  group_by(id, word) %>%
  summarise(count=n()) %>% #詞語在新聞中的出現次數
  mutate(tf=count/sum(count)) %>% #詞語在每一則新聞內的頻率
  ungroup()

# 計算詞語在各則新聞的tf*idf值
word.msg <- word.msg %>%
  left_join(word.idf, by=c("word"="word")) %>% # 合併tf與idf的計算結果
  mutate(tfidf=tf*idf) %>% #計算詞語在各則新聞的tf*idf值
  filter(tfidf>0.05) # 捨棄各則新聞中的tfidf值過小的詞語

# 對每一個詞語計算在所有新聞上的tfidf總和
word.scr <- word.msg %>%
  group_by(word) %>%
  summarise(sum.tfidf=sum(tfidf)) %>%
  arrange(desc(sum.tfidf))

# 繪製所有新聞上的tfidf總和最高的前二十個詞語的長條圖
word.scr %>%
  slice(1:20) %>%
  ggplot(aes(x=reorder(word, sum.tfidf), y=sum.tfidf)) +
  geom_col(fill = alpha("purple", 0.7)) +
  labs(title="TFIDF總和最大的前二十個詞語", x="詞語", y="TFIDF總和") +
  coord_flip()

# 選擇顏色組合
pal <- brewer.pal(8, "Dark2")
# 繪製所有新聞上的tfidf總和最高的前五十個詞語的文字雲
wordcloud(word.scr$word[1:50], word.scr$sum.tfidf[1:50],
          scale=c(4, 0.1), random.order=FALSE, random.color=TRUE,
          rot.per=0, colors=pal)
