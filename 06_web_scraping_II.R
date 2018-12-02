library(tidyverse)
library(rvest)

newsExtractor <- function(addr) {
  print(paste0("https://udn.com", addr))       #目前讀取新聞頁面
  tryCatch({
    #監看網頁取得時是否會產生錯誤
    story <- paste0("https://udn.com", addr) %>%      #新聞頁面網址
      read_html() %>%                                 #讀取HTML資料
      html_nodes(css="div#story_body_content p") %>%  #取得新聞內文節點
      html_text() %>%                                 #抽出新聞內文
      paste(collapse="\n")                            #彙整成一段
    
    return(story)
  }, error = function(e) {                         #一旦錯誤發生
    print(e)                                       #列印錯誤原因
    return(NA)                                     #傳回"無資料"
  })
}

dayStory <- function(date) {
  continue.flag <- TRUE
  news.df = data.frame(title = character(),     #所有的新聞標題與連結資料
                       link = character(),
                       stringsAsFactors = FALSE)
  ## 準備讀取目錄頁
  page.url <- paste0("https://udn.com/news/archive/2/6649/", date) # 產生目錄頁網址
  
  while (continue.flag==TRUE) {
    print(paste("Processing", page.url))
    page.cont <- read_html(page.url) # 讀取目錄頁資料
    
    # 讀取目錄頁上所有的新聞資料
    title.nodes <- html_nodes(page.cont, css="td a")
    # 將這新聞標題與內文連結一起放在同一個data frame中
    page.df <- data.frame(title=html_text(title.nodes), #目前目錄頁上的新聞標題與連結資料
                          link=html_attr(title.nodes, "href"),
                          stringsAsFactors = FALSE)
    
    # 合併先前與目前頁面的新聞資料
    news.df <- rbind(news.df, page.df)
    
    page.nodes <- html_nodes(page.cont, css=".pagelink a") # 取得頁碼標示
    
    page.text <- html_text(page.nodes) # 取得頁碼文字
    
    page.next <- grepl("下一頁", page.text) # 若頁碼標示文字為「下一頁」，則結果為TRUE，否則為FALSE
    
    continue.flag <- any(page.next) #是否有「下一頁」
    
    if (continue.flag) {
      # 如果有「下一頁」連結，則準備讀取下一個目錄頁
      url <- html_attr(page.nodes[page.next], "href") # 取得「下一頁」的連結
      page.url <- paste0("https://udn.com", url) # 產生下一個目錄頁網址
    }
  }
  
  news.df <- news.df %>%
    rowwise() %>%
    mutate(text=newsExtractor(link))
  
  return(news.df)
}

for (i in 1:15) {
  date <- sprintf("2018/09/%02d", i)
  day.news <- dayStory(date)
  
  write.csv(day.news, file=paste0("udn_", gsub("/","_",date),".csv"),
            row.names=FALSE, fileEncoding="UTF-8")
}
