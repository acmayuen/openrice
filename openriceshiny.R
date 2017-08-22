require(jiebaR)
library(wordcloud2)
library(tm)
library(tmcn)
library(stringr)
library(plyr)
library(dplyr)
library(class)
library(lubridate)
library(ggplot2)
library(plotly)

setwd("Documents/")

df<-read.csv("highlistpref.csv",stringsAsFactors = F)
shopid<-read.csv("/home/andrew/Documents/ShopList1-50000.csv")
# allshop<-read.csv("/home/andrew/Documents/opdf(ID1-2217).csv")

shop.list<-c(61,90,468,778,1441,1491,1549,1618,1754,1814,1868,2020)
# list<-shopid[(shopid$id %in% shop.list), ]

df$sum   <- df$taste+df$env+df$serv+df$hyg+df$cp
df$avg   <- df$sum/5
df$date1 <- as.Date(df$date1)
df$comm  <- as.character(df$comm)
df$user  <- as.character(df$user)
df$lg    <- nchar(df$comm)

#data.x <- merge(df, shopid, by = c("id"))

ids <- 90 #pick restaurant
no.pick     <- 5
dateperiod  <- "2015-01-01"
shop.sl <- subset(df, id == ids)

### All Review Rating
mean(shop.sl$taste)
mean(shop.sl$env)
mean(shop.sl$serv)
mean(shop.sl$hyg)
mean(shop.sl$cp)
mean(shop.sl$avg)

(smile.tt<-sum(shop.sl$pref == "smile"))
(ok.tt<-sum(shop.sl$pref == "ok"))
(cry.tt<-sum(shop.sl$pref == "cry"))
(np.tt<-sum(shop.sl$pref == "np"))
smile.tt/(smile.tt+ok.tt+cry.tt+np.tt)

shop.pick <- shop.sl
shop.pick$comm  <- gsub("[0-9]","",shop.pick$comm)
shop.pick$comm  <- gsub("[A-z]","",shop.pick$comm)
#shop.pick$comm  <- gsub("讚好","",shop.pick$comm)
sl.pick    <- table(shop.pick$user)
shop.pick  <- shop.pick[shop.pick$sum!=0, ] #take out 0
#shop.pick  <- shop.pick[shop.pick$user %in% names(sl.pick[sl.pick <= no.pick ]), ] #for comment less than 3
#shop.pick <- setDT(shop.pick)[,.SD[which.max(date1)],keyby=user] #pick user latest comment
shop.pick  <- shop.pick%>%group_by(user)%>%filter(date1==max(date1))
shop.pick  <- subset(shop.pick, date1 >= as.Date(dateperiod) ) #date after
shop.pick  <- as.data.frame(shop.pick)

mean(shop.pick$taste)
mean(shop.pick$env)
mean(shop.pick$serv)
mean(shop.pick$hyg)
mean(shop.pick$cp)
mean(shop.pick$avg)

(smile<-sum(shop.pick$pref == "smile"))
(ok<-sum(shop.pick$pref == "ok"))
(cry<-sum(shop.pick$pref == "cry"))
(np<-sum(shop.pick$pref == "np"))
smile/(smile+ok+cry+np)

### Define dictionary
jseg <- worker(user="dictionary.txt",stop_word = "stopword.txt")#
new_user_word(jseg,c("唔覺","揀唔落","不過不失","幾好","唔得","唔好","唔掂","難食","賣相一般",
                     "好貴","唔抵","好差","唔貴","服務好","服務好差","好正","好味","唔好味","唔值",
                     "超水準","唔鍾意","比較貴","紅磡","超正","日本菜","大家","唔錯","訂定位",
                     "訂左位","訂位","淋得滯","唔舒服","好平","好凍","味道","美味","口味","有機",
                     "食物","蔬菜","小食","點心","甜品","罐頭","急凍","飯盒","早餐","晚餐","午餐",
                     "西式","西餐","中式","中餐","茶樓","茶餐廳","料理","小賣部","餅店","酒樓",
                     "酒店","外賣","快餐","快餐店","火鍋","自助餐","超級市場","街市","餐牌","服務",
                     "減價","侍應","衞生","新鮮","豐富","暴飲暴食","分量","胃口","減肥","珍惜",
                     "浪費","燒烤","味精","約左","冇乜","好抵","包住","唔覺","不太喜歡","叫左",
                     "無欺場","出其","叫咗","果度","睇落","同朋友","多謝晒","落力","多人","唔多人","好多人",
                     "幾多人","清清地","好夠味","再嚟","唔會再嚟","會再嚟","嫩滑","麵包","前菜",
                     "海南雞","黑椒汁","抹茶","綠茶","再食","再黎","再嚟","冇咩驚喜","冇驚喜","唔吸引",
                     "有點鞋","有水準","似模似樣","交足貨","很好","味道好","味道唔好","味道唔得",
                     "味道好唔得","唔去","唔再去","唔會去","唔會再去","好吃","難吃","真係","真是","真正",
                     "叫左","覺得","唔覺得","好食","唔好食","好難食","邊個","個個","果個","呢個","唔會",
                     "比較","再來","麵包","食麵","很好","無咩驚喜","無驚喜","無味","食左","太大","太多",
                     "太少","太好","太差","拉麵","麵豉湯","麵店","幼麵","咁多","咁少","咁好","唔係",
                     "再來","唔來","會來","唔會再來","唔洗","鎮店之寶","好耐","不算太差"))

PW   <-c("交足貨",	"似模似樣",	"冇味精",	"再來",	"再嚟",	"再食",	"再黎",	"味道好",	"唔錯",	"太好",	"太好味",	"好吃",	"好味",	"好味道",	"好夠味",	"好正",	"好食",	"嫩滑",	"幾好",	"幾好",	"幾正",	"很好",	"會來",	"會再嚟",	"有水準",	"服務好",	"比較好",	"無味精",	"無欺場",	"美味",	"超正",	"超水準")
NW   <-c("不太喜歡",	"不好",	"不好吃",	"冇乜驚喜",	"冇咩驚喜",	"冇驚喜",	"味精",	"味道唔好",	"味道唔得",	"味道好唔得",	"咁少",	"唔來",	"唔係咁好",	"唔再來",	"唔再去",	"唔去",	"唔吸引",	"唔好",	"唔好味",	"唔好食",	"唔得",	"唔掂",	"唔會",	"唔會再來",	"唔會再去",	"唔會再嚟",	"唔會去",	"唔舒服",	"唔覺",	"唔覺得",	"唔鍾意",	"多味精",	"太少",	"太差",	"好差",	"好難食",	"幾差",	"揀唔落",	"有味精",	"有點鞋",	"服務唔好",	"服務好差",	"淋得滯",	"無味",	"無咩驚喜",	"無驚喜",	"賣相一般",	"難吃",	"難食",	"不算太差",	"不過不失",	"太多",	"太大")
ENW  <-c("唔多人",	"多人",	"太多人",	"冷氣",	"太迫",	"逼人",	"好耐",	"冇位",	"無位")
CPPW <-c("勁抵",	"唔貴",	"好平",	"好抵",	"幾平",	"減價",	"超抵")
CPNW <-c("價錢貴",	"勁貴",	"唔值",	"唔平",	"唔抵",	"好貴",	"幾貴",	"比較貴",	"超貴")



### KNN Model prediction
shop.pick2  <- unlist(lapply(lapply(shop.pick$comm,segment,jiebar=jseg),paste0,collapse=" "))
text_corpus <- Corpus(x = VectorSource(shop.pick2))
text_corpus <- tm_map(text_corpus, removeNumbers)
text_corpus <- tm_map(text_corpus, stripWhitespace)
text_corpus <- tm_map(text_corpus, removePunctuation)
dtm <- DocumentTermMatrix(x = text_corpus, control = list(wordLengths = c(4,Inf)))
dtm <- removeSparseTerms(x = dtm, sparse = 0.95)
dfa <- as.matrix(dtm)
shop.pick6  <- cbind(shop.pick,dfa)
colnames(dfa)

set.seed(100)
train <- sample(nrow(shop.pick6), ceiling(nrow(shop.pick6) * .60))
test <- (1:nrow(shop.pick6))[- train]
cl <- shop.pick6[, "pref"]

modeldata <- shop.pick6[,!colnames(shop.pick6) %in% c("pref","comm","user","id","date1")]
knn.pred  <- knn(modeldata[train, ], modeldata[test, ], cl[train])
(conf.mat <- table("Predictions" = knn.pred, Actual = cl[test]))
(accuracy <- sum(diag(conf.mat))/length(test) * 100)
df.pred   <- cbind(knn.pred, cl[test],modeldata[test, ])

### K-Means
require(factoextra)
shop.pick7<-shop.pick6[,-c(1,2,8,9,10)]
kmeans.cluster <- kmeans(shop.pick7, centers=4) 
kmeans.cluster$withinss
table(kmeans.cluster$cluster, shop.pick6$pref)  
fviz_cluster(kmeans.cluster,data = shop.pick7,geom = c("point","text"),frame.type = "norm")

### K-Medoid
require(cluster)
require(factoextra)
kmedoid.cluster <- pam(shop.pick7, k=10) 
kmedoid.cluster$objective
table(kmedoid.cluster$clustering, shop.pick6$pref) 
fviz_cluster(kmedoid.cluster,data = shop.pick7,geom = c("point"),frame.type = "norm") 



### Construct Wordcloud
f <- function(x) jseg <= x
wc.cl <- as.vector(shop.pick$comm)
wc.cl <- str_replace_all(wc.cl,"[^\\w]|[\\d+]","")
wc.cl <- gsub("0-9","",wc.cl)
wc.cl <- gsub(" ","",wc.cl)
wc.cl <- gsub('[:alpha:]',"",wc.cl)
wc.cl <- llply(wc.cl%>%paste(collapse = ","),f)%>%llply(table)
wc.fin <- data.frame(wc.cl)
#wordcloud2(wc.fin[wc.fin$Freq>20,], size = 2, color = "random-light", backgroundColor = "grey")
wordcloud2(wc.fin[wc.fin$Freq>20,],minRotation = -pi/8, maxRotation = -pi/8, minSize = 1,rotateRatio = 2)
keyterms<-c("唔好","唔掂","唔得","不行","好差","唔好味","唔鍾意")
wc.fin[wc.fin$Var1 %in%keyterms, ]

### Top 10 keyword
keys<-worker(user="dictionary.txt",stop_word = "stopword.txt",type="keywords",topn=10)
mm<-paste(shop.pick$comm,collapse=" ")
topsh<-keys<=mm
word<-as.vector(topsh)
tfidf<-names(topsh)
top<-cbind(word,tfidf)
top.t <- t(top[,2:ncol(top)])
colnames(top.t) <- top[,1] 
top.t



### Check comment similarity
dd   <- dist(dfa)
fit1 <- hclust(d = dd, method = 'mcquitty')
fit2 <- cutree(fit1,h =3)
plot(fit1,hang=-1,cex=0.6)
#plot(fit1,xlim=c(1,5),ylim = c(1,5))
#plot(as.phylo(fit1),type="fan") #need library(ape)
rect.hclust(tree = fit1, k = 3, border = 'red')

mthavg<-merge(aggregate(list(mth.avg = shop.pick$avg), list(mon = cut(shop.pick$date1, "1 month")), mean),
      aggregate(list(mth.avg = shop.pick$avg), list(mon = cut(shop.pick$date1, "1 month")), length)
      ,by = "mon")

plot_ly(mthavg, x = ~mon, y = ~mth.avg.x, name = '平均評分',type = 'scatter',mode='line+markers') %>%
add_trace(y = ~mth.avg.y, name = '食評數量',type = 'bar') #, mode = 'lines+markers'




##############################################################


KNN預測 {data-orientation=rows}
======================================================
  
  Column {.sidebar data-width=300 }
-----------------------------------------------------------------------
  
  ```{r}

```

Waiting time between

Row {data-height=80}
-----------------------------------------------------------------------
  
  ###
  ```{r}
renderValueBox({
  valueBox("最多出現次數關𨫡字", caption = " ",color='#c17028')
})
```


Row {data-height=80}
-----------------------------------------------------------------------
  
  ### 123
  
  ```{r}
renderDataTable({
  xx <-shop.pick()
  shop.pick2  <- unlist(lapply(lapply(xx$comm,segment,jiebar=jseg),paste0,collapse=" "))
  text_corpus <- Corpus(x = VectorSource(shop.pick2))
  text_corpus <- tm_map(text_corpus, removeNumbers)
  text_corpus <- tm_map(text_corpus, stripWhitespace)
  text_corpus <- tm_map(text_corpus, removePunctuation)
  dtm <- DocumentTermMatrix(x = text_corpus, control = list(wordLengths = c(4,Inf)))
  dtm <- removeSparseTerms(x = dtm, sparse = 0.9)
  dfa <- as.matrix(dtm)
  shop.pick6  <- cbind(shop.pick,dfa)
  
  set.seed(100)
  train <- sample(nrow(shop.pick6), ceiling(nrow(shop.pick6) * .60))
  test <- (1:nrow(shop.pick6))[- train]
  cl <- shop.pick6[, "pref"]
  
  modeldata <- shop.pick6[,!colnames(shop.pick6) %in% c("pref","comm","user","id","date1")]
  knn.pred  <- knn(modeldata[train, ], modeldata[test, ], cl[train])
  conf.mat <- table("Predictions" = knn.pred, Actual = cl[test])
  accuracy <- sum(diag(conf.mat))/length(test) * 100
  df.pred   <- cbind(knn.pred, cl[test],modeldata[test, ])
  
  DT::datatable(df.pred, extensions=c('FixedColumns','Buttons', 'ColReorder','Scroller'),
                options = list(pageLength=100, scrollY='400px', scrollX=TRUE, autoWidth=TRUE, searchHighlight=TRUE, scroller=TRUE, fixedColumns = list(leftColumns = 3),dom = 'Bfrtip', buttons = list('copy','print', list(extend='collection', buttons= c('csv', 'excel', 'pdf'), text='Download'), I('colvis')), colReorder = TRUE), 
                filter = 'top', class = "display cell-border")
})
```
