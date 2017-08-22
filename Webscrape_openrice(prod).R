library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
setwd("Documents/")
rm(list=ls())

Listx = NULL

#data1<-read.csv("ShopList1-20000.csv")
#data2<-read.csv("/home/andrew/Documents/ShopList20001-27259.csv")
#data5<-rbind(data1,data2)
#write.csv(data5, file = "ShopList1-50000.csv")

Listx<-read.csv("/home/andrew/Documents/ShopList1-50000.csv")

dataB<-read.csv("/home/andrew/Documents/ShopID1656-1753.csv")
CommList<-read.csv("/home/andrew/Documents/opdf(ID1-2217).csv")

smile<-read.csv("smile.csv")
ok<-read.csv("ok.csv")
cry<-read.csv("cry.csv")

opdf<-rbind(smile,ok,cry)#,data2,data3,data4,data5,data6,data7,data8,data9,data10)
opdf$X<-NULL
names(opdf)<-c("day","Mth","Yr","id","user","taste","env","serv","hyg","cp","pref")
opdf$date<-paste(opdf$day,opdf$Mth,ifelse(opdf$Yr==99,1999,opdf$Yr+2000),sep='-')
opdf[,c(1:3)]<-NULL
opdf$date1<-dmy(opdf$date)
opdf$date<-NULL
write.csv(dataX, file = "opdf(ID1-1935).csv")

Listx1<-rbind(data1,data2)
Listx$X<-NULL

###################################################

for (query in c(50001:60000)){
  Erroresult  <- tryCatch({
    result.comm <- read_html(paste('https://www.openrice.com/zh/hongkong/r-XXX-r',paste(query,'/reviews?page=1&isListView=True', sep=''),sep=''))
    result.home <- read_html(paste('https://www.openrice.com/zh/hongkong/r-XXX-r',paste(query,'/', sep=''),sep=''))
    check.status<- html_nodes(result.comm, ".poi-with-other-status")
    check.name  <- html_nodes(result.comm, ".poi-name .name")
    check.count <- html_nodes(result.comm, ".table-center .active")
    
    check.add   <- html_nodes(result.home, ".address-info-section .content a")
    check.dist  <- html_nodes(result.home, ".header-poi-district a")
    check.price <- html_nodes(result.home, ".header-poi-price a")
    check.cat   <- html_nodes(result.home, ".header-poi-categories a")
    
    address<-gsub(' ','',gsub("[\r\n]", "",paste(html_text(check.add),collapse="")))
    check1 <- paste(html_text(check.status),collapse=" ")
    name <- paste(html_text(check.name),collapse=" ")
    check3 <- paste(html_text(check.count),collapse=" ")
    
    dist  <- paste(html_text(check.dist),collapse=" ")
    price <- paste(html_text(check.price),collapse=" ")
    cat   <- paste(html_text(check.cat),collapse=" ")
    
    count1 <- as.numeric(gsub("[()]","",gsub(' ','',gsub("食評","",check3))))
    count2 <- ifelse(count1==0,1,count1 %/% 15 + ifelse(count1%%15>0,1,0))
    
    aa <- cbind(name,address,dist,price,cat,query,check1,count1,count2)
    Listx <- rbind(Listx, aa)
    print(query)
    
  }, finally = {
    next
  })}
#write.csv(Listx, file = "highcomm.csv")

###################################################
shop.list<-c(61,90,468,778,1441,1491,1549,1618,1754,1814,1868,2020)
CommList2<-CommList[(CommList$id %in% shop.list), ]
Listx <-read.csv("highcomm.csv")
Listx$smile1<-Listx$smile %/% 15 + ifelse(Listx$smile%%15>0,1,0)
Listx$ok1<-Listx$ok %/% 15 + ifelse(Listx$ok%%15>0,1,0)
Listx$cry1<-Listx$cry %/% 15 + ifelse(Listx$cry%%15>0,1,0)

Listx <-Listx[(Listx$id %in% shop.list), ]

cc=NULL
for (i in 1:12){
  shopid<-as.numeric(Listx[i,6])
  for (page in 1:as.numeric(Listx[i,15])){
    if(Listx[i,8]!=0){
    #urls = paste('https://www.openrice.com/zh/hongkong/r-XXX-r',
    #             paste(shopid,
    #                   paste('/reviews?page=',page,sep=''),'&isListView=True', sep=''),sep='')
    
    #urls2 = paste('https://www.openrice.com/zh/hongkong/r-XXX-r',
    #              paste(shopid,
    #                    paste('/reviews?page=',page,sep=''),'', sep=''),sep='')
    
    urls3 = paste('https://www.openrice.com/zh/hongkong/r-XXX-r',
                 paste(shopid,
                       paste('/reviews?page=',page,sep=''),'&isListView=True&scoreSmile=-1', sep=''),sep='')
    
    read.comm <- read_html(urls3)
    #read.comm2<- read_html(urls2)
    read.comm3<- read_html(urls3)
    
    content   <- paste(html_text(html_nodes(read.comm, ".review-list-user .name")),collapse="&%")
    comm.name <- as.vector(str_split_fixed(content, "&%", 15))
    comm.name <- as.data.frame(comm.name[comm.name != ""])
    
    #content2  <- paste(html_text(html_nodes(read.comm2, ".review-container")),collapse="<-split->")
    #comm.detail <- gsub(' ','',gsub("[\r\n]", "", content2))
    #comm.detail <- as.vector(str_split_fixed(comm.detail, "<-split->", 15))
    #comm.detail <- as.data.frame(comm.detail[comm.detail != ""])

    score <-paste(html_text(html_nodes(read.comm, ".score")),collapse=" ")
    score <-gsub('--', '0', score)
    score <- strsplit(score, " ")
    score <- unlist(score)
    score <- as.numeric(score) 
    score <- as.data.frame(matrix(score,ncol = 5, byrow=T))
    
    day <- paste(html_text(html_nodes(read.comm, ".day")),collapse=" ")
    day <- strsplit(day, " ")
    day <- unlist(day)
    day <- as.numeric(day) 
    day <- as.data.frame(matrix(day))
    
    mthyr <- paste(html_text(html_nodes(read.comm, ".month-year")),collapse=" ")
    mthyr <- strsplit(mthyr, " ")
    mthyr <- unlist(mthyr)
    #mthyr <- as.numeric(mthyr) 
    mthyr <- as.data.frame(matrix(mthyr,ncol = 2, byrow=T))
    
    #pref <- paste(html_nodes(read.comm3, "#sr2-review-container .icon"),collapse="&&&")
    #pref <- gsub("<div class=\"icon or-sprite-inline-block common_smiley_", "", pref)
    #pref <- gsub("_60x60_desktop\">", "", pref)
    #pref <- gsub("_62x60_desktop\">", "", pref)
    #pref <- gsub("</div>", "", pref)
    #pref <- as.vector(str_split_fixed(pref, "&&&", 15))
    #pref <- as.data.frame(pref[pref != ""])
    pref <- "smile"
    
    print(cat("Currently working on shopid: ", shopid, "and page number", page))
    
    bb<-cbind(day,mthyr,shopid,comm.name,score,pref)#comm.detail,
    cc<-rbind(cc,bb)}
  }
}

#cc3<-cc[!cc$shopid==2201,]
write.csv(cc3, file = "ShopID1937-2217.csv")

#add row for blank user
library(DataCombine)
comm.name <- InsertRow(comm.name, NewRow = "NA", RowNum = 13)

