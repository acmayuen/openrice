library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

##############################################################################
### checking validity of id for restaurant and downloading the basic info
##############################################################################
Listx = NULL
for (query in c(1:10)){
  Erroresult  <- tryCatch({
    ###################################################################################################################
    ###Important Note: I am not disclosing the url and node as the webscrabper may produce lots of traffic for openrice
    ###################################################################################################################
    result.comm <- read_html(paste('https://www.openrice.com/zh/hongkong/xxx',paste(query,'/xxx', sep=''),sep=''))
    result.home <- read_html(paste('https://www.openrice.com/zh/hongkong/xxx',paste(query,'/', sep=''),sep=''))
    check.status<- html_nodes(result.comm, ".xxx-status")
    check.name  <- html_nodes(result.comm, ".xxx .name")
    check.count <- html_nodes(result.comm, ".xxx .active")
    
    check.add   <- html_nodes(result.home, ".xxx a")
    check.dist  <- html_nodes(result.home, ".xxx a")
    check.price <- html_nodes(result.home, ".xxx a")
    check.cat   <- html_nodes(result.home, ".xxx a")
    
    address <-gsub(' ','',gsub("[\r\n]", "",paste(html_text(check.add),collapse="")))
    check1  <- paste(html_text(check.status),collapse=" ")
    name    <- paste(html_text(check.name),collapse=" ")
    check3  <- paste(html_text(check.count),collapse=" ")
    
    dist    <- paste(html_text(check.dist),collapse=" ")
    price   <- paste(html_text(check.price),collapse=" ")
    cat     <- paste(html_text(check.cat),collapse=" ")
    
    count1 <- as.numeric(gsub("[()]","",gsub(' ','',gsub("食評","",check3))))
    count2 <- ifelse(count1==0,1,count1 %/% 15 + ifelse(count1%%15>0,1,0))
    
    aa <- cbind(name,address,dist,price,cat,query,check1,count1,count2)
    Listx <- rbind(Listx, aa)
    print(query)
    
  }, finally = {
    next
  })}
write.csv(Listx, file = "shoplist.csv")

##############################################################################
### Extract comments from 10 restaurants based on the id validated from above process
##############################################################################
cc=NULL
for (i in 1:10){
  shopid<-as.numeric(Listx[i,6])
  for (page in 1:as.numeric(Listx[i,8])){
    if(Listx[i,8]!=0){
    urls = paste('https://www.openrice.com/zh/hongkong/r-XXX-r',
                 paste(shopid,
                       paste('/reviews?page=',page,sep=''),'&isListView=True', sep=''),sep='')
    
    urls2 = paste('https://www.openrice.com/zh/hongkong/r-XXX-r',
                  paste(shopid,
                        paste('/reviews?page=',page,sep=''),'', sep=''),sep='')
    
    urls3 = paste('https://www.openrice.com/zh/hongkong/r-XXX-r',
                 paste(shopid,
                       paste('/reviews?page=',page,sep=''),'&isListView=True&scoreSmile=-1', sep=''),sep='')
    
    read.comm <- read_html(urls1)
    read.comm2<- read_html(urls2)
    
    content   <- paste(html_text(html_nodes(read.comm, ".review-list-user .name")),collapse="&%")
    comm.name <- as.vector(str_split_fixed(content, "&%", 15))
    comm.name <- as.data.frame(comm.name[comm.name != ""])
    
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
    mthyr <- as.data.frame(matrix(mthyr,ncol = 2, byrow=T))
    
    print(cat("Currently working on shopid: ", shopid, "and page number", page))
    
    bb<-cbind(day,mthyr,shopid,comm.name,score)
    cc<-rbind(cc,bb)}
  }
}

write.csv(cc, file = "shopcomm.csv")

