  Listz=NULL
  for (query in c(1:10)){
    tryCatch({
    result.urls<-read_html(paste('https://www.openrice.com/zh/hongkong/r-XXX-r',paste(query,'/reviews?page=1&isListView=True', sep=''),sep=''))
    check.name<-html_nodes(result.urls, ".name")
    Listz=rbind(Listz,query)
    print(query)},
    finally = next)
  }
  
  #to be updated
