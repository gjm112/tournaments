library(combinat)
tourn <- permn(1:8)
tourn <- do.call(rbind,tourn)

dist <- apply(tourn, 1, function(x){
  if (sum(x[c(5:8)] == 1)>0){x[c(1:8)] <- x[c(5:8,1:4)]}
  
  if (sum(x[c(3:4)] == min(x[1:4]))>0){x[c(1:4)] <- x[c(3:4,1:2)]}
  if (sum(x[c(7:8)] == min(x[5:8]))>0){x[c(5:8)] <- x[c(7:8,5:6)]}
  
  for (i in c(1,3,5,7)){
    x[c(i:(i+1))] <- sort(x[c(i:(i+1))])
  }
  return(x)
})

dist <- t(dist)

test <- dist[!duplicated(dist),]
dim(test)


#as.data.frame(test) %>% arrange(V1, V2)