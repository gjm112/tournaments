tourn <- do.call(rbind, args = combinat::permn(1:8))

dist <- apply(X = tourn, MARGIN = 1, FUN = function(x){
  if (sum(x[5:8] == 1) > 0) {
    x[1:8] <- x[c(5:8, 1:4)]
  }
  if (sum(x[3:4] == min(x[1:4])) > 0) {
    x[1:4] <- x[c(3:4, 1:2)]
  }
  if (sum(x[7:8] == min(x[5:8])) > 0) {
    x[5:8] <- x[c(7:8, 5:6)]
  }
  for (i in c(1, 3, 5, 7)) {
    x[i:(i + 1)] <- sort(x[i:(i + 1)])
  }
return(x)
})

dist <- t(dist)
test <- dist[!duplicated(dist), ]
dim(test)

