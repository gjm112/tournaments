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

datlist <- list()
n <- 8
theta <- qunif(n:1/(n+1),0,sqrt(12)) #uniform with same variance as standard normal. 
#theta <- qnorm(n:1/(n+1)) #standard normal

expit <- function(xb){
  exp(xb)/(1+exp(xb))
}

expit(theta[1]-theta[2])

seeds <- matrix(c(1,8,2,3,4,6,5,7),byrow = TRUE, ncol = 2)
seeds <- matrix(c(1,8,6,7,2,3,4,5),byrow = TRUE, ncol = 2)
seeds <- matrix(c(1, 5, 2, 6, 3, 7, 4, 8),byrow = TRUE, ncol = 2)
seeds <- matrix(c(1, 2, 3,8, 4, 7, 5,6),byrow = TRUE, ncol = 2)

#seeds <- matrix(c(1,8,4,5,3,6,2,7),byrow = TRUE, ncol = 2)

#Round 1
expit(theta[1]-theta[8])

getprobs <- function(seeds){
seeds <- matrix(seeds,byrow = TRUE, ncol = 2)
dat <- data.frame(Team = 1:8, probadvsemi= NA, probadvfinal = NA, probwin = NA)

dat$probadvsemi[seeds[,1]] <- apply(seeds,1,function(x){expit(theta[x[1]]-theta[x[2]])})
dat$probadvsemi[seeds[,2]] <- 1-apply(seeds,1,function(x){expit(theta[x[1]]-theta[x[2]])})

#Prob advance to finals
#top half
dat$probadvfinal[seeds[1,1]] <-  dat$probadvsemi[seeds[1,1]]*(expit(theta[seeds[1,1]]-theta[seeds[2,1]])*dat$probadvsemi[seeds[2,1]] + expit(theta[seeds[1,1]]-theta[seeds[2,2]])*dat$probadvsemi[seeds[2,2]])
dat$probadvfinal[seeds[1,2]] <-  dat$probadvsemi[seeds[1,2]]*(expit(theta[seeds[1,2]]-theta[seeds[2,1]])*dat$probadvsemi[seeds[2,1]] + expit(theta[seeds[1,2]]-theta[seeds[2,2]])*dat$probadvsemi[seeds[2,2]])
dat$probadvfinal[seeds[2,1]] <-  dat$probadvsemi[seeds[2,1]]*(expit(theta[seeds[2,1]]-theta[seeds[1,1]])*dat$probadvsemi[seeds[1,1]] + expit(theta[seeds[2,1]]-theta[seeds[1,2]])*dat$probadvsemi[seeds[1,2]])
dat$probadvfinal[seeds[2,2]] <-  dat$probadvsemi[seeds[2,2]]*(expit(theta[seeds[2,2]]-theta[seeds[1,1]])*dat$probadvsemi[seeds[1,1]] + expit(theta[seeds[2,2]]-theta[seeds[1,2]])*dat$probadvsemi[seeds[1,2]])
#Bottom half 
dat$probadvfinal[seeds[3,1]] <-  dat$probadvsemi[seeds[3,1]]*(expit(theta[seeds[3,1]]-theta[seeds[4,1]])*dat$probadvsemi[seeds[4,1]] + expit(theta[seeds[3,1]]-theta[seeds[4,2]])*dat$probadvsemi[seeds[4,2]])
dat$probadvfinal[seeds[3,2]] <-  dat$probadvsemi[seeds[3,2]]*(expit(theta[seeds[3,2]]-theta[seeds[4,1]])*dat$probadvsemi[seeds[4,1]] + expit(theta[seeds[3,2]]-theta[seeds[4,2]])*dat$probadvsemi[seeds[4,2]])
dat$probadvfinal[seeds[4,1]] <-  dat$probadvsemi[seeds[4,1]]*(expit(theta[seeds[4,1]]-theta[seeds[3,1]])*dat$probadvsemi[seeds[3,1]] + expit(theta[seeds[4,1]]-theta[seeds[3,2]])*dat$probadvsemi[seeds[3,2]])
dat$probadvfinal[seeds[4,2]] <-  dat$probadvsemi[seeds[4,2]]*(expit(theta[seeds[4,2]]-theta[seeds[3,1]])*dat$probadvsemi[seeds[3,1]] + expit(theta[seeds[4,2]]-theta[seeds[3,2]])*dat$probadvsemi[seeds[3,2]])

#Prob win 
#Top half
dat$probwin[seeds[1,1]] <- dat$probadvfinal[seeds[1,1]]*(expit(theta[seeds[1,1]]-theta[seeds[3,1]])*dat$probadvfinal[seeds[3,1]] + 
                                                              expit(theta[seeds[1,1]]-theta[seeds[3,2]])*dat$probadvfinal[seeds[3,2]] + 
                                                                expit(theta[seeds[1,1]]-theta[seeds[4,1]])*dat$probadvfinal[seeds[4,1]] + 
                                                                expit(theta[seeds[1,1]]-theta[seeds[4,2]])*dat$probadvfinal[seeds[4,2]])

dat$probwin[seeds[1,2]] <- dat$probadvfinal[seeds[1,2]]*(expit(theta[seeds[1,2]]-theta[seeds[3,1]])*dat$probadvfinal[seeds[3,1]] + 
                                                           expit(theta[seeds[1,2]]-theta[seeds[3,2]])*dat$probadvfinal[seeds[3,2]] + 
                                                           expit(theta[seeds[1,2]]-theta[seeds[4,1]])*dat$probadvfinal[seeds[4,1]] + 
                                                           expit(theta[seeds[1,2]]-theta[seeds[4,2]])*dat$probadvfinal[seeds[4,2]])



dat$probwin[seeds[2,1]] <- dat$probadvfinal[seeds[2,1]]*(expit(theta[seeds[2,1]]-theta[seeds[3,1]])*dat$probadvfinal[seeds[3,1]] + 
                                                           expit(theta[seeds[2,1]]-theta[seeds[3,2]])*dat$probadvfinal[seeds[3,2]] + 
                                                           expit(theta[seeds[2,1]]-theta[seeds[4,1]])*dat$probadvfinal[seeds[4,1]] + 
                                                           expit(theta[seeds[2,1]]-theta[seeds[4,2]])*dat$probadvfinal[seeds[4,2]])

dat$probwin[seeds[2,2]] <- dat$probadvfinal[seeds[2,2]]*(expit(theta[seeds[2,2]]-theta[seeds[3,1]])*dat$probadvfinal[seeds[3,1]] + 
                                                           expit(theta[seeds[2,2]]-theta[seeds[3,2]])*dat$probadvfinal[seeds[3,2]] + 
                                                           expit(theta[seeds[2,2]]-theta[seeds[4,1]])*dat$probadvfinal[seeds[4,1]] + 
                                                           expit(theta[seeds[2,2]]-theta[seeds[4,2]])*dat$probadvfinal[seeds[4,2]])

#bottom Half
dat$probwin[seeds[3,1]] <- dat$probadvfinal[seeds[3,1]]*(expit(theta[seeds[3,1]]-theta[seeds[1,1]])*dat$probadvfinal[seeds[1,1]] + 
                                                           expit(theta[seeds[3,1]]-theta[seeds[1,2]])*dat$probadvfinal[seeds[1,2]] + 
                                                           expit(theta[seeds[3,1]]-theta[seeds[2,1]])*dat$probadvfinal[seeds[2,1]] + 
                                                           expit(theta[seeds[3,1]]-theta[seeds[2,2]])*dat$probadvfinal[seeds[2,2]])

dat$probwin[seeds[3,2]] <- dat$probadvfinal[seeds[3,2]]*(expit(theta[seeds[3,2]]-theta[seeds[1,1]])*dat$probadvfinal[seeds[1,1]] + 
                                                           expit(theta[seeds[3,2]]-theta[seeds[1,2]])*dat$probadvfinal[seeds[1,2]] + 
                                                           expit(theta[seeds[3,2]]-theta[seeds[2,1]])*dat$probadvfinal[seeds[2,1]] + 
                                                           expit(theta[seeds[3,2]]-theta[seeds[2,2]])*dat$probadvfinal[seeds[2,2]])

dat$probwin[seeds[4,1]] <- dat$probadvfinal[seeds[4,1]]*(expit(theta[seeds[4,1]]-theta[seeds[1,1]])*dat$probadvfinal[seeds[1,1]] + 
                                                           expit(theta[seeds[4,1]]-theta[seeds[1,2]])*dat$probadvfinal[seeds[1,2]] + 
                                                           expit(theta[seeds[4,1]]-theta[seeds[2,1]])*dat$probadvfinal[seeds[2,1]] + 
                                                           expit(theta[seeds[4,1]]-theta[seeds[2,2]])*dat$probadvfinal[seeds[2,2]])

dat$probwin[seeds[4,2]] <- dat$probadvfinal[seeds[4,2]]*(expit(theta[seeds[4,2]]-theta[seeds[1,1]])*dat$probadvfinal[seeds[1,1]] + 
                                                           expit(theta[seeds[4,2]]-theta[seeds[1,2]])*dat$probadvfinal[seeds[1,2]] + 
                                                           expit(theta[seeds[4,2]]-theta[seeds[2,1]])*dat$probadvfinal[seeds[2,1]] + 
                                                           expit(theta[seeds[4,2]]-theta[seeds[2,2]])*dat$probadvfinal[seeds[2,2]])

return(dat)

}


seeds <- matrix(c(1,8,2,3,4,6,5,7),byrow = TRUE, ncol = 2)
datlist[[paste0((c(t(seeds))),collapse = "")]] <- getprobs(seeds)
seeds <- matrix(c(1,8,6,7,2,3,4,5),byrow = TRUE, ncol = 2)
datlist[[paste0((c(t(seeds))),collapse = "")]] <- getprobs(seeds)
seeds <- matrix(c(1, 5, 2, 6, 3, 7, 4, 8),byrow = TRUE, ncol = 2)
datlist[[paste0((c(t(seeds))),collapse = "")]] <- getprobs(seeds)
seeds <- matrix(c(1, 2, 3,8, 4, 7, 5,6),byrow = TRUE, ncol = 2)
datlist[[paste0((c(t(seeds))),collapse = "")]] <- getprobs(seeds)


probs <- apply(test,1,getprobs)
names(probs) <- apply(test,1,function(x){paste0(x,collapse = "")})
mat <- do.call(rbind,lapply(probs, function(x){x$probwin}))
#Best case for each team 
row.names(mat)[apply(mat,2,which.max)]
row.names(mat)[apply(mat,2,which.min)]

hist(mat[,3])



# for (i in 1:4){
# dat$probadvsemi[seeds[i,1]] <- expit(theta[seeds[i,1]] - theta[seeds[i,2]])
# dat$probadvsemi[seeds[i,2]] <- 1-expit(theta[seeds[i,1]] - theta[seeds[i,2]])
# }

 
#Expected wins
do.call(rbind,lapply(datlist,function(z){apply(z[,-1],1,function(x){c(x[1]-x[2],x[2]-x[3],x[3]) %*% c(1:3)})}))

