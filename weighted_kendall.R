n <- 4
k <- 2
#Greg/Zach method
w <- matrix(0, ncol = n, nrow = 4)
w[1:k, ] <- 1
w[, 1:k] <- 1

#Ryan method
w <- matrix(0, ncol = n, nrow = 4)
w[1:k, 1:k] <- 1
salient_weights <- c(5, 3, 1)
#salient_weights <- c(1,1,1)
k <- 3

weighted_kendall <- function(rhat, k, wtype = NULL, salient_weights = NULL) {
  n <- length(rhat)
  #salient_weights needs to be a vector of length k
  
  if(is.null(salient_weights)){
    salient_weights <- rep(1,k)
  }
  
  if (is.null(wtype)) {
    w <- matrix(0, ncol = n, nrow = n)
    for (q in k:1){
      w[q, ] <- salient_weights[q]
      w[, q] <- salient_weights[q]
    }
    w <- w/2
  }
  
  if (wtype == "gregzach") {
    w <- matrix(0, ncol = n, nrow = n)
    for (q in k:1) {
      w[q, ] <- salient_weights[q]
      w[, q] <- salient_weights[q]
    }
    #w[upper.tri(w)] <- w[upper.tri(w)]/2
    #w[lower.tri(w)] <- w[lower.tri(w)]/2
  }
  
  #Ryan method
  if (wtype == "rydog"){
  w <- matrix(0, ncol = n, nrow = n)
#  w[1:k, 1:k] <- 1
  for (q in k:1) {
    w[1:q, 1:q] <- salient_weights[q]
    w[1:q, 1:q] <- salient_weights[q]
  }
  #w[upper.tri(w)] <- w[upper.tri(w)]/2
  #w[lower.tri(w)] <- w[lower.tri(w)]/2
  }
  
  
  summ <- 0
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        summ <- summ + w[i, j] * sign(i - j) * sign(rhat[i] - rhat[j])
      }
      
    }
  }
  
  out <- summ / (sum(w) - sum(diag(w)))
  weight <- (out + 1) / 2
  return(weight)
  
}




weighted_kendall(
  rhat = c(1,2,4,3),
  k = 4,
  wtype = "gregzach"
)

weighted_kendall(
  rhat = c(1, 2, 4, 3:50),
  k = 5,
  wtype = "gregzach",
  salient_weights = c(1,1,1,1,1)
)

weighted_kendall(rhat = c(4,1,3,50,2,5:49), k = 1, wtype = "gregzach",salient_weights = c(1))

weighted_kendall(rhat = c(1,2,49,50,3:48), k = 4, wtype = "rydog")
weighted_kendall(rhat = c(1,2,49,50,3:48), k = 4, wtype = "gregzach")

weighted_kendall(rhat = c(1,2,4,3), k = 2)
weighted_kendall(rhat = c(1,2,3,4), k = 2)






#vec <- c(1,2,4,3)
partial_kendall <- function(vec, k){
  
  n <- length(vec)
  if (k == n){k <- n-1}
  ref <- c(1:n)
  count <- 0
  for (i in 1:k){
    for (j in (i+1):n){
      add <-  (((vec[i] < vec[j]) & (ref[i] < ref[j])) |
                 ((vec[i] > vec[j]) & (ref[i] > ref[j]))) + 0
      #print(c(i, j, add))
      count <- count + add
    }
  }
  weight <-  (count / sum(n - (1:k)))
  return(weight) 
}

#Squared distance
#Truncated kendall's
#Number of discordant pairs. 
#Spearman's rho

#Spearman rho
(cor(c(1,2,3), c(3,2,1), method = "spearman") + 1)/2
(cor(c(1,2,3), c(1,2,3), method = "spearman") + 1)/2
(cor(c(1,2), c(1,2), method = "spearman") + 1)/2

(cor(c(1,2,3), c(20,40,60), method = "spearman") + 1)/2



1/(sum((c(1,2,3) - c(3,2,1))^2) + 1)
partial_kendall(c(3,2,1),k= 3)

partial_kendall(c(1,2,3,4),k= 1)
partial_kendall(c(1,2,3,4),k= 2)
partial_kendall(c(1,2,3,4),k= 3)
partial_kendall(c(1,2,3,4),k= 4)

partial_kendall(c(1,2,3,4),k= 1)


partial_kendall(c(4,3,2,1),k= 3)
partial_kendall(c(1,3,2,4),k= 4)
partial_kendall(c(4,2,3,1),k= 1)
partial_kendall(c(3,2,4,1),k= 4)

partial_kendall(c(1,3,2,4:50),k= 3)
partial_kendall(c(1,3,2),k= 3)

partial_kendall(c(1,3,4:50,2),k= 3)
partial_kendall(c(1,3,2,4:50),k= 3)
partial_kendall(c(50,3,2,4:49,1),k= 3)
partial_kendall(c(2,3,50,4:49,1),k= 3)

