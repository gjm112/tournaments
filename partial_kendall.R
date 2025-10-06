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

