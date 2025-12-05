
theta <- c()
theta <- qnorm((4:1)/5)
theta <- qunif((4:1)/5,0,sqrt(12))
theta <- c(0,0,0,0)
p <- matrix(NA, ncol = 4, nrow = 4)
for (i in 1:4){
  for (j in 1:4){
    p[i,j] <- exp(theta[i] - theta[j])/(1+ exp(theta[i] - theta[j]))
  }
}

perms$p <- 0
perms$p[1] <- p[1,4]*p[2,3]*p[1,2]*.5
perms$p[2] <- p[1,4]*p[2,3]*p[1,2]*.5
perms$p[7] <- p[1,4]*p[3,2]*p[1,3]*.5
perms$p[8] <- p[1,4]*p[3,2]*p[1,3]*.5
perms$p[9] <- p[1,4]*p[3,2]*p[3,1]*.5
perms$p[10] <- p[1,4]*p[3,2]*p[3,1]*.5

perms$p[11] <- p[4,1]*p[3,2]*p[3,4]*.5
perms$p[12] <- p[4,1]*p[3,2]*p[3,4]*.5
perms$p[13] <- p[4,1]*p[3,2]*p[4,3]*.5
perms$p[14] <- p[4,1]*p[3,2]*p[4,3]*.5

perms$p[19] <- p[4,1]*p[2,3]*p[2,4]*.5
perms$p[20] <- p[4,1]*p[2,3]*p[4,2]*.5
perms$p[21] <- p[4,1]*p[2,3]*p[4,2]*.5
perms$p[22] <- p[4,1]*p[2,3]*p[2,4]*.5

perms$p[23] <- p[1,4]*p[2,3]*p[2,1]*.5
perms$p[24] <- p[1,4]*p[2,3]*p[2,1]*.5

#weights for k = 4
perms$w4 <- c(1,5/6,2/3,1/2, 1/3, 1/2, 2/3, 5/6,
             2/3,1/2,1/3,1/6,0,1/6,1/3,1/2,2/3,1/2,
             1/3,1/6,1/3,1/2,2/3,5/6)

perms$w1 <- c(1,1,1,2/3,2/3,1,1,1,
              2/3,2/3,1/3,1/3,0,0,0,1/3,
              1/3,0,0,0,1/3,1/3,2/3,2/3)

sub <- perms %>% filter(p > 0)
sum(sub$w4 * sub$p * log(sub$p/((1/24)^2)))/log(24^2)
sum(sub$w1 * sub$p * log(sub$p/((1/24)^2)))/log(24^2)


perms$p <- 0
for (i in 1:24){
  if (all(perms[i, 1:2] %in% c(1, 4))) {
    perms$p[i] <- 0
  } else if (all(perms[i, 1:2] %in% c(2, 3))) {
    perms$p[i] <- 0
  } else {
    perms$p[i] <- p[perms$V1[i],5 - perms$V1[i]]*p[perms$V2[i],5 - perms$V2[i]]*p[perms$V1[i],perms$V2[i]]*p[perms$V3[i],perms$V4[i]]
  }
  
  
}


#weights for k = 4
perms$w4 <- c(1,5/6,2/3,1/2, 1/3, 1/2, 2/3, 5/6,
              2/3,1/2,1/3,1/6,0,1/6,1/3,1/2,2/3,1/2,
              1/3,1/6,1/3,1/2,2/3,5/6)

perms$w1 <- c(1,1,1,2/3,2/3,1,1,1,
              2/3,2/3,1/3,1/3,0,0,0,1/3,
              1/3,0,0,0,1/3,1/3,2/3,2/3)

sub <- perms %>% filter(p > 0)
sum(sub$w4 * sub$p * log(sub$p/((1/24)^2)))/log(24^2)
sum(sub$w1 * sub$p * log(sub$p/((1/24)^2)))/log(24^2)


