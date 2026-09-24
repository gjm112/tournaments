nfl <- read.csv("./nfl2023.csv")

library(rjags)
library(tidyverse)

nfl <- nfl %>% filter(Winner.tie != "", Loser.tie!="") %>% select(Winner.tie, Loser.tie)

teams <- sort(unique(c(nfl$Winner.tie,nfl$Loser.tie)))

x <- matrix(0, nrow = nrow(nfl), ncol = length(teams))


for (i in 1:50) {
  x[i, which(teams == nfl$Winner.tie[i])] <- -1
  x[i, which(teams == nfl$Loser.tie[i])] <- 1
}

for (i in 51:nrow(nfl)) {
  x[i, which(teams == nfl$Winner.tie[i])] <- 1
  x[i, which(teams == nfl$Loser.tie[i])] <- -1
}

y <- c(rep(0,50),rep(1,nrow(nfl)-50))
a <- glm(y ~ 0 + x, family = "binomial")
a$coefficients[32] <- 0
data.frame(teams,beta = c(a$coefficients)) %>% arrange(beta)


data <- list(
  N = nrow(x), # Number of observations
  J = ncol(x),        # Number of classes
  Y=y,     # Response variable
  X=x      #Predictor matrix Fixed effects
  
)

#With predictors
model_string <- "model{

for (i in 1:N){
  Y[i] ~ dbinom(p[i], 1)
  logit(p[i]) = inprod(X[i,], theta[]) + eps
}  


for (j in 1:(J-1)){
  theta[j] ~ dnorm(0, 0.001)
}

theta[J] <- 0

eps ~ dnorm(0,tau)
tau <- 1/sigma
sigma ~ dnorm(0,0.001)T(0,10000)

}"

setwd("/Users/gregorymatthews/")
write(model_string,"sporttau.bug")

# Run JAGS
jags <- jags.model("/Users/gregorymatthews/sporttau.bug",
                   data = data,
                   n.chains = 3, 
                   n.adapt = 100
)

update(jags, 1000)

# ztest <- jags.samples(jags,c('beta','p','b',"sigma2b"),1000, thin = 1)
# str(ztest)

z <- jags.samples(jags,c('theta','tau'),10000)
z

data.frame(sigma = 1/z$tau[,,1]) %>% ggplot(aes(x = sigma)) + geom_density()
data.frame(teams, beta = apply(z$theta[,,1],1,mean)) %>% arrange(beta)

Yij ~ binom(1,pij)

logit(pij) = gammai - gammaj

gamma[i] ~ dnorm(theta[i],sigma_sport)





