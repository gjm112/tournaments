#https://www.pro-football-reference.com/years/2010/games.htm
library(tidyverse)
nfl <- read.csv("./NFL2010.csv")
nfl <- nfl %>% mutate(Week = as.numeric(Week)) %>% filter(Week <= 17) %>% mutate(home = ifelse(X == "@",Loser.tie, Winner.tie),
                                                                                 away = ifelse(X== "@", Winner.tie, Loser.tie))
teams <- sort(unique(nfl$Winner.tie))
x <- matrix(0, nrow = nrow(nfl), ncol = length(teams))
for (i in 1:nrow(nfl)){
  for (j in 1:length(teams)){
    x[i,which(nfl$home[i] == teams)] <- 1
    x[i,which(nfl$away[i] == teams)] <- -1
  }
}

a <- glm((nfl$home == nfl$Winner.tie) ~ x, family = "binomial")
theta <- data.frame(
  team = c("int",teams),
  theta = a$coefficients)
theta$theta[length(theta$theta)] <- 0
theta %>% arrange(-theta)


