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

nfc_theta <- theta %>% filter(team %in%
  c("Atlanta Falcons", "Chicago Bears", "Philadelphia Eagles", "Seattle Seahawks", "New Orleans Saints", "Green Bay Packers")) %>%
  pull(theta)

afc_theta <- theta %>% filter(team %in%
  c("New England Patriots", "Pittsburgh Steelers", "Indianapolis Colts", "Kansas City Chiefs", "Baltimore Ravens", "New York Jets")) %>%
  pull(theta)

desired_nfc_order <- c(
  "Atlanta Falcons",
  "Chicago Bears",
  "Philadelphia Eagles",
  "Seattle Seahawks",
  "New Orleans Saints",
  "Green Bay Packers"
)

nfc_teams <- theta %>%
  filter(team %in% desired_nfc_order) %>%
  slice(match(desired_nfc_order, team)) %>%
  pull(team)

desired_afc_order <- c(
  "New England Patriots",
  "Pittsburgh Steelers",
  "Indianapolis Colts",
  "Kansas City Chiefs",
  "Baltimore Ravens",
  "New York Jets"
)

afc_teams <- theta %>%
  filter(team %in% desired_afc_order) %>%
  slice(match(desired_afc_order, team)) %>%
  pull(team)

