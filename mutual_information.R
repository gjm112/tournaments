dat <- expand.grid(x = c("12","21"),
                   y = c("12","21"))
p <- 0.9

f <- function(p){
dat$prob <- c(p,1-p,1-p,p)/2
sum(dat$prob*log(dat$prob/(0.5*0.5),2))
}
f <- Vectorize(f)
plot(0:999/1000,f(0:999/1000),xlab = "Prob Team 1 beats Team 2", ylab = "Bits", type = "l")




dat <- expand.grid(x = c("12"),
                   y = c("12","21"))
p <- 0.9

f <- function(p){
  dat$prob <- c(p,1-p)/2
  2*sum(dat$prob*log(dat$prob/(0.5*0.5),2))
}
f <- Vectorize(f)
plot(0:999/1000,f(0:999/1000),xlab = "Prob Team 1 beats Team 2", ylab = "Bits", type = "l")


dat <- expand.grid(x = c("123"),
                   y = c("123","132","213","231","312","321"))

p <- 0.9
dat$prob <- c(p,rep((1-p)/5,5))/factorial(3)
factorial(3)*sum(dat$prob*log(dat$prob/(1/factorial(3)*1/factorial(3)),2))
}

p <- 1/6 
f <- function(p){
  dat$prob <- c(rep((1-p)/5,5),p)/factorial(3)
  factorial(3)*sum(dat$prob*log(dat$prob/(1/factorial(3)*1/factorial(3)),2))
}
f <- Vectorize(f)
plot(0:999/1000,f(0:999/1000),xlab = "Prob Team 1 beats Team 2", ylab = "Bits", type = "l")
abline(v = 1/6)

f <- function(p){
  dat$prob <- c(0.01,0.95,0.01,0.01,0.01,0.01)/factorial(3)
  factorial(3)*sum(dat$prob*log(dat$prob/(1/factorial(3)*1/factorial(3)),2))
}
f <- Vectorize(f)
points(0:999/1000,f(0:999/1000),xlab = "Prob Team 1 beats Team 2", ylab = "Bits", type = "l")
abline(v = 1/6)



joint * log(joint/(marginx*marginy))


dat <- expand.grid(x = c("123","132","213","231","312","321"),
           y = c("123","132","213","231","312","321"))
dat <- expand.grid(x = c("123"),
                   y = c("123","132","213","231","312","321"))

dat <- data.frame(x1 = 1,
                   x2 = 2,
                   x3 = 3,
                   y1 = c(1,1,2,2,3,3),
                   y2 = c(2,3,1,3,1,2),
                   y3 = c(3,2,3,1,2,1), 
                  p = c(.1,.1,.1,.1,.1,0.5))
#weighted ranks          
q <- c(100,0,0)   
dat$w <- 1/(abs(dat$x1-dat$y1) + abs(dat$x2-dat$y2) + abs(dat$x3-dat$y3))
dat$w[1] <- 1
dat$w <- dat$w / sum(dat$w)

dat$w <- 1/(q[1]*abs(dat$x1-dat$y1)^2 + q[2]*abs(dat$x2-dat$y2)^2 + q[3]*abs(dat$x3-dat$y3)^2)
dat$w[1:2] <- 1
dat$w <- dat$w / sum(dat$w)
# dat$w <- 1/(abs(dat$x1-dat$y1)^2 + abs(dat$x2-dat$y2)^2 + abs(dat$x3-dat$y3)^2)
# dat$w[1] <- 1
# dat$w <- dat$w / sum(dat$w)
# 
p <- 1/6 
f <- function(p){
  dat$prob <- c(p,rep((1-p)/5,5))/factorial(3)
  factorial(3)*sum(dat$w*dat$prob*log(dat$prob/(1/factorial(3)*1/factorial(3)),2))
}
f <- Vectorize(f)
plot(0:999/1000,f(0:999/1000),xlab = "Prob Team 1 beats Team 2", ylab = "Bits", type = "l")
abline(h = 0)
abline(v = 1/6)

f <- function(p){
  dat$prob <- c(rep((1-p)/5,5),p)/factorial(3)
  factorial(3)*sum(dat$w*dat$prob*log(dat$prob/(1/factorial(3)*1/factorial(3)),2))
}
f <- Vectorize(f)
points(0:999/1000,f(0:999/1000),xlab = "Prob Team 1 beats Team 2", ylab = "Bits", type = "l", col = "red")
abline(h = 0)
abline(v = 1/6)



#########################
#4 teams
#########################
#dat <- expand.grid(x = c("123","132","213","231","312","321"),
                   y = c("123","132","213","231","312","321"))
dat <- expand.grid(x = c("123"),
                   y = c("123","132","213","231","312","321"))
library(combinat)
perms <- do.call(rbind,permn(1:4))

dat <- data.frame(x1 = 1,
                  x2 = 2,
                  x3 = 3,
                  x4 = 4,
                  y1 = perms[,1],
                  y2 = perms[,2],
                  y3 = perms[,3],
                  y4 = perms[,4],
                  p = NA)

dat$w <- 1/(abs(dat$x1-dat$y1) + abs(dat$x2-dat$y2) + abs(dat$x3-dat$y3) + abs(dat$x4 - dat$y4))
dat$w[1] <- 1
dat$w <- dat$w / sum(dat$w)

dat$w <- 1/(abs(dat$x1-dat$y1)^2 + abs(dat$x2-dat$y2)^2 + abs(dat$x3-dat$y3)^2 + abs(dat$x4-dat$y4)^2)
dat$w[1] <- 1
dat$w <- dat$w / sum(dat$w)

p <- 1/6 
dat <- dat %>% arrange(-w)
f <- function(p){
  dat$prob <- c(p,rep((1-p)/23,23))/factorial(4)
  factorial(4)*sum(dat$w*dat$prob*log(dat$prob/(1/factorial(4)*1/factorial(4)),2))
}
f <- Vectorize(f)
plot(0:999/1000,f(0:999/1000),xlab = "Prob Team 1 beats Team 2", ylab = "Bits", type = "l")
abline(h = 0)
abline(v = 1/6)


f <- function(p){
  dat$prob <- c(rep((1-p)/23,23),p)/factorial(4)
  factorial(4)*sum(dat$w*dat$prob*log(dat$prob/(1/factorial(4)*1/factorial(4)),2))
}
f <- Vectorize(f)
points(0:999/1000,f(0:999/1000),xlab = "Prob Team 1 beats Team 2", ylab = "Bits", type = "l", col = "red")
abline(h = 0)
abline(v = 1/24)



      