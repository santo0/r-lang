# Question 1
# P(X <= 2) = ?
# p = 6/10000, n=1000
# binomial and poisson (lambda is expected value which is EV=n*p)
# P(X <= 2) = P(0)+P(1)+P(2)

p<-6/10000
n<-1000
ev <- p*n
vec_p1<-rpois(n, ev)

pp <- numeric() # vector for storing probabilities
for (i in 1:3){
  pp[i] <- dpois(i-1, ev)
}
sum(pp) # cummulative probability P (X<=2)

pb <- numeric()
for (i in 1:3){
  pb[i] <- dbinom(i-1, n,p)
}
sum(pb)

# Question 2
# P(X>=2)=? 1-P(X<=1)
p<-1/1200
n<-300
ev <- p*n

pp <- numeric() # vector for storing probabilities
for (i in 1:2){
  pp[i] <- dpois(i-1, ev)
}
1-sum(pp)

# Question 3
lam <- 4
n <- 100

ceiling(n*dpois(0, lam)) # number of days without accidents

pp <- numeric() # vector for storing probabilities
for (i in 1:2){
  pp[i] <- dpois(i-1, lam)
}
ceiling(n*(1-sum(pp))) # num of days with at least two accidents
ceiling((1-ppois(1,lam))*n)

pp <- numeric() # vector for storing probabilities
for (i in 1:4){
  pp[i] <- dpois(i-1, lam)
}
n*sum(pp) # num of days with at most 3 accidents
ceiling(ppois(3,lam)*n)

# Question 4
fans<-rexp(10000, 0.0003)
plot(density(fans))
nfans <- rexp(10000, 0.00035)
lines(density(nfans),col=2)

l<-0.0003
1-pexp(10000,l)
1-pexp(10000,0.00035)

# Question 5
r <-1/8
pexp(5,r)
qexp(1-0.95,r) # minutes
qexp(1-0.95,r)*60 # seconds
