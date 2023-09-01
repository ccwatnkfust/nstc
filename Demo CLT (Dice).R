# a dice population: an uniform distribution
curve(dunif(x, 1, 6), from=0, to=7)

# tossing a dice many times (sampling)
rv <- sample(1:6,100,TRUE);rv
d <- density(rv) # returns the density data 
plot(d) # plots the results
x11()
hist(rv)
x11()
hist(rv, right = FALSE)

# The "dice" mean distribution (C.L.T)
xbar=rep(0,500)
for (i in 1:500) {
 xbar[i]=mean(sample(1:6,100,TRUE))
 }
xbar
table(cut(xbar,breaks=c(2.9,3.2,3.4,3.6,3.8,4)))
hist(xbar)
x11()
hist(xbar, prob=TRUE, col="grey") 
lines(density(xbar), col="blue", lwd=2)
