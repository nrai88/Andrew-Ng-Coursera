rm(list=ls())
gc()

# set working directory
setwd()

dat = read.csv('ex1data1.txt', header=F)

X = matrix(c(rep(1,nrow(dat)),dat$V1),nrow=nrow(dat),byrow=F)
theta = as.matrix(c(0,0))
y = as.matrix(dat$V2)

computeCost = function(X, y, theta) {
  sum((X %*% theta - y)**2)/(2*length(y))
}

computeCost(X, y, theta)


gradientDescent = function (X, y, theta, alpha, iterations) {
  a = vector()
  for(i in 1:iterations) {
    #cat(computeCost(X, y, theta),'\n')
    theta0 = theta[1,1] - alpha*sum((X %*% theta - y))/(length(y))
    theta1 = theta[2,1] - alpha*sum((X %*% theta - y)*X[,2])/(length(y))
    theta[1,1] = theta0
    theta[2,1] = theta1
    a = c(a,computeCost(X, y, theta))
  }
  cat(theta)
  return(a)
}


a  = gradientDescent(X, y, theta, alpha, 1500)
which(a==min(a))