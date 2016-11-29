rm(list=ls())
gc()

# set working directory
setwd()

dat = read.csv('../machine-learning-ex1/ex1/ex1data1.txt', header=F)

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
    theta = theta-alpha*(t(X)%*%(X %*% theta - y))/length(y)
    # crossprod((X %*% theta - y),X)
    a = c(a,computeCost(X, y, theta))
  }
  cat(theta)
  return(a)
}

alpha = 0.01
a  = gradientDescent(X, y, theta, alpha, 1500)
which(a==min(a))

require(ggplot2)
plot_data = data.frame(value=a,iteration=1:length(a))
ggplot(plot_data,aes(y=a,x=iteration))+geom_point(size=0.25)+theme_bw()

dat2 = read.csv('../machine-learning-ex1/ex1/ex1data2.txt',header=F)
X = dat2[]
featureNormalize = function(X) {
  apply(X,2,mean)
}