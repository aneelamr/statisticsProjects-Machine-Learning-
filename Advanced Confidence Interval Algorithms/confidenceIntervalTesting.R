#'---
#'title: homework 8
#'author: Aditya Neelamraju
#'date: 6/2/2020
#'---
#' #q1
#using avocado data set
fpath = paste0(getwd(),"/avocado.csv")

ksmooth.cv = function(x,y, kernel, K = 5){
  n = length(x)
  #remove the last few elements
  if(n%%K !=0){
    head(x, -(n%%K))
  }
  n = length(x)
  pi = sample(1:n)
  xpi = numeric(n)
  ypi = numeric(n)
  #permutate and populate vecotrs
  for(i in 1:n){
    xpi[i] = x[pi[i]] 
    ypi[i] = y[pi[i]]
  }
  MSE = numeric(K)
  Eh = numeric(K)
  h = (max(x)-min(x))/2
  xpointVec = numeric(K)
  ypoints = numeric(K)
  ypointsTrain = numeric(K)
  
  #' find folds and calculate validation points
  for(k in K){
    curxfold = xpi[c((n/K)*(k-1)+1:k*(n/K))]
    curyfold = ypi[c((n/K)*(k-1)+1:k*(n/K))]
    validationindex = k*n/K
    trainingIndex = length(((K-1)/K)*n)*validationindex
    xpointVec[k] = xpi[validationindex]
    res = ksmooth( xpi[trainingIndex], ypi[trainingIndex], x.points = xpi[validationindex], bandwidth = h, kernel = kernel)
    ypointsTrain[k] = ypi[trainingIndex]
    ypoints[k] = res$y[1]
    MSE[k] = (res$y[1]-ypi[validationindex])^2/(n/K)
    Eh[k] = mean(MSE[k])
  }
  #' plot degree of error 
  plot(1:K, Eh, type = "b", col = 4, pch = 15, lwd = 2, xlab = "degree", ylab = "esimated EPE")
  return(which.min(Eh))
}

#' use avocado dataset
origdat = read.csv(fpath, header = TRUE, sep = ",", stringsAsFactors = TRUE)
plot(origdat$AveragePrice, origdat$Total.Volume, xlab = "Average avocado price", ylab = "total volume of avocados sold")

#' returns mean of Eh
res = ksmooth.cv(origdat$AveragePrice, origdat$Total.Volume, kernel ="box")

#' problem 2
#' helper function
#' 
D <- function(kerneltype, t) {
  if (kerneltype == "uniform") {
    if (t < 1) {
      return(1)
    } else {
      return(0)
      
      2
      
    }
  } else if (kerneltype == "triangle") {
    return(max(1 - t, 0))
  } else if (kerneltype == "epanechnikov") {
    return(max((1 - (t^2)), 0))
  } else if (kerneltype == "quartic") {
    return((max((1 - (t^2)), 0))^2)
  } else if (kerneltype == "tricube") {
    return((max((1 - (t^3)), 0))^3)
  } else if (kerneltype == "cosine") {
    if (t < 1) {
      return(cos(pi * t/2))
    } else {
      return(0)
    }
  } else if (kerneltype == "gaussian") {
    return(exp(-t^2/2))
  }
}
boot.ksmooth = function(x,y, kernel, bandwidth, x.points, conf = 0.95, B = 999){
  n = length(x)
  m = length(y)
  
  n_ret = length(x.points)

  boot_ind=sample(1:n, n, replace = TRUE)
  x_boot = x[boot_ind]
  y_boot = y[boot_ind]
  
 for(b in 1:B){
   for(i in 1:n_ret){
    K.weights = numeric(n)
    for(j in 1:n){
      kernel_input <- abs(x_boot[i] - x[j])/bandwidth 
      K.weights[j] <- D(kernel, kernel_input)
    }
    y_boot[i] <- sum(K.weights * y)/sum(K.weights)
   }
   return(y_boot)
 }
  
}
#' trying on simulated data
f = function(x) (1 + 10*x - 5*x^2)*sin(10*x)
n = 300
X = runif(n)
Y = f(X) + rnorm(n)
res = boot.ksmooth(X,Y, kernel = "uniform", bandwidth = 10, x.points = X, conf = 0.95, B = 999)

#' we can see that since the kernel is uniform, the smoothing is also uniform. thus we're performing affective confidence interval
#' also i submitted q1 on time, can i get the point penalty only for q2? thanks