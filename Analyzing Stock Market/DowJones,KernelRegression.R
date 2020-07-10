#'---
#'
#'author: Aditya Neelamraju
#'date: 5/26/2020
#'---
#'q1
fpath = paste0(getwd(),"/DOW_01012000_05202020.csv")
origdat = read.csv(fpath, header = TRUE, sep = ",", stringsAsFactors = FALSE)
n = nrow(origdat)
dat_sorted = origdat[n:1,]
dates = dat_sorted$Date
minDate = dates[1]
dates = as.Date(dates,"%m/%d/%Y")
dates = dates - as.Date(minDate, "%m/%d/%Y")
dat_sorted$Date = dates
h_max = 100
d_poss = 1:20

for(d in d_poss){
  for(h in (d+1):h_max){
    sum_h_d = 0
    for(t in (h+1):(n-d)){
      x_sub = dat_sorted[1:t,1]
      y_sub = dat_sorted[1:t,5]
    }
    rel_ind_t_d = (abs(x_sub-dat_sorted$Date[t+d])<=h)
    fit = lm(x_sub~ y_sub)
    sum_h_d = sum_h_d +(dat_sorted$Close[t+d]- predict(fit))
  }
}
#' q2
#' accounts for 7 forms of kernels
D = function(kernel, t){
  if(kernel == "uniform"){
    if(t<1){
      return(1)
    }
    else{
      return(0)
    }
  }
  if(kernel == "triangle"){
    return (1-t)
  }
  if(kernel == "epanechnikov"){
    return (1-t^2)^2
  }
  if(kernel == "quartic"){
    return (1-t^3)^3
  }
  if(kernel == "tricube"){
    if(((1-(t^3))^3)>0){
      return(1)
    }
    else{
      return(0)
    }
  }
  if(kernel == "cosine"){
    if(t<1){
      return(cos((pi/2)*t))
    }
    else{
      return(0)
    }
  }
  if(kernel == "gaussian"){
    return(exp((-t^2)/2))
  }
}
#' our function to do the kernel smoothing
kernel.smoothing = function(x, y, kernel, bandwith){
  x_return = seq(min(x), max(x), 20)
  y_return = numeric(20)
  fhat = 0
  i = 0
  for(xval in x_return){
      y_return[match(xval, x_return)]= y[match(xval, x_return)]/D(xval, kernel)
  } 
  return(list(x_return, y_return))
}
#we can see how varying the form of distribution allows us to get different smoothing valeues
