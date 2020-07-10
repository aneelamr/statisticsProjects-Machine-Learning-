#'---
#'
#'author: Aditya Neelamraju
#'date: 4/20/2020
#'---
knitr::opts_chunk$set(comment=NA, warning= FALSE, message =FALSE, size =12)
options(warn = -1)
#question1
#assume vgsales.csv is in working directory
fpath = paste0(getwd(),"/vgsales.csv")
origdat = read.csv(fpath, header = TRUE, sep = ",", stringsAsFactors = FALSE)


#calculates the p value by comparing the original statistic to the boostrap after
#doing kolmorov-smirnov test. output is a p value. also plots companion plot
kstest = function(dat){
  
  #get global sales
  gl_s = dat$Global_Sales
  #scale data set
  gl_s = gl_s*1000000
  #original statistic
  n = length(gl_s)
  #relateivelt large value to bootsrap
  B = 10000
  D_b = numeric(B)
  #calculadde lambda for poission
  lam = sum(dat$Global_Sales)/n
  lam
  D_orig = as.numeric(ks.test(gl_s, "ppois", lambda = lam)$statistic)
  
  
  #bootstrap
  for(b in 1:B){
    dat_b = rpois(n, lam)
    lam_b = mean(dat_b)
    D_b[b] = as.numeric(ks.test(dat_b, "ppois", lambda = lam_b)$statistic)
  }
  #calculate p value by comparing bootstrap to original
  p_val = (sum(D_b>=D_orig)+1)/(B+1)
  
  #make companion plot
  t = seq(min(gl_s), max(gl_s), length.out = 10000)
  plot(t, ppois(t, lambda = lam), col='red', lwd=3)
  #return p val
  return(p_val)
}

#only select those games where year is greater than 2010
parta = origdat[origdat$Year==2010,]
#kstest calculates original statistic, does the ks test and boostraps
res = kstest(parta)
#pvalue result
res

#in part b, we are removing the outliers and running the test, we can
#see there is no big change in the p_val, so its good that we cleaned the data
#up
#remove global sales values less than 1 million
partaCleaned = parta[parta$Global_Sales<=1,]
#caculate original statistic, do the kolmogorov-smirnov test and boostrap
resb = kstest(partaCleaned)
resb

#part b
na_sales = origdat$NA_Sales
na_sales = na_sales *1000000

#the question we are asking here is the null hypothesis that the mean is less than 30,000
#Ho mu <27000
#one sided95% confidence internval for mu
t.test(na_sales, mu = 27000,alternative = "less", conf.level = 0.95)


#problem 2
#part a
flipSignTest1 = function(x, B=999){
  n = length(x)
  Y = numeric(B)
  Y_star = mean(x)
  for(b in 1:B){
    eps = sample(c(-1,1), n, replace = TRUE, prob = c(0.5,0.5))
    Y[b] = (1/n)*(as.numeric(eps%*%x))
  }
  p_val = (sum(Y>=Y_star)+1)/(B+1)
  return(p_val)
}
#part b
flipSignTest2 = function(x, B=999){
  n = length(x)
  Y = numeric(B)
  Y_star = mean(x)
  for(b in 1:B){
    eps = sample(c(-1,1), n, replace = TRUE, prob = c(0.5,0.5))
    Y[b] = (1/n)*(as.numeric(eps%*%x))
  }
  p_val = (sum(Y>=abs(Y_star))+sum(Y<=-abs(Y_star)+1)/(B+1))
  return(p_val)
}
#part a was modified by simply adding the absolute values of y_star to calculate p val
#thus the difference betweeen flip sign test 2 is that it is two sided while part a is
#one sided
#part c
diffVector = origdat$EU_Sales - origdat$JP_Sales
diffVector = diffVector*1000000

res1 = flipSignTest1(diffVector, B = 999)
res1
res2 = flipSignTest2(diffVector, B = 999)
res2

#for both the flipsigntests, the p-value is nearly 0.
#this value is as small as possible, which means that the null 
#hypothesis, where the vector a = b -c is symmetric about 0, is false
