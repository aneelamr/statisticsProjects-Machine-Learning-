#'---
#
#'author: Aditya Neelamraju
#'date: 4/27/2020
#'---
knitr::opts_chunk$set(comment=NA, warning= FALSE, message =FALSE, size =12)
options(warn = -1)
#question1
#par a
#assume vgsales.csv is in working directory
fpath = paste0(getwd(),"/vgsales.csv")
origdat = read.csv(fpath, header = TRUE, sep = ",", stringsAsFactors = FALSE)
origdat = origdat[origdat$Year==2010,]
#H0-the mean of Ps3 amd Wii are the same for the year 2010
#H1- the mean of PS3 and Wii are different for 2010

ps3dat = origdat[origdat$Platform =="PS3",]
wiidat = origdat[origdat$Platform == "Wii",]

#initializing variables
ps3gl_s = ps3dat$Global_Sales
ps3gl_s = ps3gl_s*1000000

wiigl_s = wiidat$Global_Sales
wiigl_s = wiigl_s*1000000

m = length(ps3gl_s)
n = length(wiigl_s)
B = 1e4

T_b = numeric(B)
meanPS3 = mean(ps3gl_s)
meanWii = mean(wiigl_s)
varPS3 = var(ps3gl_s)
varWii = var(wiigl_s)
#calculating original statistic
T_orig = (meanPS3-meanWii)/((varPS3/m)+(varWii/n))^0.5
se_h=((varPS3/m)+(varWii/n))^0.5

#non parametric bootstrap
for(b in 1:B){
  x_b = sample(ps3gl_s,m, replace = TRUE)
  y_b = sample(wiigl_s,n, replace = TRUE)
  T_b[b] = (mean(x_b)-mean(y_b)-(meanPS3-meanWii))/((var(x_b)/m)+(var(y_b)/n))^0.5
}
hist(T_b, breaks = 50, main = "Histogram of perumtated differences", col = 'blue')
#form a confidence interval and see if 0 is inside
#if 0 is inside, H0 isnt rejected at level alpha
#if 0 is outside, H0 is rejected at level alpha

alpha = 0.05
t_cri = as.numeric(quantile(T_b, probs = c(alpha/2, 1- alpha/2), type = ))
t_s_cri = c(meanPS3-meanWii-(t_cri[2])*se_h,meanPS3-meanWii-(t_cri[1])*se_h )

if(t_s_cri[1]<=0 && t_s_cri[2]>=0){
  print("0 is inside, H0 is not rejected at level alpha")
} else {
  print("0 is outside, H0 is  rejected at level alpha")
}
plot.new()
plot(ecdf(ps3gl_s),  verticals = TRUE, xlab = "console sales", ylab =' ', col = 'blue', main = 'Empirical CDFs', lwd = 2)
lines(ecdf(wiigl_s), verticals = TRUE, col = 'green', lwd = 2 )
legend('bottomright', legend = c("PS3", "Wii"), col = c('blue', 'green'), lty=c(1,1), lwd = c(2,2), bg = 'white')

#part b
#the question we are trying to answer here is that whether action games are just as popular as sports games

actiondat = origdat[origdat$Genre =="Action",]
action_sal = actiondat$Global_Sales
action_sal = action_sal*1000000
sportsdat = origdat[origdat$Genre =="Sports",]
sports_sal = sportsdat$Global_Sales
sports_sal = sports_sal*1000000


wilcox.test(action_sal,sports_sal , mu = 0, alt = "two.sided", conf.int = T, conf.level = 0.5, paired = F, exact = T, correct = T)
#wow turns out they are not, action games are far more popular

#problem 2
#part a

#the most appropriate null hypothesis is
#H0 - x,y come from the same distribution. that is the
#population x and y are are distributed similarly.

#part b
#helper function that given a string of Xs and Ys, counts the number of runs of X
#where x is a 0, and y is 1
countruns = function(A){
  n = length(A)
  numRuns = 0;
  if(A[1]==0){
    numRuns = 1
  }
  
  #if previous is different, increase numruns
  for(i in 2:n){
    if((A[i-1]==1)&&A[i]==0){
      numRuns = numRuns+1
    }
  }
  return(numRuns)
}

#nb.runs.test function that implements two sided version
#of abovue test
nb.runs.test = function(x,y,B){
  stringLen = length(x)+length(y)
  mat = rbind(cbind(x,0),cbind(y,1))
  #sorts matrix
  #calculates original statistic
  mat.srt = mat[order(mat[,1]),]
  labs = mat.srt[,2]
  D.obs = countruns(labs)
  D.perm = numeric(B)
  minExtremeVals = 0
  #resamping
  for(b in 1:B){
    perm = sample(labs, stringLen, replace = FALSE)
    D.perm[b]=countruns(perm)
    if(D.perm[b]<= D.obs){
      minExtremeVals = minExtremeVals+1
    }
  }
  
  p_val = (minExtremeVals+1)/B+1
  return(p_val)
}

#testing nb runs test on our dataset
p = nb.runs.test(ps3gl_s, wiigl_s, B = 999)
#the p value is calculated by  binding x as 0, y as 1, 
#sorting the string we have, and then comparing the original
#statistic to the bootstrap statistic by counting the runs
