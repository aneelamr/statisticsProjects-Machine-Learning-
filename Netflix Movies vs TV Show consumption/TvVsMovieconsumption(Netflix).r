#'---
#'title: Midterm 1
#'author: Aditya Neelamraju
#'date: 4/20/2020
#'---
#assume file is in current working directory
fpath = paste0(getwd(),"/netflix_titles.csv")
options(warn = -1)


#read the data
origdat = read.csv(fpath, header = TRUE, sep = ",", stringsAsFactors = TRUE)
#netflix titles released in 2018
#Part A
eighteenDat = 
  origdat[grep("2018", origdat$date_added),]
#netflix titles released in 2019
ninteenDat = origdat[grep("2019", origdat$date_added),]

#pie chart of 2018 movies vs tv shows

mytable2018 = table(eighteenDat$type)
par(mfrow = c(1,2))
pie(mytable2018, col = rainbow(6), main = "Movies vs TV shows 2018", radius = 1)


mytable2019 = table(ninteenDat$type)
pie(mytable2019, col = rainbow(6), main = "Movies vs TV shows 2019", radius =  1)

#we can see visually that netflix prioritizes tv shows in 2019 as compared to 2018



Counts = matrix(NA, 2,2)
Counts[1,] = mytable2018
Counts[2,] = mytable2019
#H0- X and Y have the same distribution, 2018 netflix movies and shows and 2019 netflix movies and shows come from
# the same distribution.
#H1- they have different distributions
Counts = as.data.frame(Counts)
names(Counts)[1]= "Movies"
names(Counts)[2] = "TV Shows"
row.names(Counts) = c("2018", "2019")
Counts = as.matrix(Counts)

barplot(Counts, legend = TRUE)

#the p value is obtained from the pearson chi squarted test with yates
#continuity correction
chisq.test(Counts)
#from the value of the chisq test, there is evidence that the null hypothesis is rejected.
# since the p value is very small

#partB
#in Part A, we appeared the pearson chisq test on only two samples, 2018 and 2019 data
# in part B, we will expand this, and apply the same test(since the pearson chisq test can be used on multiple samples)
# where each sample represents a year and we count the number of movies and tv shows released that year, and see
#if the distribution is same or similar for all the years. the netflix data set ranges 2008- 2020
#a limitation of this method is that the pearson chi square test is sensitive to sample size,
#and the sample size of the data varies greatly in each sample. for example, in 2010, only 1 element was added
# while in 2017 1300 titles were added on netflix, thus, relying only on counts and uniformly distributing it
#through the samples might not get as accurate as a result as possible
#H0 - the null hypothesis predicts they come from the same distribution
#H1- The alternate hypothesis predicts they come from different distributions
eightDat = origdat[grep("2008", origdat$date_added),]
nineDat = origdat[grep("2009", origdat$date_added),]
tenDat = origdat[grep("2010", origdat$date_added),]
elevenDat = origdat[grep("2011", origdat$date_added),]
tweleveDat = origdat[grep("2012", origdat$date_added),]
thirteenDat = origdat[grep("2013", origdat$date_added),]
fourteenDat = origdat[grep("2014", origdat$date_added),]
fifteenDat = origdat[grep("2015", origdat$date_added),]
sixteenDat = origdat[grep("2016", origdat$date_added),]
seventeenDat = origdat[grep("2017", origdat$date_added),]
twentyDat = origdat[grep("2020", origdat$date_added),]

partBCounts = matrix(NA, 13, 2)
partBCounts[1,] = table(eightDat$type)
partBCounts[2,] = table(nineDat$type)
partBCounts[3,] = table(tenDat$type)
partBCounts[4,] = table(elevenDat$type)
partBCounts[5,] = table(tweleveDat$type)
partBCounts[6,] = table(thirteenDat$type)
partBCounts[7,] = table(fourteenDat$type)
partBCounts[8,] = table(fifteenDat$type)
partBCounts[9,] = table(sixteenDat$type)
partBCounts[10,] = table(seventeenDat$type)
partBCounts[11,] = table(eightDat$type)
partBCounts[12,] = table(ninteenDat$type)
partBCounts[13,] = table(twentyDat$type)

partBCounts = as.data.frame(partBCounts)
names(partBCounts)[1]= "Movies"
names(partBCounts)[2] = "TV Shows"
partBCounts = as.matrix(partBCounts)
barplot(partBCounts, legend = TRUE)

#we can again see that we have a really small p value, and the null hypothesis is rejected
chisq.test(partBCounts)

#part C
#the question we want to answer here is if the ratings of tv shows and movies are uniformally distributed
#H0 -  they are uniformly distributed
#H1 - not uniformally distributed

tab = table(eighteenDat$rating)
pie(tab, radius=1, main='PIE CHART', col=rainbow(15))
chisq.test(tab)

#monte carlo sampling to simulate p value
n = length(eighteenDat)
D_obs = chisq.test(tab)$stat # observed value of the test statistic
B = 1e4 # number of Montecarlo samples
D_mc = numeric(B) # will store the simulated test statistics
for (b in 1:B) {
  X = sample(1:15, n, TRUE)
  D_mc[b] = chisq.test(table(X))$stat
}
pval = (sum(D_mc >= D_obs) + 1)/(B + 1)
pval
#we can see that the p value < 0.001, thus rejecteding the null hypothesis

###Problem 2
#part A
locationModel = function(n){
  mu = seq(0,2 ,len = 10)
  alpha = 0.1
  B = 200
  #store the values of the power curve
  wilcox = numeric(length(mu))
  #store whether wilcox test is accepted or not
  wilcoxReject = numeric(B)
  
  kstest = numeric(length(mu))
  #store whether ks test is accepted or not
  ksReject = numeric(B)
  
  for(elem in 1:10){
    #store whether ks test is accepted or not
    ksReject = numeric(B)
    #store the values of the power curve
    wilcoxReject = numeric(B)
    curElem = mu[elem]
    for(b in 1:B){
  
      x = rnorm(n, 0,1)
      y = rnorm(n, curElem,1)
      wilcoxResult = wilcox.test(x,y)$p.value
      ksResult =ks.test(x,y)$p.value
      if(wilcoxResult<alpha/2){
        wilcoxReject[b]=1
      }
      if(ksResult<alpha/2){
        ksReject[b]=1
      }
    }
    kstest[elem]= sum(ksReject)/length(ksReject)
    wilcox[elem] = sum(wilcoxReject)/length(wilcoxReject)
  }
   plot(mu, kstest-mu, col = "red", type = "b", ylab = "power" )
   lines(mu, wilcox-mu, col = "blue", type = "b")
   legend( x = 'right',legend = c('Ks-Test', 'Wilcox-test'),
          col = c("red", "blue"), pch=c(5,6), bty = 'n')
   }

scaleModel = function(n){
  mu = seq(0,2 ,len = 10)
  alpha = 0.1
  B = 200
  #store the values of the power curve
  wilcox = numeric(length(mu))
  #store whether wilcox test is accepted or not
  wilcoxReject = numeric(B)
  
  kstest = numeric(length(mu))
  #store whether ks test is accepted or not
  ksReject = numeric(B)
  
  for(elem in 1:10){
    #store whether ks test is accepted or not
    ksReject = numeric(B)
    #store the values of the power curve
    wilcoxReject = numeric(B)
    curElem = mu[elem]
    for(b in 1:B){
      
      x = rnorm(n,0,1)
      y = rnorm(n, 0, sd(mu))
      wilcoxResult = wilcox.test(x,y)$p.value
      ksResult =ks.test(x,y)$p.value
      if(wilcoxResult<alpha/2){
        wilcoxReject[b]=1
      }
      if(ksResult<alpha/2){
        ksReject[b]=1
      }
    }
    kstest[elem]= sum(ksReject)/length(ksReject)
    wilcox[elem] = sum(wilcoxReject)/length(wilcoxReject)
  }
  plot(mu, kstest-mu, col = "red", type = "b", ylab = "power" )
  lines(mu, wilcox-mu, col = "blue", type = "b")
  legend( x = 'right',legend = c('Ks-Test', 'Wilcox-test'),
          col = c("red", "blue"), pch=c(5,6), bty = 'n')

}
#part c, compare the two tests in yet another setting of your choice
# the test I am choosing to do, for sample x, we will still take the standard normal 
# distribution, for sample c, we will decrease the standard deviation to sigma/2, this way
# we can get a good idea of how much the the variability of data, here we are
#not varying it as much is a measure of the test power. this helps
#us understand the behavior of the tests better

myTest = function(n){
  mu = seq(0,2 ,len = 10)
  alpha = 0.1
  B = 200
  #store the values of the power curve
  wilcox = numeric(length(mu))
  #store whether wilcox test is accepted or not
  wilcoxReject = numeric(B)
  
  kstest = numeric(length(mu))
  #store whether ks test is accepted or not
  ksReject = numeric(B)
  
  for(elem in 1:10){
    #store whether ks test is accepted or not
    ksReject = numeric(B)
    #store the values of the power curve
    wilcoxReject = numeric(B)
    curElem = mu[elem]
    for(b in 1:B){
      
      x = rnorm(n,0,1)
      y = rnorm(n, 0, sd(mu)/2)
      wilcoxResult = wilcox.test(x,y)$p.value
      ksResult =ks.test(x,y)$p.value
      if(wilcoxResult<alpha/2){
        wilcoxReject[b]=1
      }
      if(ksResult<alpha/2){
        ksReject[b]=1
      }
    }
    kstest[elem]= sum(ksReject)/length(ksReject)
    wilcox[elem] = sum(wilcoxReject)/length(wilcoxReject)
  }
  plot(mu, kstest-mu, col = "red", type = "b", ylab = "power" )
  lines(mu, wilcox-mu, col = "blue", type = "b")
  legend( x = 'right',legend = c('Ks-Test', 'Wilcox-test'),
          col = c("red", "blue"), pch=c(5,6), bty = 'n')

}
#plotting location model curves with sample size 20,50, 100
locationModel(20)
locationModel(50)
locationModel(100)
#plottiong scale model curves with sample size 20 50 100,
scaleModel(20)
scaleModel(50)
scaleModel(100)

#plotting mytest with sample size 20 50 100
myTest(20)
myTest(50)
myTest(100)
