#'---
#'title: homework 6
#'author: Aditya Neelamraju
#'date: 5/19/2020
#'---
#' #q1
fpath = paste0(getwd(),"/netflix_titles.csv")
options(warn = -1)


#read the data
origdat = read.csv(fpath, header = TRUE, sep = ",", stringsAsFactors = TRUE)
#q1

# ' finding titles by year
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
eighteenDat = 
  origdat[grep("2018", origdat$date_added),]
ninteenDat = origdat[grep("2019", origdat$date_added),]
twentyDat = origdat[grep("2020", origdat$date_added),]

#' building counts from 2008 to 2020
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

#calculating movie to tv ration
movietoTVration = numeric(13)
for(i in 1:13){
  movietoTVration[i] = partBCounts[i,2]/partBCounts[i, 1]
}
#thus we can see that netflix is increasing its tv shows to movies
cor.test(movietoTVration, 2008:2020)

#h0 year and ratio are not correlated
#h1 year and ratio are positively correlated
any(is.na(origdat))

#q2
B = 999
n = 1e4
p_pv = numeric(B)
p_sp = numeric(B)
p_kend = numeric(B)

for(b in 1:B){
  x = runif(n, min  = -1, max = 1) 
  y = x^2
  p_pv[b] = cor.test(x,y, method = "pearson")$p.value
  p_sp[b] = cor.test(x,y, method = "spearman")$p.value
  p_kend[b] = cor.test(x,y, method = "kendall")$p.value
  #pearson test
  #spearman test
  #kendall test
  
}
#plot side by side plots
boxplot(p_pv, p_sp, p_kend, ylab = "p-value", names = c("pearson", "spearman", "kendall"), xlab = "comparing tests")
#brief comments- we can see that the pearson and kendall tests are similar, the variance is also varying
#its important to note that the p values are similar, but we can see that the null hypothesis is rejected
#q3
dat = read.table("http://www.amstat.org/publications/jse/v21n1/witt/sea_ice_data.txt", sep = '\t', skip = 1)

#parta
ice.extent = dat$V2
ice.year = dat$V1
fit1 = lm(ice.extent~ice.year)
summary(fit1)$r.sq

#partb
#' quadritic fit
fit2 = lm(ice.extent~poly(ice.year, 2, raw = TRUE))
#todo plot lines

#parct c
d = 10
r_sq = numeric(d)

for(i in 1:d){
  fit3 = lm(ice.extent~poly(ice.year, i, raw = TRUE))
  r_sq[i] = (summary(fit3))$r.squared
}

#' part d, scatter plot
X = ice.year
y = ice.extent
plot(X,y, xlab = "year", ylab = "Ice extent of Arctic Sea", main = "comparing models")
#plotting 
lines(X, predict(fit1), col = "red", lwd = 2)
lines(X, predict(fit2), col = "yellow", lwd = 2)
lines(X, predict(fit3), col = "black", lwd = 2)
legend( x = 'left',legend = c('linear', 'quadratic', 'deg 10'),
        col = c("red", "yellow", "black"), pch=c(21,21,21), bty = 'n')
#comments for part d-
#it looks like the quadratic and degree 10 polynomial fit the data better, i think we're overfitting
#a bit since the quadratic fit fits the data pretty well, and thus doesn't require us to go to higher degrees
