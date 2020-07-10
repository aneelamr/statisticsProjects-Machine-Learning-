#'---
#'
#'author: Aditya Neelamraju
#'date: 5/12/2020
#'---
#' #q1
load("./alon1999.rda")
options(warn = -1)


#' #performs 2 sample t test for matrix m specified with the prcoedure to adjust p values
multiTtest = function (M,L, procedure){
  #number of genes
  m = ncol(M)
  #number of tumor patients
  numTumor = sum(L == "tumor")
  #number of normal patients
  numNormal = sum(L == "normal")
  #vector to store the indexes of tumor patients
  tu_i = numeric(numTumor)
  #vector to store indexes of normal patients
  no_i = numeric(numNormal)
  tumorIndex = 1
  normalIndex = 1
  
  #build vector of indexes
  for(i in 1:length(L)){
    if(L[i]== "tumor"){
      tu_i[tumorIndex] = i
      tumorIndex= tumorIndex +1
    }
    if(L[i]== "normal"){
      no_i[normalIndex] = i
      normalIndex = normalIndex +1
    }
  }
  pval = numeric(m)
  control= numeric(numNormal)
  case = numeric(numTumor)
  #perform two sample t tests and store values
  for(i in 1:m){
    control = M[no_i, i]
    case = M[tu_i,i]
    pval[i] = t.test(control, case, alternative = "two.sided")$p.value
    
  }
  #adjust t test to procedure
  p.adj = p.adjust(pval, procedure)
  return(p.adj)
}
dat = gene_expression
M = dat$expression
L = gene_expression$status
procedure = "bon"
res = multiTtest(M, L, procedure)
res
 
#' #q2
#' #part A
performSpearman = function(M_s){
  m = ncol(M_s)
  resMat = matrix(nrow = m, ncol = m)
  for(i in 1:(m-1)){
    for(j in (i+1):m){
      resMat[i,j] = cor.test(M_s[,i], M_s[,j], method = "spearman")$estimate
    }
  }
  return(resMat)
}

L = dat$status
#number of tumor patients
numTumor = sum(L == "tumor")
#number of normal patients
numNormal = sum(L == "normal")
#vector to store the indexes of tumor patients
tu_i = numeric(numTumor)
#vector to store indexes of normal patients
no_i = numeric(numNormal)
tumorIndex = 1
normalIndex = 1
#build vector of indexes
for(i in 1:length(L)){
  if(L[i]== "tumor"){
    tu_i[tumorIndex] = i
    tumorIndex= tumorIndex +1
  }
  if(L[i]== "normal"){
    no_i[normalIndex] = i
    normalIndex = normalIndex +1
  }
}
Mat = dat$expression
#remove tumor patients to get normal patients
M_n = Mat[-tu_i,]
#remove normal patients to get tumor patients
M_t = Mat[-no_i,]
#' #part a
res = performSpearman(M_n)
image(res, main = "image of matrix of normal subjects")
#from this, we can know where most of the p values in the matrix lie
#' #part b
resTum = performSpearman(M_t)


image(resTum, main = "image of matrix of tumor subjects")

#part c,
#one way would be to take the difference of the two matrix
diff = res-resTum
#we can look at the difference in the values to uncover differences in the pairwise test statistic 
smoothScatter(c(diff))
