# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

generate.samples <- function(size = 100,p = 0.8 , q = 0.1){
  r <- 1 - p - q
  re<-table(sample(x=c("A","AB","B","O"),replace = T,
                   size = size,prob = c(p^2+2*r*p,2*p*q,q^2+2*r*q,r^2)))
  return(as.vector(re))
}
choose.p_i <- function(sample_n = c(4517,572,1325,3586)){
  p <- vector(mode = "numeric", length = 2)
  size_sample = sum(sample_n)
  for(i in 1:2){
    p[i] = (sample_n[2*i-1]/(3*size_sample))^(1/2)
  }
  return(p)
}
#judgeOrder() will be used to judge the result according to
#the given weight and coordinate characters,
#returning the final results of target vector of characters.
judgeOrder <- function( target = c("B","A"),
                        W = c("A","B","O"),wt =c(1,1,0)){
  Maxw <- 0
  reT <- vector()
  L <- length(target)
  for (i in 1:L){       #to avoid the invalid targets and W.
    if (length(which(W == target[i])) != 1) {
      return("Error!")
    }
    else{
      fw <- wt[which(W == target[i])]
      if(fw > Maxw){
        reT <- target[i]
        Maxw = fw
      }
      else if (fw == Maxw){
        # there is some paralleling elements like "A" & "B" in blood group
        if(length(which(reT == target[i])) == 0)reT <- c(reT,target[i])
        #add the new elements only
      }
      else {
        next
      }
    }
  }
  if (length(reT) > 1){
    #???ڲ???Ԫ??ʱ??????W?г??ֵ??Ⱥ?˳?????򣬼?AB??BA??ͬ
    i <- 1
    while(i < length(reT)){
      j <- length(reT)
      while (j > i){
        if(which(W == reT[j]) <  which(W == reT[j-1])){
          t <- reT[j]
          reT[j] <- reT[j-1]
          reT[j-1] <- t
        }
        j <- j-1
      }
      i <- i+1
    }
  }
  reT <- paste(reT,collapse = "")
  return(reT)
}
generate.samples_simulation <- function(size = 100,total = 2,
                               W=c("A","B","O"),wt=c(1,1,0),wp=c(0.8,0.1,0.1)){
  re.lastcol <- vector("character",size)
  re.newrow.f <- vector("character",total)
  for(i in 1:size){
    re.newrow.f <- sample(x=W,prob = wp,size = total,replace = TRUE)
    re.lastcol[i] <- judgeOrder(target = re.newrow.f,W = W,wt = wt)  }
  re <- as.vector(table(re.lastcol))
  return(re)
}
