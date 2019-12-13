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
