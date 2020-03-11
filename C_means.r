library(sets)
library(dplyr)
library(igraph)
centroid <- function(cor, c){
  numerator <- 0
  denominator <- 0
  #print(length(cor))
  for(i in 1: length(cor)){
    #print("h")
    numerator <- numerator + (cor[i]*(c[i]^2))
    denominator <- denominator + (c[i]^2)
    #print(numerator)
  }
  return(numerator/denominator)
}

distance <- function(x, y, c){
  dist <- c()
  for(i in 1:length(x)){
    dist[i] <- sqrt((c[1]-x[i])^2 + (c[2]-y[i])^2)
  }
  return(dist)
}

memfn <- function(c1, c2){
  mc <- c()
  for(i in 1:length(c1)){
    mc[i] <- (1/c1[i])/((1/c1[i])+(1/c2[i]))
  }
  return(mc)
}

x <- c(1, 2, 3, 4, 5, 6)
y <- c(6, 5, 8, 4, 7, 9)
c1 <- c(0.8, 0.9, 0.7, 0.3, 0.5, 0.2)
c2 <- c(0.2, 0.1, 0.3, 0.7, 0.5, 0.8)
old_centroid_c1 <- c()
old_centroid_c2 <- c()
iteration <- 0
e <- 0.05

while(TRUE){
  cat("ITERATION",iteration,"\n")
  cat('X', "\t", 'Y', "\t", 'C1', "\t", 'C2', "\n")
  for(i in 1:length(x)){
    cat(x[i], "\t", y[i], "\t", c1[i], "\t", c2[i], "\n")
  }
  ed_c1 <- 0
  ed_c2 <- 0
  centroid_c1_x = centroid(x, c1)
  #print(centroid_c1_x)
  centroid_c1_y = centroid(y, c1)
  centroid_c2_x = centroid(x, c2)
  centroid_c2_y = centroid(y, c2)
  centroid_c1 <- c(centroid_c1_x, centroid_c1_y)
  centroid_c2 <- c(centroid_c2_x, centroid_c2_y)
  
  if(iteration == 0){
    old_centroid_c1 <- centroid_c1
    old_centroid_c2 <- centroid_c2
    
  }
  else{
    
    ed_c1 <- sqrt((old_centroid_c1[1]-centroid_c1[1])^2 + (old_centroid_c1[2]-centroid_c1[2])^2)
    ed_c2 <- sqrt((old_centroid_c2[1]-centroid_c2[1])^2 + (old_centroid_c2[2]-centroid_c2[2])^2)
    print(ed_c1)
    print(ed_c2)
    if(ed_c1 < e && ed_c2 < e){
      break;
    }
    old_centroid_c1 <- centroid_c1
    old_centroid_c2 <- centroid_c2
  }
  dist_c1 <- distance(x, y, centroid_c1)
  # for(i in 1:length(dist_c1)){
  #   print(dist_c1[i])
  # }
  dist_c2 <- distance(x, y, centroid_c2)
  mem_c1 <- memfn(dist_c1, dist_c2)
  mem_c2 <- memfn(dist_c2, dist_c1)
  c1 <- mem_c1
  c2 <- mem_c2
  
  iteration <- iteration+1
  plot(x, y, xlab = "PC1", ylab = "PC2", col = ifelse(c1>c2, "red", "blue"))
}
