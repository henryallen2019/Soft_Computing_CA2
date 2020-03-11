## C_means ##

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

## Mamdani Model ##

library(sets)
library(splines)
sets_options("universe", seq(0,255))
variables <- tuple(
  SD = fuzzy_variable(NL = fuzzy_trapezoid(corners = c(-10,0,31,63)), NM = fuzzy_triangular(corners = c(31,63,95)), NS = fuzzy_triangular(corners = c(63,95,127)), ZE = fuzzy_triangular(corners = c(95,127,159)), PS = fuzzy_triangular(corners = c(127,159,191)), PM = fuzzy_triangular(corners = c(159,191,227)), PL = fuzzy_trapezoid(corners = c(191,227,255,370))),
  AC = fuzzy_variable(NL = fuzzy_trapezoid(corners = c(-10,0,31,63)), NM = fuzzy_triangular(corners = c(31,63,95)), NS = fuzzy_triangular(corners = c(63,95,127)), ZE = fuzzy_triangular(corners = c(95,127,159)), PS = fuzzy_triangular(corners = c(127,159,191)), PM = fuzzy_triangular(corners = c(159,191,227)), PL = fuzzy_trapezoid(corners = c(191,227,255,370))),
  TC = fuzzy_variable(NL = fuzzy_trapezoid(corners = c(-10,0,31,63)), NM = fuzzy_triangular(corners = c(31,63,95)), NS = fuzzy_triangular(corners = c(63,95,127)), ZE = fuzzy_triangular(corners = c(95,127,159)), PS = fuzzy_triangular(corners = c(127,159,191)), PM = fuzzy_triangular(corners = c(159,191,227)), PL = fuzzy_trapezoid(corners = c(191,227,255,370)))
)

# Fuzzy rules
rules <- set(
  fuzzy_rule(SD %is% NL && AC %is% ZE, TC %is% PL),
  fuzzy_rule(SD %is% ZE && AC %is% NL, TC %is% PL),
  fuzzy_rule(SD %is% NM && AC %is% ZE, TC %is% PM),
  fuzzy_rule(SD %is% NS && AC %is% PS, TC %is% PS),
  fuzzy_rule(SD %is% PS && AC %is% NS, TC %is% NS),
  fuzzy_rule(SD %is% PL && AC %is% ZE, TC %is% NL),
  fuzzy_rule(SD %is% ZE && AC %is% NS, TC %is% PS),
  fuzzy_rule(SD %is% ZE && AC %is% NM, TC %is% PM)
)

model <- fuzzy_system(variables, rules)
plot(model)

example.1 <- fuzzy_inference(model, list(SD = 100, AC = 70))
#membership functions
print(example.1)

#rule strength computation
ans.1 <- gset_defuzzify(example.1, "centroid")
ans.2 <- gset_defuzzify(example.1, "meanofmax")
ans.3 <- gset_defuzzify(example.1, "smallestofmax")
ans.4 <- gset_defuzzify(example.1, "largestofmax")

plot(example.1)
print("Centroid method:")
print(ans.1)

## Sugeno Model ##

varinp.mf <- matrix(c(2, 0, 20, 40, NA, 4, 20, 40, 60, 80, 3, 60, 80, 100, NA,
                      2, 0, 35, 75, NA, 3, 35, 75, 100, NA,
                      2, 0, 20, 40, NA, 1, 20, 50, 80, NA, 3, 60, 80, 100, NA,
                      2, 0, 20, 40, NA, 4, 20, 40, 60, 80, 3, 60, 80, 100, NA),
                    nrow = 5, byrow = FALSE)
                    
num.fvalinput <- matrix(c(3, 2, 3, 3), nrow=1)
varinput.1 <- c("a1", "a2", "a3")
varinput.2 <- c("b1", "b2")
varinput.3 <- c("c1", "c2", "c3")
varinput.4 <- c("d1", "d2", "d3")
names.varinput <- c(varinput.1, varinput.2, varinput.3, varinput.4)
range.data <- matrix(c(0,100, 0, 100, 0, 100, 0, 100, 0, 100), nrow=2)

type.defuz <- "WAM"
type.tnorm <- "MIN"
type.snorm <- "MAX"
type.implication.func <- "ZADEH"
name <- "Sim-0"
newdata<- matrix(c(25, 40, 35, 15, 45, 75, 78, 70, 15, 45, 38, 30), nrow= 3, byrow = TRUE)
colnames.var <- c("input1", "input2", "input3", "input4", "output1")

type.model <- "TSK"

func.tsk<-matrix(c(1, 1, 5, 2, 1, 3, 1, 0.5, 0.1, 2, 1, 3, 2, 2, 2), nrow=3, byrow=TRUE)
rule <- matrix(c("very a1","and","b1","and","slghtly c1","and","d1","->",
                 "a2","and","extremely b2","and","c2","and","d2", "->",  
                 "a3","and","b2","and","c2","and","dont_care", "->"),
               nrow=3, byrow=TRUE)
object <- frbs.gen(range.data, num.fvalinput, names.varinput, num.fvaloutput = NULL,
                   varout.mf = NULL, names.varoutput = NULL, rule,
                   varinp.mf, type.model, type.defuz = NULL, type.tnorm, type.snorm, func.tsk, colnames.var,
                   type.implication.func, name)
plotMF(object)
res <- predict(object, newdata)$predicted.val
print(res)

## Equivalence ##

is_reflexive <<- function(mat){
  for(i in 1:length(mat)){
    if(mat[[i]][[i]] != 1)
      return(FALSE)
  }
  return(TRUE)
}

is_symmetric <<- function(mat){
  for(i in 1:(length(mat)-1)){
    for(j in c((i+1):length(mat))){
      if(mat[[i]][[j]] != mat[[j]][[i]])
        return(FALSE)
    }
  }
  return(TRUE)
}

do_lambdacut <<- function(mat, lambda){
  for(i in 1:length(mat)){
    for(j in 1:length(mat)){
      if(mat[[i]][[j]] > lambda)
        mat[[i]][[j]] = 1
      else
        mat[[i]][[j]] = 0
    }
  }
  return(mat)
}

is_transitive <<- function(mat){
  for(i in 1:(length(mat))){
    for(j in c(1:(length(mat)))){
      for(k in c(1:length(mat))){
        if(mat[[i]][[k]] < min(mat[[i]][[j]],mat[[j]][[k]]))
          return(FALSE)
      }
    }
  }
  return(TRUE)
}

do_composition <<- function(mat){
  mat_2 = mat
  for(i in 1:(length(mat))){
    for(k in c(1:length(mat))){
      temp_list = list()
      for(j in c(1:length(mat))){
        temp_list[length(temp_list)+1] = min( mat[[i]][[j]] , mat[[j]][[k]] )
      }
      mat_2[[i]][[k]] = max(unlist(temp_list))
    }
  }
  return(mat_2)
}

get_class <<- function(mat){
  final_list = list()
  for(i in 1:length(mat)){
    temp_list = c()
    for(j in 1:length(mat)){
      if(mat[[i]][[j]] == 1)
        temp_list[length(temp_list)+1] = j
    }
    final_list[[length(final_list) + 1]] = temp_list
  }
  return(final_list)
}
# mat = list(c(1,0.8,0,0.1,0.2),
#            c(0.8,1,0.4,0,0.9),
#            c(0,0.4,1,0,0),
#            c(0.1,0,0,1,0.5),
#            c(0.2,0.9,0,0.5,1))
mat = list(c(1,0.836,0.913,0.683,0.981),
           c(0.836,1,0.934,0.390,0.745),
           c(0.913,0.934,1,0.44,0.818),
           c(0.683,0.390,0.44,1,0.754),
           c(0.981,0.745,0.818,0.754,1))
# mat = list(c(0.3,0.4),
#            c(0.4,0.3))

is_reflexive(mat)
is_symmetric(mat)
count = 0
while(!is_transitive(mat)){
  if(count <= length(mat)-1){
    mat = do_composition(mat)
    count = count + 1
  }
  else{
    print("not equivalence")
    break
  }
}

print(count)
print(mat)
# Performing Lamba Cut
mat = do_lambdacut(mat, 0.80)
print(mat)
print(get_class(mat))
