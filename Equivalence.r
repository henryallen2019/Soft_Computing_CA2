
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
