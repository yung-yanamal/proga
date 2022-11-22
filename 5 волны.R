#install.packages("igraph")
#library(igraph)

level <- function(v, m){
  
  if (!(is.numeric(m))){
    stop("Нужно ввести числовую матрицу")
  }
  if (any(is.na(m)==T)){
    stop("В графе есть NA")
  }
  if (!(is.matrix(m))){
    stop("Это вообще не матрица")
  }
  if (ncol(m) != nrow(m)){
    stop("Нужно ввести квадратную матрицу")
  }
  if (any(apply(m, 1, function(x){all(x == 0)}))){
    stop("Имеются нулевые строки")
  }
  if (any((m!=0)&(m!=1))){
    stop('Нужен невзвешенный граф')
  }
  for (i in 1:nrow(m)){
    for (j in 1:ncol(m)){
      if (m[i, j] != m[j, i]){
        stop('Нужно ввести симметричную матрицу')
      }
    }
  }
  
  n <- ncol(m)
  L <- m[v, ]                                      
  for (lev in 2:(n-1)){
    for (i in 1:n){
      if (L[i] == (lev-1)){ 
        for (j in 1:n){
          if ((L[j] == 0)&(m[i, j] == 1)&(j != v)){ 
            L[j] <- lev
          }
        }
      }
    }
  }
  
  return(L)
}

volna <- function(v1, v2, m){
  n <- ncol(m)
  
  if (!(v1 %in% (1:n))){
    stop("Нужно задать вершину из существующих")
  }
  if (!(is.numeric(v1))){
    stop("Нужно задать номер вершины числом")
  }
  if (!(v2 %in% (1:n))){
    stop("Нужно задать вершину из существующих")
  }
  if (!(is.numeric(v2))){
    stop("Нужно задать номер вершины числом")
  }
  if (v1 == v2){
    stop("Нужно ввести разные вершины")
  }
  
  L <- level(v1, m)

  r <- L[v2] 
  
  if (r == 1){
    return(c(v1, v2))
    }
  
  path <- c(v1, rep(NA, (r-1)), v2)    
  k<-1
  
  while (r > 1){
    for (j in 1:n){
      if ((L[j] == (r - 1)) & (m[v2, j] == 1)){ 
        path[r] <- j
        v2 <- j
        r <- L[v2]
        k <- k+1
        break()
      }
    }
  }
  
  return(list(path = path, r=k, level=L))
}

m <- matrix(c(0,1,1,0,0,0,0,0,0,
              1,0,0,1,0,0,0,0,0,
              1,0,0,0,0,1,0,0,0,
              0,1,0,0,1,0,0,0,0,
              0,0,0,1,0,0,0,0,0,
              0,0,1,0,0,0,0,0,0,
              0,0,0,0,0,0,0,1,0,
              0,0,0,0,0,0,1,0,1,
              0,0,0,0,0,0,0,1,0), nrow = 9, byrow = T)

g <- graph.adjacency(m, mode = 'undirected')
set.seed(2)
plot.igraph(g)

volna(1,6, m)
