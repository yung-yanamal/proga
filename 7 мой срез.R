#library("igraph")
simple <- function(g){
  
  if (!is.matrix(g)){
    stop("это вообще не матрица")
  }
  if (!is.numeric(g)){
    stop("нужно ввести числовую матрицу")
  }
  if (any(is.na(g)==T)) {
    stop("В матрице есть NA")
  }
  if (nrow(g) != ncol(g)) {
    stop("Матрица g не является квадратной")
  }

  n <- nrow(g)
  for (i in 1:n){
    g[i, i] <- 0
  }
  g[g != Inf & g != 0] <- 1
  g[g == Inf] <- 0
  for (i in 1:n){
    for (j in 1:n){
      if(g[i, j] == 1){
        g[j, i] <- 1
      }
    }
  }
  return(g)
}

choose_edge <- function(g){
  n_edges <- sum(g)/2
  x <- sample(1:n_edges, size = 1)
  n <- nrow(g)
  s <- 0
  for (i in 1:(n - 1)){
    for (j in (i + 1):n){
      s <- s + g[i, j]
      if (s >= x){
        return(c(i, j))
        break
      }
    }
  }
}

maxim <- function(g){
  m <- max(g)
  n <- nrow(g)
  for (i in 1:(n - 1)){
    for (j in (i + 1):n){
      if (g[i,j]==m){
        return(c(i, j))
        break
      }
    }
  }
}

contract <- function(g, i, j){
  if (i > j) {
    s <- i
    i <- j
    j <- s
  }
  g[i, ] <- g[i, ] + g[j, ]
  g[ ,i] <- g[, i] + g[, j]
  g[j, ] <- 0
  g[, j] <- 0
  g[i, i] <- 0
  return(g)
}

uninsulated <- function(g){
  n <- nrow(g)
  k <- 0
  for (i in 1:n){
    if (sum(g[i, ]) != 0){
      k <- k + 1
    }
  }
  return(k)
}

cut <- function(g){
  while (uninsulated(g) > 2){
    m <- choose_edge(g)
    a <- m[1]
    b <- m[2]
    g <- contract(g, a, b)
  }
  return(sum(g)/2)
}

min_cut <- function(g){
  g_min <- Inf
  N <- 2^nrow(g)
  for (i in (1:N)){
    x <- cut(g)
    if (x < g_min){
      g_min <- x
    }
    if (g_min == 1){
      return(g_min)
      break
    }
  }
  return(g_min)
}

d <- as.matrix(read.csv("C:/Users/Yana/Downloads/Срез_1_2.csv", sep = ';', stringsAsFactors = F))
#g <- matrix(sample(x = c(1:5,Inf), size=25, replace=T), nrow=5, byrow=T) 
g <- simple(d)
G <- graph.adjacency(g, mode = "undirected")
plot.igraph(G)
cut(g)




