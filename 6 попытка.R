#install.packages("igraph")
#library(igraph)

check_mat <- function(m){
  if (!(is.numeric(m))){
    stop("нужна числовая матрица")
  }
  if (sum(is.na(m)) > 0){
    stop("нужна матрица без NA")
  }
  if (!(is.matrix(m))){
    stop("это вообще не матрица")
  }
  if (ncol(m) != nrow(m)){
    stop("нужна квадратная матрица")
  }
  if (any(apply(m, 1, function(x) {all(x > 0)}))==F){
    stop('Матрица должна состоять только из положительных чисел и Inf')
  }
}

Mn <- function(R, A){
  min <- Inf
  m <- "clown"
  for (i in 1:length(A)) {
    if ((R[i] < min) & (A[i] == 0)) {
      m <- i
      min <- R[i]
    }
  }
  return(m)
}

Dijkstra <- function(M, v1, v2) {
  check_mat(M)
  n <- ncol(M)
  answer <- list()
  
  if (!(v1 %in% (1:n))){
    stop("такой вершины не существует")
    }
  if (!(is.numeric(v1))){
    stop("вершина должна быть задана числом")
    }
  if (!(v2 %in% (1:n))){
    stop("такой вершины не существует")
    }
  if (!(is.numeric(v2))){
    stop("вершина должна быть задана числом")
    }
  if (v1 == v2) {
    stop("нужно ввести разные вершины")
    }
  
  R <- M[v1, ]
  A <- rep(0, n) 
  A[v1] <- 1 
  P <- rep(0, n) 
  P[(M[v1, ] != Inf)] <- v1 
  
  while (sum(A) != length(A)){
    k <- Mn(R, A) 
    if (k == "clown") {break()} 
    
    for (i in 1:n) {
      if (R[i] > (R[k] + M[k, i])){ 
        R[i] <- R[k] + M[k, i]  
        P[i] <- k
      }
      A[k] <- 1
    } 
  }
  
  if (P[v2]!=0){
    path <- v2
  if (P[v2] == v1){
    path <- c(v1, v2)
  }else{
    while (P[v2] != v1){
      P[v2] <- P[path[1]]
      path <- c(P[v2], path)
    }
  }
  }else{
    path <- Inf
  }  
  
  answer <- list(length = R[v2], path = path)
  return(answer)   
}

neskolko <- function(M, v1, v2){
  otvet <- list()
  if (length(v2)==1){
    return(Dijkstra(M, v1, v2))
  }else{
    for (i in 1:length(v2)){
      m <- Dijkstra(M, v1, v2[i])
      otvet[[paste("из ", v1, " в ", v2[i], " кратчайшее расстояние равно ", m$length)]] <- m$path
    }
  }
  return(otvet)
}

M <- matrix(0, nrow = 10,ncol = 10)
M[1,] <- c(Inf,5,6,Inf,Inf,Inf,Inf,Inf,Inf,Inf)
M[2,] <- c(Inf,Inf,Inf,10,5,Inf,Inf,Inf,Inf,Inf)
M[3,] <- c(Inf,Inf,Inf,Inf,4,1,Inf,Inf,Inf,Inf)
M[4,] <- c(Inf,Inf,Inf,Inf,Inf,Inf,9,Inf,Inf,Inf)
M[5,] <- c(Inf,Inf,Inf,Inf,Inf,Inf,20,10,Inf,Inf)
M[6,] <- c(Inf,Inf,Inf,Inf,2,Inf,Inf,Inf,Inf,Inf)
M[7,] <- c(Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,10,5)
M[8,] <- c(Inf,Inf,Inf,Inf,Inf,Inf,5,Inf,Inf,Inf)
M[9,] <- c(Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,1)
M[10,] <- c(Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf)

d <- read.csv('C:/Users/Yana/Downloads/dijkstraData.csv', sep = ' ', stringsAsFactors = F)
M <- as.matrix(d)
set.seed(1)
g <- M
g[g == Inf] <- 0
#a <- graph.adjacency(g, mode = 'directed', weighted = T)
#plot.igraph(a, edge.label = c(t(g)[t(g) != 0]),
  #edge.arrow.size = 0.5,layout = layout_in_circle)
#plot.igraph(a, edge.label = c(t(g)[t(g) != 0]), edge.arrow.size = 0.5)

stol <- function(M){
  n <- ncol(M)
  mat <- matrix(rep(Inf, n*n), ncol=n, nrow=n)
  for(i in 1:n){
    for(j in 1:n){
      if (i!=j){
        f <- Dijkstra(M, i, j)
        mat[i,j] <- f$length
      }else{
        mat[i,j] <- 0
      }
    }
  }
  min <- Inf
  stolitsa <- NA
  for (i in 1:n){
    if (sum(mat[i,])<min){
      min <- sum(mat[i,])
      stolitsa <- i
    }
  }
  return(stolitsa)
}

neskolko(M, 1, 199)
#stol(M)
