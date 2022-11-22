Graph4 <- function(n){
  G <- matrix(data=0, nrow=n, ncol=n)
  return(G)
}

addEdgeW <- function(G,v1,v2,weight=1){
  
  if(v1<=0|v2<=0|(v1>ncol(G))|(v2>ncol(G))){print('неправильное значение параметра')}
  else if(v1==v2){print("в неориентированном графе ребро не может быть инцидентно одной и той же вершине")}
  else{
    if(weight>=0){
      G[v1,v2]<-weight
      G[v2,v1]<-weight
      return(G)
    }
    else{
      if(weight<0){
        print('вес ребра не может быть отрицательным')
      }
    }
  }
  return(G)  
}

adjPeak<-function(G,v){if(v<=0|(v>ncol(G))){print('не существующая вершина')
}else{
  L <- c()
  for(i in 1:ncol(G)){
    if(G[v,i]!=0){
      L <- c(L,i)
    }
  }
  return(L)
}
}

C<-Graph4(8)
C<-addEdgeW(C,5,3,2)
C<-addEdgeW(C,1,3,4)
C<-addEdgeW(C,5,4,4)
C<-addEdgeW(C,3,2,8)
C<-addEdgeW(C,7,3,1)
C<-addEdgeW(C,8,1,7)
C<-addEdgeW(C,6,4,2)
C<-addEdgeW(C,5,1,3)
C<-addEdgeW(C,4,3,10)
C<-addEdgeW(C,6,7,7)
C<-addEdgeW(C,5,2,12)

nmin<- function(R, A) {
  current_min<- Inf
  n_current_min<- 0
  for(i in 1:length(R)) {
    if(R[i] < current_min & A[i]==0) {
      current_min<- R[i]
      n_current_min<- i
    }
  }
  if(n_current_min==0 & all(R==Inf)) {
    stop("изолированная вершина")
  }
  return(n_current_min)
}

#Функция поиска минимального маршрута между двумя пунктами:
dijkstra<- function(G, V1, V2) {
  if (!(is.numeric(G))) {stop("не числовая")}
  if (sum(is.na(G)) > 0) {stop("есть NA")} 
  if (!(is.matrix(G))) {stop("не марица")}
  if (ncol(G) != nrow(G)) {stop("не квадратная")}
  if (any(apply(G, 1, function(x) {all(x == 0)})) == TRUE) {stop("есть нулевые строки")}
  if (FALSE %in% apply(G, 1, function(x) {all(x >= 0)})) {stop('есть отрицательные элементы')}
  
  n <- ncol(G)
  if (V1 == V2) {stop("одинаковые вершины")}
  if (!(V1 %in% (1:n))) {stop("вершины не существует")}
  if (!(is.numeric(V1))) {stop("задайте номер вершины числом")}
  if (!(V2 %in% (1:n))) {stop("вершины не существует")}
  if (!(is.numeric(V2))) {stop("задайте номер вершины числом")}
  
  v1 <- V1
  v2 <- V2
  n <- ncol(G)
  A <- rep(0, n)
  R <- rep(Inf, n)
  P <- rep(0, n)
  A[v1] <- 1
  
  for(i in adjPeak(G,v1)) {
    P[i] <- v1
    R[i] <- G[v1,i]
  }
  
  adj <- nmin(R, A)
  
  while(sum(A)!=n) {
    t <- adjPeak(G, adj)
    if(is.null(t)==TRUE) {
      break
    }
    for(i in t) {
      
      if(R[i] > R[adj]+G[adj,i]) {
        R[i] <- R[adj]+G[adj,i]
        P[i] <- adj
      }
      
    }
    A[adj] <- 1
    adj <- nmin(R, A)
  }
  if (P[v2] == 0) {stop("недостижима")}
  
  short <- v2
  if (P[v2] == 0) {
    short <- c(v1, v2)
  } else {
    while (short[1] != v1) {
      short <- c(P[short[1]], short)
    }
  }
  print(short)
  
  S <- R[v2]
  
  
  return(short)
}

#Функция поиска минимального расстояния между двумя пунктами:
Length_min <- function(G,v1,v2){
  l <- dijkstra(G,v1,v2)
  short <- 0
  for(i in 1:(length(l)-1)){
    short <- G[l[i],l[i+1]]+short
  }
  return(short)
} 
#Функция поиска времени встречи,если два робота:
Two_Meet_Time <- function(G,r1,r2,s1,s2){
  short <- Length_min(G,r1,r2)
  mtime <- short/(s1+s2)
  return(mtime)
}

#Функция, позводяющая найти время преодоления роботом дороги между смежными пунктами, в зависимости от скорости движения:
adj_time <- function(G,v1){
  for(i in 1:nrow(G)){
    for(j in 1:nrow(G)){
      G[i,j] <- G[i,j]/v1
    }
  }
  return(G)
}
#Функция, которая выводит длину кратчайшего пути от заданной вершины до остальных:
Dijkstra <- function(G, v1){
  A <- rep(1,nrow(G)) 
  Ff <- rep(Inf,nrow(G)) 
  early <- rep(0,nrow(G)) 
  A[v1] <- 0
  Ff[v1] <- 0
  early[v1] <- v1
  for(i in adjPeak(G,v1)){
    early[i] <- v1   
    Ff[i] <- Ff[early[i]] + G[v1, i]
  }
  D <- Ff
  D[v1] <- Inf
  while(sum(A)!= 0){
    M <- Inf
    k <- 1
    for(i in 1:length(D)){
      if(A[i]!=0){
        if(D[i] < M){
          M <- Ff[i]
          v1 <- i
        }
      }
      for(j in adjPeak(G,v1)){
        if(Ff[j] > Ff[v1]+G[v1,j]){
          Ff[j] <- Ff[v1]+G[v1,j]
          D[j] <- Ff[v1]+G[v1,j]
          early[j] <- v1
        }
      }
      D[v1] <- Inf
      A[v1] <- 0
    }
  }
  return(Ff)
}
#Функция, которая показывает время, которое надо роботу, чтобы добраться от своей позиции до всех вершин со своей скоростью:
Time_to <- function(G,pos,speed){
  if(speed==2){
    G1 <- adj_time(G,2)
    c1 <- Dijkstra(G1,pos)
  }
  if(speed==1){
    c1 <- Dijkstra(G,pos)
  }
  return(c1)
}
#Функция, которая находит время встречи трех роботов:
Three_Meet_Time <- function(A,G,K){
  for(i in 1:length(A)){
    for(j in 1:length(G)){
      for(k in 1:length(K)){
        if(A[i]!=0 & G[j]!=0 & K[k]!=0){
          if(A[i]==G[j] & G[j]==K[k]){
            return(A[i])
          }
        }
      }
    }
  }
}

#Функция, которая при начальных данных выдаст решение задачи:
Time_meet <- function(G,p1,p2,p3,s1,s2,s3,M){
  if(M==2){
    t1 <- Two_Meet_Time(G,p1,p2,s1,s2)
    if(t1>0){
      t <- paste("Время встречи двух роботов",t1,"секунд")
      return(t)
    }
  }
  if(M==3){
    c1 <- Time_to(G,p1,s1)
    c2 <- Time_to(G,p2,s2)
    c3 <- Time_to(G,p3,s3)
    
    tt <- Three_Meet_Time(c1,c2,c3)
   
    print(tt)
    
    if(tt > 0){
      t <- paste("Время встречи Трех роботов",(tt),"секунд")
    }
    else{
      t <- print("Три Робота не могут встретиться")
    }
  }
  
  return(t)
}
#Пример работы функции Time_meet:
Time_meet(C, 4, 2, 3, 2, 1, 1, 3)
Time_meet(C, 1, 2, 3, 1, 2, 1, 3)


m<- graph.adjacency(C, weighted=TRUE, mode="directed")
plot.igraph(m, vertex.color="green", vertex.label.color="black", edge.color="black", vertex.size=15, edge.label=E(m)$weight)
