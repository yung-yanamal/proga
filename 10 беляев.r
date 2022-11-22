 
source("C:/Users/Никита/Desktop/R/1 курс, 2 семестр/1 задача/Best.R")

Bubble_sort <- function(x, col) {
  n <- length(x[,col]);

  for (j in 1:(n-1)) {
    Permutations <- 0
    for (i in 1:(n-j)) {
      if (x[i, col] > x[i+1, col]) {
        temp <- x[i, ]
        x[i, ] <- x[i+1, ]
        x[i+1, ] <- temp
        Permutations <- Permutations + 1
      }
    }
    
    if (Permutations == 0) {
      break()
    }
  }
    
  return(x)
}

rank <- function(state, dis, r) {
  d <- read.csv('C:/Users/Никита/Desktop/R/1 курс, 2 семестр/1 задача/outcome-of-care-measures.csv', sep = ',', stringsAsFactors = F)

  ndis_all <- c(11, 17, 23)
  
  if (dis == "all") {
    for (i in 1:3) {
      a <- d[, ndis_all[i]] == "Not Available"
      d[a, ndis_all[i]] <- NA
      d[, ndis_all[i]] <- as.numeric(d[, ndis_all[i]])
    }
    
    d$all <- (1/3)*d[, 11] + (1/3)*d[, 17] + (1/3)*d[, 23]
    col <- ncol(d) 
    
  } else {
   
    dis_all <- c("HA", "HF", "PN")
    l <- which(dis_all == dis) # вектор, состоящий из номеров dis в dis_all (выдает номер болезни)
    if (length(l) < 1) {stop('Такой болезни нет в базе')}
    
    col <- ndis_all[l] # номер столбца в базе данных   
    
    a <- d[, col] == "Not Available"
    d[a, col] <- NA
    d[, col] <- as.numeric(d[, col])
  }
  
  
  if (state != "all") {
    l <- (d[, 7] == state) & !is.na(d[, col]) # и там нет NA
    if (sum(l) == 0) {stop('Такого штата нет в базе')} 
    
  } else {
    l <- !is.na(d[, col])
  }
  
  d2 <- d[l, ]
  
  if (is.character(r)) {
    if (r == "best") {
      r <- 1
    } else {
        if (r == "worst") {
          r <- nrow(d2)
        } else {stop("Неправильно введён ранк")}
      }
  } else {
    if (!(r %in% (1:nrow(d2)))) {stop("Неправильно введён ранк")} 
  }
  
  d3 <- Bubble_sort(d2, col)
  answer <- d3[r, col] == d3[, col]
  d4 <- d3[answer, 2]
  
  return(d4)
}

rank("CA", "all", 1)

rankall <- function(dis, r) {
  d <- read.csv('C:/Users/Никита/Desktop/R/1 курс, 2 семестр/1 задача/outcome-of-care-measures.csv', sep = ',', stringsAsFactors = F)
  t <- 0
  ndis_all <- c(11, 17, 23)
  
  if (dis == "all") {
    for (i in 1:3) {
      a <- d[, ndis_all[i]] == "Not Available"
      d[a, ndis_all[i]] <- NA
      d[, ndis_all[i]] <- as.numeric(d[, ndis_all[i]])
    }
    
    d$all <- (1/3)*d[, 11] + (1/3)*d[, 17] + (1/3)*d[, 23]
    col <- ncol(d) 
    
  } else {
    
    dis_all <- c("HA", "HF", "PN")
    l <- which(dis_all == dis) # вектор, состоящий из номеров dis в dis_all (выдает номер болезни)
    if (length(l) < 1) {stop('Такой болезни нет в базе')}
    
    col <- ndis_all[l] # номер столбца в базе данных   
    
    a <- d[, col] == "Not Available"
    d[a, col] <- NA
    d[, col] <- as.numeric(d[, col])
  }
  
  l <- !is.na(d[, col])
  d2 <- d[l, ]
  
  s <- unique(d$State)
  otv <- data.frame(state = s, hospital = NA, level = NA)
  
  if (is.character(r)) {
    if (r == "best") {
      r <- 1
    } else {
      if (r == "worst") {
        t <- 1
      } else {stop("Неправильно введён ранк")}
    }
  } else {
    if (!(r %in% (1:nrow(d2)))) {stop("Неправильно введён ранк")} 
  }
  
  for (i in 1:length(s)) {
    l <- d2[, 7] == s[i]
    d3 <- d2[l, ]
    d3 <- d3[order(d3[, col], d3[, 2]), ]
    
    if (r == "worst") {
      r <- nrow(d3)
    }
    
    otv[i,2:3] <- c(d3[r, 2], d3[r, col])
    
    if (t == 1) {r <- "worst"}
  }

  return(otv)
}

rankall("HA", "best")




set.seed(11)
x<-data.frame(v1=sample(1:10,10, replace=F), v2=sample(11:21,10, replace=F), v3=sample(22:32,10, replace=F))
x$all <- (1/3)*x[, 1] + (1/3)*x[, 2] + (1/3)*x[, 3]


