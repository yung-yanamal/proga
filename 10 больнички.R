file<-'C:/Users/Yana/Desktop/юни/прога/семестр 2/outcome-of-care-measures.csv'
d<-read.csv(file, sep=',', stringsAsFactors = F)

d<-d[,c(2,6,7,11,17,23,15,21,27,29,35,41)]
names(d)
disease<-c('ha','hf','pn')
measure<-c('mr','n','rr')
names(d)[-c(1:3)]<-paste(rep(measure,each=3),disease,sep='.')
names(d)

d[,-c(1:3)]<-apply(d[,-c(1:3)],2,
                   function(x){
                     x<-gsub(',','.',x)
                     as.numeric(x)})

bubble_sort <- function(x, col){
  n <- length(x[,col]);
  for (j in 1:(n-1)){
    per <- 0
    for (i in 1:(n-j)){
      if (x[i, col] > x[i+1, col]){
        temp <- x[i, ]
        x[i, ] <- x[i+1, ]
        x[i+1, ] <- temp
        per <- per + 1
      }
    }
    if (per == 0) {
      break()
    }
  }
  return(x)
}

perv <- function(state, dis, mea, r) {
  d <- read.csv('C:/Users/Yana/Desktop/юни/прога/семестр 2/outcome-of-care-measures.csv',
                sep = ',', stringsAsFactors = F)
  ndis_all <- c(11, 17, 23, 29, 35, 41)
  if (dis == "all"){
    for (i in 1:6){
      a <- d[, ndis_all[i]] == "Not Available"
      d[a, ndis_all[i]] <- NA
      d[, ndis_all[i]] <- as.numeric(d[, ndis_all[i]])
    }
    if (mea=="mr"){
      d$all <- (1/3)*d[, 11] + (1/3)*d[, 17] + (1/3)*d[, 23]
    }else{
      d$all <- (1/3)*d[, 29] + (1/3)*d[, 35] + (1/3)*d[, 41]
    }
    col <- ncol(d) 
  }else{
    dis_all <- c("mrha", "mrhf", "mrpn","rrha", "rrhf", "rrpn")
    l <- which(dis_all == dis)
    if (length(l) < 1) {stop('Такой болезни нет в базе')}
    col <- ndis_all[l] 
    a <- d[, col] == "Not Available"
    d[a, col] <- NA
    d[, col] <- as.numeric(d[, col])
  }
  
  if (state != "all"){
    l <- (d[, 7] == state) & !is.na(d[, col]) 
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
        } else {
    if (!(r %in% (1:nrow(d2)))) {
      stop("Неправильно введён ранк")
      } 
        }
    }
  }
  
  d3 <- bubble_sort(d2, col)
  if (mea=="rr"){
    answer <- d3[nrow(d3)-r+1, col] == d3[, col]
  }else{
    answer <- d3[r, col] == d3[, col]
  }
  
  d4 <- d3[answer, c(2,7)]
  return(d4)
}

vtor <- function(dis, mea, r) {
  d <- read.csv('C:/Users/Yana/Desktop/юни/прога/семестр 2/outcome-of-care-measures.csv', sep = ',', stringsAsFactors = F)
  t <- 0
  ndis_all <- c(11, 17, 23, 29, 35, 41)
  
  if (dis == "all") {
    for (i in 1:6) {
      a <- d[, ndis_all[i]] == "Not Available"
      d[a, ndis_all[i]] <- NA
      d[, ndis_all[i]] <- as.numeric(d[, ndis_all[i]])
    }
    if (mea=="mr"){
      d$all <- (1/3)*d[, 11] + (1/3)*d[, 17] + (1/3)*d[, 23]
    }else{
      d$all <- (1/3)*d[, 29] + (1/3)*d[, 35] + (1/3)*d[, 41]
    }
    col <- ncol(d) 
  }else{
    dis_all <- c("mrha", "mrhf", "mrpn","rrha", "rrhf", "rrpn")
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
  
  for (i in 1:length(s)){
    l <- d2[, 7] == s[i]
    d3 <- d2[l, ]
    d3 <- d3[order(d3[, col], d3[, 2]), ]
    if (r == "worst"){
      r <- nrow(d3)
    }
    otv[i,2:3] <- c(d3[r, 2], d3[r, col])
    if (t == 1) {r <- "worst"}
  }
  return(otv)
}

perv("AZ", "rrpn", "rr", "best")
vtor("rrha", "rr", "worst")

