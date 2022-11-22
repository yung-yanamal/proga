d1 <- read.csv('https://ipei.ranepa.ru/images/2019/npf/NPF-1.csv', sep=';', stringsAsFactors = F)
d3 <- read.csv('https://ipei.ranepa.ru/images/2019/npf/NPF-3.csv', sep=';', stringsAsFactors = F)
d6 <- read.csv('https://ipei.ranepa.ru/images/2019/npf/NPF-6.csv', sep=';', stringsAsFactors = F)

d <- merge(d3,d6)
d <- merge(d1,d,all=T, by.x='license', by.y='Лицензия')
names(d)
d <- d[!is.na(d$Год) & !is.na(d$license),]
d[,47] <- sub(",",".", d[,47])
d[,24] <- sub(",",".", d[,24])
d[,23] <- sub(",",".", d[,23])
d[,c(47,24,23)] <- sapply(d[,c(47,24,23)], as.numeric)

f1 <- function(d, par, id, year){
  otv <- data.frame()
  y <- c(47,24,23)
  x <- c("r", "p", "s")
    
  if ((any(id=="all"))&(any(year=="all"))){
    id <- unique(d$license)
    year <- seq(min(d$Год), max(d$Год), by=1)
    for (j in 1:length(year)){
    otv <- rbind(otv, data.frame(d$license[d$license %in% id & d$Год == year[j]],
                                 rep(year[j],length(d$license[d$license %in% id & d$Год == year[j]])),
                                 d[d$license %in% id & d$Год == year[j], y[par==x]]))
    }
  }
  
  if ((all(id!="all"))&(all(year!="all"))){
        for (j in 1:length(year)){
          otv <- rbind(otv, data.frame(d$license[d$license %in% id & d$Год == year[j]],
                                         rep(year[j],length(d$license[d$license %in% id & d$Год == year[j]])),
                                         d[d$license %in% id & d$Год == year[j], y[par==x]]))
        }
  }
  
  if ((all(id!="all"))&(any(year=="all"))){
    year <- seq(min(d$Год), max(d$Год), by=1) 
    for (j in 1:length(year)){
      otv <- rbind(otv, data.frame(d$license[d$license %in% id & d$Год == year[j]],
                                   rep(year[j],length(d$license[d$license %in% id & d$Год == year[j]])),
                                   d[d$license %in% id & d$Год == year[j], y[par==x]]))
    }
  }
  
  if ((any(id=="all"))&(all(year!="all"))){
    id <- unique(d$license)
    for (j in 1:length(year)){
      otv <- rbind(otv, data.frame(d$license[d$license %in% id & d$Год == year[j]],
                                   rep(year[j],length(d$license[d$license %in% id & d$Год == year[j]])),
                                   d[d$license %in% id & d$Год == year[j], y[par==x]]))
    }
  }
    colnames(otv) <- c("id", "year", par)
    #otv <- otv[complete.cases(otv)==T, ] 
    return(otv)
}

f1(d, "s","'41/2", 2019)


f2 <- function(d, par, action, id_q){ #выводит максимальное значение, сумму значений или среднее значение
  rr <- c()                           #для каждой лицензии
  otv <- data.frame()
  y <- c(47,24,23)
  x <- c("r", "p", "s")
  g <- unique(d$license)
  if (id_q=="alive"){
    for (i in 1:length(g)){
      m <- which(d$license==g[i])
      if (any(d[m, 22]==2019)){
        rr <- c(rr, g[i])
      }
    }
  }else{
    rr <- g
  }
  year <- seq(min(d$Год), max(d$Год), by=1) 
    if (action=="sum"){
      for (i in 1:length(rr)){
        otv[i,1] <- rr[i]
        otv[i,2] <- "sum"
        m <- which(d$license==rr[i])
        otv[i,3] <- sum(d[m, y[par==x]], na.rm=T)
      }
    }
    if (action=="mean"){
      for (i in 1:length(rr)){
        otv[i,1] <- rr[i]
        otv[i,2] <- "mean"
        m <- which(d$license==rr[i])
        otv[i,3] <- mean(d[m, y[par==x]], na.rm=T)
      }
    }
    if (action=="max"){
      for (i in 1:length(rr)){
        otv[i,1] <- rr[i]
        otv[i,2] <- "max"
        m <- which(d$license==rr[i])
        otv[i,3] <- max(d[m, y[par==x]], na.rm=T)
      }
    }
  colnames(otv) <- c("id", "action", par)
  #otv <- otv[complete.cases(otv)==T, ]         #чтоб не было NA
  return(otv)
}

f2(d, "s", "mean", "all")




f3 <- function(d, par, action, id_q){#выводит максимальное значение, сумму значений или среднее значение
  rr <- c()                         #для каждого года
  otv <- data.frame()
  y <- c(47,24,23)
  x <- c("r", "p", "s")
  g <- unique(d$license)
  if (id_q=="alive"){
    for (i in 1:length(g)){
      m <- which(d$license==g[i])
      if (any(d[m, 22]==2019)){
        rr <- c(rr, g[i])
      }
    }
  }else{
    rr <- g
  }
  year <- seq(min(d$Год), max(d$Год), by=1) 
  if (action=="sum"){
    for (i in 1:length(year)){
      otv[i,1] <- year[i]
      otv[i,2] <- "sum"
      m <- which((d$license %in% rr) & (d$Год==year[i]))
      otv[i,3] <- sum(d[m, y[par==x]], na.rm=T)
    }
  }
  if (action=="mean"){
    for (i in 1:length(year)){
      otv[i,1] <- year[i]
      otv[i,2] <- "mean"
      m <- which((d$license %in% rr) & (d$Год==year[i]))
      otv[i,3] <- mean(d[m, y[par==x]], na.rm=F)
    }
  }
  if (action=="max"){
    for (i in 1:length(year)){
      otv[i,1] <- year[i]
      otv[i,2] <- "max"
      m <- which((d$license %in% rr) & (d$Год==year[i]))
      otv[i,3] <- max(d[m, y[par==x]], na.rm=T)
    }
  }
  colnames(otv) <- c("id", "action", par)
  #otv <- otv[complete.cases(otv)==T, ] #чтоб не было NA
  return(otv)
}

f3(d, "r", "mean", "alive")
