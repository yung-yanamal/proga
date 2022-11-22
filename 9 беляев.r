library(igraph)
dat <- read.csv('liquid_50.csv',
                sep=';',stringsAsFactors = F)
#чтобы два графика на одном полотне рисовать
par(mfcol = c(1,2))
#функция для вычисления накопленных доходностей
stock_return <- function(d){
  n <- length(d)
  #вычисляем приросты в стоимости акции за день t в сравнении с днём t - 1
  x <- d[2:n]/d[1:(n - 1)]
  #теперь вычисляем стоимость акции в процентах от начального значения в день t
  y <- 100 * cumprod(c(1,x))
  return(y)
}
#функция для поиска уровней
levels <- function (g, v) {
  n <- nrow(g)
  L <- g[v,]
  for (level in 2:(n-1)) {
    for (i in 1:n) {
      if (L[i] ==  level - 1) {
        for (j in 1:n) {
          if(L[j] == 0 & j != v & g[i,j] == 1) {
            L[j] <- level
          }
        }
      }
    }
  }
  return(L)
}
analyze <- function(dat, corr, start_of_period = "2017-01-03", end_of_period = "2020-04-17"){
  #берём данные за выбранный период
  if ((corr < 0) | (corr > 1)){
    stop("Порог для корелляции выбран некорректно")
  }
  dat$date <- as.Date(dat$date)
  start_of_period <- as.Date(start_of_period)
  end_of_period <- as.Date(end_of_period)
  dat <- dat[(dat$date >= start_of_period & dat$date <= end_of_period), ]
  #считаем накопленную доходность по всем акциям
  n <- nrow(dat)
  #список акций
  stocks <- names(dat)[-1]
  nc <- length(stocks)
  #считаем накопленные доходности по каждой акции
  returns <- matrix(nrow = n, ncol = nc)
  for(i in 2:(nc + 1)){
    returns[, (i - 1)] <- stock_return(dat[, i])
  }
  #теперь рисуем график накопленных доходностей для всех бумаг
  plot(x = dat$date,
       y = returns[,1],
       type = "n",
       ylim = range(returns),
       ylab = "Накопленная доходность (в процентах)",
       xlab = "Год")
  for (i in 1:nc) {
    lines(dat$date, returns[, i])
  }
  #таск про акции
  #сильнее всех выросла
  n_high <- which.max(returns[nrow(returns),])
  high <- stocks[n_high]
  #сильнее всех упала
  n_low <- which.min(returns[nrow(returns),])
  low <- stocks[n_low]
  #корелляцию будем делать по доходностям
  #могли бы и по ценам, но там получается одно и то же
  cor_matrix <- matrix(nrow = nc, ncol = nc)
  for (i in 1:nc){
    for(j in 1:nc){
      cor_matrix[i,j] <- cor(returns[,i], returns[,j])
    }
  }
  #составляем корелляционную 
  cor_matrix <- abs(cor_matrix)
  g <- matrix(0, nrow = nc, ncol = nc)
  for(i in 1:nc){
    for(j in 1:nc){
      if(cor_matrix[i,j] > corr & (i != j)){
        g[i,j] <- 1
      }
    }
  }
  #отрисовка большого графа с акциями
  colnames(g) <- stocks
  G <- graph.adjacency(g, mode = "undirected")
  plot.igraph(G)
  #ищем подграфы,заодно и количество изолированных вершин
  n_isolated <- 0
  #просмотренные вершины
  A <- rep(0, times = nc)
  #список с подграфами
  subgraphs <- list()
  #перебираем вершины
  for(i in 1:nc){
    #если вершина еще не отмечена (не отнесена к какому-либо подграфу)
    if(A[i] != 1){
      #находим изолированные вершины
      if(sum(g[i,]) == 0){
        A[i] <- 1
        n_isolated <- n_isolated + 1
      } else {
        #находим другие вершины, достижимые из данной
        l <- levels(g,i)
        #добавляем их в подграф
        subgraphs[[(length(subgraphs) + 1)]] <- c(i, (1:nc)[l != 0])
        #отмечаем как просмотренные все эти вершины (в том числе и текущую)
        A[i] <- 1
        A[l != 0] <- 1
      }
    }
  }
  #список с информацией о подграфах
  x <- list()
  #число подграфов
  ls <- length(subgraphs)
  if (ls != 0){
    #перебираем подграфы
    for(i in 1:ls){
      #смотрим на вершины в подграфах
      vertexes <- subgraphs[[i]]
      gp <- g[vertexes,]
      n <- nrow(gp)
      #считаем эксцентриситеты для каждой вершины
      #так как граф неориентированный и невзвешенный
      #то эксцентриситет - это просто максимальный уровень для данной вершины
      ex <- numeric(n)
      for(j in 1:n){
        l <- levels(gp, j)
        ex[j] <- max(l)
      }
      #радиус подграфа
      rad <- min(ex)
      #диаметр подграфа
      diam <- max(ex)
      #центраьные вершины
      rad_vertexes <- vertexes[ex == rad]
      #периферийные вершины
      diam_vertexes <- vertexes[ex == diam]
      x[[i]] <- list("Число вершин" = length(subgraphs[[i]]),
                     "Радиус" = rad,
                     "Диаметр" = diam,
                     "Центральные вершины" = stocks[rad_vertexes],
                     "Периферийные вершины" = stocks[diam_vertexes])
    }
  }
  return(list("Сильнее всех выросла (выросли) бумага (бумаги)" = high,
              "Сильнее всех упала (упали) бумага (бумага)" = low,
              "Количество изолированных вершин" = n_isolated,
              "Количество подграфов" = ls, 
              "Информация о подграфах" = x))
}
analyze(dat, corr = 0.9)
