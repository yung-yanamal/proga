setwd("C:/specdata")
list.files(".")
otvet <- data.frame(id=NA, complete=NA, naS=NA, naN=NA, MeanN=NA, MeanS=NA, correlation=NA)
nitsulf <- function(ids,t){
  for(i in 1:length(ids)){
    id<-ids[i]
    if((id*10)%%10 !=0) { 
      stop("непонятный id")
    }
    if((id<=0) | (id>332)) {
      stop("непонятный id")
    }
    if(id<10){
      id <- paste0("00", id, ".csv")
    } else { if(id<100){
      id <- paste0("0",id,".csv")
    }else{
      id <- paste0(id,".csv")
    }
    }
    d <- read.csv(id,stringsAsFactors = F)
    d$Date <- as.Date(d$Date,"%Y-%m-%d")
    otvet[i, "id"] = ids[i]
    otvet[i, "complete"] = sum(!is.na(d$nitrate)&!is.na(d$sulfate))
    otvet[i, "naS"] = sum(!is.na(d$sulfate))
    otvet[i, "naN"] = sum(!is.na(d$nitrate))
    d <- d[!is.na(d$nitrate)&!is.na(d$sulfate),]
    otvet[i, "MeanS"] = mean(d$sulfate)
    otvet[i, "MeanN"] = mean(d$nitrate)
    par(mfcol=c(1,2))
    plot(d$nitrate, d$sulfate,type="p",col="brown")
    plot(d$Date,d$nitrate,type="l")
    lines(d$Date,d$sulfate,type="l",col="brown")
    
    if(nrow(d)>=t){
      otvet[i, "correlation"] = cor(d$nitrate,d$sulfate)
    } else{
      NA
    }
  }
  return(otvet)
}

nitsulf(c(2, 4, 78), 474)
