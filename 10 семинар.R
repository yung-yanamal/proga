file<-'Q:/#PUBLIC/ÀÏ 2020-2021/outcome-of-care-measures.csv'
d<-read.csv(file,sep=',',stringsAsFactors = F)
names(d)

d<-d[,c(2,6,7,11,17,23,15,21,27,29,35,41)]
disease<-c('ha','hf','pn')
measure<-c('mr','n','rr')

names(d)[-c(1:3)]<-paste(rep(measure,each=3),disease,sep='.')
sapply(d,class)
d[4,10]<-'18,2'

d[,-c(1:3)]<-apply(d[,-c(1:3)],2,
                   
                   function(x){
                     
                     x<-gsub(',','.',x)
                     
                     print(unique(x[!is.na(x) & is.na(as.numeric(x))]))
                     
                     as.numeric(x)
                     
                   })



d$l<-as.logical(apply(!is.na(d[,-c(1:3)]),1,sum))

sum(!d$l)

d<-d[d$l,]

d<-d[,-ncol(d)]



apply(d[,-c(1:3)],2,mean,na.rm=T)

summary(d[,-c(1:3)])



plot(d$n.hf,d$mr.hf,pch=19)

plot(d$mr.hf,d$rr.hf,pch=19)



q<-aggregate(.~State,d[,-c(1,2)],mean,na.rm=T)

aggregate(mr.ha~State,d,mean,na.rm=T)

q1<-aggregate(mr.ha~State,d,
              
              function(x){
                
                c(mean=mean(x,na.rm=T),
                  
                  sd=sd(x,na.rm=T),
                  
                  numh=length(x),
                  
                  numh.ha=sum(!is.na(x))
                  
                )
                
              })

q1<-do.call(data.frame,q1)



# 1 - descriptive stats



# 2 - state with best/worst care

# input:

# disease: c('ha','hf','pn','all')

# measure: c('mr','rr','both')

# type: c('best','worst')

# output: state

# example: f1('ha','both','best')

# example: f1('all','mr','worst')



# 3 - hospital with best/worst care

# input:

# disease: c('ha','hf','pn','all')

# measure: c('mr','rr','both')

# type: c('best','worst')

# output: hospital name & state

# example: f3('ha','both','best')

# example: f3('all','mr','worst')



# 4 - hospital ranking

# input:

# disease: c('ha','hf','pn','all')

# measure: c('mr','rr','both')

# rank: integer or c('best','worst')

# output:data.frame

# state, hname, all 9 columns, criteria

ot<-data.frame(state=unique(d$State),
               
               hname=NA,
               
               mr.ha=NA,
               
               criteria=NA)