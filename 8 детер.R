deter <- function(A){
  
  if (is.matrix(A)==F){
    stop("Íóæíî ââåñòè ìàòğèöó")
  }
  if (ncol(A)!=nrow(A)){
    stop("Íóæíî ââåñòè êâàäğàòíóş ìàòğèöó")
  }
  if (!(is.numeric(A))){
    stop("Íóæíî ââåñòè ÷èñëîâóş ìàòğèöó")
  }
  if (any(is.na(A)==T)){
    stop("Íóæíî ââåñòè ìàòğèöó áåç NA")
  }
  
  s <- 0
  n <- nrow(A)
  
  if (nrow(A)==1){
    return(A[1,1])
  }else if (nrow(A)==2){
      return(A[1,1]*A[2,2]-A[1,2]*A[2,1])
    }else if(nrow(A)==3){
      return(A[1,1]*A[2,2]*A[3,3]+A[2,1]*A[3,2]*A[1,3]+A[1,2]*A[2,3]*A[3,1]-
               A[1,3]*A[2,2]*A[3,1]-A[2,3]*A[3,2]*A[1,1]-A[1,2]*A[2,1]*A[3,3])
    }else{
      for (k in 1:n){
        s <- s + (-1)^(1+k)*A[1,k]*deter(as.matrix(A[-1,-k]))
      }
      return(s)
    } 
}

set.seed(6)
A <- matrix(c(2,4,5,1,-6,8,0,3,-2), nrow=3, ncol=3, byrow=T)
det(A)
a <- Sys.time()
deter(A)
Sys.time()-a
