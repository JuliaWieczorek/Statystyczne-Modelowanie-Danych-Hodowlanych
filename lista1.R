MatrixA <- function(dane){
  o = matrix(dane[,1], ncol=1) #dziecko
  s = matrix(dane[,2], ncol=1) #ojciec
  d = matrix(dane[,3], ncol=1) #matka
  n = max(dane)
  A = matrix(0,n,n)
  
  for (i in 1:n){
    for (j in 1:n){
      #oboje rodzice znani
      if (s[i]!=0 && d[i]!=0 && A[i,i]>1){
        A[i,j]=0.5*(A[s[i]]+A[d[i]])
        A[i,i]=1+0.5*(A[s[i],d[i]])
      }
      #jeden rodzic nieznany
      
      #tylko ojciec jest znany
      if (s[i]!=0 && d[i]==0){
        A[i,j] = 0.5*(A[s[i]]) #A[s[i],j]
        A[i,i] = 1
      }
      #tylko matka jest znana
      if (s[i]==0 && d[i]!=0){
        A[i,j] = 0.5*(A[d[i]]) #A[d[i],j]
        A[i,i] = 1
      }
      #oboje rodzice nieznani
      if (s[i]==0 && d[i]==0){
        A[i,i] = 1
        A[i,j] = 0
      }
      write.table(A, file='C:/Users/julia/Documents/bioinformatyka/semestrX/Statystyczne_modelowanie_danych_hodowlanych/A.txt')
    }
  }
}

MatrixT <- function(dane){
  o = matrix(dane[,1], ncol=1) #dziecko
  s = matrix(dane[,2], ncol=1) #ojciec
  d = matrix(dane[,3], ncol=1) #matka
  n = max(dane)
  t = matrix(0,n,n)
  
  for (i in 1:n){
    for (j in 1:n){
      t[i,i] = 1
      #oboje rodzice znani
      if (s[i]!=0 && d[i]!=0 && t[i,i]>1){
        t[i,j]=0.5*(t[s[i]]+t[d[i]])
      }
      
      #jeden rodzic nieznany
      
      #tylko ojciec jest znany
      if (s[i]!=0 && d[i]==0){
        t[i,j] = 0.5*(t[s[i]]) 
      }
      #tylko matka jest znana
      if (s[i]==0 && d[i]!=0){
        A[i,j] = 0.5*(t[d[i]]) 
      }
      #oboje rodzice nieznani
      if (s[i]==0 && d[i]==0){
        t[i,j] = 0
      }
      write.table(A, file='C:/Users/julia/Documents/bioinformatyka/semestrX/Statystyczne_modelowanie_danych_hodowlanych/t.txt')
    }
  }
}

MatrixD <- function(dane){ ##nie jestem tego pewna
  o = matrix(dane[,1], ncol=1) #dziecko
  s = matrix(dane[,2], ncol=1) #ojciec
  d = matrix(dane[,3], ncol=1) #matka
  n = max(dane)
  D = matrix(0,n,n)
  
  for (i in 1:n){
    for (j in 1:n){
      F[i]=0.5*a[s[i]]
      
      #oboje rodzice znani
      if (s[i]!=0 && d[i]!=0 && t[i,i]>1){
        d[i,i]=0.5-0.25*(F[s[i]]+F[d[i]])
      }
      
      #jeden rodzic nieznany
      
      #tylko ojciec jest znany
      if (s[i]!=0 && d[i]==0){
        d[i,i] = 0.75 - 0.25 *(F[s[i]]) 
      }
      #tylko matka jest znana
      if (s[i]==0 && d[i]!=0){
        d[i,i] = 0.75 - 0.25 *(F[d[i]]) 
      }
      #oboje rodzice nieznani
      if (s[i]==0 && d[i]==0){
        d[i,i] = 1
      }
      write.table(A, file='C:/Users/julia/Documents/bioinformatyka/semestrX/Statystyczne_modelowanie_danych_hodowlanych/t.txt')
    }
  }
}