swap <- function(m,n){
  temp = m
  m = n
  n = temp
  return (c(m,n))
}

check_condition <- function(list){
  c1  <- list[1] < (list[2] + list[3])
  c2  <- list[2] < (list[4] + list[5])
  c3  <- list[6] < (list[8] + list[9])
  c4  <- (list[7] + list[8]) < (list[5] + list[10])
  c5  <- list[11] < list[13]
  c6  <- list[12] < list[10]
  
  c7  <- (list[1] * (list[3] + list[4] + list[5])) == ((list[2] + list[3]) * (list[2] + list[3]))
  c8  <- (list[6] * (list[7] + list[8] + list[9])) == ((list[6] + list[7]) * (list[6] + list[7]))
  c9  <- (list[6] * (list[5] + list[9] + list[10])) == ((list[6] + list[7]) * (list[7] + list[8] + list[9]))
  c10 <- ((list[11] + list[12]) * (list[10] * list[13])) == ((list[12] * list[13]) * (list[12] * list[13]))
  
  if (c1 & c2 & c3 & c4 & c5 & c6 & c7 & c8 & c9 & c10){
    return(TRUE)
  } 
  
  else {
    return(FALSE)
  }
  
}

permutation <- function(list, start, end){
  if (start == end){
    return()
  } 
  
  else {
    for (i in c(start:end)){
      
      if (check_condition(list)){
        ans_1 <<- c(ans_1, list)
        break
      }
      
      a <- swap(list[i], list[start])
      list[i] <- a[1]
      list[start] <- a[2]
      
      permutation(list, start+1, end)
      
      b <- swap(list[i], list[start])
      list[i] <- b[1]
      list[start] <- b[2]
      
    } 
  }
}

ans_1 <<- vector()
permutation(c(1:13), 1, 13) 

save(ans_1, file = "2740430299_05.RData")
load("2740430299_05.RData")