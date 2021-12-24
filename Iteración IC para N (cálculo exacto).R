# calculo exacto de un IC para N

R_mayuscula <- 10000 # R
n <- 5000 # n
r <- 945 # r
alfa<-0.1 # nivel de significacion del IC

ifelse(R_mayuscula >= n, i<-R_mayuscula, i<-n) # i es N superior
while (TRUE) {
  if (phyper(r-1, R_mayuscula, (i- R_mayuscula ), n, lower.tail = FALSE) <= (alfa/2)) {
    # el software calcula P(X>x)
    break()
  }
  else {
    i<-i+1
  }
}
j<-i # j es N inferior
while (TRUE) {
  if (phyper(r, R_mayuscula, (j- R_mayuscula ), n) <= (alfa/2)) {
    # el software calcula P(X<=x)
    break()
  }
  else {
    j<-j-1
  }
}
if (j < R_mayuscula | j < n){ #el LI no puede ser realmente menor a R ni a n
  if (R_mayuscula > n){
    j <- R_mayuscula
  }
  else {
    j <- n
  }
}
print(c(N_inf = j, N_sup = i))
