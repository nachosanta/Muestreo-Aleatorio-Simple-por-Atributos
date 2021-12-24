# calculo exacto de un IC para R

total <- 1000 #N
n <- 200 #n (tamaño de muestra)
r <- 10 #r (cantidad de exitos observados)
alfa <- 0.05 # nivel de significacion del IC

i<-0 # i es R superior
while (TRUE) {
  if (phyper(r, i, (total-i), n) <= (alfa/2)) {
    # el software calcula P(X<=x)
    break()
  }
  else {
    i<-i+1
  }
}
j<-i # j es R inferior
while (TRUE) {
  if (phyper(r-1, j, (total-j), n, lower.tail = FALSE) <= (alfa/2)) {
    # el software calcula P(X>x)
    break()
  }
  else {
    j<-j-1
  }
}
if (j < r){ #el LI no puede ser realmente menor a r
  j <- r
}
print(c(R_inf = j, R_sup = i))
