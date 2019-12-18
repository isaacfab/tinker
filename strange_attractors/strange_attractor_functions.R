clifford_attractor <- function(a,b,c,d,x_n,y_n){
  
  x_n_1 <- sin(a*y_n) + c*cos(a*x_n)
  y_n_1 <- sin(b*x_n) + d*cos(b*y_n)
  
  return <- c(x_n_1,y_n_1)
}