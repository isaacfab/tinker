###########################################
# Number Knock Out Solver Support Functions
# by: Isaac J. Faber
###########################################

operation <- function(n1,n2,op){
  if(op =='a'){
    x <- n1 + n2
  }
  if(op =='s'){
    x <- n1 - n2
  }
  if(op =='d'){
    x <- n1 / n2
  }
  if(op =='m'){
    x <- n1 * n2
  }
  return(x)
}