###########################################
# Number Knock Out Solver
# by: Isaac J. Faber
###########################################

numbs <- c(2,4,6)

expos <- c(0,1,2,3,4,5,6,7,8,9,10)

ops <- c('a','s','d','m')



for(n1 in numbs){ #iterate over the first number
  a <- n1
  lo <- numbs[!(numbs %in% a)]
  for(n2 in lo){ #iterate over the second and thrid number
    b <- n2
    c <- lo[!(lo %in% b)]
    for(ae in expos){ #iterate over the first exponent
      a <- a^ae
    } 
      for(be in expos){ #iterate over the second exponent
        b <- b^be
      }
        for(ce in expos){ #iterative over the thrid exponent
          c <- c^ce
        }
          for(o1 in ops){ #first operation
            for(o2 in ops){ #second operation
              
            }
          }
  }
}
