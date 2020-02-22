###########################################
# Number Knock Out Solver
# by: Isaac J. Faber
###########################################
library(progress)

numbs <- c(2,4,6)

expos <- c(0,1,2,3,4,5,6,7,8,9,10,(1/2),(3/2),(5/2),(7/2),(9/2))

ops <- c('a','s','d','m')
pb <- progress_bar$new(total = (length(expos)^3*3*2*4*4))
source('./solver_support.R')

results <- data.frame()

for(n1 in numbs){ #iterate over the first number
  a <- n1
  lo <- numbs[!(numbs %in% a)]
  for(n2 in lo){ #iterate over the second and thrid number
    b <- n2
    c <- lo[!(lo %in% b)]
    for(ae in expos){ #iterate over the first exponent
      a2 <- a^ae
      for(be in expos){ #iterate over the second exponent
        b2 <- b^be
        for(ce in expos){ #iterative over the thrid exponent
          c2 <- c^ce
          for(o1 in ops){ #first operation
            ir <- operation(a2,b2,o1)
            for(o2 in ops){ #second operation
              fr <- operation(ir,c2,o2)
              temp <- data.frame(a = a,
                                 b = b,
                                 c= c,
                                 ae = ae,
                                 be = be,
                                 ce = ce,
                                 o1 = o1,
                                 o2 = o2,
                                 fr = fr)
              results <- rbind(results, temp)
              pb$tick()
            }
          }
        }
      }
    } 
  }
}

#filter only integer values
results$t <- results$fr%%1==0
results_int <- results[results$t==TRUE,]