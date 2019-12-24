library(Rcpp)

clifford_attractor <- function(a,b,c,d,x_n,y_n){
  
  x_n_1 <- sin(a*y_n) + c*cos(a*x_n)
  y_n_1 <- sin(b*x_n) + d*cos(b*y_n)
  
  return <- c(x_n_1,y_n_1)
}

cppFunction('DataFrame clifford_attractor_cpp(int n, double x_n, double y_n,
            double a, double b, double c, double d) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            x[0]=x_n;
            y[0]=y_n;
            for(int i = 1; i < n; ++i) {
            x[i] = sin(a*y[i-1])+c*cos(a*x[i-1]);
            y[i] = sin(b*x[i-1])+d*cos(b*y[i-1]);
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')