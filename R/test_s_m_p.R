## Data
## Use same data as test_get_xx_norm.for
x <- c(1,2,3,3.5)
y <- c(10.0,20.0,10.0,5.0)
N <- length(x)


read.table("./alt_get_mixed/Photran/test_get_xx_norm/testdata1.txt")
nr=2

m=round(N/nr)

ni_l <- c()
for (i in 2:nr){
  u <- m*(i-1)+1
  ni <- c(ni_l, u)
}
ni <- c(1,ni, N+1)