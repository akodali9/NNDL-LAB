input <- matrix(c(1, 0.8, 0.6),nrow = 1, ncol = 3)

w1 <- matrix(c(0.11,0.12, 0.13, 0.14, 0.15, 0.16), nrow = 3, ncol = 2, byrow = TRUE)

w2 <- matrix(c(0.21, 0.22, 0.23, 0.24, 0.25, 0.26), nrow = 2, ncol = 3, byrow = TRUE)

b1 <- 0.35

b2 <- 0.60

target <- input

eta = 0.1

for ( i in 1:1000){
  h <- 1/(1+exp(-((input%*%w1)+b1)))
  y <- 1/(1+exp(-(h%*%w2)+b2))
  Error = sum((tgt - y))
  dw2 <- sweep(w2, 2, (-(target-y)*y*(1-y)), "*")
  dw1 <- -t(dw2*t((h*(1-h)))%*%input)
  w2 <- -w2-eta*dw2
  w1 <- -w1-eta*dw1
  print('Error: ')
  print(Error)
}

