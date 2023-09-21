install.packages("microbenchmark")

obs <- matrix(rep(1.96, 2), nrow=2, ncol=1)
system.time(mvnpdf(x=obs, Log=FALSE))

obs <- rep(1.96, 2)
system.time(mvtnorm::dmvnorm(obs))

library(microbenchmark)
mb <- microbenchmark(mvtnorm::dmvnorm(rep(1.96, 2)),
                     mvnpdf(x = matrix(rep(1.96,2)), Log = FALSE),
                     times = 1000)
mb

plot(mb)

n <- 10e4
pdfval <- mvnpdf(x=matrix(1.96, nrow = 2, ncol = n), Log=FALSE)

n <- 10e4
pdfval <- mvnpdfsmart(x=matrix(1.96, nrow = 2, ncol = n), Log=FALSE)

n <- 1000
mb <- microbenchmark(mvnpdf(x = matrix(1.96, nrow = 2, ncol = n), Log = FALSE),
                     mvtnorm::dmvnorm(matrix(1.96, nrow = n, ncol = 2)),
                     times=100L)
mb

n <- 1000
mb <- microbenchmark(mvtnorm::dmvnorm(matrix(1.96, nrow = n, ncol = 2)),
                     mvnpdf(x=matrix(1.96, nrow = 2, ncol = n), Log=FALSE),
                     mvnpdfsmart(x=matrix(1.96, nrow = 2, ncol = n), Log=FALSE),
                     times=100L)
mb
plot(mb)

n <- 1000
mb <- microbenchmark(mvtnorm::dmvnorm(matrix(1.96, nrow = n, ncol = 2)),
                     mvnpdf(x=matrix(1.96, nrow = 2, ncol = n), Log=FALSE),
                     mvnpdfsmart(x=matrix(1.96, nrow = 2, ncol = n), Log=FALSE),
                     mvnpdfoptim(x=matrix(1.96, nrow = 2, ncol = n), Log=FALSE),
                     times=100L)
mb
plot(mb)

n <- 10e5
library(mvtnorm)
profvis::profvis(mvtnorm::dmvnorm(matrix(1.96, nrow = n, ncol = 2)))
plot(mb)
