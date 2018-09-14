library(ggplot2)
install.packages('tinytex')
tinytex::install_tinytex()
set.seed(123)
nlist <- c(100, 1000, 10000)
tlist <- c(0.0, 0.67, 0.84, 1.28, 1.65, 2.32, 2.58, 3.09, 3.72)
repno <- 100

## generate normal distribution data and calculate approx. CDF
cdf.cal.fun <- function(n, tlist){
  x <- rnorm(n, mean = 0, sd = 1)
  cdf <- double(length(tlist))
  for(t in tlist){
    cdf[which(tlist == t)] <- sum(x <= t) /n
}
  cdf
}
tble <- data.frame(t.value = tlist, truevalue.cdf= pnorm(tlist, mean = 0, sd = 1))
## result table
for(n in nlist){
  cdf <- cdf.cal.fun(n = n, tlist = tlist)
  ## table
  tble <- cbind(tble, cdf)
  
  cdf.rep <- replicate(100, cdf.cal.fun(n = n, tlist = tlist))
  bias <- cdf.rep - pnorm(tlist, mean = 0, sd = 1)
  # print(bias)
  df <- data.frame(
    t.value = factor(rep(tlist, each = 100)),
    bias = as.vector(t(bias))
      #c(bias[1, ], bias[2, ], bias[3, ]....) ## how to improve?
  )
  
  p <- ggplot(df, aes(x=t.value, y=bias, fill = t.value)) +
    geom_boxplot() + ggtitle(paste("n = ", n))
  print(p)
  ## boxplot(bias~tlist, xlab = "t", ylab = "BIAS",
       ## main=paste("Bias \n n=", n), col = rainbow(length(tlist)))
}
colnames(tble) <- c("t value", "true value of cdf", "n = 100", "n = 1000", "n = 10000")
tble

?.Machine
