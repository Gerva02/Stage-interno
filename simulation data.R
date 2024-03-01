library(tidyverse)
n <- 1000
n_change_points <- 5
tau <- seq(from=0, to=1000, length.out= n_change_points+1)
means <- rbeta(n_change_points,shape=10, shape2 = 50)
probs <- rep(NA, n)
for (i in 1:n_change_points){
  probs[ (tau[i]+1) : tau[i+1] ]<- rep(means[i],tau[i+1] - tau[i])
}

noise <- rbinom(n,size=1, prob= probs )

series <- tibble(
  t = 1:n ,
  series = cumsum(noise))

series %>%
  ggplot(aes(y=series, x= t)) + 
  geom_line()
  
