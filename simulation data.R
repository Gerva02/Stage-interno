library(tidyverse)
n <- 1000
n_change_points <- 5
tau <- seq(from=0, to=1000, length.out= n_change_points+1)  # the time t in wich we have a change
means <- rbeta(n_change_points,shape=10, shape2 = 50) # the means of the binomial 
probs <- rep(NA, n) #mean reapeated
latent_state <- rep(NA, n)

for (i in 1:n_change_points){
  probs[ (tau[i]+1) : tau[i+1] ]<- rep(means[i],tau[i+1] - tau[i])
  latent_state[ (tau[i]+1) : tau[i+1] ]<- rep(i,tau[i+1] - tau[i])
}

noise <- rbinom(n,size=1, prob= probs )

series <- tibble(
  t = 1:n ,
  series = cumsum(noise),
  ls = as.factor(latent_state)
  )

series %>%
  ggplot(aes(y=series, x= t, color= ls)) + 
  geom_line()
  
