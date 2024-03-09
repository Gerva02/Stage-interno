library(tidyverse)
n <- 1000
n_change_points <- 5 #numero di change points
tau <- seq(from=0, to=1000, length.out= n_change_points+1)  #tempo t in cui si campia
means <- rbeta(n_change_points,shape=10, shape2 = 50) # le medie non ripetute delle binomiali
probs <- rep(NA, n) #le medie ripetute 
latent_state <- rep(NA, n) #questo rappresenta il numero dello stato latente s_t (chibbs)

for (i in 1:n_change_points){
  probs[ (tau[i]+1) : tau[i+1] ]<- rep(means[i],tau[i+1] - tau[i])
  latent_state[ (tau[i]+1) : tau[i+1] ]<- rep(i,tau[i+1] - tau[i])
}

noise <- rbinom(n,size=1, prob= probs) # qui vengono generati i singoli 0 e 1 

series <- tibble(
  t = 1:n ,
  series = cumsum(noise), #qua si fa la somma cumulata dei noise
  ls = as.factor(latent_state)
  )

series %>%
  ggplot(aes(y=series, x= t, color= ls)) + 
  geom_line()
  
