library(tidyverse)
library(haven)

read_data <- function(df)
{
  full_path <- paste("https://github.com/scunning1975/mixtape/raw/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

training_bias_reduction <- read_data("training_bias_reduction.dta") %>% 
  mutate(
    Y1 = case_when(Unit %in% c(1,2,3,4) ~ Y),
    Y0 = c(4,0,5,1,4,0,5,1))

train_reg <- lm(Y ~ X, training_bias_reduction)

training_bias_reduction <- training_bias_reduction %>% 
  mutate(u_hat0 = predict(train_reg))


d <- tibble(
  Y = c(5,2,10,6,4,0,5,1),
  X = c(11,7,5,3,10,8,4,1)
)

plot(Y~X , data = d[5:8,])

reg <-lm(Y ~ X , data = d[5:8, ])
sos <-predict(newdata= data.frame(X = d$X) , reg )


d1 <- tibble(
  d , 
  uhat = sos
)

tau_ate = 0 
attach(d1)
for (i in 1:4) {
  sum_element <- Y[i]-Y[i+4]- (uhat[1]-uhat[1+4])
  tau_ate <- sum_element + tau_ate
}
detach(d1)
1/4*tau_ate # the adjustment completly flips


