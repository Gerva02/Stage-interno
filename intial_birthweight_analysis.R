library(tidyverse)
library(MASS)
library(GGally)
data("birthwt")
?birthwt
birthwt <- tibble(birthwt) 

birthwt <- birthwt %>%
  mutate( across(c(race:ftv,low) , ~as.factor(.))   )

str(birthwt)
summary(birthwt)
birthwt$age

birthwt%>%
  dplyr::select(smoke | where(is.numeric )) %>% # per evitare il clash con il select di MASS
  ggpairs(mapping = aes(color = birthwt$low))
