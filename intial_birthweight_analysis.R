library(tidyverse)
library(MASS)
library(GGally)
library(Matching)
library("rbounds")

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

tr <- pull(birthwt, smoke)
Y <- ifelse(pull(birthwt, low)[i] ==1 , )

fit <- glm(data=birthwt, smoke ~ age + race,family = binomial(link="logit"))
grups<-cut(fit$fitted.values,5)

rr1 <-  Match(Y = Y , X = fit$fitted.values, Tr = as.vector(tr) ) #tr va convertito
summary(rr1)#p value troppo grande se non dividiamo nei 5 gruppi 

#e quindi Baldi divide in 5 gruppi rispetto il propensity score e testa il fisher sharp null 
#usando il test chi quadro di Cochran

#questo è sicuramente sbagliato perché non stimamo il test chi quadro 
rr2 <-  Match(Y = Y , X = as.numeric(buckets), Tr = as.vector(tr) ) 
summary(rr2) # qua stima il t value non 
#non so quanto sia valido come risultato però vediamo un ate >0 (dunque c'è un effetto causale tra il fumare e il basso peso del feto)

