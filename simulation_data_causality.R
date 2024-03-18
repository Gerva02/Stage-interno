library(tidyverse)

n <- 1000# con numeri più alti SDO = ATE
age <- runif(n, 0 , 100)

Yobs_lik <- function(min,age, tr=1 ){
  if (tr==1){  
    Yobs = dpois(min,age/2 ) 
  }
  if (tr==0){  
    Yobs = dpois(min,age/1.4 ) 
  }
  Yobs
} # la likelihood è ovviamente per trattamento e no

switching <- function(anni, tr=1 ){
  Y_obs <- tr*rpois(n,anni/2)+ (1-tr)*rpois(n,anni/1.4) #questa è esattamente la switching equation
}

possibile_ages <- seq(from=1 , to = 100, length.out= 10)
possibile_min <- 0:90
grid_1 <- outer(possibile_min,possibile_ages, Yobs_lik )
grid_0 <- outer(possibile_min,possibile_ages, Yobs_lik, tr=0 ) 
persp(possibile_min, possibile_ages,grid_1) #questo è il grafico f(y|X, D=1) (z è una probabilità)
persp(possibile_min, possibile_ages,grid_0) #questo è il grafico f(y|X, D=0) (z è una probabilità)
#vediamo quindi che con l'età il malditesta dura di più e per i non trattati dura di più

ATE <- 50.5/2 - 50.5/1.4
#previsione teorica che deriva da prob condizionata
#empirico.tex spiegherà il processo per arrivare a questo.

rct <- tibble(
  age = sample(1:100, n , replace = T),
  D = sample(0:1, n , replace = T),
  headacke_length = switching(anni = age , tr= D)
)
rct
# tibble con y_obs (headacke_length) 
# abbiamo un random control trial ( no selection bias)


rct %>%
  ggplot(aes(x=age, y=headacke_length,color=factor(D)))+
  geom_point(alpha = 0.8)+
  scale_color_discrete(name = "Trattemento",
                      labels = c("Placebo", "Medicina"))
#più si va avanti più la lunghezza dei mal di testa aumenta come la varianza (poisson)


SDO <- rct%>% filter(D==1) %>% pull(headacke_length)%>% mean() - rct %>% filter(D==0) %>% pull(headacke_length) %>% mean()
#Qua calcoliamo quello che il mixtape chiama SDO che essendo questo un random control trial  
#sappiamo che SDO == ATE con n -> inf

SDO
ATE
