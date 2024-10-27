
library(tidyverse)

prob_cumple <- function(n){
  prod = 1
  for (i in seq(365-n+1,365)){
    prod = prod * i
  }
  prob = 1 - prod/(365^n)
  return(prob)
}

lista = c()

n=100

for (n in seq(n)){
  lista = c(lista,prob_cumple(n))
}

df = data.frame("Personas" = seq(n),"Prob" = lista )

plot(lista, type = "l")

ggplot(data = df,aes(x = Personas,y = Prob))+geom_line(size = 1) + 
  geom_segment(aes(x = 0, xend = 12, y = 0.167, yend = 0.167), size = 0.6, linetype = 2) + 
  geom_segment(aes(x = 12, xend = 12, y = 0, yend = 0.167), size = 0.6, linetype = 2) +
  geom_segment(aes(x = 0, xend = 23, y = 0.507, yend = 0.507), size = 0.6, linetype = 2) + 
  geom_segment(aes(x = 23, xend = 23, y = 0, yend = 0.507), size = 0.6, linetype = 2) +
  geom_segment(aes(x = 0, xend = 40, y = 0.891, yend = 0.891), size = 0.6, linetype = 2) + 
  geom_segment(aes(x = 40, xend = 40, y = 0, yend = 0.891), size = 0.6, linetype = 2) +
  geom_point(aes(x=c(23), y=c(0.507)),size = 2, color = "blue") + geom_point(aes(x = 12, y = 0.167),size = 2, color = "blue") + geom_point(aes(x = 40, y = 0.891), size = 2, color = "blue") +
  scale_x_continuous(name="Personas", breaks = c(0,12,23,40,50,75,100)) +
  scale_y_continuous(name="Probabilidad", breaks = c(0,round(df$Prob[df$Personas == 12],3),0.25,round(df$Prob[df$Personas == 23],3),0.75,round(df$Prob[df$Personas == 40],3),1)) +
  ggtitle("Probabilidad de que en un grupo al menos 2 personas cumplan el mismo día") +
  theme(plot.title = element_text(hjust = 0.5))


