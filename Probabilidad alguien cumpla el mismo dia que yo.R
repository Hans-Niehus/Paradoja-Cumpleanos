
prob_mismo_cumple <- function(n){
  prob = 1-(364/365)^n
  return(prob)
}

lista = c()

n = 900

for (n in seq(n)){
  lista = c(lista,prob_mismo_cumple(n))
}

df = data.frame("Personas" = seq(n),"Prob" = lista )

plot(lista, type = "l")

ggplot(data = df,aes(x = Personas,y = Prob))+geom_line(size = 1) + 
  geom_segment(aes(x = 0, xend = 100, y = 0.240, yend = 0.240), size = 0.6, linetype = 2) + 
  geom_segment(aes(x = 100, xend = 100, y = 0, yend = 0.240), size = 0.6, linetype = 2) +
  geom_segment(aes(x = 0, xend = 200, y = 0.422, yend = 0.422), size = 0.6, linetype = 2) + 
  geom_segment(aes(x = 200, xend = 200, y = 0, yend = 0.422), size = 0.6, linetype = 2) +
  geom_segment(aes(x = 0, xend = 500, y = 0.746, yend = 0.746), size = 0.6, linetype = 2) + 
  geom_segment(aes(x = 500, xend = 500, y = 0, yend = 0.746), size = 0.6, linetype = 2) +
  geom_segment(aes(x = 0, xend = 750, y = 0.872, yend = 0.872), size = 0.6, linetype = 2) + 
  geom_segment(aes(x = 750, xend = 750, y = 0, yend = 0.872), size = 0.6, linetype = 2) +
  geom_point(aes(x=500, y=0.746),size = 2, color = "blue") + geom_point(aes(x = 100, y = 0.240), size = 2, color = "blue") + geom_point(aes(x = 200, y = 0.422),size = 2, color = "blue") + geom_point(aes(x = 750, y = 0.872),size = 2, color = "blue") +
  scale_x_continuous(name="Personas", breaks = c(0,100,200,500,750,900)) +
  scale_y_continuous(name="Probabilidad", breaks = c(0,round(df$Prob[df$Personas == 100],3),round(df$Prob[df$Personas == 200],3),0.5,round(df$Prob[df$Personas == 500],3),round(df$Prob[df$Personas == 750],3),round(df$Prob[df$Personas == 900],3),1),1) +
  ggtitle("Probabilidad de que en un grupo al menos otra persona cumpla un día específico") +
  theme(plot.title = element_text(hjust = 0.5))


