library(dplyr)
library(tidyr)


juego <- function(sims){

#las figuras son 6, una por cada lado del dado.
figuras <- c(1:6)
#solamente se puede apostar en multiplos de 50
apuestas <- seq(50,1001, by=50)

m <- 2
historial_j <- 0
historial_casa <- 0
saldo_casa <- 0

while (m <= sims){

#creacion de los vectores que simulan el resultado del jugador y el resultao de la casa
#de acuerdo a la probabilidad de cada una de las 6 figuras en salir
  
n_figuras <- sample(1:6, size = 1, prob = c(0.248, 0.205, 0.173, 0.156, 0.114, 0.104))
jugador <- sample(figuras, size = n_figuras)
dados_casa <- sample(figuras, size = 3, replace = TRUE)
  
n <- 1

while (n <= length(jugador)){

g_xjuego <- 0
g_casa <- 0
  
for (i in jugador){

  apuesta_jugador <- sample(apuestas, size = 1)
  match <- i == dados_casa
  n_match <- length(which(match == "TRUE"))
  
  if(n_match == 1){
    ganancia <- apuesta_jugador * 2
    g_xjuego[n] <- ganancia
    g_casa[n] <- apuesta_jugador-ganancia
  } 
  else if (n_match == 2){
    ganancia <- apuesta_jugador * 3
    g_xjuego[n] <- ganancia
    g_casa[n] <- apuesta_jugador-ganancia
  }
  else if (n_match == 3){
    ganancia <- apuesta_jugador * 4
    g_xjuego[n] <- ganancia
    g_casa[n] <- apuesta_jugador - ganancia
  }
  else {
    ganancia <- 0
    g_xjuego[n] <- ganancia
    g_casa[n] <- apuesta_jugador
  }
  n <- n+1
  
}
  g_total <- sum(g_xjuego)
  gcasa_total <- sum(g_casa)
}

historial_j[m] <- g_total
historial_casa[m] <- gcasa_total
o <- m-1
saldo_casa[m] <- saldo_casa[o] + historial_casa[m]
m <- m+1
}

#creacion de reporte 

reporte <- cbind(historial_j,historial_casa,saldo_casa) %>%
  data.frame()
colnames(reporte) <- c("Ganancia_Jugador","Ganancia_casa", "Saldo_casa")

num_perdidas_casa <- length(which(reporte[-1,"Ganancia_casa"] < 0))
num_perdidas_jugador <- length(which(reporte[-1,"Ganancia_Jugador"] == 0))


print(reporte[-1,])
print(cbind(num_perdidas_jugador,num_perdidas_casa))


}


juego(50)

