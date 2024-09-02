# Inicializar el tablero
iniciar_tablero <- function() {
  return(matrix(" ", nrow = 3, ncol = 3))
}

# Imprimir el tablero
imprimir_tablero <- function(tablero) {
  for(i in 1:3) {
    cat("|", tablero[i,1], "|", tablero[i,2], "|", tablero[i,3], "|\n")
    if(i < 3) cat("-------------\n")
  }
}

# Verificar si hay un ganador
verificar_ganador <- function(tablero) {
  # Verificar filas y columnas
  for(i in 1:3) {
    if(all(tablero[i,] == "X") || all(tablero[,i] == "X")) return("X")
    if(all(tablero[i,] == "O") || all(tablero[,i] == "O")) return("O")
  }
  # Verificar diagonales
  if(all(diag(tablero) == "X") || all(diag(tablero[,3:1]) == "X")) return("X")
  if(all(diag(tablero) == "O") || all(diag(tablero[,3:1]) == "O")) return("O")
  # Si no hay ganador
  return(NA)
}

# Jugar
jugar_gato <- function() {
  tablero <- iniciar_tablero()
  jugador <- "X"
  
  while(TRUE) {
    imprimir_tablero(tablero)
    cat("Turno del jugador", jugador, "\n")
    fila <- as.integer(readline("Ingrese la fila (1-3): "))
    columna <- as.integer(readline("Ingrese la columna (1-3): "))
    
    if(tablero[fila, columna] == " ") {
      tablero[fila, columna] <- jugador
      ganador <- verificar_ganador(tablero)
      if(!is.na(ganador)) {
        imprimir_tablero(tablero)
        cat("¡El jugador", ganador, "ha ganado!\n")
        break
      }
      if(all(tablero != " ")) {
        imprimir_tablero(tablero)
        cat("¡Empate!\n")
        break
      }
      jugador <- ifelse(jugador == "X", "O", "X")
    } else {
      cat("Esa casilla ya está ocupada. Intente de nuevo.\n")
    }
  }
}

# Iniciar el juego
jugar_gato()
