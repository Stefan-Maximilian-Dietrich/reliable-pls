l <- nrow(generate_priori_simplex(1:2, step = 0.01))


# Alle Kombinationen von 5 nicht-negativen Ganzzahlen mit Summe 5
mat <- matrix(NA, nrow = 100, ncol = 100)
for(i in 1:100) {
  for(k in 1:100) {
    mat[k,i] <- count_prioris(k, i)
  }
}
View(mat)

count_prioris <- function(refinement, k) {

  n <- refinement - k
  anzahl <- choose(n + k - 1, k - 1)
  return(anzahl)
}

count_prioris <- function(refinement, levels) {
  k <- length(levels)
  step <- 1/refinement
  n <- refinement - k
  anzahl <- choose(n + k - 1, k - 1)
  return(anzahl)
}

combinations <- function(n, k) {
  if (k == 1) {
    return(matrix(n, nrow = 1))
  }
  
  result <- NULL
  for (i in 0:n) {
    rest <- combinations(n - i, k - 1)
    result <- rbind(result, cbind(i, rest))
  }
  return(result)
}

gerate_priori_simplex_rec <- function(levels, refinement){
  k <- length(levels)
  n <- refinement - k
  priori_matrix <- combinations(n, k)
  priori_matrix <- priori_matrix + 1
  priori_matrix <- priori_matrix/refinement
  colnames(priori_matrix) <- as.character(levels)
  return(priori_matrix)
}

# Aufruf
result <- gerate_priori_simplex_rec(c(1,2,3,4,5), 10)


dim2 <- 100 # 0.01
dim3 <- 20 # 0.05
dim5 <- 10 #0.1
