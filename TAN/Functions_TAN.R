# Paket installieren & laden
install.packages("infotheo")
library(infotheo)
library(igraph)
library(bnlearn)

data <- data_set[[1]]

TAN_RMEP <- function(data) {
  data_discrete <- discretize_rec_min_ent(data)
  X_discrete <- data_discrete[, -c(1)]
  S_disc <- data_discrete[, c(1)]
  
  # Leere Matrix zur Speicherung der bedingten MI
  n <- ncol(X_discrete)
  cmi_matrix <- matrix(0, n, n)
  colnames(cmi_matrix) <- colnames(X_discrete)
  rownames(cmi_matrix) <- colnames(X_discrete)
  
  options(scipen = -999)  # bevorzugt wissenschaftliche Notation stark
  # Berechne CMI für alle Paare (außer Diagonale)
  for (i in 1:n) {
    for (j in 1:n) {
      cmi_matrix[i, j] <- conditional_mutual_info(
        X = X_discrete[, i],
        Y = X_discrete[,j],
        Z = S_disc
      )
      
    }
  }

  # bevorzugt wissenschaftliche Notation stark
  
  # 4. Erstelle ungerichteten gewichteten Graph
  g <- graph_from_adjacency_matrix(cmi_matrix, mode = "undirected", weighted = TRUE, diag = FALSE)
  
  E(g)$weight <- -E(g)$weight  # invertieren, um Maximum-Spanning-Tree zu erhalten
  mst <- mst(g)
  E(mst)$weight <- -E(mst)$weight
  
  m <- as.matrix(as_adjacency_matrix(mst)) * cmi_matrix
  direct <- function(m, start) {
    j <- start
    for(i in 1:nrow(cmi_matrix)) {
      if(m[j,i]>0) {
        m[i, j] <- 0
      }
    }
    new_starts <- which(m[j,]>0)
    for(k in new_starts) {
      m <- direct(m, k)
    }
    return(m)
  }
  
  for(k in 1:ncol(m)) {
    m <- direct(m, k)
  }
  direction <- m
  target <- rep(1, times = ncol(m))
  tan_mat_row <- rbind(target, direction)
  target <-  c(0, rep(0, times = ncol(m) ))
  tan_mat <- (cbind(target,tan_mat_row ))
  
  options(scipen =0)  # bevorzugt wissenschaftliche Notation stark
  
  ###
  
  bn <- empty.graph(nodes = colnames(tan_mat))
  
  kanten <- NULL
  for(i in 1:ncol(tan_mat)) {
    from <- rownames(tan_mat)[i]
    to <- names(which(tan_mat[i, ]>0))
    kanten_neu <- expand.grid(from, to)
    kanten <- rbind(kanten, kanten_neu)
  }
  kanten_mat <- as.matrix(kanten)
  
  arcs(bn) <- kanten_mat
  plot(bn)

  
  fitted_bn <- bn.fit(bn, data = data_discrete) ##### möglich das ganze disret zu machen 
  options(scipen = 0)  
  return(fitted_bn)
}

NB_RMEP <- function(data) {
  data_discrete <- discretize_rec_min_ent(data)
  X_discrete <- data_discrete[, -c(1)]
  S_disc <- data_discrete[, c(1)]
  
  options(scipen = -999)  # bevorzugt wissenschaftliche Notation stark
  
  n <- ncol(X_discrete)
  nb_mat <- matrix(0, n+1,n +1)
  colnames(nb_mat) <- c("target", colnames(X_discrete))
  rownames(nb_mat) <- c("target", colnames(X_discrete))
  nb_mat[,1] <- 1
  nb_mat[1,] <- 0
  
  bn <- empty.graph(nodes = colnames(nb_mat))
  
  kanten <- NULL
  for(i in 1:ncol(nb_mat)) {
    from <- rownames(nb_mat)[i]
    to <- names(which(nb_mat[i, ]>0))
    kanten_neu <- expand.grid(from, to)
    kanten <- rbind(kanten, kanten_neu)
  }
  kanten_mat <- as.matrix(kanten)
  
  arcs(bn) <- kanten_mat
  plot(bn)
  
  
  fitted_bn <- bn.fit(bn, data = data_discrete) ##### möglich das ganze disret zu machen 
  options(scipen = 0)  
  
  return(fitted_bn)
}



data_set <- sampler_NB_up(50, 0, data, formula)

dirc_test_dats <- discretize_rec_min_ent_test(data_set[[1]], data_set[[3]])


fitted_bn <- TAN_RMEP(data_set[[1]])
fitted_bn_nb <- NB_RMEP(data_set[[1]])
model <- gaussian_naive_bayes(x = as.matrix(data_set[[1]][,c(-1)]), y = as.factor(data_set[[1]]$target))


predicted_tan <- predict(fitted_bn, node = "target", data = dirc_test_dats, prob = TRUE)
predicted_nb <- predict(fitted_bn_nb, node = "target", data = dirc_test_dats, prob = TRUE)
posterior_gnb <- predict(model, newdata = as.matrix(data_set[[3]][,c(-1)]), type = "prob")

TAN <- caret::confusionMatrix(predicted_tan, data_set[[3]]$target)$overall[1]
NB <- caret::confusionMatrix(predicted_nb, data_set[[3]]$target)$overall[1]
GNB <- caret::confusionMatrix(posterior_gnb, data_set[[3]]$target)$overall[1]

c(TAN = TAN, NB = NB, GNB = GNB)

fitted_bn$target  # enthält die Marginalverteilung P(A)
fitted_bn$Length  # enthält die Marginalverteilung P(A)
fitted_bn$Bottom  # enthält die Marginalverteilung P(A)

predicted <- predict(fitted_bn, node = "target", data = new)
caret::confusionMatrix(predicted, new$target)
###
######## Vegleich mit Naiv Bayes 



bn_nb <- empty.graph(nodes = colnames(nb_mat))

kanten <- NULL
for(i in 1:ncol(nb_mat)) {
  from <- rownames(nb_mat)[i]
  to <- names(which(nb_mat[i, ]>0))
  kanten_neu <- expand.grid(from, to)
  kanten <- rbind(kanten, kanten_neu)
}
kanten_mat <- as.matrix(kanten)

arcs(bn_nb) <- kanten_mat



fitted_bn$target  # enthält die Marginalverteilung P(A)
fitted_bn$Length  # enthält die Marginalverteilung P(A)
fitted_bn$Bottom  # enthält die Marginalverteilung P(A)


fitted_bn_nb <- bn.fit(bn_nb, data = train) ##### möglich das ganze disret zu machen 
fitted_bn <- bn.fit(bn, data = train) ##### möglich das ganze disret zu machen 

predicted <- predict(fitted_bn, node = "target", data = test_data)
predicted_nb <- predict(fitted_bn_nb, node = "target", data = test_data)

caret::confusionMatrix(predicted, new$target)
caret::confusionMatrix(predicted_nb, new$target)

###