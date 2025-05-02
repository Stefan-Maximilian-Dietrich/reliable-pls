freq_prob <- function(S, C) {
  prob <- nrow(S[S$target == C,])/nrow(S)
  return(prob)
}

class_entropy <- function(S) {
  labels <- unique(S$target)
  c_entropy <- 0
  for(C in labels){
    c_entropy <- c_entropy - freq_prob(S, C) * log(freq_prob(S, C))
  }
  return(c_entropy)
}

class_info_entropy <- function(S, A, t) {
  S1 <- S[S[ ,A] >= t,]
  S2 <- S[S[ ,A] < t, ]
  nrow(S1)/nrow(S) * class_entropy(S1) + nrow(S2)/nrow(S) * class_entropy(S2) 
}

t_min_brutforce <- function(S, A) {
  order_unique_points <- unique(S[,A])[order(unique(S[,A]))]
  potential_points <- order_unique_points[-c(length(order_unique_points))] + (order_unique_points[-c(1)] - order_unique_points[-c(length(order_unique_points))] ) / 2
  entropy_t <- do.call(rbind, lapply(potential_points, function(t) c(point = t, entropy = class_info_entropy(S,A,t))))
  #plot(entropy_t, main = A)
  t_min <- entropy_t[which.min(entropy_t[, 2]), 1]
}

Gain <- function(S, A, t) {
  gain <- class_entropy(S) - class_info_entropy(S, A, t)
  return(gain)
}

Delta <- function(S, A, t) {
  k <- length(unique(S$target))
  k1 <- length(unique(S$target[S[,A] >= t]))
  k2 <- length(unique(S$target[S[,A] < t]))
  S1 <- S[S[ ,A] >= t,]
  S2 <- S[S[ ,A] < t, ]
  
  delta <- log2(3^k-2) - (k * class_entropy(S) - k1 * class_entropy(S1) - k2 * class_entropy(S2))  
  return(delta)
}

stop_creteria <- function(S, A, t) {
  N <- nrow(S)
  stop <- Gain(S, A, t) < log2(N-1)/N + Delta(S, A, t)/N
  return(stop)
}

rec_boudrys <- function(S, A){
  t <- t_min_brutforce(S, A)
  if(!stop_creteria(S,A,t)) {
    S1 <- S[S[ ,A] >= t,]
    S2 <- S[S[ ,A] < t, ]
    t1 <- c(t, rec_boudrys(S1, A))
    t2 <- c(t, rec_boudrys(S2, A))
    t_out <- c(t1, t2)
    return(t_out)
  } else {
    return(NULL)
  }
}

bins <- function(data) {
  features <- colnames(data[, -c(1)] )
  bin_list <-  lapply(features, function(x) numeric(0))
  names(bin_list) <- features
  
  for(A in features) {
    S <- data
    bin_list[[A]] <- c(bin_list[[A]], sort(unique(rec_boudrys(S, A))))
  }
  
  
  for(A in features) {
    if(length(bin_list[[A]]) < 1) {
      S <- data
      bin_list[[A]] <- t_min_brutforce(S, A)
    }
  }
  
  return(bin_list)
}

discretize_rec_min_ent_test <-function(data_train, data_test) {
  features <- colnames(data_train[, -c(1)] )
  disc_data <- as.data.frame(data_test$target)
  bin_list <- bins(data_train)
  
  for(A in features) {
    disc_data<- cbind(disc_data, as.factor(rowSums((outer(data_test[, A],unlist(bin_list[A]), ">"))*1)))
  }
  
  colnames(disc_data) <- c("target", features)
  return(disc_data)
} 

discretize_rec_min_ent <- function(data) {
  features <- colnames(data[, -c(1)] )
  disc_data <- as.data.frame(data$target)
  bin_list <- bins(data)
  for(A in features) {
    disc_data<- cbind(disc_data, as.factor(rowSums((outer(data[, A],unlist(bin_list[A]), ">"))*1)))
  }
  colnames(disc_data) <- c("target", features)
  return(disc_data)
} 

P_xyz <- function(X,x,Y,y,Z,z) {
  bool_mat <- cbind(X==x, Y==y, Z==z) 
  count <- sum(apply(bool_mat, 1, function(e) all(e)) *1)
  return(count/length(X))
}

P_xy_z <- function(X,x,Y,y,Z,z) {
  bool_mat <- cbind(X==x, Y==y, Z==z) 
  count <- sum(apply(bool_mat, 1, function(e) all(e)) *1)
  condid <- sum((Z==z) *1)
  return(count/condid)
}

P_x_z <- function(X,x,Z,z) {
  bool_mat <- cbind(X==x, Z==z) 
  count <- sum(apply(bool_mat, 1, function(e) all(e)) *1)
  condid <- sum((Z==z) *1)
  return(count/condid)
}
  
con_mut_info <- function(X,x,Y,y,Z,z) {
  cmi <- P_xyz(X,x,Y,y,Z,z) * log(P_xy_z(X,x,Y,y,Z,z) / ( P_x_z(X,x,Z,z)  *  P_x_z(Y,y,Z,z) ))
  if(is.nan(cmi)) {
    cmi <-0
  }
  return(cmi)
}

conditional_mutual_info <- function(X,Y,Z) {
  x_cat <- unique(X)
  y_cat <- unique(Y)
  z_cat <- unique(Z)
  all_cat <- expand.grid(X = x_cat, Y = y_cat, Z = z_cat )
  result <- sum(apply(all_cat, 1, function(a) con_mut_info(X, a[1],Y, a[2], Z, a[3])))
  return(result)
}

