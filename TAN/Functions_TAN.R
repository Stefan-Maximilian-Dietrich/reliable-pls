sampler_NB_up <- function(n_labled, n_unlabled, data, formula) {
  variables <- all.vars(formula) 
  target <- variables[1]
  data_used <- data[, variables]
  
  categories <- unique(data_used[, target])
  if(length(categories)*2  > n_labled) {
    stop("Labeld date less than reqiert to fit a GNB")
  }
  
  train <- NULL
  for(cat in categories){
    data_temp <- data_used[data_used[,target]==cat, ]
    first <- data_temp[sample(1:nrow(data_temp), 1), , drop=FALSE]
    train <- rbind(train, first)
    col_to_check <- 2:ncol(data_temp)
    #x <- data_temp[1,]
    indicator <- !apply(data_temp, 1, function(x) {as.numeric(first[col_to_check]) == as.numeric(x)[-1]})
    
    witch_diff <- apply(indicator, 2, all)
    if(!any(witch_diff)) {
      stop(paste("Data set is not viable for GAUSIAN Naive Bayes. Problem in", cat))
    }
    
    data_temp2 <- data_temp[witch_diff, ]
    second <- data_temp2[sample(1:nrow(data_temp2), 1), , drop=FALSE]
    train <- rbind(train, second)
  }
  
  filterd <- data_used[!apply(data_used, 1, function(x) any(apply(train, 1, function(y) all(x == y)))), , drop=FALSE]
  n <- nrow(filterd)
  k <- nrow(train)
  
  train_idx <- sample(1:n, size = n_labled-k)  
  remaining_idx <- setdiff(1:n, train_idx)
  unlabeld_idx <- sample(remaining_idx, size = n_unlabled) 
  test_idx <- setdiff(remaining_idx, unlabeld_idx)  
  
  train_rest <- filterd[train_idx, ]
  train <- rbind(train, train_rest)
  unlabed <- filterd[unlabeld_idx,]
  test <- filterd[test_idx,]
  
  
  return(list(train, unlabed, test))
  
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

get_TAN_structur <- function(dt_train) {
  dt_train <- as.data.frame(dt_train)
  X_discrete <- dt_train[, -c(1)]
  S_disc <- dt_train[, c(1)]
  
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
  
  
  if(TRUE){
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
  }
  
  
  
  options(scipen =0)  # bevorzugt wissenschaftliche Notation stark
  return(tan_mat)
  ###
}

get_NB_structure <- function(dt_train) {
  names_vector <- colnames(dt_train)
  zero_matrix <- matrix(0, nrow = length(names_vector), ncol = length(names_vector),dimnames = list(names_vector, names_vector))
  zero_matrix[1, -1] <- 1
  return(zero_matrix)
  
}

get_modell <- function(structure ,dt_train) {
  dt_train[, datID := NULL]
  if(structure == "TAN"){
    tan_structur <- get_TAN_structur(dt_train)
  }
  if(structure == "NB"){
    tan_structur <-get_NB_structure(dt_train)
  }
  
  dt_arc <- as.data.table(as.table(tan_structur))[N != 0][, C := "target"][, N := NULL][, c( "V2", "C", "V1")]# [, arcID := 1:.N]
  setnames(dt_arc,  c( "X", "C", "Y")) 
  VarVal <- unique(melt(dt_train[, datID := 1:.N], id.vars = "datID", measure.vars = colnames(dt_train)[-length(dt_train)], variable.name = "Var", value.name = "val")[, datID:= NULL])#[, YID := .GRP, by = Y][, combID:= 1:.N]
  modell_structure <- na.omit(dt_arc[VarVal[Var != "target"], on = .(X = Var), allow.cartesian = TRUE][VarVal[Var == "target"], on = .(C = Var), allow.cartesian = TRUE][VarVal, on = .(Y = Var), allow.cartesian = TRUE])[, c( "X", "val", "C", "i.val", "Y", "i.val.1")]
  setnames(modell_structure, c( "X", "x", "C", "c", "Y", "y")) 
  modell_structure <- modell_structure[!(Y == "target" & C == "target" & c != y) ] 
  return(modell_structure)
}

get_cond_prob <- function(dt_train) {
  dt_id <- dt_train[, ID := seq_len(.N)]
  vars_y <- setdiff(names(dt_id), "ID")
  vars_x <- setdiff(names(dt_id), c("target","ID"))
  
  long_Y <- melt(dt_id, id.vars = "ID", measure.vars = vars_y, variable.name = "Y", value.name = "y")
  long_X <- melt(dt_id, id.vars = "ID", measure.vars = vars_x, variable.name = "X", value.name = "x")
  long_C <- melt(dt_id, id.vars = "ID", measure.vars = "target", variable.name = "C", value.name = "c")
  
  long_YX <- long_Y[long_X, on = .(ID = ID), allow.cartesian = TRUE]
  long_XYC <- long_C[long_YX, on = .(ID = ID), allow.cartesian = TRUE]
  
  p_X_CY <- long_XYC[, .N, by = c( "X", "x", "C", "c", "Y", "y")][, prob := N / sum(N), by = .( X, C, c, Y, y)]
  
  return(p_X_CY)
}

get_Evidence <- function(structure, dt_train, prioris) {
  prioris[, prioID := 1:.N]
  dt_train[, datID := 1:.N][, ID := NULL]
  modell_structure <- get_modell(structure ,dt_train[,datID:= NULL])[, modID := 1:.N]
  
  data_x_modell <- CJ(datID = 1:nrow(dt_train), modID = 1:nrow(modell_structure))
  
  modell <- modell_structure[data_x_modell, on = "modID", allow.cartesian = TRUE ]
  
  vars_x <- colnames(dt_train)[-length(colnames(dt_train))]
  
  long_X <- melt(dt_train, id.vars = "datID", measure.vars = vars_x, variable.name = "X", value.name = "x")
  modell_all <- modell[long_X, on = .(datID = datID, X=X,x=x), nomatch = 0]
  
  modell_dep <- modell_all[Y != "target"][long_X, on = .(datID = datID, Y=X,y=x), nomatch = 0]
  
  mod <- rbind(modell_all[Y == "target"], modell_dep)
  
  P <- get_cond_prob(dt_train[, datID := NULL])
  
  
  evi <- mod[P, on = c("X", "x", "C", "c", "Y", "y"), nomatch = 0]
  
  prio_vars <- colnames(prioris)[-length(colnames(prioris))]
  priori_long <- melt(prioris, id.vars = "prioID", measure.vars = prio_vars, variable.name = "c", value.name = "prio")
  Join_prio <- CJ(prioID = 1:nrow(prioris), datID = 1:nrow(dt_train))
  evi_prio_join <- evi[Join_prio, on = "datID", allow.cartesian = TRUE ]
  evi_prio <- evi_prio_join[priori_long, on = c("prioID", "c")]
  
  eevi_prio1 <-evi_prio[, pxy := prod(prob), by = .(c, prioID, datID)]
  eevi_prio2 <- unique(eevi_prio1[,c("datID", "prioID","c", "prio", "pxy")])
  eevi_prio3 <- eevi_prio2[, px := pxy * prio ]
  eevi_prio4 <- unique(eevi_prio3[, pX := sum(px), by = .(prioID, datID)][, c("datID", "prioID", "pX")])
  eevi_prio5 <- unique(eevi_prio4[, evidenze := prod(pX), by = .(prioID)][, c("prioID", "evidenze")])
  ln_eevi_prio5 <- eevi_prio5
  #ln_eevi_prio5$evidenze <- log(ln_eevi_prio5$evidenze)
  #plot(ln_eevi_prio5$evidenze )
  return(eevi_prio5)
  
} 

do_alpha_cut <- function(priori_evidence, alpha) {
  cut <- min(priori_evidence$evidenze) + (max(priori_evidence$evidenze) - min(priori_evidence$evidenze))*alpha
  cut_prioris <- priori_evidence[priori_evidence$evidenze >= cut]
  
  #p <- ggplot(priori_evidence, aes(x = prioID, y = evidenze)) +
  #geom_point() +
  #geom_hline(yintercept = cut, color = "red", linetype = "dashed", size = 1)
  #print(p)
  return(cut_prioris)
}

get_max_priori <- function(cut_priori, prioris) {
  id_max <- cut_priori[which.max(cut_priori$evidenze),][1,]$prioID
  max_prior <- prioris[prioID == id_max]
  prio_vars <- colnames(max_prior)[-length(colnames(max_prior))]
  result <- melt(max_prior, id.vars = "prioID", measure.vars = prio_vars, variable.name = "c", value.name = "prio")[, prioID := NULL]
  return(result)
}

predict_tan <- function(structure ,priori, dt_train, dt_test){
  P <- get_cond_prob(dt_train[, datID := NULL])[, N:=NULL]
  modell <- get_modell(structure = structure,dt_train[, datID := NULL][, ID := NULL])
  cond_modell <- P[modell, on = .( X=X,x=x, C=C,c=c ,Y=Y,y=y)]
  
  # NA bearbeitung 
  cond_modell[is.na(prob)][, "prob"] <- 0
  cond_modell[, "prob"] = cond_modell[, "prob"]  + 1/(2*nrow(dt_train))
  #
  cond_prio_modell <- cond_modell[priori, on = .(c)][, modID := 1:.N]
  
  
  #einaml für alle ungelabelten daten 
  dt_test[, ID:= NULL][, TdatID:= 1:.N][, target:= NULL]
  var_udat <- colnames(dt_test)[- length(colnames(dt_test))]
  dt_test_long <- melt(dt_test, id.vars = "TdatID", measure.vars = var_udat, variable.name = "X", value.name = "x")
  
  data_x_modell <- CJ(TdatID = 1:nrow(dt_test), modID = 1:nrow(cond_prio_modell))
  
  modell_data <- cond_prio_modell[data_x_modell, on = .(modID)]
  x_clean <- modell_data[dt_test_long, on = .(TdatID, X, x),  nomatch = 0]
  y_clean <-  x_clean[Y != "target"][dt_test_long, on = .(TdatID = TdatID, Y=X,y=x), nomatch = 0]
  predict_modell <- rbind(x_clean[Y == "target"], y_clean)
  
  predict_modell2 <- unique(predict_modell[, p_xyc := prod(prob), by = .(TdatID, c)][,c("TdatID", "c", "p_xyc", "prio")])
  predict_modell3 <- predict_modell2[, P:= p_xyc*prio][, c("TdatID", "c", "P")]
  predictions_pre <- predict_modell3[, a := max(P), by = .(TdatID) ][, whichMAX := (P==a)][which(whichMAX), ][, c("c")]
  colnames(predictions_pre) <- "target"
  return(predictions_pre)
}

give_pseudo_label <- function(structure = "TAN",priori, dt_train, dt_unlabled){
  P <- get_cond_prob(dt_train[, datID := NULL])[, N:=NULL]
  modell <- get_modell(structure = structure,dt_train[, datID := NULL][, ID := NULL])
  cond_modell <- P[modell, on = .( X=X,x=x, C=C,c=c ,Y=Y,y=y)]
  
  # NA bearbeitung 
  cond_modell[is.na(prob)][, "prob"] <- 0
  cond_modell[, "prob"] = cond_modell[, "prob"]  + 1/(2*nrow(dt_train))
  #
  cond_prio_modell <- cond_modell[priori, on = .(c)][, modID := 1:.N]
  
  
  #einaml für alle ungelabelten daten 
  dt_unlabled[, ID:= NULL][, UdatID:= 1:.N][, target:= NULL]
  var_udat <- colnames(dt_unlabled)[- length(colnames(dt_unlabled))]
  dt_unlabled_long <- melt(dt_unlabled, id.vars = "UdatID", measure.vars = var_udat, variable.name = "X", value.name = "x")
  
  data_x_modell <- CJ(UdatID = 1:nrow(dt_unlabled), modID = 1:nrow(cond_prio_modell))
  
  modell_data <- cond_prio_modell[data_x_modell, on = .(modID)]
  x_clean <- modell_data[dt_unlabled_long, on = .(UdatID, X, x),  nomatch = 0]
  y_clean <-  x_clean[Y != "target"][dt_unlabled_long, on = .(UdatID = UdatID, Y=X,y=x), nomatch = 0]
  predict_modell <- rbind(x_clean[Y == "target"], y_clean)
  
  predict_modell2 <- unique(predict_modell[, p_xyc := prod(prob), by = .(UdatID, c)][,c("UdatID", "c", "p_xyc", "prio")])
  predict_modell3 <- predict_modell2[, P:= p_xyc*prio][, c("UdatID", "c", "P")]
  predictions_pre <- predict_modell3[, a := max(P), by = .(UdatID) ][, whichMAX := (P==a)][which(whichMAX), ][, c("c")]
  colnames(predictions_pre) <- c("target")
  return(cbind(predictions_pre, dt_unlabled))
}

give_pseudo_label_all <- function(structure = "TAN",priori, dt_train, dt_unlabled){
  P <- get_cond_prob(dt_train[, datID := NULL])[, N:=NULL]
  modell <- get_modell(structure = structure,dt_train[, datID := NULL][, ID := NULL])
  cond_modell <- P[modell, on = .( X=X,x=x, C=C,c=c ,Y=Y,y=y)]
  
  # NA bearbeitung 
  cond_modell[is.na(prob)][, "prob"] <- 0
  cond_modell[, "prob"] = cond_modell[, "prob"]  + 1/(2*nrow(dt_train))
  #
  cond_prio_modell <- cond_modell[priori, on = .(c)][, modID := 1:.N]
  
  
  #einaml für alle ungelabelten daten 
  dt_unlabled[, ID:= NULL][, UdatID:= 1:.N][, target:= NULL]
  var_udat <- colnames(dt_unlabled)[- length(colnames(dt_unlabled))]
  dt_unlabled_long <- melt(dt_unlabled, id.vars = "UdatID", measure.vars = var_udat, variable.name = "X", value.name = "x")
  
  data_x_modell <- CJ(UdatID = 1:nrow(dt_unlabled), modID = 1:nrow(cond_prio_modell))
  
  modell_data <- cond_prio_modell[data_x_modell, on = .(modID)]
  x_clean <- modell_data[dt_unlabled_long, on = .(UdatID, X, x),  nomatch = 0]
  y_clean <-  x_clean[Y != "target"][dt_unlabled_long, on = .(UdatID = UdatID, Y=X,y=x), nomatch = 0]
  predict_modell <- rbind(x_clean[Y == "target"], y_clean)
  
  predict_modell2 <- unique(predict_modell[, p_xyc := prod(prob), by = .(UdatID, c)][,c("UdatID", "c", "p_xyc", "prio")])
  predict_modell3 <- predict_modell2[, P:= p_xyc*prio][, c("UdatID", "c", "P")]
  colnames(predict_modell3) <- c("UdatID", "target", "prob")
  return(predict_modell3)
}

get_Expected_utilitys <- function(structure = "TAN",dt_pseudolabelt, dt_train, cut_priori, prioris) {
  prio_long <- melt(prioris[cut_priori[, prioID], ],  id.vars = "prioID",  variable.name = "c", value.name = "prio")
  
  ## Pseudol albelt data 
  dt_train[, datID := 1:.N]
  train_x_pseudo <- CJ(datID = 1:nrow(dt_train), UdatID = 1:nrow(dt_pseudolabelt))
  dt_train_uid <- dt_train[train_x_pseudo, on = .(datID)]
  dt_pseudolabelt[, datID := 0] 
  dat_pseudo <- rbind(dt_train_uid, dt_pseudolabelt)
  dat_pseudo_truth <- dat_pseudo[, .(datID ,UdatID, target)]
  
  
  vars_y <- setdiff(names(dat_pseudo), c("datID","UdatID"))
  vars_x <- setdiff(names(dat_pseudo), c("target","datID","UdatID"))
  
  long_Y <- melt(dat_pseudo, id.vars = c("datID","UdatID"), measure.vars = vars_y, variable.name = "Y", value.name = "y")
  long_X <- melt(dat_pseudo, id.vars = c("datID","UdatID"), measure.vars = vars_x, variable.name = "X", value.name = "x")
  long_C <- melt(dat_pseudo, id.vars = c("datID","UdatID"), measure.vars = "target", variable.name = "C", value.name = "c")
  
  long_YX <- long_Y[long_X, on = .(datID,UdatID ), allow.cartesian = TRUE]
  long_XYC <- long_C[long_YX, on = .(datID, UdatID), allow.cartesian = TRUE]
  
  P <- long_XYC[, .N, by = .( UdatID, X,x, C, c, Y, y)][, prob := N / sum(N), by = .(UdatID, X, C, c, Y, y)]
  modell <- get_modell(structure = structure,dt_train[, datID := NULL][, ID := NULL])[, modID := 1:.N]
  
  modell_x_udat <- CJ(modID = 1:nrow(modell), UdatID = 1:nrow(dt_pseudolabelt))
  
  modell_all_u <- modell[modell_x_udat, on = .(modID)]
  cond_modell <- P[modell_all_u, on = .(UdatID, X, x, C,c, Y, y)]
  
  # NA bearbeitung 
  cond_modell[is.na(prob)][, "prob"] <- 0
  cond_modell[, "prob"] = cond_modell[, "prob"]  + 1/(2*nrow(dt_train))
  #
  
  long_dat_pseudo <- melt(dat_pseudo, id.vars = c("datID", "UdatID"), variable.name = "X", value.name = "x")
  cond_xcy <- long_dat_pseudo[cond_modell, on = .(UdatID, X, x),  allow.cartesian = TRUE]
  cond_c <- cond_xcy[C == Y]
  cond_cy <- long_dat_pseudo[cond_xcy[C != Y], on = .(UdatID,datID , X =Y, x =y)]
  colnames(cond_cy) <- c("datID","UdatID" , "X" , "x" ,"Y"  ,  "y"    ,  "C"    ,     "c"   ,  "N" , "prob" ,"modID")
  cond <- rbind(cond_c, cond_cy)
  cond_truth <- cond[dat_pseudo_truth, on = .(UdatID,datID)]
  P_XCY <- unique(cond_truth[, truth:= (c==target)][, p_xcy := prod(prob), by = .(datID, UdatID, c)][, .(datID, UdatID, c, truth, p_xcy)])
  exp_utility_table <- unique(P_XCY[prio_long, on = .(c),  allow.cartesian = TRUE][, truth := -1 + 2* truth][, P := p_xcy*prio*truth][, exp_utility := sum(P), by = .(prioID,  UdatID)][, .(UdatID, prioID, exp_utility)])
  
  return(exp_utility_table)
}

### criteria DT
M_MaxiMin_a <- function(exp_utility_table) {
  min <- exp_utility_table[exp_utility_table[, .( min = which.min(exp_utility)), by = .(prioID)], on =.(UdatID = min, prioID)]
  max <- min[min[, which.max(exp_utility)],][,UdatID][1]
  return(max)
}

M_MaxiMax_a <- function(exp_utility_table) {
  maxi <- exp_utility_table[exp_utility_table[, .( min = which.max(exp_utility)), by = .(prioID)], on =.(UdatID = min, prioID)]
  max <- maxi[maxi[, which.max(exp_utility)],][,UdatID][1]
  return(max)
}

E_admissible <- function(exp_utility_table) {
  e_ad <- unique(exp_utility_table[exp_utility_table[, .( min = which.max(exp_utility)), by = .(prioID)], on =.(UdatID = min, prioID)][,UdatID])
  return(e_ad)
}

maximal_a <- function(exp_utility_table) {
  id_elemets_a <- copy(exp_utility_table)[, IDa:= 1:nrow(exp_utility_table)]
  id_elemets_b<- copy(exp_utility_table)[, IDb:= 1:nrow(exp_utility_table)]
  a_x_b <- CJ(IDa = 1:nrow(exp_utility_table), IDb = 1:nrow(exp_utility_table))
  maxim <- id_elemets_a[a_x_b, on = .(IDa)][id_elemets_b, on = .(IDb)][, compare :=  exp_utility >= i.exp_utility][, any(compare), by = .(UdatID, i.UdatID)][, all(V1), by = .(UdatID)][which(V1)][, UdatID]
  return(maxim)
}

SSL_a <- function(dt_pseudolabelt_all) {
  action <- dt_pseudolabelt_all[, max := max(prob), ][max == prob,][1,][, UdatID]
  return(action)
}

SSL_variance_a <- function(dt_pseudolabelt_all) {
  dt_diff <- dt_pseudolabelt_all[
    , {
      sorted <- sort(prob, decreasing = TRUE)
      diff <- if (length(sorted) >= 2) sorted[1] - sorted[2] else NA
      .(differenz = diff)
    }, 
    by = UdatID
  ]
  action <- dt_diff[which.max(differenz)][, UdatID]

  return(action)
}

SSL_entropy_a <- function(dt_pseudolabelt_all) {
  dt_entropy <- dt_pseudolabelt_all[
    , .(entropy = -sum(prob * log2(prob))), 
    by = UdatID
  ]
  action <- dt_entropy[which.max(-entropy)][, UdatID]

  return(action)
}


### criteria Normal 


gerate_priori_simplex_rec <- function(levels, refinement){
  k <- length(levels)
  n <- refinement - k
  priori_matrix <- combinations(n, k)
  priori_matrix <- priori_matrix + 1
  priori_matrix <- priori_matrix/refinement
  colnames(priori_matrix) <- as.character(levels)
  return(priori_matrix)
}
