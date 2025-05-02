function(dt_data_discrete, tan_mat)

dt_data_discrete <- as.data.table(discretize_rec_min_ent(data_set[[1]]))
dt_data_new <- as.data.table(discretize_rec_min_ent_test(data_train = data_set[[1]], data_test = data_set[[3]]))[,target := NULL]
## Modell
dt_id <- dt_data_discrete[, ID := seq_len(.N)]
vars <- setdiff(names(dt_id), "ID")

long_Y <- melt(dt_id, id.vars = "ID", measure.vars = vars, variable.name = "Yvar", value.name = "Yval")
long_X <- melt(dt_id, id.vars = "ID", measure.vars = vars, variable.name = "Xvar", value.name = "Xval")
long_C <- melt(dt_id, id.vars = "ID", measure.vars = "target", variable.name = "Xvar", value.name = "Xval")

long_YX <- long_Y[long_X, on = .(ID = ID), allow.cartesian = TRUE]
long_XYC <- long_C[long_YX, on = .(ID = ID), allow.cartesian = TRUE]
setnames(long_XYC,  c("ID", "C", "c", "X", "x", "Y", "y")) 

p_X_CY <- long_XYC[, .N, by = c( "X", "x", "C", "c", "Y", "y")][, rel_freq := N / sum(N), by = .( X, C, c, Y, y)]
##

## Ifo Unlabelt 

## Predict
dt_arcs <- as.data.table(as.table(tan_mat))[N != 0][, C := "target"][, N := NULL][, arcID := 1:.N][, c("arcID", "V2", "C", "V1")]
setnames(dt_arcs,  c("arcID", "X", "C", "Y")) 
dt_categories <- data.table(catID = 1:length(unique(dt_data_discrete$target)), c = unique(dt_data_discrete$target))
dt_id <- CJ(ID = 1:nrow(dt_data_new), arcID = 1:nrow(dt_arcs), catID = 1:length(unique(dt_data_discrete$target)))
dt_all_arcs <- dt_id[dt_arcs, on =  "arcID"][dt_categories, on = "catID"]
dt_empty <- dt_all_arcs[, y := ifelse(Y == "target", as.character(c), NA)][, x := NA]
setcolorder(dt_empty,c( "ID", "arcID", "catID", "X", "x", "C", "c", "Y", "y")) 



jaja2[dara_log_unlablet, on = .(X = Yvar, ID = id), x := Yval]





result_merge <- merge(p_X_CY, dt_long_ordert, by = c("X", "C", "Y"))
result_merge <- result_merge[order(c)]
setcolorder(result_merge,c( "X", "x", "C", "c", "Y", "y"))  # gewünschte Reihenfolge


data_log <- melt(   as.data.table(data_discrete)[, id := seq_len(.N)], id.vars = "id", measure.vars = colnames(data_discrete),
                    variable.name = "Yvar", value.name = "Yval")

dara_log_unlablet <- data_log[Yvar != "target"]

n_unlabelt <- nrow(data_discrete)


jaja2[, x := as.character(x)]

# Jetzt den Join durchführen
jaja2[dara_log_unlablet, on = .(X = Yvar, ID = id), x := Yval]

jaja2[, y := as.character(y)]

# Jetzt den Join durchführen
jaja2[is.na(y)] <- jaja2[is.na(y)][dara_log_unlablet, on = .(Y = Yvar, ID = id), y := Yval]

jaja2[result_merge, on = .(X, x, C, c, Y, y), cond_prob := rel_freq]

nein <- jaja2[, .(prod_cond_prob = prod(cond_prob)), by = .(ID, c)]

jaja2[ID == 1,]
