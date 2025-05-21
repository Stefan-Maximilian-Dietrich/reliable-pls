function(dt_data_discrete, tan_mat)

dt_data_discrete <- as.data.table(discretize_rec_min_ent(data_set[[1]]))
dt_data_new <- as.data.table(discretize_rec_min_ent_test(data_train = data_set[[1]], data_test = data_set[[3]]))[,target := NULL][, ID:= 1:.N]
## Modell
dt_id <- dt_data_discrete[, ID := seq_len(.N)]
vars <- setdiff(names(dt_id), "ID")

long_Y <- melt(dt_id, id.vars = "ID", measure.vars = vars, variable.name = "Yvar", value.name = "Yval")
long_X <- melt(dt_id, id.vars = "ID", measure.vars = vars, variable.name = "Xvar", value.name = "Xval")
long_C <- melt(dt_id, id.vars = "ID", measure.vars = "target", variable.name = "Xvar", value.name = "Xval")

long_YX <- long_Y[long_X, on = .(ID = ID), allow.cartesian = TRUE]
long_XYC <- long_C[long_YX, on = .(ID = ID), allow.cartesian = TRUE]
setnames(long_XYC,  c("ID", "C", "c", "X", "x", "Y", "y")) 

p_X_CY <- long_XYC[, .N, by = c( "X", "x", "C", "c", "Y", "y")][, prob := N / sum(N), by = .( X, C, c, Y, y)]
##

## Ifo Unlabelt 
vars_new <- setdiff(names(dt_data_new), "ID")
dt_new_inforamtions <-  melt(dt_data_new, id.vars = "ID", measure.vars = vars_new, variable.name = "Var", value.name = "val")

## Modell
dt_arcs <- as.data.table(as.table(tan_mat))[N != 0][, C := "target"][, N := NULL][, arcID := 1:.N][, c("arcID", "V2", "C", "V1")]
setnames(dt_arcs,  c("arcID", "X", "C", "Y")) 
dt_categories <- data.table(catID = 1:length(unique(dt_data_discrete$target)), c = unique(dt_data_discrete$target))
dt_id <- CJ(ID = 1:nrow(dt_data_new), arcID = 1:nrow(dt_arcs), catID = 1:length(unique(dt_data_discrete$target)))
dt_all_arcs <- dt_id[dt_arcs, on =  "arcID"][dt_categories, on = "catID"]
dt_empty <- dt_all_arcs[, y := ifelse(Y == "target", as.character(c), NA)][, x := character(0)]
setcolorder(dt_empty,c( "ID", "arcID", "catID", "X", "x", "C", "c", "Y", "y")) 

## Predict 
dt_new_full <- dt_empty[dt_new_inforamtions, on = .(X = Var, ID = ID), x:= val]
dt_new_full[is.na(y)] <- dt_new_full[is.na(y)][dt_new_inforamtions, on = .(Y = Var, ID = ID), y := val]
predict_arcs <- dt_new_full[p_X_CY, on = .(X, x, C, c, Y, y), prop := prob]



