Accuracy_Result <- function(Result_List) {
  result <- data.frame(round = numeric(0), accumarray = numeric(0))
  names(result) <- c("Runde", "Genauichkeit")
  for(i in 1:length(Result_List)) {
    aktuell <- c(round = i, accumarray = as.numeric(Result_List[[i]][1]))
    result <- rbind(result,aktuell)
  }
  names(result) <- c("Runde", "Genauichkeit")
  return(result)
}
