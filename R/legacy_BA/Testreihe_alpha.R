#Simuliert
seq <- seq(from = 0.05, to = 0.95, by = 0.05)
res <- c()
for(a in seq) {
  print("### a ###")
  source(paste(getwd(),"/benchmarks/run_benchmarks_simulated_data_multi_model_p=6.R", sep = ""))
  load(paste(getwd(),"/results/alpaha_cut_",share_unlabeled,"_",data, "_n=", as.character(n), "_p=", as.character(p), "_a=", as.character(alpha), sep=""))
  res <- c(res,saved_results$`Inductive on-the-fly mean`[length(saved_results$`Inductive on-the-fly mean`)])
  View(res)
}

#Banknoten
seq <- seq(from = 0.05, to = 0.95, by = 0.05)
res <- c()
for(a in seq) {
  print("### a ###")
  source(paste(getwd(),"/benchmarks/run_benchmarks_banknote.R", sep = ""))
  load(paste(getwd(),"/results/alpaha_cut_",share_unlabeled,"_",data, "_n=", as.character(n), "_p=", as.character(p), "_a=", as.character(alpha), sep=""))
  res <- c(res,saved_results$`Inductive on-the-fly mean`[length(saved_results$`Inductive on-the-fly mean`)])
  View(res)
}


