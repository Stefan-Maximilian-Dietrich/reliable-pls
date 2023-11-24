n <- 60
p <- 6
seq <- seq(from = 0.05, to = 0.65, by = 0.05)

reso <- c()
for(i in 1:length(seq)){
  j <- seq[i]
  load(paste(getwd(),"/results/alpaha_cut_",share_unlabeled,"_","simulated", "_n=", as.character(n), "_p=", as.character(p), "_a=", as.character(j), sep=""))
  k <- length(saved_results$`Inductive on-the-fly mean`)
  reso <- c(reso, saved_results$`Inductive on-the-fly mean`[k])
}

Alpha <- as.data.frame(cbind(alpha = seq, result = reso))

ggplot(data = Alpha, aes(x = alpha, y = result)) +
  geom_line() +
  geom_point()