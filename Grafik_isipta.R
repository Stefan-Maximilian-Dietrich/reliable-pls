
meth <- methods[3]

make_all_Graphics_new <- function() {
  ordner_pfad <- "/Users/Stefan/Desktop/Forschung/results"
  Experiments_adress <-  list.files(path = ordner_pfad, full.names = TRUE, recursive = FALSE)
  methods <- c("SL", "SSL", "e_admissible", "SSL_variance", "SSL_entropy", "maximal", "M_MaxiMin", "M_MaxiMax")
  exp_ad <- Experiments_adress[50]
  for(exp_ad in Experiments_adress) {
    b <- TRUE
    
    tryCatch({
      load(paste0(exp_ad, "/", "e_admissible" ,"/ID_", i))
    }, error = function(e) {
      b <<- FALSE
    })
    
    means_df <- NULL
    if(b) {
      for(meth in methods) {
        all_a <- NULL
        for(i in 1:60) {
          print(paste0(exp_ad, "/", meth ,"/ID_", i))
          load(paste0(exp_ad, "/", meth ,"/ID_", i))
          
          confusion_list <- get(meth)
          vec <- unlist(lapply(confusion_list, function(x) {x$overall[1]})) #hier entscheidet sich welcher richtwert benutzt wird
          all_a <- rbind(all_a,vec )
        }
        means_df <- rbind(means_df, colMeans(all_a))
      }
      rownames(means_df) <- names(methods)
      colnames(means_df) <- as.character(0:(ncol(means_df)-1))
      means_df <- as.data.frame(t(means_df))
      
      means_df$Pseudolabel <- 0:(nrow(means_df) - 1)
      
      # Long Format erzeugen
      df_long <- pivot_longer(
        means_df,
        cols = -Pseudolabel,
        names_to = "Methode",
        values_to = "Accuracy"
      )
      
      plot <- ggplot(df_long, aes(x = Pseudolabel, y = Accuracy, color = Methode, group = Methode)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 2) +
        labs(title = exp_ad, x = "pseudolabeld Data", y = "Accuracy")
      dateiname <- sub(".*/", "", exp_ad)
      
      ggsave(paste("/Users/Stefan/Desktop/ISIPTA_Grafiken/", dateiname, ".png", sep = ""), width = 20, height = 20, units = "cm", dpi = 300,plot = plot)
      
    }
    }

  
  
}

ci_lower <- apply(all_a, 2, function(x) quantile(x, 0.025, na.rm = TRUE))
ci_upper <- apply(all_a, 2, function(x) quantile(x, 0.975, na.rm = TRUE))
median <- apply(all_a, 2, function(x) quantile(x, 0.975, na.rm = TRUE))
mean <- apply(all_a, 2, function(x) mean(x))

make_all_Graphics_new()




########

experimet_method <- function(exp, method) {
  ground_path <-   ordner_pfad <- "/Users/Stefan/Desktop/Forschung/results/"
  adress <- paste0(ground_path, exp,"/", method)
  results_adress <-  list.files(path = adress, full.names = TRUE, recursive = FALSE)
  all_list <- list()
  for(i in 1:length(results_adress)) {
    load(results_adress[i])
    all_list[[i]] <- get(method)
  }
  result_matrix <- sapply(all_list, function(inner) {sapply(inner, function(x) x$overall[1]) })
  ci_lower <- apply(result_matrix, 1, function(x) quantile(x, 0.025, na.rm = TRUE))
  ci_upper <- apply(result_matrix, 1, function(x) quantile(x, 0.975, na.rm = TRUE))
  median <- apply(result_matrix, 1, function(x) quantile(x, 0.5, na.rm = TRUE))
  mean <- apply(result_matrix, 1, function(x) mean(x))
  
  result <- data.frame(nr = 1:length(mean), method = method, ci_lower = ci_lower, median = median, mean = mean, ci_upper = ci_upper)
  return(result)
}
experimetn <- function(exp) {
  methods <- c("SL", "SSL", "e_admissible", "SSL_variance", "SSL_entropy", "maximal", "M_MaxiMin", "M_MaxiMax")
  results <- NULL
  for(j in 1:length(methods)) {
    df <- experimet_method(exp = exp, method = methods[j])
    results <- rbind(results, df)
  }
  return(results)
}

results <- experimetn("Simplex_L_48_U_384_alp_0.3_grid_10")
df_long <- results %>%
  pivot_longer(cols = c(ci_lower, median, mean, ci_upper),
               names_to = "metric",
               values_to = "value")

# Plot: Linien für jede Metrik, getrennt nach Methode
ggplot(df_long, aes(x = nr, y = value, color = metric, linetype = metric)) +
  geom_line(size = 1) +
  facet_wrap(~method) +  # optional: eine Grafik pro Methode
  theme_minimal() +
  labs(title = "Konfidenzmetriken pro Methode",
       x = "nr",
       y = "Wert",
       color = "Metrik",
       linetype = "Metrik")



### All in one 
ggplot(results, aes(x = nr, group = method)) +
  # Shaded confidence band
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = method), alpha = 0.2) +
  
  # Mean line (solid)
  geom_line(aes(y = mean, color = method), size = 1) +
  
  # Median line (dotted)
  geom_line(aes(y = median, color = method), linetype = "dotted", size = 1) +
  
  # Optional: show points at medians
  # geom_point(aes(y = median, color = method), shape = 16, size = 1.5) +
  
  theme_minimal() +
  labs(
    title = "Model Performance over nr",
    x = "nr",
    y = "Performance",
    fill = "Method",
    color = "Method"
  )

#####
ggplot(results, aes(x = nr)) +
  # Shaded ribbon between ci_lower and ci_upper
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = method), alpha = 0.2) +
  
  # Solid line for mean
  geom_line(aes(y = mean, color = method), size = 1) +
  
  # Dotted line for median
  geom_line(aes(y = median, color = method), linetype = "dotted", size = 1) +
  
  # One plot per method
  facet_wrap(~method) +
  
  theme_minimal() +
  labs(
    title = "Performance Metrics with Confidence Intervals per Method",
    x = "nr",
    y = "Accuracy",
    fill = "Method",
    color = "Method"
  )