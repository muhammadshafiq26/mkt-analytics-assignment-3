normalize <- function(x) {
  Minimum = min(x)
  Maximum = max(x)
  normalized = (x - Minimum) / (Maximum - Minimum)
  return(normalized)
}

standardize <- function(x) {
  M = mean(x)
  SD = sd(x)
  n = length(x)
  standardized = (x - M) / SD
  return(standardized)
}

calculate_metrics_classification <- function(TP, FP, FN, TN) {
  # Calculate performance measures
  misclassification <- (FP + FN) / (TP + FP + FN + TN)
  accuracy <- (TP + TN) / (TP + FP + FN + TN)
  sensitivity <- TP / (TP + FN)
  precision <- TP / (TP + FP)
  specificity <- TN / (TN + FP)
  
  # Return named vector
  return(c(Misclassification = misclassification,
           Accuracy = accuracy,
           Sensitivity = sensitivity,
           Precision = precision,
           Specificity = specificity))
}

calculate_metrics_prediction <- function(actual, predicted) {
  # Ensure inputs are numeric vectors
  actual <- as.numeric(actual)
  predicted <- as.numeric(predicted)
  
  # Calculate error metrics
  errors <- actual - predicted
  
  rmse <- sqrt(mean(errors^2))  # Root Mean Squared Error (RMSE)
  me <- mean(errors)             # Mean Error (ME)
  mad <- mean(abs(errors))       # Mean Absolute Deviation (MAD)
  mpe <- mean(errors / actual * 100) # Mean Percentage Error (MPE)
  
  # Return named vector
  metrics <- c(RMSE = rmse, ME = me, MAD = mad, MPE = mpe)
  return(metrics)
}

cumulative_lift_chart <- function(actual, predicted) {
  # Ensure actual is a factor
  actual <- as.factor(actual)
  
  # Order data by predicted probabilities (descending)
  ordered_indices <- order(predicted, decreasing = TRUE)
  actual <- actual[ordered_indices]
  predicted <- predicted[ordered_indices]
  
  # Compute cumulative number of positive cases
  cumulative_positives <- cumsum(actual == 1)
  
  # Generate number of cases
  total_samples <- length(actual)
  number_of_cases <- seq_len(total_samples)
  
  # Add (0, 0) as the starting point
  number_of_cases <- c(0, number_of_cases)
  cumulative_positives <- c(0, cumulative_positives)
  
  # Plot the cumulative lift chart
  plot(number_of_cases, cumulative_positives, type = "l",
       col = "blue", lwd = 2, xlab = "The Number of Cases",
       ylab = "Cumulative",
       main = "Cumulative Lift Chart")
  
  # Add baseline (random model)
  lines(number_of_cases, number_of_cases * (sum(actual == 1) / total_samples),
        col = "orange", lty = 1, lwd = 2)
}

roc_curve <- function(actual, predicted) {
  # Ensure inputs are numeric
  actual <- as.numeric(actual)
  predicted <- as.numeric(predicted)
  
  # Sort by predicted scores in decreasing order
  sorted_index <- order(predicted, decreasing = TRUE)
  actual <- actual[sorted_index]
  predicted <- predicted[sorted_index]
  
  # Initialize TPR and FPR vectors with (0,0) starting point
  TPR <- c(0)
  FPR <- c(0)
  thresholds <- unique(predicted)
  
  for (threshold in thresholds) {
    predicted_labels <- ifelse(predicted >= threshold, 1, 0)
    
    TP <- sum(predicted_labels == 1 & actual == 1)
    FP <- sum(predicted_labels == 1 & actual == 0)
    FN <- sum(predicted_labels == 0 & actual == 1)
    TN <- sum(predicted_labels == 0 & actual == 0)
    
    TPR <- c(TPR, TP / (TP + FN))
    FPR <- c(FPR, FP / (FP + TN))
  }
  
  # Compute AUC using trapezoidal rule
  AUC <- sum(diff(FPR) * (TPR[-1] + TPR[-length(TPR)]) / 2)
  
  # Print AUC to console
  cat("AUC:", round(AUC, 4), "\n")
  
  # Plot ROC curve
  plot(FPR, TPR, type = "l", col = "blue", lwd = 2, xlab = "1 - Specificity", ylab = "Sensitivity", main = "ROC Curve")
  text(0.5, 0.1, paste("AUC =", round(AUC, 4)), col = "black", cex = 1.2)
  abline(a = 0, b = 1, lty = 1, col = "orange", lwd = 2) # Diagonal reference line
}

roc_curve_smooth <- function(actual, predicted) {
  # Ensure inputs are numeric
  actual <- as.numeric(actual)
  predicted <- as.numeric(predicted)
  
  # Sort by predicted scores in decreasing order
  sorted_index <- order(predicted, decreasing = TRUE)
  actual <- actual[sorted_index]
  predicted <- predicted[sorted_index]
  
  # Initialize TPR and FPR vectors with (0,0) starting point
  TPR <- c(0)
  FPR <- c(0)
  thresholds <- unique(predicted)
  
  for (threshold in thresholds) {
    predicted_labels <- ifelse(predicted >= threshold, 1, 0)
    
    TP <- sum(predicted_labels == 1 & actual == 1)
    FP <- sum(predicted_labels == 1 & actual == 0)
    FN <- sum(predicted_labels == 0 & actual == 1)
    TN <- sum(predicted_labels == 0 & actual == 0)
    
    TPR <- c(TPR, TP / (TP + FN))
    FPR <- c(FPR, FP / (FP + TN))
  }
  
  # Compute AUC using trapezoidal rule
  AUC <- sum(diff(FPR) * (TPR[-1] + TPR[-length(TPR)]) / 2)
  
  # Print AUC to console
  cat("AUC:", round(AUC, 4), "\n")
  
  # Smooth the ROC curve using loess (local regression)
  smoothed_TPR <- loess(TPR ~ FPR, span = 0.3)  # Adjust span for smoothness
  smooth_curve <- predict(smoothed_TPR, seq(0, 1, length.out = 100))
  
  # Ensure the plot starts from (0,0)
  FPR_seq <- seq(0, 1, length.out = 100)
  smooth_curve[1] <- 0  # Force the first point to be (0,0)
  
  # Plot ROC curve
  plot(FPR_seq, smooth_curve, type = "l", col = "blue", lwd = 2, xlab = "1 - Specificity", ylab = "Sensivity", main = "ROC Curve")
  text(0.5, 0.1, paste("AUC =", round(AUC, 4)), col = "black", cex = 1.2)
  abline(a = 0, b = 1, lty = 1, col = "orange", lwd = 2) # Diagonal reference line
}

confusion_matrix <- function(actual, predicted, cutoff = 0.5, metrics = TRUE) {
  # Convert predicted probabilities to binary class predictions
  predicted_class <- ifelse(predicted >= cutoff, 1, 0)
  
  # Create the confusion matrix
  tp <- sum(actual == 1 & predicted_class == 1)
  fp <- sum(actual == 0 & predicted_class == 1)
  fn <- sum(actual == 1 & predicted_class == 0)
  tn <- sum(actual == 0 & predicted_class == 0)
  
  conf_matrix <- matrix(c(tn, fp, fn, tp), nrow = 2, byrow = TRUE,
                        dimnames = list("Actual" = c("0", "1"), "Predicted" = c("0", "1")))
  
  # Print confusion matrix first
  print(conf_matrix)
  
  # Compute metrics if requested
  if (metrics) {
    measures <- data.frame(
      Metric = c("Misclassification Rate", "Accuracy", "Sensitivity", "Precision", "Specificity"),
      Value = round(c((fp + fn) / (tp + fp + fn + tn),
                      (tp + tn) / (tp + fp + fn + tn),
                      tp / (tp + fn),
                      tp / (tp + fp),
                      tn / (tn + fp)), 4)
    )
    
    print(measures, row.names = FALSE)
  }
}


decile_lift_chart <- function(actual, predicted) {
  # Create a data frame
  data <- data.frame(actual = actual, predicted = predicted)
  
  # Arrange in descending order of predicted probabilities
  data <- data[order(-data$predicted), ]
  
  # Add deciles
  data$decile <- cut(rank(-data$predicted, ties.method = "random"), breaks = 10, labels = FALSE)
  
  # Calculate lift by decile
  lift_table <- aggregate(actual ~ decile, data, sum)
  total_positives <- sum(actual)
  lift_table$lift <- lift_table$actual / (total_positives / 10)
  
  # Print the lift table
  print(lift_table)
  
  # Create the bar chart using base R plotting
  barplot(lift_table$lift, names.arg = lift_table$decile, col = "blue",
          xlab = "Decile", ylab = "Lift", main = "Decile-wise Lift Chart", border = "black")
}


matching_coefficient <- function(data) {
  # Convert vector input to data frame if necessary
  if (is.vector(data)) {
    data <- as.data.frame(data)
  }
  
  # Ensure data is a data frame
  if (!is.data.frame(data)) {
    stop("Input should be a vector or a data frame.")
  }
  
  # Number of rows
  n <- nrow(data)
  
  # Initialize the similarity matrix
  similarity_matrix <- matrix(0, nrow = n, ncol = n)
  
  # Compute matching coefficients for all pairs
  for (i in 1:n) {
    for (j in i:n) {
      match_count <- sum(data[i, ] == data[j, ], na.rm = TRUE)
      total_count <- sum(!is.na(data[i, ]) & !is.na(data[j, ]))
      
      # Compute similarity
      similarity <- match_count / total_count
      similarity_matrix[i, j] <- round(similarity, 4) # Round to 4 decimal places
      similarity_matrix[j, i] <- similarity_matrix[i, j] # Ensure symmetry
    }
  }
  
  # Convert matrix to data frame with simple row/column labels
  similarity_df <- as.data.frame(similarity_matrix)
  rownames(similarity_df) <- 1:n
  colnames(similarity_df) <- 1:n
  
  return(similarity_df)
}
