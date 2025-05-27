weighted_cumsum_parallel <- function(M, C) {
  n <- ncol(C)  # Number of columns in C
  cumulative_result <- matrix(0, nrow = nrow(C), ncol = ncol(C))  # Initialize the result matrix
  
  # Set up parallel backend
  no_cores <- detectCores() - 1  # Use one less than the available cores
  cl <- makeCluster(no_cores)
  registerDoParallel(cl)
  
  # Parallel computation for each column
  cumulative_result <- foreach(j = 1:n, .combine = cbind, .packages = "expm") %dopar% {
    cumulative_sum <- matrix(0, nrow = nrow(C), ncol = 1)  # Reset cumulative sum for each column
    
    # For each column j, accumulate the weighted terms up to j
    for (k in 1:j) {
      # Compute M^(j-k) (power of M for each term)
      M_k <- M %^% (j - k)  
      
      # Multiply M^k with the k-th column of C
      weighted_term <- M_k %*% C[, k, drop = FALSE]
      
      # Accumulate the weighted term into cumulative sum for column j
      cumulative_sum <- cumulative_sum + weighted_term
    }
    
    # Return the cumulative sum for column j
    cumulative_sum[, 1]
  }
  
  # Stop parallel backend
  stopCluster(cl)
  
  return(cumulative_result)
}



########
# weighted_cumsum_parallel <- function(M, C) {
#   n <- ncol(C)  # Number of columns in C
#   cumulative_result <- matrix(0, nrow = nrow(C), ncol = ncol(C))  # Initialize the result matrix
#   
#   # Set up parallel backend
#   no_cores <- detectCores() - 1  # Use one less than the available cores
#   cl <- makeCluster(no_cores)
#   registerDoParallel(cl)
#   
#   # Parallel computation for each column
#   cumulative_result <- foreach(j = 1:n, .combine = cbind, .packages = "expm") %dopar% {
#     cumulative_sum <- matrix(0, nrow = nrow(C), ncol = 1)  # Reset cumulative sum for each column
#     
#     # For each column j, accumulate the weighted terms up to j
#     for (k in 1:j) {
#       # Compute M^(n-k) (power of M for each term)
#       M_k <- M %^% (n - k)  
#       
#       # Multiply M^k with the k-th column of C
#       weighted_term <- M_k %*% C[, k, drop = FALSE]
#       
#       # Accumulate the weighted term into cumulative sum for column j
#       cumulative_sum <- cumulative_sum + weighted_term
#     }
#     
#     # Return the cumulative sum for column j
#     cumulative_sum[, 1]
#   }
#   
#   # Stop parallel backend
#   stopCluster(cl)
#   
#   return(cumulative_result)
# }

