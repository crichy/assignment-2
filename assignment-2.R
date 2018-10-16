iris_subset_1 <- iris[c(89:94, 108:112),]
iris_subset_2 <- iris[88:114,]

#différence in means, on reprend la fonction différence in median mais on change la formule finale en means
difference_in_means <- function(d, var, grouping_var, group1, group2) {
  d_1 <- dplyr::filter(d, get(grouping_var) == group1)
  d_2 <- dplyr::filter(d, get(grouping_var) == group2)
  d1_var <- rep(0, nrow(d_1))
  d2_var <- rep(0, nrow(d_2))

  
  for (i in 1:nrow(d_1)){
    d1_var[i] <- d_1[i, var]
  } 
  for (i in 1:nrow(d_2)){
    d2_var[i] <- d_2[i, var]
  }
  result<- mean(d1_var) - mean(d2_var)
  return(result)
}

randomize <- function(d, var) {
  d[[var]] <- sample(d[[var]], replace= F)
  return(d) 
}

permutation_twogroups <- function(d, var, grouping_var, group1, group2, statistic,
                                  n_samples=9999) {
  observed_statistic <- statistic(d, var, grouping_var, group1, group2)
  permutation_statistics <- rep(0, n_samples)
  for (i in 1:n_samples) {
    new_d <- randomize(d, var)
    permutation_statistics[i] <- statistic(new_d, var, grouping_var, group1, group2)
  }
  result <- list(observed=observed_statistic,
                 permuted=permutation_statistics)
  return(result)
}
