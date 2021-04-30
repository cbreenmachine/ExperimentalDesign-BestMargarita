
generate_initial_design <- function(p){
  # Creates an X matrix with p columns. Each column is composed of 1s and -1s.
  # The patterns of + and - are different in each column. For example:
  # Col 1: + - + - + -...
  # Col 2: + + - - + +...
  # p is the number of factors and is only required (accepted) input.
  
  runs <- 2^p
  X <- matrix(rep(0, runs*p), nrow = runs)
  colnames(X) <- LETTERS[seq( from = 1, to = p )] # assign column names from alphabet
  
  for (jj in 1:p){
    pattern_multiplier <- 2^(jj - 1)
    string_multiplier <- 2^(p-jj)
    
    ix <- c(rep(1, pattern_multiplier), rep(-1, pattern_multiplier))
    X[,jj] <- rep(ix, string_multiplier)
  }  
  
  return(X)
}


multiply_effects <- function(str_vec) {
  # helper function, multiply letter strings
  # E.g. multiply_effects("ABC", "CDE") --> "ABDE"
  N <- length(str_vec)
  while (N > 2) {
    str_vec <- c(multiply_effects(str_vec[1:2]), str_vec[3:N])
    N <- length(str_vec)
  } 
  counts <- table(strsplit(paste0(str_vec[1],str_vec[2]), ""))
  # If the number of occurrences is odd, that letter stays (was not canceled)
  reduced <- paste0(names(counts[counts %% 2 == 1]), collapse="")
  return(reduced)
  
}

add_effect <- function(X, effect) {
  # X is a matrix (likely made by `generate_initial_design`)
  # effect is an effect represented by string of letters (e.g. "AB" or "ADE")
  # OR effect is a an integer
  factors <- colnames(X)
  
  if (typeof(effect) == "double"){
    
    
    combos <- t(combn(factors, 2))
    all_effects <- apply(combos, 1, paste0, collapse = "")
  } else {
    all_effects <- effect
  }
  
  for (string in all_effects){
    v <- strsplit(string, split = "")[[1]]
    X <- cbind(X, apply(X[, v], 1, prod))
    colnames(X)[ncol(X)] <- string
  }
  return(X)
}


X <- (generate_initial_design(3) * -1) %>%
  add_effect("AB") %>%
  add_effect("AC") %>%
  add_effect("BC") %>%
  add_effect("ABC") %>%
  as.data.frame()

X$Y <- c(60,72,54,68,52,83,45,80)


lm <- lm(Y ~ ., data=X)

lm.2 <- lm(Y~.-ABC, data=X)
sigma(lm.2)^2 / 2

lm.3 <- lm(Y ~ A+B+C, data=X)
sigma(lm.3)^2 / 2

