#Import library
suppressWarnings(library(CVXR, warn.conflicts=FALSE))
suppressWarnings(library(tidyverse, warn.conflicts=FALSE))
# Input Variables
num_goods <- 2
num_buyers <- 2

budgets <- c(0.5, 0.5)
valuations <- matrix(c(0.5, 0.5, 0.7, 0.3), byrow = T, nrow = num_buyers, ncol = num_goods)
rho <- 0.5

get_prices <- function(utilities) {
  

# Variables to solve for
allocations <- Variable(num_buyers, num_goods)

# Common constraints across all programs for all utilities
constraint1 <- sum_entries(allocations, axis = 2) <= 1
constraint2 <- allocations >= 0  

if (utilities == "linear") {
    # Linear Prices
  objective <- Maximize(sum( budgets * log(sum_entries( valuations*allocations, axis = 1 ) ) )  ) 
  problem <- Problem(objective, constraints = list(constraint1, constraint2))
  result <- solve(problem)
  prices <- result$getDualValue(constraint1)
  
  } else if (utilities == "Leontief") {
    # Leontief Prices
  utils <- Variable(num_buyers)
  constraint3 <- utils <= min_entries(allocations/valuations, axis = 1)

  objective <- Maximize(t(budgets) %*% log(utils))
  problem <- Problem(objective, constraints = list(constraint1, constraint2, constraint3))
  result <- solve(problem)
  prices <- result$getDualValue(constraint1)
  
  } else if (utilities == "CES") {
    # CES Prices
    objective <- Maximize( t(budgets) %*% ((1/rho)*log((sum_entries(valuations*(allocations^(rho)), axis = 1)))))
    problem <- Problem(objective, constraints = list(constraint1, constraint2))
    result <- solve(problem)
    prices <- result$getDualValue(constraint1)
  
    } else {
    # Cobb-Douglas prices
    prices <- t(valuations) %*% budgets
    }
  return(prices)
}
cd_prices <- round(get_prices(""), 3)
linear_prices <-  round(get_prices("linear"), 3)
leontief_prices <-  round(get_prices("Leontief"), 3)
prices <- rbind(cd_prices, linear_prices, leontief_prices)

prices_data <- data.frame(cbind(prices, rep(paste("Good", 1:num_goods), 3)), c(rep("Cobb-Douglas", num_goods), rep("Linear", num_goods), rep("Leontief", num_goods)))
colnames(prices_data) <- c("Prices", "Good", "Utilities")
ggplot(data = prices_data, aes(fill = Utilities, y = Prices, x = Good)) + geom_col(position = "dodge") 

