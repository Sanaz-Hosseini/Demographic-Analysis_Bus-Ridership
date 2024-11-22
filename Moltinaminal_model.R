# Load necessary libraries
library(readr)   # For reading CSV files
library(dplyr)   # For data manipulation
library(glmnet)  # For ridge regression in multinomial logistic regression
library(boot)    # For bootstrapping
library(Matrix)  # For handling sparse matrix types


# Load the dataset
df <- read_csv("/cloud/project/NEW Regression Models/Without Smartphone/FCO_NominalSimple.csv")

# Prepare the data by transforming variables into factors and setting reference levels
df <- df %>%
  mutate(
    Age = factor(Age, levels = c("1", "2", "3", "4")),
    Age = relevel(Age, ref = "3"),
    Gender = factor(Gender, levels = c("1", "2")),
    Gender = relevel(Gender, ref = "1"),
    Race = factor(Race, levels = c("1", "2", "3")),
    Race = relevel(Race, ref = "2"),
    Living_area = factor(Living_area, levels = c("1", "2", "3", "4", "5", "6")),
    Living_area = relevel(Living_area, ref = "1"),
    Income = factor(Income, levels = c("1", "2", "3", "4")),
    Income = relevel(Income, ref = "2"),
    Bus_usage = factor(Bus_usage, levels = c("1", "2", "3", "4", "5")),
    Bus_usage = relevel(Bus_usage, ref = "1"),
    Privacy_concernbus = as.factor(Privacy_concernbus), 
    Getting_tostation = as.factor(Getting_tostation), 
    Ticket_location = as.factor(Ticket_location), 
    Concern_newapp = as.factor(Concern_newapp) 
  )


# Assuming your data frame is loaded into 'df'
# Exclude observations where Getting_tostation is 0 (assuming 0 means NA)
df <- df %>% filter(Concern_newapp != 0)

# Check the distribution of each class level after consolidation
class_counts <- table(df$Concern_newapp)

# Print the class counts to confirm which levels exist and their frequencies
print(class_counts)

# Exclude classes with fewer than 6 observations
min_count <- 6
valid_classes <- names(class_counts[class_counts >= min_count])

# Filter the data to include only those classes that have a sufficient count
df <- df %>%
  filter(Concern_newapp %in% valid_classes) %>%
  droplevels()

# Verify the new distribution after filtering
print(table(df$Concern_newapp))

# Re-prepare the predictor matrix
x <- model.matrix(~ Age + Gender + Race + Living_area + Income + Bus_usage, df)[,-1]  # Exclude intercept

# Ensure the response variable is properly adjusted
y <- as.factor(df$Concern_newapp)

# Fit the ridge regression model with cross-validation
ridge_model <- cv.glmnet(x, y, family = "multinomial", alpha = 0, lambda.min.ratio = 1e-5, nlambda = 100)

# Extract the best lambda (smallest cross-validation error)
best_lambda <- ridge_model$lambda.min
print(paste("Best lambda: ", best_lambda))

# Fit final model with the best lambda
final_model <- glmnet(x, y, family = "multinomial", alpha = 0, lambda = best_lambda)

# Function to simulate coefficients' variability for approximated standard errors
simulate_std_error <- function(model, x_matrix, y_vector, lambda_value, num_simulations = 100) {
  num_classes <- length(levels(y_vector))
  coef_matrix <- matrix(NA, nrow = num_simulations, ncol = num_classes * (ncol(x_matrix) + 1))
  
  for (i in 1:num_simulations) {
    # Predict class probabilities using the fitted model
    y_prob <- predict(model, newx = x_matrix, type = "response", s = lambda_value)
    
    # Simulate new y values by sampling from predicted class probabilities
    y_sim <- apply(y_prob, 1, function(row) {
      class_names <- dimnames(y_prob)[[2]]
      sample(class_names, 1, prob = row)
    })
    
    # Refit the model using the simulated y values
    sim_model <- glmnet(x_matrix, as.factor(y_sim), family = "multinomial", alpha = 0, lambda = lambda_value)
    coef_matrix[i, ] <- as.vector(do.call(cbind, lapply(coef(sim_model, s = "lambda.min"), as.matrix)))
  }
  
  # Calculate standard errors as the standard deviation of simulated coefficients
  apply(coef_matrix, 2, sd, na.rm = TRUE)
}

# Retrieve coefficients for the best lambda
coef_list <- coef(final_model, s = best_lambda)
coef_matrix <- as.vector(do.call(cbind, lapply(coef_list, as.matrix)))

# Estimate standard errors using the simulation function
std_errors <- simulate_std_error(final_model, x, y, best_lambda)

# Calculate z-values and p-values
z_values <- coef_matrix / std_errors
p_values <- 2 * pnorm(-abs(z_values))

# Assign significance levels
significance <- ifelse(p_values < 0.001, "***",
                       ifelse(p_values < 0.01, "**",
                              ifelse(p_values < 0.05, "*",
                                     ifelse(p_values < 0.1, ".", " "))))

# Organize data into a summary table with separators
predictor_names <- c("Intercept", colnames(x))
response_levels <- names(coef_list)
var_names <- as.vector(t(outer(response_levels, predictor_names, paste, sep = "_")))

# Identify intercepts for each response level
intercept_rows <- grep("_Intercept$", var_names)

# Insert separator rows with empty values
separator_row <- function(index, name) {
  data.frame(
    Variable = name,
    Coefficient = NA,
    StdError = NA,
    Zvalue = NA,
    Pvalue = NA,
    Significance = ""
  )
}

# Create a data frame with separator rows
summary_table <- data.frame(
  Variable = var_names,
  Coefficient = coef_matrix,
  StdError = std_errors,
  Zvalue = z_values,
  Pvalue = p_values,
  Significance = significance
)

# Add separators
for (i in rev(intercept_rows)) {
  level_name <- summary_table$Variable[i]
  separator <- separator_row(i, paste0("---- ", level_name, " ----"))
  summary_table <- rbind(summary_table[1:(i-1), ], separator, summary_table[i:nrow(summary_table), ])
}

# Print the summary table
print(summary_table, row.names = FALSE)








































































































# Check the distribution of each class level after consolidation
class_counts <- table(df$Concern_newapp)

# Print the class counts to confirm which levels exist and their frequencies
print(class_counts)

# Exclude classes with fewer than 10 observations
min_count <- 10
valid_classes <- names(class_counts[class_counts >= min_count])


# Filter the data to include only those classes that have a sufficient count
df <- df %>%
  filter(Concern_newapp %in% valid_classes) %>%
  droplevels()

# Verify the new distribution after filtering
print(table(df$Concern_newapp))


# Re-prepare the predictor matrix
x <- model.matrix(~ Age + Gender + Race + Living_area + Income + Bus_usage, df)[,-1]  # Exclude intercept

# Ensure the response variable is properly adjusted
y <- as.factor(df$Concern_newapp)

# Fit the ridge regression model with cross-validation
ridge_model <- cv.glmnet(x, y, family = "multinomial", alpha = 0, lambda.min.ratio = 1e-5, nlambda = 100)


# Extract the best lambda (smallest cross-validation error)
best_lambda <- ridge_model$lambda.min
print(paste("Best lambda: ", best_lambda))

# Fit final model with the best lambda
final_model <- glmnet(x, y, family = "multinomial", alpha = 0, lambda = best_lambda)

# Function to simulate coefficients' variability for approximated standard errors
simulate_std_error <- function(model, x_matrix, y_vector, lambda_value, num_simulations = 100) {
  num_classes <- length(levels(y_vector))
  coef_matrix <- matrix(NA, nrow = num_simulations, ncol = num_classes * (ncol(x_matrix) + 1))
  
  for (i in 1:num_simulations) {
    # Predict class probabilities using the fitted model
    y_prob <- predict(model, newx = x_matrix, type = "response", s = lambda_value)
    
    # Simulate new y values by sampling from predicted class probabilities
    y_sim <- apply(y_prob, 1, function(row) {
      class_names <- dimnames(y_prob)[[2]]
      sample(class_names, 1, prob = row)
    })
    
    # Refit the model using the simulated y values
    sim_model <- glmnet(x_matrix, as.factor(y_sim), family = "multinomial", alpha = 0, lambda = lambda_value)
    coef_matrix[i, ] <- as.vector(do.call(cbind, lapply(coef(sim_model, s = "lambda.min"), as.matrix)))
  }
  
  # Calculate standard errors as the standard deviation of simulated coefficients
  apply(coef_matrix, 2, sd, na.rm = TRUE)
}

# Retrieve coefficients for the best lambda
coef_list <- coef(final_model, s = best_lambda)
coef_matrix <- as.vector(do.call(cbind, lapply(coef_list, as.matrix)))

# Estimate standard errors using the simulation function
std_errors <- simulate_std_error(final_model, x, y, best_lambda)

# Calculate z-values and p-values
z_values <- coef_matrix / std_errors
p_values <- 2 * pnorm(-abs(z_values))

# Assign significance levels
significance <- ifelse(p_values < 0.001, "***",
                       ifelse(p_values < 0.01, "**",
                              ifelse(p_values < 0.05, "*",
                                     ifelse(p_values < 0.1, ".", " "))))

# Organize data into a summary table with separators
predictor_names <- c("Intercept", colnames(x))
response_levels <- names(coef_list)
var_names <- as.vector(t(outer(response_levels, predictor_names, paste, sep = "_")))

# Identify intercepts for each response level
intercept_rows <- grep("_Intercept$", var_names)

# Insert separator rows with empty values
separator_row <- function(index, name) {
  data.frame(
    Variable = name,
    Coefficient = NA,
    StdError = NA,
    Zvalue = NA,
    Pvalue = NA,
    Significance = ""
  )
}

# Create a data frame with separator rows
summary_table <- data.frame(
  Variable = var_names,
  Coefficient = coef_matrix,
  StdError = std_errors,
  Zvalue = z_values,
  Pvalue = p_values,
  Significance = significance
)

# Add separators
for (i in rev(intercept_rows)) {
  level_name <- summary_table$Variable[i]
  separator <- separator_row(i, paste0("---- ", level_name, " ----"))
  summary_table <- rbind(summary_table[1:(i-1), ], separator, summary_table[i:nrow(summary_table), ])
}

# Print the summary table
print(summary_table, row.names = FALSE)










































# Check the distribution of each class level after consolidation
class_counts <- table(df$Getting_tostation)

# Print the class counts to confirm which levels exist and their frequencies
print(class_counts)

# Exclude classes with fewer than 10 observations
min_count <- 10
valid_classes <- names(class_counts[class_counts >= min_count])


# Filter the data to include only those classes that have a sufficient count
df <- df %>%
  filter(Getting_tostation %in% valid_classes) %>%
  droplevels()

# Verify the new distribution after filtering
print(table(df$Getting_tostation))


# Re-prepare the predictor matrix
x <- model.matrix(~ Age + Gender + Race + Living_area + Income + Bus_usage, df)[,-1]  # Exclude intercept

# Ensure the response variable is properly adjusted
y <- as.factor(df$Getting_tostation)

# Fit the ridge regression model with cross-validation
ridge_model <- cv.glmnet(x, y, family = "multinomial", alpha = 0, lambda.min.ratio = 1e-5, nlambda = 100)


# Extract the best lambda (smallest cross-validation error)
best_lambda <- ridge_model$lambda.min
print(paste("Best lambda: ", best_lambda))

# Fit final model with the best lambda
final_model <- glmnet(x, y, family = "multinomial", alpha = 0, lambda = best_lambda)

# Function to simulate coefficients' variability for approximated standard errors
simulate_std_error <- function(model, x_matrix, y_vector, lambda_value, num_simulations = 100) {
  num_classes <- length(levels(y_vector))
  coef_matrix <- matrix(NA, nrow = num_simulations, ncol = num_classes * (ncol(x_matrix) + 1))
  
  for (i in 1:num_simulations) {
    # Predict class probabilities using the fitted model
    y_prob <- predict(model, newx = x_matrix, type = "response", s = lambda_value)
    
    # Simulate new y values by sampling from predicted class probabilities
    y_sim <- apply(y_prob, 1, function(row) {
      class_names <- dimnames(y_prob)[[2]]
      sample(class_names, 1, prob = row)
    })
    
    # Refit the model using the simulated y values
    sim_model <- glmnet(x_matrix, as.factor(y_sim), family = "multinomial", alpha = 0, lambda = lambda_value)
    coef_matrix[i, ] <- as.vector(do.call(cbind, lapply(coef(sim_model, s = "lambda.min"), as.matrix)))
  }
  
  # Calculate standard errors as the standard deviation of simulated coefficients
  apply(coef_matrix, 2, sd, na.rm = TRUE)
}

# Retrieve coefficients for the best lambda
coef_list <- coef(final_model, s = best_lambda)
coef_matrix <- as.vector(do.call(cbind, lapply(coef_list, as.matrix)))

# Estimate standard errors using the simulation function
std_errors <- simulate_std_error(final_model, x, y, best_lambda)

# Calculate z-values and p-values
z_values <- coef_matrix / std_errors
p_values <- 2 * pnorm(-abs(z_values))

# Assign significance levels
significance <- ifelse(p_values < 0.001, "***",
                       ifelse(p_values < 0.01, "**",
                              ifelse(p_values < 0.05, "*",
                                     ifelse(p_values < 0.1, ".", " "))))

# Organize data into a summary table with separators
predictor_names <- c("Intercept", colnames(x))
response_levels <- names(coef_list)
var_names <- as.vector(t(outer(response_levels, predictor_names, paste, sep = "_")))

# Identify intercepts for each response level
intercept_rows <- grep("_Intercept$", var_names)

# Insert separator rows with empty values
separator_row <- function(index, name) {
  data.frame(
    Variable = name,
    Coefficient = NA,
    StdError = NA,
    Zvalue = NA,
    Pvalue = NA,
    Significance = ""
  )
}

# Create a data frame with separator rows
summary_table <- data.frame(
  Variable = var_names,
  Coefficient = coef_matrix,
  StdError = std_errors,
  Zvalue = z_values,
  Pvalue = p_values,
  Significance = significance
)

# Add separators
for (i in rev(intercept_rows)) {
  level_name <- summary_table$Variable[i]
  separator <- separator_row(i, paste0("---- ", level_name, " ----"))
  summary_table <- rbind(summary_table[1:(i-1), ], separator, summary_table[i:nrow(summary_table), ])
}

# Print the summary table
print(summary_table, row.names = FALSE)









































# Check the distribution of each class level after consolidation
class_counts <- table(df$Ticket_location)

# Print the class counts to confirm which levels exist and their frequencies
print(class_counts)

# Exclude classes with fewer than 10 observations
min_count <- 10
valid_classes <- names(class_counts[class_counts >= min_count])

# Filter the data to include only those classes that have a sufficient count
df <- df %>%
  filter(Ticket_location %in% valid_classes) %>%
  droplevels()

# Verify the new distribution after filtering
print(table(df$Ticket_location))


# Re-prepare the predictor matrix
x <- model.matrix(~ Age + Gender + Race + Living_area + Income + Bus_usage, df)[,-1]  # Exclude intercept

# Ensure the response variable is properly adjusted
y <- as.factor(df$Ticket_location)

# Fit the ridge regression model with cross-validation
ridge_model <- cv.glmnet(x, y, family = "multinomial", alpha = 0, lambda.min.ratio = 1e-5, nlambda = 100)


# Extract the best lambda (smallest cross-validation error)
best_lambda <- ridge_model$lambda.min
print(paste("Best lambda: ", best_lambda))

# Fit final model with the best lambda
final_model <- glmnet(x, y, family = "multinomial", alpha = 0, lambda = best_lambda)

# Function to simulate coefficients' variability for approximated standard errors
simulate_std_error <- function(model, x_matrix, y_vector, lambda_value, num_simulations = 100) {
  num_classes <- length(levels(y_vector))
  coef_matrix <- matrix(NA, nrow = num_simulations, ncol = num_classes * (ncol(x_matrix) + 1))
  
  for (i in 1:num_simulations) {
    # Predict class probabilities using the fitted model
    y_prob <- predict(model, newx = x_matrix, type = "response", s = lambda_value)
    
    # Simulate new y values by sampling from predicted class probabilities
    y_sim <- apply(y_prob, 1, function(row) {
      class_names <- dimnames(y_prob)[[2]]
      sample(class_names, 1, prob = row)
    })
    
    # Refit the model using the simulated y values
    sim_model <- glmnet(x_matrix, as.factor(y_sim), family = "multinomial", alpha = 0, lambda = lambda_value)
    coef_matrix[i, ] <- as.vector(do.call(cbind, lapply(coef(sim_model, s = "lambda.min"), as.matrix)))
  }
  
  # Calculate standard errors as the standard deviation of simulated coefficients
  apply(coef_matrix, 2, sd, na.rm = TRUE)
}

# Retrieve coefficients for the best lambda
coef_list <- coef(final_model, s = best_lambda)
coef_matrix <- as.vector(do.call(cbind, lapply(coef_list, as.matrix)))

# Estimate standard errors using the simulation function
std_errors <- simulate_std_error(final_model, x, y, best_lambda)

# Calculate z-values and p-values
z_values <- coef_matrix / std_errors
p_values <- 2 * pnorm(-abs(z_values))

# Assign significance levels
significance <- ifelse(p_values < 0.001, "***",
                       ifelse(p_values < 0.01, "**",
                              ifelse(p_values < 0.05, "*",
                                     ifelse(p_values < 0.1, ".", " "))))

# Organize data into a summary table with separators
predictor_names <- c("Intercept", colnames(x))
response_levels <- names(coef_list)
var_names <- as.vector(t(outer(response_levels, predictor_names, paste, sep = "_")))

# Identify intercepts for each response level
intercept_rows <- grep("_Intercept$", var_names)

# Insert separator rows with empty values
separator_row <- function(index, name) {
  data.frame(
    Variable = name,
    Coefficient = NA,
    StdError = NA,
    Zvalue = NA,
    Pvalue = NA,
    Significance = ""
  )
}

# Create a data frame with separator rows
summary_table <- data.frame(
  Variable = var_names,
  Coefficient = coef_matrix,
  StdError = std_errors,
  Zvalue = z_values,
  Pvalue = p_values,
  Significance = significance
)

# Add separators
for (i in rev(intercept_rows)) {
  level_name <- summary_table$Variable[i]
  separator <- separator_row(i, paste0("---- ", level_name, " ----"))
  summary_table <- rbind(summary_table[1:(i-1), ], separator, summary_table[i:nrow(summary_table), ])
}

# Print the summary table
print(summary_table, row.names = FALSE)

















































# Prepare matrix for glmnet
x <- model.matrix(~ Age + Gender + Race + Living_area + Income + Bus_usage, df)[,-1]  # Exclude intercept
y <- as.factor(df$Privacy_concernbus)

# Fit ridge regression model using cross-validation
ridge_model <- cv.glmnet(x, y, family = "multinomial", alpha = 0, lambda.min.ratio = 1e-5, nlambda = 100)

# Extract the best lambda (smallest cross-validation error)
best_lambda <- ridge_model$lambda.min
print(paste("Best lambda: ", best_lambda))

# Fit final model with the best lambda
final_model <- glmnet(x, y, family = "multinomial", alpha = 0, lambda = best_lambda)

# Function to simulate coefficients' variability for approximated standard errors
simulate_std_error <- function(model, x_matrix, y_vector, lambda_value, num_simulations = 100) {
  num_classes <- length(levels(y_vector))
  coef_matrix <- matrix(NA, nrow = num_simulations, ncol = num_classes * (ncol(x_matrix) + 1))
  
  for (i in 1:num_simulations) {
    # Predict class probabilities using the fitted model
    y_prob <- predict(model, newx = x_matrix, type = "response", s = lambda_value)
    
    # Simulate new y values by sampling from predicted class probabilities
    y_sim <- apply(y_prob, 1, function(row) {
      class_names <- dimnames(y_prob)[[2]]
      sample(class_names, 1, prob = row)
    })
    
    # Refit the model using the simulated y values
    sim_model <- glmnet(x_matrix, as.factor(y_sim), family = "multinomial", alpha = 0, lambda = lambda_value)
    coef_matrix[i, ] <- as.vector(do.call(cbind, lapply(coef(sim_model, s = "lambda.min"), as.matrix)))
  }
  
  # Calculate standard errors as the standard deviation of simulated coefficients
  apply(coef_matrix, 2, sd, na.rm = TRUE)
}

# Retrieve coefficients for the best lambda
coef_list <- coef(final_model, s = best_lambda)
coef_matrix <- as.vector(do.call(cbind, lapply(coef_list, as.matrix)))

# Estimate standard errors using the simulation function
std_errors <- simulate_std_error(final_model, x, y, best_lambda)

# Calculate z-values and p-values
z_values <- coef_matrix / std_errors
p_values <- 2 * pnorm(-abs(z_values))

# Assign significance levels
significance <- ifelse(p_values < 0.001, "***",
                       ifelse(p_values < 0.01, "**",
                              ifelse(p_values < 0.05, "*",
                                     ifelse(p_values < 0.1, ".", " "))))

# Organize data into a summary table with separators
predictor_names <- c("Intercept", colnames(x))
response_levels <- names(coef_list)
var_names <- as.vector(t(outer(response_levels, predictor_names, paste, sep = "_")))

# Identify intercepts for each response level
intercept_rows <- grep("_Intercept$", var_names)

# Insert separator rows with empty values
separator_row <- function(index, name) {
  data.frame(
    Variable = name,
    Coefficient = NA,
    StdError = NA,
    Zvalue = NA,
    Pvalue = NA,
    Significance = ""
  )
}

# Create a data frame with separator rows
summary_table <- data.frame(
  Variable = var_names,
  Coefficient = coef_matrix,
  StdError = std_errors,
  Zvalue = z_values,
  Pvalue = p_values,
  Significance = significance
)

# Add separators
for (i in rev(intercept_rows)) {
  level_name <- summary_table$Variable[i]
  separator <- separator_row(i, paste0("---- ", level_name, " ----"))
  summary_table <- rbind(summary_table[1:(i-1), ], separator, summary_table[i:nrow(summary_table), ])
}

# Print the summary table
print(summary_table, row.names = FALSE)

