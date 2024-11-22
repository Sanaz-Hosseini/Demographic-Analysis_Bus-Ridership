library(readr)
library(dplyr)
library(MASS)  # For polr()

# Load the dataset
Ordinal_cols <- read_csv("/cloud/project/NEW Regression Models/FCO_Ordinal.csv")

# Prepare your variables as before, then convert your ordinal response variable
Ordinal_cols <- Ordinal_cols %>%
  mutate(
    # Convert categorical variables to factors and set reference groups
    Gender = factor(Gender, levels = c("1", "2")),
    Gender = relevel(Gender, ref = "1"),
    Age = factor(Age, levels = c("1", "2", "3", "4")),
    Age = relevel(Age, ref = "3"),
    Race = factor(Race, levels = c("1", "2", "3")),
    Race = relevel(Race, ref = "2"),
    Income = factor(Income, levels = c("1", "2", "3", "4")),
    Income = relevel(Income, ref = "2"),
    Living_area = factor(Living_area, levels = c("1", "2", "3", "4", "5", "6")),
    Living_area = relevel(Living_area, ref = "1"),
    Bus_usage = factor(Bus_usage, levels = c("0", "1", "2", "3", "4", "5")),
    Bus_usage = relevel(Bus_usage, ref = "1"),
    # Convert the ordinal response variable to an ordered factor
    Wait_timebus = factor(Wait_timebus, levels = c("0", "1", "2", "3", "4"), ordered = TRUE),
    Commute_time = factor(Commute_time, levels = c("0", "1", "2", "3", "4"), ordered = TRUE),
    Time_spend = factor(Time_spend, levels = c("0", "1", "2", "3", "4"), ordered = TRUE),
    Concern_COVID19 = factor(Concern_COVID19, levels = c("1", "2", "3"), ordered = TRUE),
    Satisfaction_CATS = factor(Satisfaction_CATS, levels = c("0", "1", "2", "3", "4"), ordered = TRUE),
    Desired_timeline = factor(Desired_timeline, levels = c("1", "2", "3", "4"), ordered = TRUE),
    Willingness_travelapp = factor(Willingness_travelapp, levels = c("0", "1", "2", "3", "4"), ordered = TRUE)
  )


# Fit the ordinal logistic regression model
model_ord <- polr(Wait_timebus ~ Age + Gender + Race + Income + 
                    Living_area + Bus_usage, data = Ordinal_cols, Hess = TRUE)

# Obtain the summary of the model
model_summary <- summary(model_ord)

# Calculate p-values using the t-values and degrees of freedom
p_values <- 2 * pt(abs(model_summary$coefficients[, "t value"]), df = df.residual(model_ord), lower.tail = FALSE)

# Combine the coefficients, standard errors, t-values, and p-values into a data frame for easier reading
results_df <- data.frame(
  Coefficient = rownames(model_summary$coefficients),
  Estimate = model_summary$coefficients[, "Value"],
  StdError = model_summary$coefficients[, "Std. Error"],
  TValue = model_summary$coefficients[, "t value"],
  PValue = p_values
)

# Add a column for significance codes based on p-values
results_df$Significance <- ifelse(results_df$PValue < 0.001, "***", 
                                  ifelse(results_df$PValue < 0.01, "**", 
                                         ifelse(results_df$PValue < 0.05, "*", 
                                                ifelse(results_df$PValue < 0.1, ".", " "))))

# Print the results with p-values and significance codes
print(results_df)












# Fit the ordinal logistic regression model
model_ord <- polr(Commute_time ~ Age + Gender + Race + Income + 
                    Living_area + Bus_usage, data = Ordinal_cols, Hess = TRUE)

# Obtain the summary of the model
model_summary <- summary(model_ord)

# Calculate p-values using the t-values and degrees of freedom
p_values <- 2 * pt(abs(model_summary$coefficients[, "t value"]), df = df.residual(model_ord), lower.tail = FALSE)

# Combine the coefficients, standard errors, t-values, and p-values into a data frame for easier reading
results_df <- data.frame(
  Coefficient = rownames(model_summary$coefficients),
  Estimate = model_summary$coefficients[, "Value"],
  StdError = model_summary$coefficients[, "Std. Error"],
  TValue = model_summary$coefficients[, "t value"],
  PValue = p_values
)

# Add a column for significance codes based on p-values
results_df$Significance <- ifelse(results_df$PValue < 0.001, "***", 
                                  ifelse(results_df$PValue < 0.01, "**", 
                                         ifelse(results_df$PValue < 0.05, "*", 
                                                ifelse(results_df$PValue < 0.1, ".", " "))))

# Print the results with p-values and significance codes
print(results_df)









# Fit the ordinal logistic regression model
model_ord <- polr(Time_spend ~ Age + Gender + Race + Income + 
                    Living_area + Bus_usage, data = Ordinal_cols, Hess = TRUE)

# Obtain the summary of the model
model_summary <- summary(model_ord)

# Calculate p-values using the t-values and degrees of freedom
p_values <- 2 * pt(abs(model_summary$coefficients[, "t value"]), df = df.residual(model_ord), lower.tail = FALSE)

# Combine the coefficients, standard errors, t-values, and p-values into a data frame for easier reading
results_df <- data.frame(
  Coefficient = rownames(model_summary$coefficients),
  Estimate = model_summary$coefficients[, "Value"],
  StdError = model_summary$coefficients[, "Std. Error"],
  TValue = model_summary$coefficients[, "t value"],
  PValue = p_values
)

# Add a column for significance codes based on p-values
results_df$Significance <- ifelse(results_df$PValue < 0.001, "***", 
                                  ifelse(results_df$PValue < 0.01, "**", 
                                         ifelse(results_df$PValue < 0.05, "*", 
                                                ifelse(results_df$PValue < 0.1, ".", " "))))

# Print the results with p-values and significance codes
print(results_df)









# Fit the ordinal logistic regression model
model_ord <- polr(Concern_COVID19 ~ Age + Gender + Race + Income + 
                    Living_area + Bus_usage, data = Ordinal_cols, Hess = TRUE)

# Obtain the summary of the model
model_summary <- summary(model_ord)

# Calculate p-values using the t-values and degrees of freedom
p_values <- 2 * pt(abs(model_summary$coefficients[, "t value"]), df = df.residual(model_ord), lower.tail = FALSE)

# Combine the coefficients, standard errors, t-values, and p-values into a data frame for easier reading
results_df <- data.frame(
  Coefficient = rownames(model_summary$coefficients),
  Estimate = model_summary$coefficients[, "Value"],
  StdError = model_summary$coefficients[, "Std. Error"],
  TValue = model_summary$coefficients[, "t value"],
  PValue = p_values
)

# Add a column for significance codes based on p-values
results_df$Significance <- ifelse(results_df$PValue < 0.001, "***", 
                                  ifelse(results_df$PValue < 0.01, "**", 
                                         ifelse(results_df$PValue < 0.05, "*", 
                                                ifelse(results_df$PValue < 0.1, ".", " "))))

# Print the results with p-values and significance codes
print(results_df)







# Fit the ordinal logistic regression model
model_ord <- polr(Satisfaction_CATS ~ Age + Gender + Race + Income + 
                    Living_area + Bus_usage, data = Ordinal_cols, Hess = TRUE)

# Obtain the summary of the model
model_summary <- summary(model_ord)

# Calculate p-values using the t-values and degrees of freedom
p_values <- 2 * pt(abs(model_summary$coefficients[, "t value"]), df = df.residual(model_ord), lower.tail = FALSE)

# Combine the coefficients, standard errors, t-values, and p-values into a data frame for easier reading
results_df <- data.frame(
  Coefficient = rownames(model_summary$coefficients),
  Estimate = model_summary$coefficients[, "Value"],
  StdError = model_summary$coefficients[, "Std. Error"],
  TValue = model_summary$coefficients[, "t value"],
  PValue = p_values
)

# Add a column for significance codes based on p-values
results_df$Significance <- ifelse(results_df$PValue < 0.001, "***", 
                                  ifelse(results_df$PValue < 0.01, "**", 
                                         ifelse(results_df$PValue < 0.05, "*", 
                                                ifelse(results_df$PValue < 0.1, ".", " "))))

# Print the results with p-values and significance codes
print(results_df)








# Fit the ordinal logistic regression model
model_ord <- polr(Desired_timeline ~ Age + Gender + Race + Income + 
                    Living_area + Bus_usage, data = Ordinal_cols, Hess = TRUE)

# Obtain the summary of the model
model_summary <- summary(model_ord)

# Calculate p-values using the t-values and degrees of freedom
p_values <- 2 * pt(abs(model_summary$coefficients[, "t value"]), df = df.residual(model_ord), lower.tail = FALSE)

# Combine the coefficients, standard errors, t-values, and p-values into a data frame for easier reading
results_df <- data.frame(
  Coefficient = rownames(model_summary$coefficients),
  Estimate = model_summary$coefficients[, "Value"],
  StdError = model_summary$coefficients[, "Std. Error"],
  TValue = model_summary$coefficients[, "t value"],
  PValue = p_values
)

# Add a column for significance codes based on p-values
results_df$Significance <- ifelse(results_df$PValue < 0.001, "***", 
                                  ifelse(results_df$PValue < 0.01, "**", 
                                         ifelse(results_df$PValue < 0.05, "*", 
                                                ifelse(results_df$PValue < 0.1, ".", " "))))

# Print the results with p-values and significance codes
print(results_df)



# The last Model is with clm and different with Polr

# Load necessary library
library(ordinal)

# Assuming Ordinal_cols is already loaded and preprocessed correctly
# Fit the ordinal logistic regression model
model_ord_clm <- clm(Willingness_travelapp ~ Age + Gender + Race + Income + 
                       Living_area + Bus_usage, data = Ordinal_cols)

# Obtain the summary of the model
model_summary <- summary(model_ord_clm)

# Extracting the coefficients table (assuming model_summary has a component named 'coefficients' or similar)
results_df <- data.frame(model_summary$coefficients)

# Rename columns appropriately if needed
names(results_df)[4] <- "PValue"

# Add a column for significance codes based on p-values
results_df$Significance <- ifelse(results_df$PValue < 0.001, "***", 
                                  ifelse(results_df$PValue < 0.01, "**", 
                                         ifelse(results_df$PValue < 0.05, "*", 
                                                ifelse(results_df$PValue < 0.1, ".", " "))))


# Print the results with p-values and significance codes
print(results_df)
