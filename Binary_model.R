library(readr)
library(dplyr)

# Load the dataset
Binary_cols <- read_csv("/cloud/project/NEW Regression Models/Without Smartphone/FCO_Binary.csv")

# Convert factors to numeric based on your provided codings
Binary_cols <- Binary_cols %>%
  mutate(
    Gender = factor(Gender, levels = c("1", "2")), # Convert Gender to a factor
    Gender = relevel(Gender, ref = "1"), # Set "1" as the reference level for Gender
    Age = factor(Age, levels = c("1", "2", "3", "4")), # Convert Age to a factor
    Age = relevel(Age, ref = "3"), # Set "3" as the reference level for Age
    Race = factor(Race, levels = c("1", "2", "3")), # Convert Race to a factor
    Race = relevel(Race, ref = "2"), # Set "2" as the reference level for Race
    Income = factor(Income, levels = c("1", "2", "3", "4")), # Convert Income to a factor
    Income = relevel(Income, ref = "2"), # Set "2" as the reference level for Income
    Living_area = factor(Living_area, levels = c("1", "2", "3", "4", "5", "6")), # Convert Living_area to a factor
    Living_area = relevel(Living_area, ref = "1"), # Set "1" as the reference level for Living_area
    Bus_usage = factor(Bus_usage, levels = c("0", "1", "2", "3", "4", "5")), # Convert Bus_usage to a factor
    Bus_usage = relevel(Bus_usage, ref = "1"), # Set "1" as the reference level for Bus_usage
    # Convert variables to ordered factors
    Willingness_Uber = as.factor(Willingness_Uber), # Ensure Willingness_Uber is treated as a factor
    Willingness_newapp = as.factor(Willingness_newapp),
    Smartphone = as.factor(Smartphone)
    # Ensure Willingness_newapp is treated as a factor
  )

# Fitting the binary logistic regression model
model_bin_1 <- glm(Willingness_Uber ~ Age + Gender + Race + Income + Living_area + Bus_usage,
                   family = binomial(link = "logit"), data = Binary_cols, na.action = na.exclude)

# Display the summary of the model
summary(model_bin_1)




# Fitting the binary logistic regression model
model_bin_2 <- glm(Willingness_newapp ~ Age + Gender + Race + Income + Living_area + Bus_usage,
                   family = binomial(link = "logit"), data = Binary_cols, na.action = na.exclude)

# Display the summary of the model
summary(model_bin_2)


# Fitting the binary logistic regression model
model_bin_3 <- glm(Smartphone ~ Age + Gender + Race + Income + Living_area + Bus_usage,
                   family = binomial(link = "logit"), data = Binary_cols, na.action = na.exclude)

# Display the summary of the model
summary(model_bin_3)








