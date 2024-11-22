library(readr)
library(dplyr)
library(MASS)  # For glm function

# Load the dataset
df <- read_csv("/cloud/project/NEW Regression Models/FCO_NominalMultiple.csv")  # Adjust the path as needed

df <- df %>%
  mutate(
    Age = factor(Age, levels = c("1", "2", "3", "4")),
    Age = relevel(Age, ref = "3"),  # Assuming '3' is a common or baseline age group
    Gender = factor(Gender, levels = c("1", "2")),
    Gender = relevel(Gender, ref = "1"),  # Assuming '1' as the reference group
    Race = factor(Race, levels = c("1", "2", "3")),
    Race = relevel(Race, ref = "2"),  # Assuming '2' is the reference group
    Living_area = factor(Living_area, levels = c("1", "2", "3", "4", "5", "6")),
    Living_area = relevel(Living_area, ref = "1"),  # Assuming '1' is the reference area
    Income = factor(Income, levels = c("1", "2", "3", "4")),
    Income = relevel(Income, ref = "2"),  # Assuming '2' as the reference income group
    Bus_usage = factor(Bus_usage, levels = c("1", "2", "3", "4", "5")),
    Bus_usage = relevel(Bus_usage, ref = "1"),  # Assuming '1' is the reference level
    # Convert variables to ordered factors
    Limited_routes = as.factor(Limited_routes), 
    Inadeuate_service = as.factor(Inadeuate_service), 
    Highcost = as.factor(Highcost),
    Preferown = as.factor(Preferown),    
    Safety = as.factor(Safety),    
    Insufficient_info = as.factor(Insufficient_info),    
    Carry_items = as.factor(Carry_items),
    Disabilities = as.factor(Disabilities),   
    Overcrowded_1 = as.factor(Overcrowded_1),   
    Buslanes = as.factor(Buslanes),   
    App_1 = as.factor(App_1),   
    Servicesm = as.factor(Servicesm),   
    Hybrid = as.factor(Hybrid),   
    Hours = as.factor(Hours),   
    Sharing = as.factor(Sharing),   
    Loyalty = as.factor(Loyalty),   
    Work = as.factor(Work),   
    Education = as.factor(Education),   
    Medical = as.factor(Medical),   
    Leisure = as.factor(Leisure),   
    Shopping = as.factor(Shopping),   
    Church = as.factor(Church),   
    Other_1 = as.factor(Other_1),   
    Overcrowded_2 = as.factor(Overcrowded_2),   
    Masks = as.factor(Masks),   
    Socialdistance = as.factor(Socialdistance),   
    Smartphone = as.factor(Smartphone),   
    Google = as.factor(Google),   
    CATS = as.factor(CATS),   
    Schedule = as.factor(Schedule),   
    Userfriendly = as.factor(Userfriendly),   
    Slow = as.factor(Slow),   
    Other_3 = as.factor(Other_3),   
    Wait_time = as.factor(Wait_time),   
    Trip = as.factor(Trip),   
    Privacy = as.factor(Privacy),   
    App_2 = as.factor(App_2)
   
  )


# Logistic models for 'Challenges'
model_limited_routes <- glm(Limited_routes ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_inadequate_service <- glm(Inadeuate_service ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_high_cost <- glm(Highcost ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_prefer_own <- glm(Preferown ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_safety <- glm(Safety ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_insufficient_info <- glm(Insufficient_info ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_carry_items <- glm(Carry_items ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_disabilities <- glm(Disabilities ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_overcrowded_1 <- glm(Overcrowded_1 ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())

summary(model_limited_routes)
summary(model_inadequate_service)
summary(model_high_cost)
summary(model_prefer_own)
summary(model_safety)
summary(model_insufficient_info)
summary(model_carry_items)
summary(model_disabilities)
summary(model_overcrowded_1)
# Save to CSV




# Logistic models for 'Improvemnets'
model_Buslanes <- glm(Buslanes ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_App_1 <- glm(App_1 ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_Servicesm <- glm(Servicesm ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_Hybrid <- glm(Hybrid ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_Hours <- glm(Hours ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_Sharing <- glm(Sharing ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_Loyalty <- glm(Loyalty ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())

summary(model_Buslanes)
summary(model_App_1)
summary(model_Servicesm)
summary(model_Hybrid)
summary(model_Hours)
summary(model_Sharing)
summary(model_Loyalty)




# Logistic models for 'Purpose'
model_Work <- glm(Work ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_Education <- glm(Education ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_Medical <- glm(Medical ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_Leisure <- glm(Leisure ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_Shopping <- glm(Shopping ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_Church <- glm(Church ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_Other_1 <- glm(Other_1 ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())

summary(model_Work)
summary(model_Education)
summary(model_Medical)
summary(model_Leisure)
summary(model_Shopping)
summary(model_Church)
summary(model_Other_1)





# Logistic models for 'Reasons_COVID19'
model_Overcrowded_2 <- glm(Overcrowded_2 ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_Masks <- glm(Masks ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_Socialdistance <- glm(Socialdistance ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_Smartphone <- glm(Smartphone ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())

summary(model_Overcrowded_2)
summary(model_Masks)
summary(model_Socialdistance)
summary(model_Smartphone)





# Logistic models for 'Application_used'
model_Google <- glm(Google ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_CATS <- glm(CATS ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_Other_2 <- glm(Other_2 ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())

summary(model_Google)
summary(model_CATS)
summary(model_Other_2)






# Logistic models for 'Reasons_CATS'
model_Schedule <- glm(Schedule ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_Userfriendly <- glm(Userfriendly ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_Slow <- glm(Slow ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_Other_3 <- glm(Other_3 ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())

summary(model_Schedule)
summary(model_Userfriendly)
summary(model_Slow)
summary(model_Other_3)





# Logistic models for 'Interested_intech'
model_Wait_time <- glm(Wait_time ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_Trip <- glm(Trip ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_Privacy <- glm(Privacy ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())
model_App_2 <- glm(App_2 ~ Age + Gender + Race + Living_area + Income + Bus_usage, data = df, family = binomial())

summary(model_Wait_time)
summary(model_Trip)
summary(model_Privacy)
summary(model_App_2)







# Load necessary libraries
library(broom)
library(nnet)  # Assuming this is needed for multinom if used

# Assuming 'model_overcrowded_1' is already fitted
# Tidy the model results
tidied_model <- tidy(model_overcrowded_1)

# Add significance codes
tidied_model$significance <- ifelse(tidied_model$p.value < 0.001, "***",
                                    ifelse(tidied_model$p.value < 0.01, "**",
                                           ifelse(tidied_model$p.value < 0.05, "*",
                                                  ifelse(tidied_model$p.value < 0.1, ".", " "))))

# Specify the path where the file should be saved
file_path <- "/cloud/project/NEW Regression Models/Model_Results.csv"

# Save the tidied model to the specified CSV file
write.csv(tidied_model, file_path, row.names = FALSE)

