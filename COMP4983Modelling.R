# Course: COMP-4983 - Capstone Project
# Supervisor: Dr. Greg Lee
# Author: Liam Tingley (100161568)
# Date: April 2025


# Reading in libraries and packages.
library(dplyr)
library(car)
library(Metrics)

# Reading in dataset.
database <- read.csv("database.csv")

# Data preprocessing.
database$year <- as.factor(database$year)
database$gpName <- as.factor(database$gpName)
database$weather <- as.factor(database$weather)
database$driver <- as.factor(database$driver)
database$team <- as.factor(database$team)
database$dnf <- as.factor(database$dnf)

pairs(database[c(2,4:6,10:12)])

# One hot encoding teams attribute.

teams <- unique(database$team)

database <- mutate(database, Alpine = ifelse(team == "Alpine", 1, 0))
database <- mutate(database, "Aston Martin" = ifelse(team == "Aston Martin", 1, 0))
database <- mutate(database, Ferrari = ifelse(team == "Ferrari", 1, 0))
database <- mutate(database, Haas = ifelse(team == "Haas", 1, 0))
database <- mutate(database, McLaren = ifelse(team == "McLaren", 1, 0))
database <- mutate(database, Mercedes = ifelse(team == "Mercedes", 1, 0))
database <- mutate(database, RB = ifelse(team == "RB", 1, 0))
database <- mutate(database, "Red Bull" = ifelse(team == "Red Bull", 1, 0))
database <- mutate(database, Sauber = ifelse(team == "Sauber", 1, 0))
database <- mutate(database, Williams = ifelse(team == "Williams", 1, 0))

# summary(database)


# Preparation of model 1 (baseline, no DNFs)
database1 = database[c(1:11)]
model1 = lm(finish ~ ., data = database1)

summary(model1)
# par(mfrow = c(1,1))
# plot(model1, which=1:2)

# Preparation of model 2 (introduction of DNFs)
database2 = database[c(1:12)]
model2 = lm(finish ~ ., data = database2)
# plot(model2, which=1:2)

summary(model2)

# Comparing the performance between model1 and model2.
anova(model1, model2)


### FOR DEMONSTRATION

# Predicting a race result provided a model and race vector.
# Output will display in order of model's predicted finishing results, bracketed value indicates difference between observed finishing position and predicted finishing position.
predictRace <- function(raceVect, model) {
  
  # Model makes predictions for each of the entries in the raceVect.
  racePreds <- predict(model, raceVect)
  
  # The predictions are then bound to the raceVect.
  raceVect$preds <- racePreds
  
  # The dataFrame is sorted in ascending order of predictions (lower number = better finish).
  raceVect <- raceVect[order(raceVect$preds),]
  
  # Output iterates over each row, returning this format: #pos: driverName (residual)
  print("PREDICTED RESULTS:")
  for(i in 1:nrow(raceVect)) {
    
    # Converting driver names back to characters for display.
    raceVect$driver <- as.character(raceVect$driver)
    
    # Assigning row for current iteration.
    row <- raceVect[i,]
    
    # Computing residual.
    # Positive => Driver was predicted to finish higher than they actually did.
    # Negative => Driver was predicted to finish lower than they actually did.
    residual <- row$finish - i
    
    # Printing driver result to console.
    cat("#", i, ": ", row$driver,"(",residual,")","\n")
    
  }
  
}

## 2022 Bahrain GP = databaseX[1:20]
## 2024 Abu Dhabi GP = databaseX[1331:1350]

# Obtaining a vector containing a race.
raceVector1 <- database1[1:20,]
raceVector2 <- database2[1:20,]

predictRace(raceVector1, model1)
predictRace(raceVector2, model2)

### END DEMONSTRATION


# INCOMPLETE: Preparation of model 3 (One hot encoding team names, train/test split)
database3 <- database %>% dplyr::select(-team)
database3$id <- 1:nrow(database3)

# Setting seed for reproducibility.
set.seed(100)

# Segmenting the dataset into training and test sets.
train <- database3 %>% dplyr::sample_frac(0.70)
test <- dplyr::anti_join(database3, train, by = "id")
train <- train %>% dplyr::select(-id)
test <- test %>% dplyr::select(-id)

model3 = lm(finish ~ ., data = train)

# Analysis of model 3.
# summary(model3)
# anova(model3)

# Generating predictions for the test set.
predictions = predict(model3, test)

# Computing performance metrics.
rmseVal = rmse(test$finish, predictions)
mseVal = mse(test$finish, predictions)
r2 = cor(test$finish, predictions)^2