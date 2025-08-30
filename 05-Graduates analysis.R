# load packages -----------------------------------------------------------
# (needed once per R instance)
library(knitr)
library(ggplot2)
library(ggformula)
library(mosaic)
library(dplyr)
library(supernova)
library(car)
library(ltm)
library(lsr)
library(nycflights13)
library(fueleconomy)
library(palmerpenguins)
library(Lock5Data)

# Import data -------------------------------------------------------------
myDataLocation <- "C:/Users/NoBull/Desktop/ANA605/Week 1"
setwd(myDataLocation)
admitdata <- read.csv(file = "admission_data.csv", header = TRUE)

# Inspect data ------------------------------------------------------------
str(admitdata)

head(admitdata)
tail(admitdata)

#Do not need favstats for ALL variables (Serial No), but it is quick and clean
sapply(admitdata, favstats)

# Tidy data --------------------------------------------------------------
# I do not see any data points that should be cleaned up now.
# (Having fun with Sections)

# Transform data ----------------------------------------------------------
#As discussed in class, this change should correct cor(Research)
admitdata$Research <- factor(
  admitdata$Research, 
  levels = c(0, 1), 
  labels = c("No", "Yes"))

head(admitdata)

# Visualize data ----------------------------------------------------------
# I ran a handful of plots, it helped to confirm some of the later analysis 
# None needed for this assignment

# Model data --------------------------------------------------------------
empty_model <- lm(Chance.of.Admit ~ NULL, data = admitdata)
Research_model <- lm(Chance.of.Admit ~ Research, data = admitdata)
GPA_model <- lm(Chance.of.Admit ~ CGPA, data = admitdata)
GRE_model <- lm(Chance.of.Admit ~ GRE.Score, data = admitdata)
TOEFL_model <- lm(Chance.of.Admit ~ TOEFL.Score, data = admitdata)

# Could also run 4 lines of cor()/list()/supernova() for each explanatory
# Removed 'Research_model': "Error in stats::cor(x, y) : 'x' must be numeric"
# (R properly responded to Research_model as factors)
cor(admitdata[, c('Chance.of.Admit', 'CGPA', 'GRE.Score', 'TOEFL.Score')])

list(empty_model, Research_model, GPA_model, GRE_model, TOEFL_model)

lapply(list(Research_model, GPA_model, GRE_model, TOEFL_model), supernova)

# Q6 Make predictions -----------------------------------------------------
subject_indices <- c(40, 56, 78, 93)

# Extract data for subjects
subjects_data <- admitdata[subject_indices, ]

# Predict each subject using previous models
predictions <- data.frame(
  Subject = subject_indices,
  Research_Prediction = predict(Research_model, newdata = subjects_data),
  GPA_Prediction = predict(GPA_model, newdata = subjects_data),
  GRE_Prediction = predict(GRE_model, newdata = subjects_data),
  TOEFL_Prediction = predict(TOEFL_model, newdata = subjects_data))

# Calculate mean of predictions for each subject.
# (Considered ranking each score and averaging the ranks, 
# but subjects 40 and 56 are too close) Also ran it without the Research_model 
# included, since it is the same for all, but resulted in the same outcome.
predictions$Mean_Prediction <- rowMeans(predictions[, -1])
# (stackoverflow.com, n.d.)

# Identify subject with the highest chance of admission
best_subject_mean <- predictions[which.max(predictions$Mean_Prediction), "Subject"]

# Return predictions and best subject by mean
list(predictions = predictions, best_subject_mean = best_subject_mean)

# Q8 ----------------------------------------------------------------------
# Looking at dataset, Subject 100 had an 8.88 GPA
admitdata$CGPA[100]

# Calculate the prediction for respondent 100
-1.0715 + (0.2088 * admitdata$CGPA[100])

sum(.79-.783)
# Hello Bond!