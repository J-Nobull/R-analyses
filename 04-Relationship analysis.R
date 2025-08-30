myDataLocation <- "C:/Users/NoBull/Desktop/ANA600/R-Studio"
setwd(myDataLocation)
mydata <- read.csv(file = "student-mat.csv", header = TRUE)
#-------------------------------------------------------------------------
library(knitr)
library(ggplot2)
library(ggformula)
library(mosaic)
library(dplyr)
library(supernova)
library(car)
library(ltm)
library(lsr)
library(Lock5Data)
library(nycflights13)
library(fueleconomy)
library(palmerpenguins)

str(mydata)

#2. Data Description
#a. Identify all categorical-type variables
chr_var <- sapply(mydata, is.character)
# Tally values for each categorical-type variable
lapply(mydata[, chr_var], table)

#b. Identify all quantitative-type variables
int_var <- sapply(mydata, is.integer)
# Apply favstats to each quantitative-type variable
lapply(mydata[, int_var], favstats)

#c. Descriptive stats for outcome and explanatory variables
favstats(mydata$absences)
tally(mydata$romantic)

#d. Histogram of the absences variable
gf_histogram(~absences, data = mydata, bins = 75, fill = "blue", color = "black") +
  labs(title = "Distribution of Absences",
       x = "Number of Absences",
       y = "Frequency")

#e. Visualize the research question
gf_boxplot(absences ~ romantic, data = mydata) +
  labs(title = "Absences by Romantic Relationship Status",
       x = "Romantic Relationship Status",
       y = "Number of Absences")

#3.c. Comparing the two models
empty_model <- lm(absences ~ NULL, data = mydata)
relationship_model <- lm(absences ~ romantic, data = mydata)
supernova(relationship_model)