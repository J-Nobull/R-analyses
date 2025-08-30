
# INSTALL and load packages -----------------------------------------------

# LOAD REQUIRED PACKAGES
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
myDataLocation <- "C:/Users/NoBull/Desktop/ANA605/Week4"
setwd(myDataLocation)
Admit <- read.csv(file="admissions.csv", header=TRUE)

# Inspect data ------------------------------------------------------------

str(Admit)
dim(Admit)
nrow(Admit)
ncol(Admit)
head(Admit)
summary(Admit)
# Do not need favstats for ALL variables, but this is quick and clean
sapply(Admit, favstats)  

#range, good to help estimate bins
max(Admit$CGPA) - min(Admit$CGPA)

tally(Admit$Research) 
tally(Admit$Research, format = "proportion")

cor.test(Chance.of.Admit ~ Research, data = Admit)
cor.test(Chance.of.Admit ~ CGPA, data = Admit)
cor.test(Chance.of.Admit ~ LOR, data = Admit)
cor.test(Chance.of.Admit ~ SOP, data = Admit)

model_4 <- lm(Chance.of.Admit ~ Research + CGPA + LOR + SOP, data = Admit)
model_4
summary(model_4)
# Get the 95% confidence intervals for the coefficients
confint(model_4)

model_1 <- lm(Chance.of.Admit ~ CGPA, data = Admit)
model_1
summary(model_1)
confint(model_1)

model_2 <- lm(Chance.of.Admit ~ Research + CGPA, data = Admit)
model_3 <- lm(Chance.of.Admit ~ Research + CGPA + LOR, data = Admit)

model_2
summary(model_2)
confint(model_2)

model_3
summary(model_3)
confint(model_3)
