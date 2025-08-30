## SETUP

#CHANGE BETWEEN QUOTES IN THIS LINE TO REFLECT FILE DIRECTORY OF DATA
myDataLocation <- "C:/Users/NoBull/Desktop/ANA600/R-Studio"
#SET WORKING DIRECTORY TO LOCATION OF DATA FILE
setwd(myDataLocation)
#IMPORT DATA AND PUT INTO DATAFRAME
myData <- read.csv(file = file.path(myDataLocation, "consumer_data.csv"), header = TRUE)
## Load and install required packages
install.packages("mosaic") 
# Uncomment this line if you have not already installed mosaic
library(mosaic)

## THE ASSIGNMENT BEGINS HERE

## For each question, write the code you would use to answer the question (some questions have more than one possible answer)
## For questions asking for both code and a short written response, use the # to write a short response to the question 
## Don't forget to save your work directly in this codefile
## Rename this codefile with your name (ex. ANA600_Assignment1_Eric.R)
## Sumbit this codefile with your code to the instructor along with a writeup as described in the instruction document

#Q1. Display the contents of the dataframe.
  str(myData)

#Q2. How many cases were sampled in the dataframe?
  # 2000

#Q3. How many variables are in the dataframe?
  # 14

#Q4. Display the top and bottom six rows of the dataframe.
  head(myData)
  tail(myData)

#Q5.(Respond with code and short written response)
  summary(myData)
#a. Which variables are quantitative?
  # household, kids, vehicles, priceExpected, incomeExpected, income, age, hoursPerWeek
#b. Which variables are categorical?
  # businessExpected, financialStability, investments, employmentSector, region

#Q6. Create a frequency table per the state and employment sectors variables.
#a. state
  # no state variable, changed it to region
  tally(myData$region)
#b. Employment Sector
  tally(myData$employmentSector)

#Q7. Create a new variable with categories of the household variable as a factor, and assign levels and labels to its values.  
  # I selected 3-5 household as medium as it appears to be the typical (normal, most common, traditional "nuclear" size)
  # That allows the outliers to be categorized separately; family size of 2 is small
  # and any family larger than 5 is grouped in the large case. This keeps small and large close to equal in size.
  myData <- myData |>
    mutate(
      family = case_when(
        household <= 2 ~ "Small",
        household >= 6 ~ "Large",
        TRUE ~ "Medium"
      )
    )

#Q8. Filter observations that have missing data on the age variable in a new dataframe.
  myData_fil <- filter(myData, myData$age != "NA")

#Q9. How many observations were dropped, based on missing values in the age variable?
  # 156 obs. were dropped

#Q10. Create a histogram of the age variable of the filter_dat dataframe.
  hist(
    myData_fil$age,
    col = "skyblue",
    border = "black",
    main = "Histogram of Age (hist)",
    xlab = "Age",
    ylab = "Frequency"
  )

#Q11. Draw a random sample of 10 observations from your filter_dat dataframe in a new dataframe.
  myData_sam <- sample(myData_fil, 10)
  str(myData_sam)

#Q12. Create a histogram of the age variable from the random sample.
  hist(
    myData_sam$age,
    col = "tomato",
    border = "black",
    main = "Histogram of Age (sampled)",
    xlab = "Age",
    ylab = "Frequency"
  )

#Q13. What do you notice is different between the random sample and dataset histograms? (Respond with code and short written response)
  # Set up the plotting area to display two histograms side by side
  par(mfrow = c(1, 2)) # 1 row, 2 columns
    hist(
      myData_fil$age,
      col = "skyblue",
      border = "black",
      main = "Histogram of Age (filtered)",
      xlab = "Age",
      ylab = "Frequency"
    )
    hist(
      myData_sam$age,
      col = "tomato",
      border = "black",
      main = "Histogram of Age (sampled)",
      xlab = "Age",
      ylab = "Frequency"
    )
  # The dataset histogram has a normal looking distribution. The random set I got is skewed right
    # and could also be looked at to be bimodal.

#Q14. Create a new variable of the income variable, called income3 with 3 levels, and assign levels and labels to its values.
    # I used the data from summary(myData) 1stQ=37 and 3rdQ=55, but I may have looked at income levels for middle class,
    # or even created groups that were more equal NTILE function.
    myData <- myData |>
      mutate(
        income3 = case_when(
          income <= 36 ~ "Low",
          income >= 56 ~ "Upper",
          TRUE ~ "Middle"
        )
      )

#Q15. Use the aggregate() function to compute the mean and standard deviation of three quantitative variables, by one categorical variable
  aggregate(
    household ~ investments,
    myData_fil,
    FUN = mean
  )
  aggregate(
    household ~ investments,
    myData_fil,
    FUN = sd
  )
  summary(myData_fil$household)
    
  aggregate(
    vehicles ~ investments,
    myData_fil,
    FUN = mean
  )
  aggregate(
    vehicles ~ investments,
    myData_fil,
    FUN = sd
  )
  summary(myData_fil$vehicles)
  
  aggregate(
    priceExpected ~ investments,
    myData_fil,
    FUN = mean
  )
  aggregate(
    priceExpected ~ investments,
    myData_fil,
    FUN = sd
  )
  summary(myData_fil$priceExpected)
  