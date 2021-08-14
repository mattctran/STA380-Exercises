# STA380-Exercises
#Question 1 - Green Buildings
library(tidyverse)
library(car)
library(mosaic)
library(ggplots)
greenhouse <- read.csv("https://raw.githubusercontent.com/jgscott/STA380/master/data/greenbuildings.csv")

#Combine Class Variable
greenhouse$class_col <- 0
for (i in 1:nrow(greenhouse)){
  n = 0
  if (greenhouse$class_a[i] == 1){
    n = 2
  }
  if (greenhouse$class_b[i] == 1){
    n = 1
  }
  greenhouse$class_col[i] = n
}

greenhouse <- greenhouse %>% 
  mutate(class_col = factor(class_col, levels=c(0,1,2), labels=c("C", "B", "A")))

greenhouse <- subset(greenhouse, select = -c(class_a, class_b))

#Factor Categorical Variables
greenhouse <- greenhouse %>% 
  mutate(LEED = factor(LEED, levels=c(0,1), labels=c("No", "Yes")), 
         Energystar = factor(Energystar, levels=c(0,1), labels=c("No", "Yes")),
         amenities = factor(amenities, levels=c(0,1), labels=c("No", "Yes")),
         renovated = factor(renovated, levels=c(0,1), labels=c("No", "Yes")),
         green_rating = factor(green_rating, levels=c(0,1), labels=c("No", "Yes")),
         net = factor(net, levels=c(0,1), labels=c("No", "Yes")))

#Filter Comparable Buildings
comparable <- greenhouse %>% 
  filter(stories>=9, stories<=26)

comparable <- comparable %>% 
  filter(size>=150000, size<=350000)

comparable <- comparable %>% 
  filter(leasing_rate>10)

comparable <- comparable %>% 
  filter(age<21)

#Separate Data
compGreen <- comparable %>% 
  filter(green_rating == 'Yes')

hist(compGreen$Rent)
fav_stats(compGreen$Rent)

hist(compGreen$leasing_rate)
fav_stats(compGreen$leasing_rate)

paybackGreen <- 105000000/(35.01*0.91395*250000)
paybackGreen

compNon <- greenhouse %>% 
  filter(green_rating == 'No')

hist(compNon$Rent)
fav_stats(compNon$Rent)

hist(compNon$leasing_rate)
fav_stats(compNon$leasing_rate)

paybackNon <- 105000000/(25*0.8917*250000)
paybackNon
