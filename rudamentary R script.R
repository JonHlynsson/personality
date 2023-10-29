## clean the environment -------------------------------------------------------
rm(list = ls(all=T)) # remove all variables (all=T takes care of hidden objects)
graphics.off() # turn off plots
cat("\014") # clear the console
## -----------------------------------------------------------------------------


# Load necessary libraries -----------------------------------------------------
library(readxl)
library(haven)
library(ggpubr)
library(gridExtra)
library(cowplot)
library(gridGraphics)
library(psych)
library(ggfortify)
library(parameters)
library(robust)
library(gtsummary)
library(caret)
library(mirt)
library(shiny)
library(ggmirt)
library(grid)
library(directlabels)
library(KernSmoothIRT)
library(ggpubr) # for convenient plotting 
library(psych)
library(gmodels)
library(DescTools)
library(Hmisc)
library(GPArotation)
library(misty)
library(lavaan) # for CFA
library(lavaanPlot) # for CFA
library(tidyverse)
library(kableExtra) # for nice tables
library(knitr) # for kable function and markdown output
library(apaTables)
library(semTools)
library(tidyverse)

options(scipen = 999) #show as integers instead of exponential notations
options(digits=4) #Number of digits in output
# ------------------------------------------------------------------------------


d <- read_sav(file = "sameinad2023.sav")



hexaco <- d[, c(321:380)]

omega(hexaco, nfactors = 6)


## EPQ --------------------------------------------------
epq <- d[, c(19:66)]

epqN <- epq[, c(1, 5, 9, 13, 17, 21, 25, 30, 34, 38, 42, 46)]


epqN$epq1 <- ifelse(epqN$epq1 == 1, 1, 0)
epqN$epq5 <- ifelse(epqN$epq5 == 1, 1, 0)
epqN$epq9 <- ifelse(epqN$epq9 == 1, 1, 0)
epqN$epq13 <- ifelse(epqN$epq13 == 1, 1, 0)
epqN$epq17 <- ifelse(epqN$epq17 == 1, 1, 0)
epqN$epq21 <- ifelse(epqN$epq21 == 1, 1, 0)
epqN$epq25 <- ifelse(epqN$epq25 == 1, 1, 0)
epqN$epq30 <- ifelse(epqN$epq30 == 1, 1, 0)
epqN$epq34 <- ifelse(epqN$epq34 == 1, 1, 0)
epqN$epq38 <- ifelse(epqN$epq38 == 1, 1, 0)
epqN$epq42 <- ifelse(epqN$epq42 == 1, 1, 0)
epqN$epq46 <- ifelse(epqN$epq46 == 1, 1, 0)

epqN$kyn <- d$kyn

epqN_New <- epqN$epq1 + epqN$epq5 + epqN$epq9 + epqN$epq13 + epqN$epq17 + epqN$epq21 + epqN$epq25 + epqN$epq30 + epqN$epq34 + epqN$epq38 + epqN$epq42 + epqN$epq46

boxplot(x = d$kyn, epqN_New)

# Type D: Útreikningar! --------------------------------------------------------
typeD <- d[, c(4:17)]

typeD$DS1 <- ifelse(typeD$DS1 == 0, NA, typeD$DS1)
typeD$DS2 <- ifelse(typeD$DS2 == 0, NA, typeD$DS2)
typeD$DS3 <- ifelse(typeD$DS3 == 0, NA, typeD$DS3)
typeD$DS4 <- ifelse(typeD$DS4 == 0, NA, typeD$DS4)
typeD$DS5 <- ifelse(typeD$DS5 == 0, NA, typeD$DS5)
typeD$DS6 <- ifelse(typeD$DS6 == 0, NA, typeD$DS6)
typeD$DS7 <- ifelse(typeD$DS7 == 0, NA, typeD$DS7)
typeD$DS8 <- ifelse(typeD$DS8 == 0, NA, typeD$DS8)
typeD$DS9 <- ifelse(typeD$DS9 == 0, NA, typeD$DS9)
typeD$DS10 <- ifelse(typeD$DS10 == 0, NA, typeD$DS10)
typeD$DS11 <- ifelse(typeD$DS11 == 0, NA, typeD$DS11)
typeD$DS12 <- ifelse(typeD$DS12 == 0, NA, typeD$DS12)
typeD$DS13 <- ifelse(typeD$DS13 == 0, NA, typeD$DS13)
typeD$DS14 <- ifelse(typeD$DS14 == 0, NA, typeD$DS14)

typeD$DS1 <- typeD$DS1-1
typeD$DS2 <- typeD$DS2-1
typeD$DS3 <- typeD$DS3-1
typeD$DS4 <- typeD$DS4-1
typeD$DS5 <- typeD$DS5-1
typeD$DS6 <- typeD$DS6-1
typeD$DS7 <- typeD$DS7-1
typeD$DS8 <- typeD$DS8-1
typeD$DS9 <- typeD$DS9-1
typeD$DS10 <- typeD$DS10-1
typeD$DS11 <- typeD$DS11-1
typeD$DS12 <- typeD$DS12-1
typeD$DS13 <- typeD$DS13-1
typeD$DS14 <- typeD$DS14-1

typeD$DS1 <- recode(.x = typeD$DS1, "0"=4, "1"=3, "2"=2, "3"=1, "4"=0)
typeD$DS3 <- recode(.x = typeD$DS3, "0"=4, "1"=3, "2"=2, "3"=1, "4"=0)



typeD$negative <- typeD$DS2 +typeD$DS4 + typeD$DS5 + typeD$DS7 + typeD$DS9 + typeD$DS12 + typeD$DS13

typeD$social <- typeD$DS1 + typeD$DS3 + typeD$DS6 + typeD$DS8 + typeD$DS10 + typeD$DS11 + typeD$DS14

typeD$D <- ifelse(typeD$negative > 9 & typeD$social > 9, 1, 0)

typeD %>% psych::describe() # Það eru 40% með týpu D!

#
alpha(typeD[, c(1:14)])

## -----------------------------------------------------------------------------



typeD %>% gtsummary::tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd}) [{min}-{max}]"), 
                                 digits = list(all_categorical() ~ 1), )




