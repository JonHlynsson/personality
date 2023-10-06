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


epq <- d[, c(19:66)]

omega(epq, nfactors = 4)
