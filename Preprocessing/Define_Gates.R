#Daphne van Ginneken
#Updated: May 2021

library(flowCore)
library(openxlsx)
library(sjmisc)
library(stringr)
library(ggcyto)

#1. Define main directory
directory = ".../Preprocessing/"

#3. Define the studies for which you want to create density plots
studies = c("...") #example: 112, 113, 305

#4. Source the plot functions
source(paste0(directory,"Plot.functions.R"))
source(paste0(directory, "Preprocess.functions.R"))

#5. Create density plots for each batch
Analyse_Density(studies, directory)


