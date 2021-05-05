################################################################################
#CyTOF Preprocessing pipeline
#Daphne van Ginneken
#Updated april 2021
#
#This pipeline is to preprocess FCS file from immport for studies which belong 
#to the FluPRINT database.
################################################################################

#1. Load libraries
library(flowCore)
library(openxlsx)
library(CytoNorm)
library(sjmisc)
library(stringr)

#2. Set your main directory
directory = ".../Preprocessing/"

#3. Define the studies you want to preprocess
studies = c("...") #example: 112, 113, 305

#4. Source the preprocessing functions
source(paste0(directory,"Preprocess.functions.R"))

#5. Define the markers you want to use in the preprocessing
markers <- Markers(paste0(directory,"Panel.xlsx"))

#6. Perform the preprocessing with control samples
Preprocess(directory, markers, studies, control = TRUE)

#7. Perform the preprocessing without the control samples
Preprocess(directory, markers, studies, control = FALSE)



