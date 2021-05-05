#1. Load libraries
library(stringr)

#2. Set variables
test_variable = "Age"            #The feature to predict
directory = ".../Prediction/"    #Directory

#3. Source model functions
source(paste0(directory, "BuildModel.functions.R"))

#4. Import percentage matrix
csv <- read.csv(paste0(directory, "... file name... .csv"),
                stringsAsFactors = FALSE)
colnames(perc) <- c(paste0("CL",1:"..."), "Age", "Ethnicity", "Gender", "Study", 
                    "Subject")    #Change the column names

#5. Create a sample ID (Study ID + Subject ID)
Sample <- paste0(perc$Study, perc$Subject)
perc <- cbind(perc, Sample)

#6. Perform a log transformation
perc[,1:"..."] <- log(perc[,1:"..."]) #Only select column which contain cluster data

#7. Get train and evaluation data
train <- read.csv(paste0(directory, "Train_data.csv"),
                  stringsAsFactors = FALSE)
sample <- paste0(train$study_id, train$subject_id)
train <- cbind(train, sample)
evaluate <- read.csv(paste0(directory, "Evaluation_data.csv"),
                     stringsAsFactors = FALSE)
sample <- paste0(evaluate$study_id, evaluate$subject_id)
evaluate <- cbind(evaluate, sample)

#8. Define outer folds
folds <- OuterFolds(train_data, train, directory)

#9. Select train data from the percentages
train_data <- perc[perc$Sample %in% train$sample, ]

#10. Transform log transformed data into z-scores
z_table <- CalculateZ(train_data, directory)

#11. Define the optimum number of clusters to use
n_clusters <- DefineParameters('Age', directory, folds, z_table)
n_clusters <- round(mean(n_clusters), digits = 0)

#12. Train de age-predicting model
model <- TrainModel("Age", directory, z_table, n_clusters)
save(model, file = paste0(directory, "Model.RData"))

#13. Select evaluation data from the matrix of percentages
eval_data <- perc[perc$Sample %in% evaluate$sample, ]

#14. Tranform the percentages of the evaluation data into z-scores
z_table <- CalculateZ(eval_data, directory)

#15. Evaluate data with the model
EvaluateModel(z_table, model)