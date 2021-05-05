################################################################################
#Define the subjects in de outer fold (5-fold cross validation)
#Arguments:
# perc         = Matrix with cell percentages for each subject in the train data
# train        = Subjects in the train data
# directory    = Directory to save results
#Return:
# folds        = Data frame with the subjects in each fold
################################################################################
OuterFolds <- function(perc, train, directory){
  #1. Prior sampling on age
  subjects <- unique(perc$Subject)
  sub_young <- unique(perc[perc$Age<=45,"Subject"])
  sub_old <- unique(perc[perc$Age>45,"Subject"])
  
  #2. Define train and testset for the 5 outer folds
  test_used = character()
  folds = matrix(nrow = 600, ncol = 0)
  for (fold in 1:5){
    test_outer_sub <- c(sample(sub_young[sub_young %!in% test_used], length(sub_young)/5,
                               replace = FALSE), sample(sub_old[sub_old %!in% test_used], 
                                                        length(sub_old)/5,
                                                        replace = FALSE))
    test_used <- c(test_used, test_outer_sub)
    train_outer_sub <- subjects[subjects%!in%test_outer_sub]
    folds = cbind(folds, c(test_outer_sub, rep(NA, 600 - length(test_outer_sub))
    ), c(train_outer_sub, rep(NA, 600 - length(train_outer_sub))))
  }
  colnames(folds) <- c("test1", "train1", "test2", "train2", "test3", "train3",
                       "test4", "train4","test5", "train5")
  write.csv(folds, file = paste0(directory, "Folds.csv"),
            row.names = FALSE)
  
  #3. Save metadata for the 5 folds
  for (fold in c(1, 3, 5, 7, 9)){
    test <- folds[1:64,fold]
    matrix <- matrix(ncol = 5, nrow = 0)
    for (subject in test){
      row <- train[train$subject_id==subject,]
      for (i in 1:nrow(row)){
        matrix <- rbind(matrix, c(row$subject_id[i], row$study_id[i], row$age_id[i],
                                  row$ethnicity_id[i], row$gender_id[i]))
      }
    }
    colnames(matrix) <- c("Subject", "Study", "Age", "Ethnicity", "Gender")
    write.csv(matrix, file = paste0(directory, "Fold",
                                    fold,".csv"), row.names = FALSE)
  }
  return(folds)
}

################################################################################
#Train a linear model
#Arguments:
# test_variable   = The feature to predict. Example: "Age"
# directory       = Directory to save results
# data            = Data to train the model
# n               = The number of clusters to used
#Return:
# model           = A linear model
################################################################################
TrainModel <- function(test_variable, directory, data, n){
  p_values <- ClusterTests(data,test_variable,directory, 
                           cls=colnames(data[,6:ncol(z_table)]))
  p_values <- p_values[order(p_values[,"p_adj"]),]
  write.csv(p_values, "C:/Users/Daphne/Downloads/Pvalues.csv", row.names = FALSE)
  sign_clusters <- rownames(p_values)[1:n]
  model <- CreateLM(sign_clusters, data)
  return(model)
}


################################################################################
#Perform a nested cross fold validation to select hyperparameters.
#Arguments:
# test_variable   = The feature to predict. Example: "Age"
# directory       = Directory to save results
# folds           = Data Frame with the subjects in each fold
# z_table         = A matrix with z-scaled values
#Return:
# optimum_clusters  = The optimum number of clusters to use for each fold.
################################################################################
DefineParameters <- function(test_variable, directory, folds, z_table){
  png(paste0(directory, "Cluster_Optimum.png"))
  colors = rainbow(5)
  plot(NULL, xlim=c(0,60), ylim=c(0.6,0.92), xlab = "Number of clusters used",
       ylab = "Correlation Coefficient")
  outer_evaluation <- matrix(ncol=6)
  colnames(outer_evaluation) <- c("subject", "study", "age", "predicted age",
                                  "Deviation", "fold")
  optimum_clusters <- integer()
  for (fold in 1:5){
    test_outer_sub <- na.omit(folds[,paste0("test", fold)])
    train_outer_sub <- na.omit(folds[,paste0("train", fold)])
    test_outer <- z_table[z_table$Subject %in% test_outer_sub,]
    train_outer <- z_table[z_table$Subject %in% train_outer_sub,]
    evaluation <- matrix(ncol=6)
    colnames(evaluation) <- c("subject", "study", "age", "predicted age",
                              "Deviation", "nr of clusters")
    for (subject in train_outer_sub){
      test_inner <- train_outer[train_outer$Subject == subject,]
      train_inner <- train_outer[train_outer$Subject != subject,]
      for (n in 1:60){
        p_values <- ClusterTests(train_inner,test_variable,directory, 
                                 cls=colnames(z_table[,6:ncol(z_table)]))
        p_values <- p_values[order(p_values[,"p_adj"]),]
        sign_clusters <- rownames(p_values)[1:n]
        model <- CreateLM(sign_clusters, train_inner)
        evaluation <- TestModel(test_inner, model, evaluation, n)
      }
    }
    write.csv(evaluation, 
              paste0(directory, "/LM_evaluation",
                     fold,".csv"), row.names = FALSE)  
    evaluation = evaluation[-1,]
    correlation_matrix <- matrix(ncol=3)
    colnames(correlation_matrix) <- c("Top n clusters", "Cor.Coef", "P-value")
    for (i in 1:60){
      eval <- evaluation[evaluation[,"nr of clusters"]==i,]
      test <- cor.test(as.numeric(eval[,"age"]), as.numeric(eval[,"predicted age"]))
      correlation_matrix <- rbind(correlation_matrix, c(i, test$estimate, 
                                                        test$p.value))
    }
    correlation_matrix = correlation_matrix[-1,]
    optimum = correlation_matrix[correlation_matrix[,"Cor.Coef"]==max(correlation_matrix[,"Cor.Coef"]),]
    lines(correlation_matrix[,"Top n clusters"], correlation_matrix[,"Cor.Coef"], 
          type = "l", lty = 1, col=colors[[fold]])
    points(optimum[1], optimum[2], pch = 21, bg = "black")
    optimum_clusters <- c(optimum_clusters, optimum["Top n clusters"])
    p_values <- ClusterTests(train_outer,test_variable,directory, 
                             cls=colnames(z_table[,6:ncol(z_table)]))
    p_values <- p_values[order(p_values[,"p_adj"]),]
    sign_clusters <- rownames(p_values)[1:optimum["Top n clusters"]]
    model <- CreateLM(sign_clusters, train_outer)
    outer_evaluation <- TestModel(test_outer, model, outer_evaluation, fold)
  }
  legend("bottomright", legend = c("Fold 1", "Fold 2", "Fold 3", "Fold 4", "Fold 5"), 
         fill = colors)
  print(plot)
  dev.off()
  write.csv(outer_evaluation, paste0(directory, "Outer_evaluation.csv"),
            row.names = FALSE)
  return(optimum_clusters)
}

################################################################################
#Evaluate the linear model against evaluation data.
#Arguments:
# data        = Evaluation data
# model       = Linear model
################################################################################
EvaluateModel <- function(data, model){
  evaluation <- matrix(ncol=6)
  colnames(evaluation) <- c("subject", "study", "age", "predicted age", 
                            "Deviation", "-")
  evaluation <- TestModel(data, model, evaluation, n=0)
  write.csv(evaluation[-1,1:5], paste0(directory, "Final_Evaluation.csv"),
            row.names = FALSE)
  writeLines("Finished evaluation.")
}

################################################################################
#Function to check if an element is NOT in a vector.
#Arguments:
# x     = element to check
# y     = vector
#Return:
# Boolean
################################################################################
'%!in%' <- function(x,y)!('%in%'(x,y))

################################################################################
#Store results of the evaluation in a matrix
#Arguments:
# test        = Data to test the model on
# model       = Linear model
# evaluation  = Matrix to store results
# n           = Number of clusters used
#Return:
# evaluation  = Matrix with evaluation results
################################################################################
TestModel <- function(test, model, evaluation, n){
  predicted_ages <- numeric()
  for (sample in 1:nrow(test)){
    p_age <- PredictAge(model, test[sample,])
    predicted_ages <- c(predicted_ages, p_age)
    abs_dev <- as.numeric(test[sample,"Age"])-as.numeric(p_age)
    row <- c(test[sample,"Subject"], test[sample,"Study"], test[sample,"Age"],
             p_age, abs_dev, n)
    evaluation <- rbind(evaluation, row)
  }
  return(evaluation)
}

################################################################################
#Predict Ages.
#Arguments:
# lm      = Linear model
# subject = The subject for which to predict age.
#Return:
# age     = The predicted age
################################################################################
PredictAge <- function(lm, subject){
  age = coef(lm)[["(Intercept)"]]
  for (x in names(coef(lm))[-1]){
    coef <- coef(lm)[[x]]
    cluster <- names(coef(lm)[x])
    cluster <- str_sub(cluster, start = 12, end = -2)
    age = age + coef*as.numeric(subject[,cluster])
  }
  return(age)
}

################################################################################
#Create a multiple linear regression model
#Arguments:
# sign_clusters = The clusters to use in the model
# train         = The train dataset
#Return:
# lm            = Linear regression model
################################################################################
CreateLM <- function(sign_clusters, train){
  form <- "as.numeric(Age)~"
  for (cl in sign_clusters){
    form <- paste0(form,"as.numeric(",cl,")+")
  }
  form <- str_sub(form, end = -2)
  FORM <- as.formula(form)
  lm <- lm(FORM,dat=train)
  return(lm)
}

################################################################################
#Calculate the z-scores
#Arguments:
# celperc   = Relative frequency table
# directory = Directory to store results
#Return:
# z_table   = A matrix with z-scores
################################################################################
CalculateZ <- function(celperc, directory){
  z_table <- data.frame(celperc[85:89], stringsAsFactors = FALSE)
  colnams <- character()
  for (cluster in colnames(celperc[,1:84])){
    gem <- mean(as.numeric(celperc[,cluster]))
    std <- sd(as.numeric(celperc[,cluster]))
    z_cluster <- numeric()
    for (sample in 1:nrow(celperc)){
      value <- as.numeric(celperc[sample,cluster])
      z <- (value - gem)/std
      z_cluster <- c(z_cluster, z)
    }
    if (is.nan(z_cluster) == FALSE){
      z_table <- cbind(z_table, z_cluster)
      colnams <- c(colnams, cluster)
    }  
  }
  colnames(z_table) <- c("Age", "Ethnicity", "Gender", "Study", "Subject", colnams)
  write.csv(z_table, paste0(directory, "z_scores.csv"), row.names = FALSE)
  return(z_table)
}

################################################################################
#Test the association between each cluster and a feature such as age.
#Arguments:
# dat           = Train dataset
# test_variable = Feature to predict. Example: "Age"
# directory     = Directory to save resutls
# cls           = The clusters to test
#Return:
# result        = A matrix with results for each cluster
################################################################################
ClusterTests <- function(dat,test_variable,directory,cls){
  result <- matrix(ncol = 4, nrow = 0)
  colnames(result) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  for (cluster in cls){
    FORM <- as.formula(paste0(cluster,"~as.numeric(",test_variable,")"))
    model <- lm(FORM,dat=dat)
    result <- rbind(result, coefficients(summary(model))[2,])
  }
  rownames(result) <- cls
  p <- as.vector(result[,"Pr(>|t|)"])
  p_adj <- p.adjust(p, "bonferroni")
  result <- cbind(result, p_adj)
  return(result)
}
