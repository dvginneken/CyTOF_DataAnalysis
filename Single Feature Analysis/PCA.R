#1. Load libraries
library(factoextra)

#2. Import the percentage matrix and select columns to use in this PCA
csv <- read.csv("..")     #matrix name
perc <- csv[,"..."]       #column names

#3. Perform a Principal Component Analysis
pca <- prcomp(perc, scale = FALSE)

#4. Create a scree plot
fviz_eig(pca)

#5. Plot the clusters
fviz_pca_var(pca,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)

#5. Create age groups
age = character()
for (row in 1:nrow(csv)){
  if (as.numeric(csv[row,"Age"]) < 35){
    age <- c(age, "Young")
  }else if(as.numeric(csv[row,"Age"]) > 60){
    age <- c(age, "Old")
  }else{
    age <- c(age, "Middle")
  }
}


#6. Create PCA plot for age
fviz_pca_ind(pca,
             col.ind = as.factor(age),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = FALSE, 
             legend.title = "Age",
             repel = TRUE,
             geom = "point"
)

#7. Create PCA plot for gender
fviz_pca_ind(pca,
             col.ind = as.factor(csv$Gender), 
             palette = c(rgb(136,189,230, maxColorValue = 255), 
                         rgb(246, 170, 201, maxColorValue = 255)),
             addEllipses = FALSE, 
             legend.title = "Gender",
             repel = TRUE,
             geom = "point"
)

#8. Create PCA plot for ethnicity
fviz_pca_ind(pca,
             col.ind = as.factor(csv$Ethnicity), # color by groups
             palette = c(rgb(144, 205, 151, maxColorValue = 255), 
                         rgb(188, 153, 199, maxColorValue = 255)),
             addEllipses = FALSE, # Concentration ellipses
             legend.title = "Ethnicity",
             repel = TRUE,
             geom = "point"
)
