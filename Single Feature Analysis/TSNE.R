#1. Load libraries
library(Rtsne)
library(RColorBrewer)
library(ggplot2)

#2. Import the percentage matrix and select columns to use in this t-SNE
csv <- read.csv() #name of csv file
perc <- csv[,"..."] #names of the columns

#3. Create TSNE coordinates
tsne_out <- Rtsne(perc, check_duplicates = FALSE, pca = FALSE)
tsne_df <- data.frame(tSNE1 = tsne_out$Y[, 1], tSNE2 = tsne_out$Y[, 2],
                      perc)

#4. Create TSNE plot colored on age
ggplot(tsne_df,  aes(x = tSNE1, y = tSNE2, color = as.numeric(Age))) +
  geom_point(size = 4) +
  theme_bw() +
  scale_color_gradientn("Age", 
                        colours = colorRampPalette(rev(brewer.pal(n = 11, name = "Spectral")))(50))

#5. Create TSNE plot colored on gender
ggplot(tsne_df,  aes(x = tSNE1, y = tSNE2, color = as.factor(Gender))) +
  geom_point(size = 4) +
  theme_bw() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c(rgb(136,189,230, maxColorValue = 255), 
                                rgb(246, 170, 201, maxColorValue = 255)), labels = c("Male", "Female")) +
  guides(color = guide_legend(override.aes = list(size = 6), ncol = 1, title = "Gender"))

#6. Create TSNE plot colored on ethnicity
ggplot(tsne_df,  aes(x = tSNE1, y = tSNE2, color = as.factor(Ethnicity))) +
  geom_point(size = 4) +
  theme_bw() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c(rgb(144, 205, 151, maxColorValue = 255), rgb(188, 153, 199, maxColorValue = 255)), 
                     labels = c("Hispanic or Latino", "Not Hispanic or Latino")) +
  guides(color = guide_legend(override.aes = list(size = 6), ncol = 1, title = "Ethnicity"))


