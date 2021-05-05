#Daphne van Ginneken
#Updated: April 2021

library(BiocManager)
library(flowCore)
library(openxlsx)
library(devtools)
library(dplyr)
library(Rtsne)
library(ggplot2)
library(RColorBrewer)

#1. Define main directory
directory = ".../Preprocessing/"

#2. Source preprocess and plot functions
source(paste0(directory,"Preprocess.functions.R"))
source(paste0(directory,"Plot.functions.R"))

#2. Load the RData file of the flowset you want to visualize
fcs <- load("... .RData")

#3. Read the metadata file
metadata_df <- read.xlsx("... .xlsx")

#4. Define phenotypical markers for dimension reduction
markers <- Markers(paste0(directory,"Panel.xlsx"))
phenotypical_markers <- markers$phenotypical_markers

#5. Create an expression matrix with the study ID for each cell
expr <- fsApply(fcs, exprs)
study_ids <- CreateIDS(metadata_df$study_id, fcs_norm)
expr_metadata <- cbind(expr, study_ids)

#6. Subsample the dataset equally amongst all samples
sub <- SubSample(expr_metadata, size = 50000)

#7. Add TSNE coordinates to te dataset
tsne_df <- CoordinatesTSNE(sub, phenotypical_markers)

#8. Create a TSNE plot
colors = brewer.pal(length(unique(expr_metadata$study_ids)), "Set1")
png("...") #name of the plot
plot <- ggplot(tsne_df,  aes(x = tSNE1, y = tSNE2, color = as.factor(study_ids))) +
  geom_point(size =0.85) +
  theme_bw() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = colors) +
  guides(color = guide_legend(override.aes = list(size = 6), ncol = 1, title = "Study"))
print(plot)
dev.off()

