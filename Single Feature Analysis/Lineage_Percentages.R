#1. Load libraries
library(openxlsx)

#2. Import count table and select the columns to use
counts <- read.csv("...")       #File name of count matrix
raw_counts <- counts[,"..."]    #Column names

#3. Import table with clusters per lineage
lineages <- read.xlsx("...")    #File name of lineage table

#4. Create an empty matrix
total <- matrix(ncol = 4, nrow=0)
colnames(total) <- c("B Cellen", "T Cellen", "Myeloid", "NK Cellen")

#5. Add lineage percentages for each sample to the matrix
for (sample in 1:nrow(raw_counts)){
  cells <- sum(raw_counts[sample,])
  bcells <- sum(raw_counts[sample, paste0("CL", na.omit(lineages$B.cellen))])
  tcells <- sum(raw_counts[sample, paste0("CL", na.omit(lineages$T.cellen))])
  myeloid <- sum(raw_counts[sample, paste0("CL", na.omit(lineages$Myeloid))])
  nkcells <- sum(raw_counts[sample, paste0("CL", na.omit(lineages$NK.cellen))])
  b_perc <- bcells / cells * 100
  t_perc <- tcells / cells * 100
  m_perc <- myeloid / cells * 100
  nk_perc <- nkcells / cells * 100
  total <- rbind(total, c(b_perc, t_perc, m_perc, nk_perc))
}
total <- cbind(total, counts$Study, counts$Subject, counts$Age, counts$Gender, counts$Ethnicity)
colnames(total) <- c("B_Cellen", "T_Cellen", "Myeloid", "NK_Cellen", "Study", "Subject",
                     "Age", "Gender", "Ethnicity")

#6. Plot age against the percentage of B Cells
ggplot(as.data.frame(total), aes(x = Age, y = B_Cellen)) +
  geom_point() +  
  stat_smooth(aes(group = 1)) +
  ggtitle("Change in the amount of B Cells with aging")

#7. Plot age against the percentage of T Cells
ggplot(as.data.frame(total), aes(x = Age, y = T_Cellen)) +
  geom_point() +  
  stat_smooth(aes(group = 1)) +
  ggtitle("Change in the amount of T Cells with aging")

#8. Plot age against the percentage of NK Cells
ggplot(as.data.frame(total), aes(x = Age, y = NK_Cellen)) +
  geom_point() +  
  stat_smooth(aes(group = 1)) +
  ggtitle("Change in the amount of Natural Killer Cells with aging")

#9. Plot age against the percentage of Myeloid Cells
ggplot(as.data.frame(total), aes(x = Age, y = Myeloid)) +
  geom_point() +  
  stat_smooth(aes(group = 1)) +
  ggtitle("Change in the amount of Myeloid Cells with aging")

