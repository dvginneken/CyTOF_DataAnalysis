################################################################################
#Create density plot to identify dead cells and doublets
#Arguments:
# fcs       = Flowset
# directory = Directory to save the plots
# study     = Study ID
# batch     = Batch number
################################################################################
DensityPlots <- function(fcs, directory, study, batch){
  png(paste0(fcs_dir,"/debris1.png"), width = 1000)
  plot <- autoplot(fcs, "Dead", "DNA2")
  print(plot)
  dev.off()
  
  png(paste0(fcs_dir,"/debris2.png"), width = 1000)
  plot <- autoplot(fcs, "DNA1", "DNA2")
  print(plot)
  dev.off()
  
  png(paste0(fcs_dir,"/debris3.png"), width = 1000)
  plot <- autoplot(fcs, "DNA1", "Cell_length")
  print(plot)
  dev.off()
}


################################################################################
#Loop through each batch to tranform the data and create density plots.
#Arguments:
# studies   = vector of studies ID's
# directory = Directory to save the plots
################################################################################
Analyse_Density <- function(studies, directory){
  for (study in studies){
    study_dir <-paste0(directory, "SDY", study, "/")
    nr_batches = length(dir(study_dir, pattern = "Batch"))
    metadata_study = read.xlsx(paste0(study_dir, "Metadata_Cytof_SDY",study,".xlsx"))
    for (batch in seq(1:nr_batches)){
      fcs_dir <- paste0(study_dir, "Batch",batch)
      metadata_df_batch <- CreateMetadata(fcs_dir, batch, study, metadata_study)
      flowset_raw <- CreateFlowset(fcs_dir, metadata_df_batch)
      flowset_raw <- ChangeNames(flowset_raw)
      
      #1. Transform data
      fcs <- Transform(flowset_raw, 1:ncol(exprs(flowset_raw[[1]])))
      rm(flowset_raw)
      
      #2. Create density plots
      DensityPlots(fcs, fcs_dir, study, batch)
    }
  }
  writeLines("Created density plots")
}


  

