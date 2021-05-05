#Daphne van Ginneken
#Updated: April 2021


################################################################################
#Preprocess pipeline for raw FCS files downloaded from Immport.
#Arguments:
# directory           = The main directory
# markers             = A dataframe with the phenotypical and technical markers
# studies             = Study ID numbers
# control             = Boolean. Wether to use control samples (TRUE) or average
#                       samples (FALSE)
################################################################################
Preprocess <- function(directory, markers, studies, control){
  phenotypical_markers <- markers$phenotypical_markers
  technical_markers <- na.omit(markers$technical_markers)
  for (study in studies){
    study_dir <-paste0(directory, "SDY", study, "/")
    nr_batches = length(dir(study_dir, pattern = "Batch"))
    metadata_study = read.xlsx(paste0(study_dir, "Metadata_Cytof_SDY",study,".xlsx"))
    for (batch in seq(1:nr_batches)){
      #1. Preprocess each batch within the study
      fcs_dir <- paste0(study_dir, "Batch",batch)
      metadata_df_batch <- CreateMetadata(fcs_dir, batch, study, metadata_study)
      flowset_raw <- CreateFlowset(fcs_dir, metadata_df_batch)
      flowset_raw <- ChangeNames(flowset_raw)
      
      #2. Transform data
      fcs <- Transform(flowset_raw, c(phenotypical_markers, technical_markers))
      rm(flowset_raw)
      
      #3. Remove debris
      fcs <- RemoveDebris(fcs, batch, study, directory)
      
      #4. Remove samples with little amount off cells (<10000)
      samples_ind <- RemoveSamples(fcs, metadata_df_batch)
      if (length(unique(samples_ind)) > 1){
        fcs <- fcs[samples_ind]
        metadata_df_batch <- metadata_df_batch[samples_ind,]
      }
      
      #5. Optional: create average sample for this batch in the study
      if (nr_batches > 1 && control == FALSE){
        control_ind <- metadata_df_batch[,"control_id"]==0
        metadata_df_batch <- metadata_df_batch[control_ind,]
        fcs <- fcs[control_ind]
        fcs <- CreateControl(fcs, fcs_dir, batch)
        metadata_df_batch <- rbind(metadata_df_batch,
                                   c(paste0("MeanSample_SDY",batch,"_885.fcs"),885,batch,1,0,study,0,0))
      }
      print(paste0("Preprocessed batch ", batch, " of SDY", study))
      if (batch == 1){
        fcs_all <- fcs
        metadata_df_all <- metadata_df_batch
      }
      else{
        fcs_all <- rbind2(fcs_all, fcs)
        metadata_df_all <- rbind(metadata_df_all, metadata_df_batch)
      }
      rm(metadata_df_batch, fcs)
    }
    
    #6. Normalize between batches within the study
    if (nr_batches > 1){
      NormalizeFlowset(fcs_all, metadata_df_all, phenotypical_markers, study_dir, 
                       norm_factor = "batch_id")
      metadata_df_norm <- CreateNewMetadata(metadata_df_all)
      fcs <- CreateFlowset(paste(study_dir, "Normalized", sep = "/"), metadata_df_norm)
      unlink(paste(study_dir, "Normalized", sep = "/"), recursive = TRUE)
    }else{
      fcs <- fcs_all
      metadata_df_norm <-metadata_df_all}
    save(fcs, file = paste0(study_dir, "/fcs_normalized_control",control,"_SDY",study,".RData"))
    
    #7. Create an average sample for this study
    if (length(studies)>1){
      fcs <- CreateControl(fcs, study_dir, study)
      metadata_df_norm <- rbind(metadata_df_norm,
                                c(paste0("MeanSample_SDY",study,"_885.fcs"),885,0,1,0,study,0,0))
    }
    
    print(paste0("Preprocessed study SDY",study))
    if (exists("fcs_studies") == TRUE){
      metadata_df <- rbind(metadata_df, metadata_df_norm)
      fcs_studies <- rbind2(fcs_studies, fcs)
    }else{
      fcs_studies <- fcs
      metadata_df <- metadata_df_norm
    }
    rm(metadata_df_norm, metadata_df_all, fcs)
  }
  
  #5. Normalize between studies
  if (length(studies)>1){
    NormalizeFlowset(fcs_studies, metadata_df, phenotypical_markers, directory, norm_factor = "study_id")
    metadata_df <- CreateNewMetadata(metadata_df)
    fcs_studies <- CreateFlowset(paste(directory, "Normalized", sep = "/"), metadata_df_norm)
  }
  
  write.xlsx(metadata_df, paste0(directory,"/metadata.xlsx"))
  
  #6. Downsample each sample to 150000 cells
  fcs_norm <- DownSample(fcs_studies)
  
  #7. Save FlowSet as RData object
  save(fcs_norm, file = paste0(directory,"/fcs_processed.RData"))
  
  #8. Save CSV with metadata
  expr <- fsApply(fcs_norm, exprs)
  study_ids <- CreateIDS(metadata_df$study_id, fcs_norm)
  subject_ids <- CreateIDS(metadata_df$subject_id, fcs_norm)
  age_ids <- CreateIDS(metadata_df$age_id, fcs_norm)
  gender_ids <- CreateIDS(metadata_df$gender_id, fcs_norm)
  ethnicity_ids <- CreateIDS(metadata_df$ethnicity_id, fcs_norm)
  expr_metadata <- cbind(expr, study_ids, subject_ids,
                         age_ids, gender_ids,
                         ethnicity_ids)
  write.csv(expr_metadata, file = paste0(directory,"/expr_matrix_final.csv"))
  writeLines("Saved CSV file.")
}

################################################################################
#Define phenotypical and technical markers
#Arguments:
# file_name       = Name of the panel file
#Return:
# markers_df      = Dataframe of the markers
################################################################################
Markers <- function(file_name){
  panel <- read.xlsx(file_name)
  panel_df <- data.frame(panel)
  panel_df$Antigen <- gsub("-", "_", panel_df$Antigen)
  (phenotypical_markers <- panel_df$Antigen[panel_df$Phenotypical == 1])
  (technical_markers <- panel_df$Antigen[panel_df$Technical == 1])
  technical_markers <- c(technical_markers, rep(NA, length(phenotypical_markers) - 
                                                    length(technical_markers)))
  markers_df <- data.frame(phenotypical_markers, technical_markers)
  return(markers_df)
}

################################################################################
#Create metadata.
# Control_ID: 0 = sample, 1 = control sample
# Gender_ID:  1 = female, 0 = male
# Ethnicity_ID: 1 = Not Hisp of Lat, 0 = Hisp or Lat
#Arguments:
# fcs_dir         = Directory of FCS files
# batch           = Batch number
# study           = Study ID
# metadata_study  = Dataframe with metadata from Immport
#Return:
# metadata_df     = Data frame of metadata
################################################################################
CreateMetadata <- function(fcs_dir, batch, study, metadata_study){
  file_name <- dir(fcs_dir, pattern = ".fcs")
  n <- length(file_name)
  subject_id <- as.integer(rep(0,n))
  control_id <- as.integer(rep(0, n))
  age_id <- as.integer(rep(1,n))
  study_id <- as.integer(rep(study,n))
  batch_id <- as.integer(rep(batch,n))
  gender_id <- as.integer(rep(0,n))
  ethnicity_id <- as.integer(rep(0,n))
  metadata_df <- data.frame(file_name, subject_id, batch_id, control_id, age_id, 
                            study_id, gender_id, ethnicity_id, stringsAsFactors = FALSE)
  for (row in seq(1:nrow(metadata_df))){
    if(str_contains(metadata_df$file_name[row], 885) || 
       str_contains(metadata_df$file_name[row], 284)){
      metadata_df$control_id[row] <- 1
    }else{metadata_df$control_id[row] <- 0}
    row2 <- metadata_study[metadata_df$file_name[row] == metadata_study[,"File.Name"],]
    metadata_df$age_id[row] <- as.numeric(row2$Subject.Age)
    if (row2$Gender == "Female"){
      metadata_df$gender_id[row] <- 1
    }else{metadata_df$gender_id[row] <- 0}
    if (row2$Ethnicity == "Not Hispanic or Latino"){
      metadata_df$ethnicity_id[row] <- 1
    }else{metadata_df$ethnicity_id[row] <- 0}
    metadata_df$subject_id[row] <- as.integer(str_sub(row2$Subject.Accession, start = 4))
  }
  return(metadata_df)
}

################################################################################
#Create a Flowset
#Arguments:
# directory       = Directory of FCS files
# metadata_df     = Dataframe of metadata
#Return:
# fcs_raw         = Flowset
################################################################################
CreateFlowset <- function(directory, metadata_df){
  setwd(directory)
  fcs_raw <- read.flowSet(metadata_df$file_name, transformation = FALSE, 
                          truncate_max_range = FALSE)
  return(fcs_raw)
}


################################################################################
#Change the column names of the flowset.
#Arguments:
# fcs_raw         = Flowset of raw FCS files
#Return:
# fcs_raw         = Flowset with changed columnnames
################################################################################
ChangeNames <- function(fcs_raw){
  panel_fcs <- pData(parameters(fcs_raw[[1]]))
  for (item in seq(1:nrow(panel_fcs))){
    if (is.na(panel_fcs[item,]$desc)){
      if (panel_fcs[item,]$name %in% c("Time", "Cell_length")){
        panel_fcs[item,]$desc <- panel_fcs[item,]$name
      }else{
        name <- str_sub(panel_fcs[item,]$name, end = -10)
        panel_fcs[item,]$desc <- name
      }
    }
  }
  panel_fcs$desc <- gsub("-", "_", panel_fcs$desc)
  colnames(fcs_raw) <- panel_fcs$desc
  return(fcs_raw)
}

################################################################################
#Write new FCS files for a flowSet
#Arguments:
# fcs       = FlowSet
# dir_all   = Directory to save the new FCS files
################################################################################
WriteFCS <- function(fcs, dir_all){
  dir.create(dir_all, showWarnings = FALSE)
  fsApply(fcs, function(x){
    file_name <- paste(dir_all,keyword(x)$GUID, sep = "/")
    write.FCS(x, file_name)
  })
}

################################################################################
#Normalize the FCS files of a FlowSet
#Arguments:
# fcs             = Flowset which needs to be normalized
# metadata_df     = Dataframe of the metadata
# lineage_markers = List of markers to use for normalization
# dir             = Directory to save normalized FCS files
# norm_factor     = Normalization factor (between batches or between studies)
################################################################################
NormalizeFlowset <- function(fcs, metadata_df, lineage_markers, dir, norm_factor){
  WriteFCS(fcs, dir)
  rm(fcs)
  setwd(dir)
  train <- metadata_df[metadata_df$control_id == 1, ]
  validate <- metadata_df[metadata_df$control_id == 0, ]
  transformList <- flowCore::transformList(lineage_markers,
                                           cytofTransform)
  transformList.reverse <- flowCore::transformList(lineage_markers,
                                                   cytofTransform.reverse)
  model <- CytoNorm.train(files = train$file_name,
                          labels = train[,norm_factor],
                          channels = lineage_markers,
                          transformList = transformList,
                          FlowSOM.params = list(nCells = 6000, 
                                                xdim = 5,
                                                ydim = 5,
                                                nClus = 10,
                                                scale = FALSE),
                          normMethod.train = QuantileNorm.train,
                          normParams = list(nQ = 101,
                                            goal = "mean"),
                          seed = 1,
                          verbose = TRUE)
  CytoNorm.normalize(model = model,
                     files = validate$file_name,
                     labels = validate[,norm_factor],
                     transformList = transformList,
                     transformList.reverse = transformList.reverse,
                     normMethod.normalize = QuantileNorm.normalize,
                     outputDir = "Normalized",
                     prefix = "Norm_",
                     clean = TRUE,
                     verbose = TRUE)
  writeLines("Normalization finished.")
  for (file in dir(dir, pattern = ".fcs")){
    unlink(paste(dir,file, sep = "/"))
  }
}

################################################################################
#Create new metadata for the normalized FCS files.
#Arguments:
# metadata_df     = Dataframe of metadata
#Return:
# metadata_df_all = Metadata dataframe for the normalized FCS files
################################################################################
CreateNewMetadata <- function(metadata_df){
  validate <- metadata_df[metadata_df$control_id == 0, ]
  for (row in 1:nrow(validate)){
    validate$file_name[row] <- paste0("Norm_",validate$file_name[row])
    validate <- validate[c("file_name", "subject_id", "batch_id", "control_id", "age_id", 
                           "study_id", "gender_id", "ethnicity_id")]
  }
  metadata_df_all <- validate
  return(metadata_df_all)
}

################################################################################
#Create a mean sample to use as control sample and add it to the flowset
#Arguments:
# flowset     = A flowset
# study_dir   = The directory containing the FCS files
# study       = Study ID
#Return:
# fcs         = A flowset containing the control sample
################################################################################
CreateControl <- function(flowset, study_dir, study){
  control_matrix <- matrix(ncol = length(colnames(flowset)))
  for (sample in seq(1:length(flowset))){
    matrix <- exprs(flowset[[sample]])
    IND <- sample(nrow(matrix), size = round((nrow(matrix)/length(flowset)), digits = 0))
    sub <- matrix[IND,]
    control_matrix <- rbind(control_matrix, sub)
  }
  ff <- new("flowFrame", exprs = control_matrix[-1,])
  write.FCS(ff, paste0(study_dir,"/MeanSample_SDY",study,"_885.fcs"))
  fcs <- rbind2(flowset,ff)
  sampleNames(fcs) <- c(sampleNames(flowset), paste0("MeanSample_SDY",study,"_885.fcs"))
  print(paste0("Created a control sample for study SDY", study))
  return(fcs)
}

################################################################################
#Transformation of the flowset
#Arguments:
# fcs_raw         = Flow Set of raw FCS files
# markers         = Dataframe of the markers
#Return:
# fcs             = Transformed Flow Set
################################################################################
Transform <- function(fcs_raw, markers){
  fcs <- fsApply(fcs_raw, function(x, cofactor = 5){
    expr <- exprs(x)
    expr <- asinh(expr[, markers] / cofactor)
    exprs(x) <- expr
    x
  })
  return(fcs)
}

################################################################################
#Remove dead and double cells
#Arguments:
# fcs           = FlowSet
# batch         = Batch number
# directory     = Directory of the batch
#Return:
# fcs_singlets2 = FlowSet with only alive and single cells
################################################################################
RemoveDebris <- function(fcs, batch, study, directory){
  gates = read.xlsx(paste0(directory,"Gates.xlsx"))
  gates = gates[gates$Study == study,]
  gates = gates[gates$Batch == batch,]
  

  cellcounts <- matrix(ncol = 5)
  colnames(cellcounts) <- c("Study", "Batch", "File", "Before", "After")
  cellcounts <- fsApply(fcs, function(x){
    file = identifier(x)
    cells = nrow(exprs(x))
    cellcounts <- rbind(cellcounts, c(study, batch, file, cells, 0))
  })
  cellcounts <- na.omit(cellcounts)
  
  AliveGate <- rectangleGate(filterId="Alive", "Dead"=c(gates[,3], gates[,4]),
                             "DNA2"=c(gates[,5], gates[,6]))
  fcs_alive <- Subset(fcs, AliveGate)
  SingletsGate <- rectangleGate(filterId="Singlets", "DNA1"=c(gates[,7], gates[,8]),
                                "DNA2"=c(gates[,9], gates[,10]))
  fcs_singlets <- Subset(fcs_alive, SingletsGate)
  SingletsGate2 <- rectangleGate(filterId = "Singlets2", "DNA1"=c(gates[,11], gates[,12]),
                                 "Cell_length"=c(gates[,13], gates[,14]))
  fcs_singlets2 <- Subset(fcs_singlets, SingletsGate2)
  rm(fcs, fcs_alive, fcs_singlets)
  
  cellcounts[,"After"] <- fsApply(fcs_singlets2, function(x){
      file = identifier(x)
      cells <- nrow(exprs(x))
      cellcounts[cellcounts[,"File"]==file,"After"] <- cells
    })
  dir.create(paste0(directory, "/Debris/", showWarnings = FALSE))
  write.csv(na.omit(cellcounts), file = paste0(directory, "/Debris/", study, "_",
                                               batch, "_Debris.csv"), row.names = FALSE)
  
  return(fcs_singlets2)
}

################################################################################
#Create ID's for different metadata
#Arguments:
# metadata        = Type of metadata
# fcs             = A flowset
#Return:
# ids             = A list of ids
################################################################################
CreateIDS <- function(metadata, fcs){
  ids <- integer()
  for (row in seq(1:length(metadata))){
    id <- rep(metadata[[row]], nrow(fcs[[row]]))
    ids <- c(ids, as.integer(id))
  }
  return(ids)
}

################################################################################
#Remove samples which have less than 100000 cells.
#Arguments:
# fcs             = A flowset
# metadata        = Metadata dataframe
#Return:
# IND             = List of booleans which indicate the samples that have to be
#                   removed
################################################################################
RemoveSamples <- function(fcs, metadata){
  samples <- character()
  samples <- fsApply(fcs, function(x){
    if (nrow(x) < 100000){
      samples <- c(samples, identifier(x))
    }
  })
  IND <-metadata$file_name %!in% samples
  return(IND)
}

################################################################################
#A reverse of %in%. Return the indices that are NOT in a vector.
#Arguments:
# x = vector to check
# y = vector with elements
################################################################################
'%!in%' <- function(x,y)!('%in%'(x,y))

################################################################################
#For each sample in a flowset, take a random sample of 150000 cells and remove
#the remaining cells from this sample.
#Arguments:
# fcs   = Flowset
#Return:
# fcs   = Downsampled flowset
################################################################################
DownSample <- function(fcs){
  fcs <- fsApply(fcs, function(x){
    if (nrow(x) > 150000){
      expr <- exprs(x)
      IND <- sample(nrow(expr), size=150000)
      expr <- expr[IND,]
      exprs(x) <- expr
    }
    x
  })
  return(fcs)
}




