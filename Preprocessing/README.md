# Preprocessing
These scripts preprocess raw CyTOF samples.  
The preprocessing includes: 
* An arcsine tranformation of the raw expression values
* The removal of dead/double cells and debris
* A quality control step to remove certain samples
* An intra-study normalization
* An inter-study normalization
* Downsampling to prepare for clustering  

The pipeline results in preprocessed FCS files and an expression matrix in CSV format.  

## Import Data
