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

## Input Data
This pipeline is used to preprocess CyTOF samples from studies in the FluPRINT database (https://fluprint.com/#/about). These raw FCS file can be downloaded from https://immport.org/shared/home. The studies which were preprocessed with this pipeline are: SDY112, SDY113, SDY305, SDY311, SDY315, SDY472, SDY478, SDY515, SDY519, SDY1466, SDY1468, SDY1471.

## Preprare Data
After downloading it is important to organize the data in a specific way by following these steps:
1. Download this folder
2. Within this folder: create a folder for each study you want to preprocess in the format: SDY[study]
3. Within each SDY folder: Create a folder for each batch in the format: Batch[batch number]. When this study only has 1 batch create the folder: Batch1
4. Save the metadata files from Immport in each corresponding SDY folder, change the name of the file to: Metadata_Cytof_SDY[study].
5. Save the raw FCS files from Immport in their designated folder.
6. Run the script Define_Gates.R to create density plots
7. Determine live/dead gates based on the density plots and write the gates in a file called Gates.xlsx (see example)
8. Create a file called Panel.xlsx (see example) to define which markers to use in the analysis

## Run the preprocessing pipeline
