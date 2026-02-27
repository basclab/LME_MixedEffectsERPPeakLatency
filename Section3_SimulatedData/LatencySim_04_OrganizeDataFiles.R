# Peak Latency Simulation Script: 4. Organize Data Files

# This script imports the peak latency .txt file for each simulated dataset
# (sample). Subject ID and stimuli-related information for each trial are extracted 
# from the bin label column, which is formatted as [SUBJECTID]_:_[trial-specific bin label] 
# (e.g., 01_:_60101):
  # - The first two digits of the label is the subject ID (e.g., 01)
  # - The first digit after the _:_ is the emotion condition (e.g., 6)
  # - The next two digits are the actor ID (e.g., 01)
  # - The last two digits are the trial presentation number (e.g., 01, first presentation)

# Then, the subject data log is merged with the dataframe, subject-related 
# information is extracted, and trial-level peak latency missingness is induced.
# The final dataframe for each specified percentage of low trial-count subjects
# is saved as a .csv file containing single-trial and mean-averaged peak latencies.
# This script is run separately for each combination of missingness pattern, peak 
# amplitude magnitude, and resting state noise profile.

# ***See Section3_SimulatedData README.md available on the LME_MixedEffectsERPPeakLatency
# GitHub for pipeline details: https://github.com/basclab/LME_MixedEffectsERPPeakLatency/tree/main/Section3_SimulatedData

# Requirements: 
  # - Needs R Version 3.6.1 and packages listed below
  # - importParentFolder: Folder containing two subfolders with simulated files
  #   created by LatencySim_03_SimulateERPData.m: 01_P1PeakLatencyOutput_PreMerge 
  #   (contains peak latency output files) and 01_SubjectDataLog (contains logs of 
  #   each simulated sample and subject's assigned attentional orienting speed/age 
  #   group and induced missing trials for the specified missingness pattern and
  #   percentages of low trial-count subjects)
  # - saveFolder: Folder for saving the merged dataframe containing each subject's
  #   peak latency output, assigned attentional orienting speed and age group, and
  #   stimuli-related information for each trial and condition-level bin. The 
  #   dataframe is saved as a .csv file at the end of the script.

# Script Functions:
  # 1. Import each sample's peak latency .txt file 
  # 2. Extract subject ID and stimuli-related information for each row
  # 3. Merge each sample's subject data log with the dataframe
  # 4. Induce trial-level missingness and save final long dataframe as a .csv file 
  #    for each percentage of low trial-count subjects

# Outputs: 
  # - Each sample will have a .csv file formatted as a long dataframe with the 
  #   following columns. Column names are formatted based on the convention that
  #   lowercase variables describe fixed effects (e.g., emotion) and capital-letter 
  #   variables describe random effects (e.g., SUBJECTID).
    # - SUBJECTID: Simulated subject ID (e.g., 01, 02, ...)
    # - oSpeed: Simulated attentional orienting speed group (i.e., higOSpeed, lowOSpeed)
    # - age: Simulated age group (i.e., youngerAgeGroup, olderAgeGroup)
    # - peakLatencyType: Trial-level (i.e., single-trial) or condition-level
    #   (i.e., mean-averaged) peak latency extraction
    # - emotion: Simulated emotion condition (i.e., A)
    # - ACTOR: Simulated stimulus actor ID (i.e., 01, 02, 03, 04, 05)
    # - presentNumber: Trial presentation number of specific emotion condition/
    #   actor combination ranging from 1 to 10
    # - peakLatencyP1: Simulated peak latency value (in units of ms)

# Copyright 2026 Serena K. Mon, Megan J. Heise, Lindsay C. Bowman
# Brain and Social Cognition Lab, University of California Davis, Davis, CA, USA.

# Permission is hereby granted, free of charge, to any person obtaining a 
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense, 
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included 
# in all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

library(data.table) # V.1.13.2; fread function
library(stringr) # V.1.4.0; str_sub function

#-------------------------------------------------------------------------------
# DATA ENVIRONMENT

# Specify noise characteristics to import 
missType <- 'Miss1' # Formatted as 'Miss#'
peakType <- '10MicroV' # Formated as '#MicroV'
noiseProfile <- 'PreschoolNoise' # Formatted as '[Sample]Noise'

# Specify folder location of simulated output files
importParentFolder='C:/Users/basclab/Desktop/Section3_SimulatedData'
# Create variables corresponding to each subfolder
importPeakLatencyFolder = paste0(importParentFolder, '/01_P1PeakLatencyOutput_PreMerge/', missType, '_', peakType, '_', noiseProfile)
importSubjectDataLogFolder = paste0(importParentFolder, '/01_SubjectDataLog')

# Make directory of all .txt files in importPeakLatencyFolder
peakLatencyDir = list.files(path = importPeakLatencyFolder, pattern = ".txt", full.names = TRUE, recursive = FALSE)

# Import subject data log for script 4 containing each subject's assigned attentional 
# orienting speed/age group and missing trials for the specified missingness 
# pattern and percentages of low trial-count subjects
subjectDataLogFilename = paste0(importSubjectDataLogFolder, '/', missType, '_SubjectDataLog_sampleN1000_subN48_forScript4.csv')
subjectDataLog <- fread(subjectDataLogFilename)
# Extract necessary columns from subject data log 
subjectDataLog$SUBJECTID <- str_pad(subjectDataLog$SUBJECTID, 2, pad = "0")
subjectDataLog$oSpeed <- substr(subjectDataLog$oSpeed_age, 1, 9)
subjectDataLog$age <- substr(subjectDataLog$oSpeed_age, 11, 25)

# Specify folder location for saving each sample's long dataframe
saveFolder <- paste0(importParentFolder, '/02_P1PeakLatencyOutput_Final/', missType, '_', peakType, '_', noiseProfile)
# Verify that saveFolder contains the following subfolders
caseDeletionPctArray <- c('Pop','000','006','011','032','050','060','070','080')
file.exists(paste0(saveFolder, '/', caseDeletionPctArray)) 

#-------------------------------------------------------------------------------
# For each simulated sample: Format peak latency output file, merge with 
# subject data log file, induce trial-level missingness, and export final dataframe
# as .csv file

for (peakLatencyFile in peakLatencyDir) { # Loop through each sample
  # Extract sample ID number from filename (e.g., extract '0001' from
  # 'C:/Users/basclab/Desktop/Section3_SimulatedData/01_P1PeakLatencyOutput_PreMerge/Miss1_10MicroV_PreschoolNoise/Sample0001-P1PeakLatencyOutput_PreMerge.txt')
  sampleID <- str_sub(peakLatencyFile,-37,-34) 
  # Create filename for final dataframe
  saveFilename = paste0(saveFolderCaseDeletionPct, '/Sample', sampleID, '-P1PeakLatencyOutput.csv')
  
  #-----------------------------------------------------------------------------
  # 1. IMPORT EACH SAMPLE'S PEAK LATENCY .TXT FILE 
  
  importCol <- c("worklat", "binlabel") # Specify columns for import
  dfOriginalRaw <- fread(peakLatencyFile, select=importCol)
  
  #-----------------------------------------------------------------------------
  # 2. EXTRACT SUBJECT ID AND STIMULI-RELATED INFORMATION FOR EACH ROW
  
  dfOriginalRaw$SUBJECTID <- substr(dfOriginalRaw$binlabel,1,2) # Extract subject ID (e.g., 01)
  dfOriginalRaw$eventBin <- substr(dfOriginalRaw$binlabel,6,13) # Extract bin label (e.g., trial-level bin: 60101; condition-level bin: 6____Pop)
  dfOriginalRaw$emotion <- as.factor('A') # Specify emotion condition ID (only one condition was simulated)

  # Create trial versus condition-level distinction based on eventBin labeling (i.e., trial-level bins contain 5 characters, condition-level bins contain 8)
  dfOriginalRaw$peakLatencyType <- ifelse(nchar(dfOriginalRaw$eventBin) == 5, "Trial", "Cond") 
  # Extract indices for each type of peak latency extraction
  trialIdx <- which(dfOriginalRaw$peakLatencyType == "Trial")
  condIdx <- which(dfOriginalRaw$peakLatencyType == "Cond")
  
  # Create placeholder columns that will be updated below
  dfOriginalRaw$ACTOR <- NA
  dfOriginalRaw$presentNumber <- NA
  dfOriginalRaw$caseDeletionPct <- NA
  
  # For trial-level bins only, add actor and trial presentation number information
  dfOriginalRaw$ACTOR[trialIdx] <- as.factor(substr(dfOriginalRaw$eventBin[trialIdx],3,3)) # Extract actor ID (e.g., 1)
  dfOriginalRaw$presentNumber[trialIdx] <- as.numeric(substr(dfOriginalRaw$eventBin[trialIdx],4,5)) # Extract trial presentation number (e.g, 01)
  
  # For condition-level bins only, add information on percentage of low trial-count subjects
  dfOriginalRaw$caseDeletionPct[condIdx] <- substr(dfOriginalRaw$eventBin[condIdx],6,9)
  
  # Create a peakLatencyP1 column for exporting peak latency values
  dfOriginalRaw$peakLatencyP1 <- gsub("\\[|\\]", "", dfOriginalRaw$worklat)
  dfOriginalRaw$peakLatencyP1[dfOriginalRaw$peakLatencyP1 == "NaN"] <- NA # Update missing value designation
  
  #-----------------------------------------------------------------------------
  # 3. MERGE EACH SAMPLE'S SUBJECT DATA LOG WITH THE DATAFRAME
  
  # Extract subject data log rows for this sample
  subjectDataLog_colForMerge <- c('SUBJECTID', 'oSpeed', 'age')
  subjectDataLog_oneSample <- subjectDataLog[subjectDataLog$sample == as.numeric(sampleID), ..subjectDataLog_colForMerge]
  
  # Merge dataframe with extracted subject data log rows
  dfOriginalRaw <- merge(dfOriginalRaw, subjectDataLog_oneSample, by = "SUBJECTID")
  
  #-----------------------------------------------------------------------------
  # 4. INDUCE TRIAL-LEVEL MISSINGNESS AND SAVE FINAL LONG DATAFRAME AS A .CSV FILE FOR EACH PERCENTAGE OF LOW TRIAL-COUNT SUBJECTS
  
  # Loop through each percentage of low trial-count subjects
  for (caseDeletionPct in na.omit(unique(dfOriginalRaw$caseDeletionPct))) {
    # Specify subfolder for this percentage
    saveFolderCaseDeletionPct <- paste0(saveFolder, '/', caseDeletionPct)
    
    # Check that this subfolder exists
    if (!(file.exists(saveFolderCaseDeletionPct))) {
      stop(paste0("This subfolder does not exist! ", saveFolderCaseDeletionPct))
    }
    
    # If this subfolder is for the population level (i.e., no missing trials or subjects),
    # export peak latency for all trial-level bins and the corresponding condition-level
    # bin
    if (caseDeletionPct == 'Pop') {
      subsetIdx <- which(dfOriginalRaw$peakLatencyType == "Trial" | dfOriginalRaw$caseDeletionPct == 'Pop')
      dfOriginalExport <- dfOriginalRaw[subsetIdx,]
    }
    
    # Otherwise this subfolder contains some amount of missing trials
    else {
      subjectArray <- unique(dfOriginalRaw$SUBJECTID) # Extract unique subjects for this sample
      dfOriginalExport_list <- vector("list", length(subjectArray)) # Preallocate empty list to store each subject's dataframe 
      
      # Loop through each subject 
      for (s in 1:length(subjectArray)) {
        # Extract this subject's rows from the sample's dataframe
        subject = subjectArray[s]
        dfOriginalRaw_subject <- dfOriginalRaw[dfOriginalRaw$SUBJECTID == subject,]
        
        # Extract the corresponding missTrials column for the specified subject
        # and low trial-count percentage
        missTrials_colName <- paste0('missTrials_', caseDeletionPct)
        missTrials <- subjectDataLog[subjectDataLog$sample == as.numeric(sampleID) & subjectDataLog$SUBJECTID == subject,..missTrials_colName]
        
        # Format missing trials for subsequent removal from dataframe
        missTrialsFmt <- str_split(missTrials, ';', simplify = TRUE)
        
        # Set peak latency of missing trials to NA
        dfOriginalRaw_subject$peakLatencyP1[dfOriginalRaw_subject$eventBin %in% missTrialsFmt] <- NA
        
        # Export peak latency for all trial-level bins and the corresponding 
        # condition-level bin and store in list
        subsetIdx <- which(dfOriginalRaw_subject$peakLatencyType == "Trial" | dfOriginalRaw_subject$caseDeletionPct == caseDeletionPct)
        dfOriginalExport_list[[s]] <- dfOriginalRaw_subject[subsetIdx,]
      }
      dfOriginalExport <- do.call(rbind, dfOriginalExport_list) # Bind data from all subjects into one dataframe
    }
    # Specify the columns that we want to save in the exported file
    columnOrder <- c("SUBJECTID","oSpeed","age","peakLatencyType","emotion","ACTOR","presentNumber","peakLatencyP1")
    dfOriginalExport <- dfOriginalExport[, ..columnOrder] 
    
    fwrite(dfOriginalExport, file = saveFilename, row.names = FALSE) # Save long dataframe in desired folder
  }
}